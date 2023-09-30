library(pacman)
p_load(shiny, tidyverse, reshape2, plotly, viridis)

##This app processes raw profilometer data into depth mapping
#The user uploads the raw file form Surface mapping cut PE/Data
#An uncut region is specified to fit a reference plane
#The difference between the reference plane and measurement is depth
#Outputs CSV containing x,y,depth values in Processed/MapData
#Outputs CSV containing details of how data was processed in Processed/MetaData


# Define the UI
ui <- fluidPage(titlePanel("Depth map from profiler data"),
                sidebarLayout(
                  sidebarPanel(
                    h3("Upload (X, Y, Z) data"),
                    helpText('ASCII table, first 6 rows skipped in upload'),
                    fileInput("fileInput", "select .ASC file:"),
                    
                    
                    h3("Reference planar region"),
                    sliderInput(
                      "Xrange",
                      "x range [mm]",
                      min = 0,
                      max = 1,
                      value = c(0, 1)
                    ),
                    sliderInput(
                      "Yrange",
                      "y range [mm]",
                      min = 0,
                      max = 1,
                      value = c(0, 1)
                    ),
                    
                    h3("Calculate & plot depth"),
                    actionButton("calculate", "Calculate & plot"),
                    
                    h3("Export depth map as CSV"),
                    downloadButton("saveMap", "Save map data"),
                    downloadButton("saveMetaData", 'Save meta data'),
                    helpText("MetaData saves plane fit parameters"),
                  ),
                  
                  mainPanel(
                    splitLayout(
                      cellWidths = c("50%", "50%"),
                      plotlyOutput("plot3d"),
                      plotlyOutput("plotDepth")
                    ),
                    
                    #Contour plots
                    splitLayout(
                      cellWidths = c("25%", "75%"),
                      tableOutput("mapDataTable"),
                      plotOutput("mapContours")
                    ),
                  )
                ))



# Define the server logic
server <- function(input, output, session) {
  
  # Initialize reactive values
  data <- reactiveVal(NULL) #X [mm], Y [mm], Z [nm] raw data triplets
  fileName <- reactiveVal(NULL) #base file name of input file
  numScans <- reactiveVal(NULL) #number of scans (along y) from input 
  ptsPerScan <- reactiveVal(NULL) #data points per scan 
  wideDataMatrix <- reactiveVal(NULL) #format required for plotly 
  dataFiltered <- reactiveVal(NULL) #XYZ triplets for reference plane fit
  modelCoefs <- reactiveVal(NULL) #A + B*X + C*Y plane coefficients
  depthMap <- reactiveVal(NULL) #x [mm], y [mm], d [um] 
  processData <- reactiveVal(NULL) #meta data to re-create map file
  
  
  
  ## USER HAS UPLOADED ASC FILE
  #  function to upload XYZ data from .ASC file
  readData <- function () {
    req(input$fileInput)
    rawData <-
      read_table(input$fileInput$datapath,
                 skip = 6,
                 col_names = FALSE, 
                 col_types = cols())
    colnames(rawData) <- c("X", "Y", "Z")
    data(rawData)
    
    # extract filename for documentation and ease of naming
    filename <- str_remove(input$fileInput$name, "\\.ASC$")
    fileName(filename)
    
    # number of scans in map file
    numScans <- rawData %>%
      select("X") %>%
      distinct() %>%
      count() %>%
      as.numeric()
    numScans(numScans)
    
    # number of points per scan
    ptsPerScan <- rawData %>%
      select("Y") %>%
      distinct() %>%
      count() %>%
      as.numeric()
    ptsPerScan(ptsPerScan)
  }
  
  # update slider values with physical X,Y bounds and increments
  observeEvent(input$fileInput, {
    readData() 
    req(data())
    
    #X values
    dfX <- data() %>%
      select("X") %>%
      distinct()
    
    dx <- dfX$X[2] - dfX$X[1]
    minX <- min(dfX$X)
    maxX <- max(dfX$X)
    
    #Y values
    dfY <- data() %>%
      select("Y") %>%
      distinct()
    
    dy <- dfY$Y[2] - dfY$Y[1]
    minY <- min(dfY$Y)
    maxY <- max(dfY$Y)
    
    # update sliders
    updateSliderInput(
      session,
      "Xrange",
      min = minX,
      max = maxX,
      value = c(minX, maxX),
      step = dx
    )
    updateSliderInput(
      session,
      "Yrange",
      min = minY,
      max = maxY,
      value = c(minY, maxY),
      step = dy
    )
  })
  
  
  #Generate wideDataMatrix for 3D visualization with plotly 
  #acast(Y ~ X, value.var = "Z") generates a matrix where:
    #X values become column names
    #Y values become row names
    #Z fills cells
  widenData <- function() {
    req(input$fileInput)
    data <- data()
    wideDataMatrix(acast(data, Y ~ X, value.var = "Z"))
  }
  
  
  #Plot raw data in interactive surface plot
  output$plot3d <- renderPlotly({
    req(data())
    widenData()
    req(wideDataMatrix())
    z_vals <- wideDataMatrix()
    
    plot_ly() %>%
      add_surface(x = colnames(z_vals),
                  y = rownames(z_vals),
                  z = z_vals / 1000, #convert nm to um
                  contours = list(
                    z = list(
                    show=TRUE,
                    usecolormap=TRUE,
                    highlightcolor="#ff0000",
                    project=list(z=TRUE)
                              )
                        )
                  )%>%
  
      layout(scene = list(
        xaxis = list(title = 'X [mm]'),
        yaxis = list(title = 'Y [mm]'),
        zaxis = list(title = 'Z [um]'),
        camera = list(eye = list(
          x = -1.7, y = -1.7, z = 0.75
        ))
      ),
      title = list(text = 'Original scan')
      )
  })
  
  
  ## USER HAS PRESSED 'CALCULATE & PLOT' BUTTON
  # Generate linear fit to reference plane specified by sliders
  observeEvent(input$calculate, {
    req(data())
    filtered <- data()
    
    # filter on floating point numbers using dplyr
    # group by X and filter Y values in each scan using between()
    filtered <- filtered %>%
      group_by(X) %>%
      filter(between(Y, input$Yrange[1], input$Yrange[2])) %>%
      ungroup()
    
    # keep only X values that correspond to slider values
    filtered <- filtered %>%
      group_by(Y) %>%
      filter(between(X, input$Xrange[1], input$Xrange[2])) %>%
      ungroup()
    dataFiltered(filtered)
    
    model <- lm(Z ~ X + Y, data = filtered)
    modelCoefs(coef(model))
    
    # generate reference plane values for all X,Y to estimate depth
    # use pull() to extract vectors X and Y from data tibble
    grid <-
      expand_grid(
        X = data() %>% select(X) %>% distinct() %>% pull("X"),
        Y = data() %>% select(Y) %>% distinct() %>% pull("Y")
      )
    grid$Z_ref <- predict(model, newdata = grid)
    
    # perform inner join to add Z_ref to original data
    map <- inner_join(data(), grid, by = c("X", "Y"))
    
    # create depth map of X, Y, Depth
    map <- map %>%
      mutate(Depth = (Z_ref - Z) / 1000) %>% #[um], Depth is (+)
             select(c("X", "Y", "Depth"))
             depthMap(map)
  })
  

    # Plot depth in interactive surface plot
    output$plotDepth <- renderPlotly({
      req(depthMap())
      
      depth <- depthMap() %>%
        acast(Y ~ X, value.var = "Depth")
      zVals <- as.matrix(depth)
      
      plot_ly() %>%
        add_surface(x = colnames(zVals),
                    y = rownames(zVals),
                    z = -1*zVals,
                    contours = list(
                      z = list(
                        show=TRUE,
                        usecolormap=TRUE,
                        highlightcolor="#ff0000",
                        project=list(z=TRUE)
                      )
                    )
        ) %>%
        layout(
          scene = list(
            xaxis = list(title = 'X [mm]'),
            yaxis = list(title = 'Y [mm]'),
            zaxis = list(title = 'Depth [um]'),
            camera = list(eye = list(
              x = -1.7, y = -1.7, z = 0.75
            ))
          ),
          title = list(text = 'Calculated depth')
        )
    })
    
    
    #Plot depth contours for each scan 
    output$mapContours <- renderPlot({
    req(depthMap())
    df <- depthMap() %>%
      group_by(X) %>%
      mutate(Y_offset = Y + 0.1 * match(X, unique(X))) %>%
      filter(between(Depth, 0, max(Depth)))
    
    #show only 10 colors in legend for simplicity
    levelsX <- unique(df$X)
    inds <- round(seq(from = 1, to = length(levelsX), length.out = 10))
    print(inds)
    breaksX <- levelsX[inds]
    print(breaksX)
    
    p <- ggplot(df, aes(x = Y_offset, y = -1*Depth, color = as.factor(X)))+
      geom_line(linewidth = 0.75) +
    
      scale_color_viridis_d(option = "cividis", 
                            direction = -1, 
                            breaks = breaksX, 
                            labels = as.character(breaksX))+
      theme_bw() +
      theme(plot.title = element_text(size = 18),
            axis.title = element_text(size = 16),
            legend.text = element_text(size = 12),
            axis.text = element_text(size = 10)) +
      labs(title = "Offset Contours of Y vs Depth",
           y = "Depth [um]", x = "Y [mm]") +
      guides(color = guide_legend(title="X [mm]")) 
    
    print(p)
    })
    
    
    #Calculate meta data to save - enables recreation of depth map
    observeEvent(input$calculate, {
      fitRange <- dataFiltered() %>%
        summarize(
          "xMin" = min(X),
          "xMax" = max(X),
          "yMin" = min(Y),
          'yMax' = max(Y)
        )
      processData <- tibble(
        numScans = round(numScans()),
        ptsPerScan = round(ptsPerScan()),
        referenceRegion = fitRange,
        modelCoefs = modelCoefs()
      )
      processData(processData)
    })
    
    #Render tables showing depth map and process data
    output$mapDataTable <- renderTable({
      req(depthMap())
      head(depthMap() %>% group_by(X), 10)
    }, rownames = FALSE)
    
    output$metaDataTable <- renderTable({
      req(processData())
      processData()
    }, rownames = FALSE)
    
    #When saveMap download button is pressed save map
    #User navigates to folder and saves there
    #Save in MapData folder
    output$saveMap <- downloadHandler(
      filename = function() {
        paste0(fileName(), "_Processed.csv")
      },
      content = function(file) {
        data <- depthMap() %>%
          rename("X_mm" = "X",
                 "Y_mm" = "Y",
                 "depth_um" = "depth")
        write.csv(data, file)
        #show notification message
        shiny::showNotification("Map data saved successfully!",
                                type = "message")
      }
    )
    #Save in MetaData folder
    output$saveMetaData <- downloadHandler(
      filename = function() {
        paste0(fileName(), "_MetaData.csv")
      },
      content = function(file) {
        write.csv(processData(), file, row.names = FALSE)
        #show confirmation message
        shiny::showNotification("Meta data saved successfully!",
                                type = "message")
      }
    )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
