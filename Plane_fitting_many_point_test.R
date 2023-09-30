#Plane fitting for many points

library(tidyverse)
library(plotly)
library(broom)

#Specify 
x = c(1,4,12,5,10,2,3,4)
y = c(2,6,11,14,19,20,14,12)
z = c(3,9,9,4,12,15,16,10)

data <- tibble(x = x, y = y, z = z)
glimpse(data)

# Fit a model 
model <- lm(z ~ x + y, data = data)
coefs <- coef(model)
print(coefs)

#Create a grid to predict over 
grid <- expand.grid(x = seq(min(data$x), max(data$x), length.out = 10),
                    y = seq(min(data$y), max(data$y), length.out = 10))

#Predict over the grid
grid$z <- predict(model, newdata = grid)

#Convert data and model grid to long format
#one column with the variable, another with the value
#generates (number of points) x (dimensionality of point) (e.g. 8 3D -> 24 rows)
data_long <- data %>%
  pivot_longer(c(x,y,z), names_to = "variable", values_to = "value")

grid_long <- grid %>%
  pivot_longer(c(x,y,z), names_to = "variable", values_to = "value")

#Create the plot
p1 <- plot_ly() %>%
  add_trace(data = data,
            x = ~x, y = ~y, z = ~z, 
            type = "scatter3d", 
            mode = "markers",  
            color = I('blue')) %>%
  add_trace(data = grid, 
            x = ~x, y = ~y, z = ~z, 
            type = 'mesh3d', 
            name = 'Fit', 
            opacity = 0.5, 
            color = I('red')) %>%
  layout(scene = list(xaxis = list(title = 'X'), yaxis = list(title = 'Y'), zaxis = list(title = 'Z')))

p1
