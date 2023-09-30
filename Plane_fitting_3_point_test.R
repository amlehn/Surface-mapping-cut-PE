#Plane fitting for the following:
#   3 points

library(tidyverse)
library(plotly)
library(broom)

#In this seciton, verify the plane fitting works for 3 points with known solution
# Points = (1,2,3), (4,6,9) and (12,11,9)
# Plane equation is 30x - 48y + 17z = -15
# The lm model will convert this into z = A + B*x + C*y
# coef(model) should be A = -15/17 (-0.88), B = -30/17 (-1.76), C = 48/17 (2.82)
x = c(1,4,12)
y = c(2,6,11)
z = c(3,9,9)

data3pts <- tibble(x = x, y = y, z = z)
glimpse(data3pts)

# Fit a model 
model <- lm(z ~ x + y, data = data3pts)
coefs <- coef(model)
print(coefs)

#Create a grid to predict over 
grid <- expand.grid(x = seq(min(data3pts$x), max(data3pts$x), length.out = 10),
                    y = seq(min(data3pts$y), max(data3pts$y), length.out = 10))

#Predict over the grid
grid$z <- predict(model, newdata = grid)

#Convert data and model grid to long format
data_long <- data3pts %>%
  pivot_longer(c(x,y,z), names_to = "variable", values_to = "value")

grid_long <- grid %>%
  pivot_longer(c(x,y,z), names_to = "variable", values_to = "value")

#Create the plot
p1 <- plot_ly() %>%
  add_trace(data = data3pts,
            x = ~x, y = ~y, z = ~z, 
            type = "scatter3d", 
            mode = "markers",  
            name = "3 data points", 
            color = I('blue')) %>%
  add_trace(data = grid, 
            x = ~x, y = ~y, z = ~z, 
            type = 'mesh3d', 
            name = 'Fit', 
            opacity = 0.5, 
            color = I('red')) %>%
  layout(scene = list(xaxis = list(title = 'X'), yaxis = list(title = 'Y'), zaxis = list(title = 'Z')))

p1


