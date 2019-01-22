#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(RCurl)
library(dplyr)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  weatherHistory <- read.csv("C:/Users/jreyes3/Documents/DevelopingDataProductsExercise/weatherHistory.csv")  
  # Rename columms to be used for readability
  weatherHistory <- weatherHistory %>% rename(date = Formatted.Date, apparentTemperature = Apparent.Temperature..C., temperature = Temperature..C. )
  model1 <- lm(weatherHistory$temperature~ weatherHistory$Humidity, data = weatherHistory)
  model2 <- lm(weatherHistory$apparentTemperature~ weatherHistory$Humidity, data = weatherHistory)
  
 model1pred<- reactive({
    disInput <- input$sliderdis
    predict(model1, newdata = data.frame(Humidity = disInput))
  })
  
  model2pred <- reactive({
    disInput <- input$sliderdis
    predict(model2, newdata = data.frame(Humidity = disInput))
  })
  output$plot1 <- renderPlot({
    disInput <- input$sliderdis
    
    #plot(weatherHistory$Humidity, y = ifelse(input$showModel1,weatherHistory$apparentTemperature, weatherHistory$temperature) , xlab="Humidity", ylab = "Temperature",
     #    bty="n", pch = 16)
    if(input$radio == 1)
    {
      plot(weatherHistory$Humidity, weatherHistory$apparentTemperature,bty="n", pch = 16, col = "blue")
    }
    else
    {
      plot(weatherHistory$Humidity, weatherHistory$temperature,bty="n", pch = 16, col = "green")
    }
    
    
  })
  
})
