#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
BostonHousing2$dissp <- ifelse(BostonHousing2$dis - 3 > 0, BostonHousing2$dis - 3, 0)
  model1 <- lm(cmedv~ dis, data = BostonHousing2)
  model2 <- lm(cmedv~ dissp + dis, data = BostonHousing2)
  
  model1pred<- reactive({
    disInput <- input$sliderdis
    predict(model1, newdata = data.frame(dis = disInput))
  })
  
  model2pred <- reactive({
    disInput <- input$sliderdis
    predict(model2, newdata = data.frame(dis = disInput,
                                         dissp = ifelse(disInput - 3 > 0,
                                                        disInput - 3,0)))
  })
  output$plot1 <- renderPlot({
    disInput <- input$sliderdis
    
    plot(BostonHousing2$dis, BostonHousing2$cmedv, xlab="Distance from Business Center", ylab = "Housing Value",
         bty="n", pch = 16, xlim=c(1, 15), ylim = c(1,60))
    if(input$showModel1){
      abline(model1, col = "red", lwd = 2)
    }
    if(input$showModel2){
      model2lines <- predict(model2, newdata = data.frame(
        dis = 1:15, dissp = ifelse(1:15 - 3 > 0, 1:15 -3, 0) 
          ))
  lines(1:15, model2lines, col = "blue", lwd = 2)
    }
    legend(10, 50, c("Model 1 Prediction", "Model 2 Prediction"), pch =16,col = c("red", "blue"), bty = "n", cex = 1.2)
    points(disInput, model1pred(), col = "red", pch =16, cex = 2)
    points(disInput, model2pred(), col = "blue", pch = 16, cex = 2)
  })
  output$pred1 <- renderText({
    model1pred()
  })
  
  output$pred2 <- renderText({
    model2pred()
  })
})
