#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(mlbench)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Prediction of House Value from Distance from Employment Centers"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("sliderdis",
                   "What is the distance of the house from the employment centers?",
                   min = 1,
                   max = 16,
                   value = 5),
       checkboxInput("showModel1", "Show/Hide Model 1", value =TRUE),
       checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Readme", br(), 
                           h2("Prediction Algorithm for Boston Housing Data"),
                           p("This data represents the Housing data for 506 census tracts of Boston from the 1970 census. The algorithm makes use of the following variables:"),
                           p("dis - weighted mean of distances to five Boston employment centres"),
                           p("cmedv - median value of owner-occupied homes in $1000s."),
                           br(),
                           h3("Usage"),
                           p("- Drag the slider to a specific distance value to display the predicted price of the house"),
                           p("- Model 1 represents the fitted linear regression of the housing value based on the distance"),
                           p("- Model 2 represents the fitted linear regression of the housing value based on the distance and a break at point 3"),
                           br(),
                           h4("Source"),
                           p("Harrison, D. and Rubinfeld, D.L. (1978) Hedonic prices and the demand for clean air. J. Environ. Economics and Management 5, 81-102."),
                           p("Belsley D.A., Kuh, E. and Welsch, R.E. (1980) Regression Diagnostics. Identifying Influential Data and Sources of Collinearity. New York: Wiley.")
                           ),
                  tabPanel("Plot", br(), plotOutput("plot1"),
                           h3("Predicted Housing Value from Model 1:"),
                           textOutput("pred1"),
                           h3("Predicted Housing Value from Model 2:"),
                           textOutput("pred2")))
       
    )
  )
))
