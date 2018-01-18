#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

shinyUI(fluidPage(
        titlePanel("Predict potential Carat Weight from Price"),
        sidebarLayout(
                sidebarPanel(
                        sliderInput("sliderPrice", "Diamond Price", 326, 18823, value = 3933),
                        checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE),
                        checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE)
                ),
                
                mainPanel(
                        plotOutput("plot1"),
                        h3("Predicted Carat Weight from Model 1:"),
                        textOutput("pred1"),
                        h3("Predicted Carat Weight from Model 2:"),
                        textOutput("pred2")
                )
        )
))