#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
        diamonds$price2 <- ifelse(diamonds$price - 3933 > 0, diamonds$price - 3933, 0)
        model1 <- lm(carat~price, data = diamonds)
        model2 <- lm(carat~price2 + price, data = diamonds)
        model1pred <- reactive({
                priceInput <- input$sliderPrice
                predict(model1, newdata = data.frame(price=priceInput))
        })
        model2pred <- reactive({
                priceInput <- input$sliderPrice
                predict(model2, newdata =
                                data.frame(price = priceInput,
                                           price2 = ifelse(priceInput - 3933 > 0,
                                                           priceInput - 3933, 0)))
        })
                output$plot1 <- renderPlot({
                       priceInput <- input$sliderPrice
                        plot(diamonds$price, diamonds$carat, xlab = "US Dollars",
                             ylab = "Carat Weight", bty = "n", pch = 16,
                             xlim = c(325,18823), ylim = c(0, 5))
                        if(input$showModel1){
                                abline(model1, col = "red", lwd = 2)
                        }
                        if(input$showModel2){
                                model2lines <- predict(model2, newdata = data.frame(
                                        price = 326:18823, price2 = ifelse(326:18823 - 3933 > 0, 326:18823 - 3933, 0)
                                ))
                                lines(326:18823, model2lines, col = "blue", lwd = 2)
                        }
                        legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16,
                               col = c("red", "blue"), bty = "n", cex = 1.2)
                        points(priceInput, model1pred(), col = "red", pch = 16, cex = 2)
                        points(priceInput, model2pred(), col = "blue", pch = 16, cex = 2)
                })
                output$pred1 <- renderText({
                        model1pred()
                })
                output$pred2 <- renderText({
                        model2pred()
                })
        })