library(shiny)
source("functions.R")


# server.R

shinyServer(function(input, output) {
        
        calc.paras <- reactive({ CalculateParameters(input$d, input$S.plus, input$q) })
        
        
        initd.vectors <- reactive({ InitializeVectors(input$C.init, input$S.plus, input$E.init, input$lamda.init,
                                                      input$A.init, input$weeks, input$no.simulations, input$d)
        })
        
        list.output <- InitializeList()
        
        
        
        
        
        output$test <- renderPrint({ calc.paras() })
        
        # return(list(list.output = list.output, addiction.end = addiction.end))
        
        
        
        #        reactive({ CalculateSuccess() })
        #        
        #        
        #        output$time.plot <- renderPlot({
        #                MakeGraphs(input$graph.type, input$graph.success)
        #        })
        #        
        #        output$success.rate <- renderText({
        #                paste("The patient was NOT addicted in ", success.percent, "% of simulations.")
        #        })
        
        }
        
)