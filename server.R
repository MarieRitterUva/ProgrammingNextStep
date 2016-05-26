library(shiny)
source("functions.R")


# server.R

shinyServer(function(input, output) {

        
        parameters <- reactive({
                CalculateParameters(input$d, input$S.plus, input$q)
        })
        
        
        vectors <- reactive({
                InitializeVectors(input$C.init, input$S.plus, input$E.init, input$lamda.init,
                                  input$A.init, input$weeks, input$no.simulations, input$d)
        })
        
        list.output <- InitializeList()
        
        output.addiction <- reactive({
                SimulateMultiple(input$no.simulations, vectors(), parameters(), input$S.plus, input$q,
                                 input$weeks, input$d, list.output)  
        }) 
        
        success.list <- reactive({
                CalculateSuccess(output.addiction(), input$no.simulations)
        })
        
        
        
        output$time.plot <- renderPlot({
                MakeGraphs(input$graph.type, input$graph.success, output.addiction(), success.list(), input$q, input$S.plus)
        })
        
        output$success_rate <- renderText({
                paste("The patient was NOT addicted at the end ", success.list()$success.percent, "% of the simulations.")
        })
        
        
        # Tests
        # output$test <- renderTable({ output.addiction()[[50]][[1]] })
        # output$test <- renderPrint({ success.list() })
        
}

)