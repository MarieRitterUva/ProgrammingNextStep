library(shiny)
source("functions.R")


# server.R

shinyServer(function(input, output, session) {
        
        
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
        
        observeEvent(input$reset, {
                updateNumericInput(session, "q", value = 0.8)
                updateSliderInput(session, "E.init", value = 0)
                updateSliderInput(session, "S.plus", value = 0.5)
                updateSliderInput(session, "d", value = 0.2)
                updateSliderInput(session, "C.init", value = 0)
                updateSliderInput(session, "A.init", value = 0)
                updateNumericInput(session, "lamda.init", value = 0)
                updateNumericInput(session, "weeks", value = 25)
                updateNumericInput(session, "no.simulations", value = 20)
        })
        
        
        ####################################################################################
        
        # bifurcation diagram code
        
        observeEvent(input$go.bifurc, {
                
                
                output$bifurcation <- renderPlot({
                        withProgress(message = "Please wait. We are making the plot.", {
                                
                                MakeBifurcationDiagram(input$bifurc, input$Y, input$S.plus, input$q, input$d,
                                                       input$C.init, input$E.init, input$lamda.init, input$A.init)
                                
                                incProgress(0.5, detail = "Please wait. We are making the plot.")
                        })
                        
                })
                
        })
        
        
        
        
}

)