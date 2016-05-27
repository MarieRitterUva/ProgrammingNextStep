library(shiny)
source("functions.R")


# server.R

test.success <- function(input, success.list) {
        if (input == TRUE) {
                if (length(success.list[[2]]) != 0) {
                        NULL
                } else {
                        "There are no successful runs to display!"
                }
        } else if ( input == FALSE) {
                if (length(success.list[[3]]) != 0) {
                        NULL
                } else {
                        "There are no unsuccessful runs to display!"
                }
        }
}

shinyServer(function(input, output, session) {
        
        
        parameters <- reactive({
                
                validate(
                        need(input$q != "", "Please enter a value for ``q´´!"),
                        need(input$q > 0, "Please enter a positive value for ``q´´!")
                )
                
                CalculateParameters(input$d, input$S.plus, input$q)
        })
        
        
        vectors <- reactive({
                
                validate(
                        need(input$lamda.init != "", "Please enter a value for ``initial lamda´´!"),
                        need(input$weeks != "", "Please enter a value for ``weeks´´!"),
                        need(input$no.simulations != "", "Please enter a number for ``No. of simulations´´!"),
                        need(input$weeks > 0, "Please enter a positive value for ``weeks´´!"),
                        need(input$weeks < 1001, "Please enter a smaller value for ``weeks´´!"),
                        need(input$no.simulations > 0, "Please enter a positive value for ``No. of simulations´´!"),
                        need(input$no.simulations < 3001, "Please enter a smaller value for ``No. of simulations´´!")
                )
                
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
                
                
                validate(
                        test.success(input$graph.success, success.list())
                )
                
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
        
        ####################################################################################
        
        # download page
        
        output$downloadData <- downloadHandler(
                filename = "data.zip",
                content = function(fname) {
                        tmpdir <- tempdir()
                        setwd(tempdir())
                        print(tempdir())
                        
                        fs <- paste("data", 1:input$no.simulations, ".csv", sep = "")
                        for (i in 1:input$no.simulations) {
                                thisi <- i
                                write.csv(output.addiction()[[thisi]][[1]], file = paste("data", thisi, ".csv", sep = ""))
                        }
                        print (fs)
                        
                        zip(zipfile=fname, files=fs)
                        if(file.exists(paste0(fname, ".zip"))) {
                                file.rename(paste0(fname, ".zip"), fname)
                        }
                },
                contentType = "application/zip"
        )
        
        output$downloadPlot <- downloadHandler(
                filename = function() { paste("plot_", input$graph.type,".png", sep = "") },
                content = function(file) {
                        png(file, width = 650)
                        print(MakeGraphs(input$graph.type, input$graph.success, output.addiction(), success.list(), input$q, input$S.plus))
                        dev.off()
                })
        
        
}

)