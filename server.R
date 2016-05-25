library(shiny)
source("functions.R")


# server.R

shinyServer(function(input, output) {
        
        calc.paras <- reactive({ CalculateParameters(input$d, input$S.plus, input$q) })
        
        
        initd.vectors <- reactive({ InitializeVectors(input$C.init, input$S.plus, input$E.init, input$lamda.init,
                                                      input$A.init, input$weeks, input$no.simulations, input$d)
        })
        
        list.output <- InitializeList()
        
        
        loop <- reactive ({
                
                for (i in input$no.simulations) {
                        
                        # actual simulation
                        results.simulation1 <- reactive({
                                simlt.vectors <- SimulateAddictionComponents(initd.vectors()[[1]], initd.vectors()[[2]], initd.vectors()[[3]],
                                                                             initd.vectors()[[4]], initd.vectors()[[5]], initd.vectors()[[6]],
                                                                             initd.vectors()[[7]], calc.paras()[1], calc.paras()[2], calc.paras()[3],
                                                                             calc.paras()[4], input$d, input$S.plus, input$q, input$weeks)
                        })
                        
                        
                        results.simulation2 <- reactive({
                                # get output (weeks, cues, A, Crav, S, V, E, lamda)
                                df.output <- BuildOutputDataframe(input$weeks, results.simulation1()$cues, results.simulation1()$A,
                                                                  results.simulation1()$Crav, results.simulation1()$S, results.simulation1()$V,
                                                                  initd.vectors()[[3]], initd.vectors()[[4]])
                        })
                        
                        results.simulation3 <<- reactive({
                                list.output <- BuildOutputList(results.simulation2(), results.simulation1()$addiction, list.output())
                        })
                        
                        addiction.end <- reactive({
                                addiction.end <- BuildVectorOutcome(i)
                        })
                        
                }
                
        })
        
        
        output$test <- renderPrint({ loop() })
        
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