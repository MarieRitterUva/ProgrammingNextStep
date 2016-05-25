library(shiny)
source("functions.R")


# server.R

shinyServer(function(input, output) {
       
        CalculateParameters(d, S.plus, q)
        
        InitializeVectors(C.init, S.plus, E.init, lamda.init, A.init, weeks, no.simulations)
        
        InitializeList()
        
        # actual simulation
        for (i in 1:no.simulations) {
                SimulateAddictionComponents(Crav, S, V, A, E, lamda, cues, weeks, b, d, p, S.plus, h, k, q)
                
                # get output
                BuildOutputDataframe(weeks, A, Crav, S, E, lamda, cues, V)
                
                # save output in list
                BuildOutputList()
                
                BuildVectorOutcome(i)
        }
        
        CalculateSuccess()
        
        
        output$time.plot <- renderPlot({
                MakeGraphs(V.plot = TRUE, successfull = FALSE)
        })
        
        
}
        
)