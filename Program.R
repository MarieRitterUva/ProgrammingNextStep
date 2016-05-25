## Programming: The next step - Simulating Addiction with Difference Equations
## Marie Ritter - UvA

# ToDo: Bifurcation diagrams

# ToDo: Treatment - new display logic
  # Treatment Weeks (if statements)
  # Success output

# ToDo: Write input ifs, error handling, error messages
# ToDO: Simple GUI input and output
# ToDo: Display Logic GUI
# ToDo: Speed of Simulation
# ToDo: Beautify GUI
# ToDo: Saving graphs and output
# ToDo: Manual and Instructions
# ToDo: Make code citable, check comments, etc.
# ToDo: Update Git-Readme
# ToDo: Host on Server (?) ask Raoul

#############################################################################

rm(list = ls())
setwd("/home/marie/Dokumente/Uni/Courses/Programming/Repo/ProgrammingNextStep")

#############################################################################

# input -> later from GUI

q <- 0.8  # maximum consumption

E.init <- 1  # external influences (-1, 1)

S.plus <- 0.5  # maximum self-control (0, 1)

d <- 0.2  # decay of craving (0,1)

C.init <- 0  # craving at time 0

A.init <- 0.5 * q  # consumption at time 0 (0, 1), actually factor is parameter, add calculate function later

weeks <- 100  # number of weeks

lamda.init <- 0.5  # intensity of external influences

no.simulations <- 100  # how many times simulation should be run

#############################################################################
# Functions

# calculate parameters
# - needs to be done in beginning and if input parameters change

CalculateParameters <- function (d = 0.2, S.plus = 0.5, q = 0.8) {
        p <<- 2*d  # resilience parameter
        k <<- (p * S.plus) / q
        h <<- p * S.plus
        b <<- (2*d) / q
}


# initialize vectors
# - needs to be done in beginning and if input parameters change

InitializeVectors <- 
        function (C.init = 0, S.plus = 0.5, E.init = 0, lamda.init = 0.5, A.init = 0.4,
                  weeks = 20, no.simulations = 100) {
        
        Crav <<- numeric(length = weeks+1)
        Crav[1] <- C.init
        
        S <<- numeric(length = weeks+1)
        S[1] <<- S.plus
        
        E <<-  numeric(length = weeks+1)
        E[1] <<- E.init
        for (i in 1:weeks) {
                E[i+1] <<- round((E[i] - d * E[1]), 2)
        }
        
        lamda <<- numeric(length = weeks+1)
        lamda[1] <<- lamda.init
        for (i in 1:weeks) {
                lamda[i+1] <<- lamda[i] + d * lamda[1]
        }
        
        cues <<- numeric(length = weeks+1)
        
        V <<- numeric(length = weeks+1)
        V[1] <<- min(c(1, max(c(0, (S.plus - Crav[1] - E[1]) )) ))
        
        A <<- numeric(length = weeks+1)
        A[1] <<- A.init
        
        addiction.end <<- logical(no.simulations)
}


# Simulate C, S, V, and A

SimulateAddictionComponents <- function (Crav, S, V, A, E, lamda, cues, weeks, b, d, p, S.plus, h, k, q) {
        
        for (i in 1:weeks) {
                # calculate C(t+1)
                Crav[i+1] <- Crav[i] + b * min(c(1, 1-Crav[i])) * A[i] - d * Crav[i]
                
                # calculate S(t+1)
                S[i+1] <- S[i] + p * max(c(0, (S.plus - S[i])) ) - h * Crav[i] - k * A[i]
                
                # calculate V(t+1)
                V[i+1] <- min(c(1, max(c(0, Crav[i] - S[i] - E[i]))))
                
                # calculate A(t+1)
                # generate random component (cues)
                # set.seed(1)  # for testing purposes - TAKE OUT AT THE END!!!
                cues[i] <- rpois(1, lamda[i])
                f <- cues[i] * (q/7)
                
                # calculate A(t+1)
                if (f <= (q * (1 - V[i])) ) {  # stay under max
                        
                        A[i+1] <- q * V[i] + f
                        
                } else {
                        A[i+1] <- q
                }
        }
        
        if (V[weeks+1] != 1) {
                addiction <- FALSE
        } else {
                addiction <- TRUE
        }
        
        cues <<- cues
        A <<- A
        Crav <<- Crav
        S <<- S
        V <<- V
        addiction <<- addiction
        
}

# Initialize list for datastorage

InitializeList <- function () {
        list.output <<- list()
}

# build output dataframe

BuildOutputDataframe <- function (weeks, A, Crav, S, E, lamda, cues, V) {
        output <- data.frame("t" = (1: (weeks+1))-1,  "A" = A, "C" = Crav, "S" = S,
                             "E" = E, "lamda" = lamda, "cues" = cues, "V" = V)
        df.output <<- output
}

BuildOutputList <- function () {
        output.success <- list(df.output, addiction)  # includes success data
        output.addition <- c(list.output, list(output.success))  # adds the new data to the list
        list.output <<- output.addition
}

BuildVectorOutcome <- function (loop) {
        addiction.end[loop] <- addiction
        addiction.end <<- addiction.end
}

CalculateSuccess <- function () {
        success.percent <- (max(0, (sum(addiction.end == FALSE) - 1)) / weeks) * 100
        trials.success <- ((which(addiction.end == FALSE))[-1] - 1)  # discard first number for week 0
                                                                 # substract one to get actual week number
        trials.fail <- ((which(addiction.end == TRUE))[-1] - 1)
        
        success.percent <<- success.percent
        trials.success <<- trials.success
        trials.fail <<- trials.fail
        
}


# GRAPHS

# Graphs over time
MakeGraphs <- function (graph.type,
                        successfull = TRUE) {
        # SINGLE PLOTS
        # addictive acts
        if (graph.type == 1 & successfull == TRUE) {
        plot(list.output[[ trials.success[1] ]][[1]]$t, 100*(list.output[[ trials.success[1] ]][[1]]$A),
             bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
             type = "l", ylab = "A(t) in alcoholic beverages", ylim = c(0, 100*q),
             main = "Frequency of addictive acts A(t) over time")
        } else if (graph.type == 1 & successfull == FALSE) {
                plot(list.output[[ trials.fail[1] ]][[1]]$t, 100*(list.output[[ trials.fail[1] ]][[1]]$A),
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "A(t) in alcoholic beverages", ylim = c(0, 100*q),
                     main = "Frequency of addictive acts A(t) over time")
        } 
        
        # self-control
        if (graph.type == 3 & successfull == TRUE) {
        plot(list.output[[ trials.success[1] ]][[1]]$t, list.output[[ trials.success[1] ]][[1]]$S,
             bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
             type = "l", ylab = "S(t)", ylim = c(0,S.plus),
             main = "Self-control over time")
        } else if (graph.type == 3 & successfull == FALSE) {
                plot(list.output[[ trials.fail[1] ]][[1]]$t, list.output[[ trials.fail[1] ]][[1]]$S,
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "S(t)", ylim = c(0,S.plus),
                     main = "Self-control over time")
        }
        
        # craving
        if (graph.type == 2 & successfull == TRUE) {
        plot(list.output[[ trials.success[1] ]][[1]]$t, list.output[[ trials.success[1] ]][[1]]$C,
             bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
             type = "l", ylab = "C(t)",
             main = "Craving C(t) over time")
        } else if (graph.type == 2 & successfull == FALSE) {
                plot(list.output[[ trials.fail[1] ]][[1]]$t, list.output[[ trials.fail[1] ]][[1]]$C,
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "C(t)",
                     main = "Craving C(t) over time")
        }
        
        # vulnerability
        if (graph.type == 4 & successfull == TRUE) {
        plot(list.output[[ trials.success[1] ]][[1]]$t, list.output[[ trials.success[1] ]][[1]]$V,
             bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
             type = "l", ylab = "V(t)", ylim = c(0,1),
             main = "Vulnerability V(t) over time")
        } else if (graph.type == 4 & successfull == FALSE) {
                plot(list.output[[ trials.fail[1] ]][[1]]$t, list.output[[ trials.fail[1] ]][[1]]$V,
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "V(t)", ylim = c(0,1),
                     main = "Vulnerability V(t) over time")
        }
        
        # DOUBLE PLOTS
        # S and V
        if (graph.type == 6 & successfull == TRUE) {
                plot(list.output[[ trials.success[1] ]][[1]]$t, list.output[[ trials.success[1] ]][[1]]$V,
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "V(t) and S(t)",
                     main = "Vulnerability V(t) and Self-Control S(t) over time")
                lines(list.output[[ trials.success[1] ]][[1]]$t, list.output[[ trials.success[1] ]][[1]]$S,
                      lty = 2, lwd = 2)
                legend("bottomright", legend = c("S(t)", "V(t"), lty = c(2, 1), lwd = 2)
        } else if (graph.type == 6 & successfull == FALSE) {
                plot(list.output[[ trials.fail[1] ]][[1]]$t, list.output[[ trials.fail[1] ]][[1]]$V,
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "V(t) and S(t)",
                     main = "Vulnerability V(t) and Self-Control S(t) over time")
                lines(list.output[[ trials.fail[1] ]][[1]]$t, list.output[[ trials.fail[1] ]][[1]]$S,
                      lty = 2, lwd = 2)
                legend("bottomright", legend = c("S(t)", "V(t"), lty = c(2, 1), lwd = 2)
        }
        
        # A and C
        if (graph.type == 5 & successfull == TRUE) {
                plot(list.output[[ trials.success[1] ]][[1]]$t, list.output[[ trials.success[1] ]][[1]]$A,
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "A(t) and C(t)",
                     main = "Addictive acts A(t) and craving C(t) over time")
                lines(list.output[[ trials.success[1] ]][[1]]$t, list.output[[ trials.success[1] ]][[1]]$C,
                      lty = 2, lwd = 2)
                legend("bottomright", legend = c("C(t)", "A(t"), lty = c(2, 1), lwd = 2)
        } else if (graph.type == 5 & successfull == FALSE) {
                plot(list.output[[ trials.fail[1] ]][[1]]$t, list.output[[ trials.fail[1] ]][[1]]$A,
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "A(t) and C(t)",
                     main = "Addictive acts A(t) and craving C(t) over time")
                lines(list.output[[ trials.fail[1] ]][[1]]$t, list.output[[ trials.fail[1] ]][[1]]$C,
                      lty = 2, lwd = 2)
        }
}

# Bifurcation diagrams

MakeBifurcationDiagram <- function (bifurc = "E", Y = "C") {
        
        if (bifurc == "E" & Y == "C") {
                
                min <- -1
                max <- 1
                bifur.sequence <- seq(min, max, 0.05)  # sequence for E's
                
                # create empty plot
                plot(c(min, max), c(0, 1), type = "n", pch = ".", xlab = bifurc, ylab = Y,
                     bty = "n", las = 1)
                
                for (i in bifur.sequence) {
                        
                        weeks.bifur <- 500  # simulate 500, burn 250
                        
                        CalculateParameters(d, S.plus, q)
                        
                        InitializeVectors(C.init, S.plus, i, lamda.init, A.init, weeks.bifur)
                        
                        InitializeList()
                        
                        SimulateAddictionComponents(Crav, S, V, A, E, lamda, cues, weeks.bifur, b, d, p, S.plus, h, k, q)
                        
                        BuildOutputDataframe(weeks.bifur, A, Crav, S, E, lamda, cues, V)
                        
                        BuildOutputList()
                        
                        # simulate 500, burn 250
                        for (j in 250:weeks.bifur) {  
                                points(i, list.output[[1]][[1]]$C[j])
                        }
                        
                }
                
        } else if (bifurc == "E" & Y == "S") {
                min <- -1
                max <- 1
                bifur.sequence <- seq(min, max, 0.05)  # sequence for E's
                
                # create empty plot
                plot(c(min, max), c(0, 1), type = "n", pch = ".", xlab = bifurc, ylab = Y,
                     bty = "n", las = 1)
                
                for (i in bifur.sequence) {
                        
                        weeks.bifur <- 500  # simulate 500, burn 250
                        
                        CalculateParameters(d, S.plus, q)
                        
                        InitializeVectors(C.init, S.plus, i, lamda.init, A.init, weeks.bifur)
                        
                        InitializeList()
                        
                        SimulateAddictionComponents(Crav, S, V, A, E, lamda, cues, weeks.bifur, b, d, p, S.plus, h, k, q)
                        
                        BuildOutputDataframe(weeks.bifur, A, Crav, S, E, lamda, cues, V)
                        
                        BuildOutputList()
                        
                        # simulate 500, burn 250
                        for (j in 250:weeks.bifur) {  
                                points(i, list.output[[1]][[1]]$S[j])
                        }
                }
        }
}




dev.off()
###############################################################################
# initializing

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


MakeGraphs(V.plot = TRUE, successfull = FALSE)

MakeBifurcationDiagram()

###############################################################################
