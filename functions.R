# Functions for sourcing

# Calculate parameters
# takes input from GUI
# returns vector with calculated parameters
CalculateParameters <- function (d, S.plus, q) {
        p <- 2*d  # resilience parameter
        k <- (p * S.plus) / q
        h <- p * S.plus
        b <- (2*d) / q
        
        return(c(p, k, h, b))
}


# Initialize vectors
# takes input from GUI
# returns list with initialized vectors
InitializeVectors <- 
        function (C.init, S.plus, E.init, lamda.init, A.init, weeks, no.simulations, d) {
                
                Crav <- numeric(length = weeks+1)
                Crav[1] <- C.init
                
                S <- numeric(length = weeks+1)
                S[1] <- S.plus
                
                E <-  numeric(length = weeks+1)
                E[1] <- E.init
                for (i in 1:weeks) {
                        E[i+1] <- round((E[i] - d * E[1]), 2)
                }
                
                lamda <- numeric(length = weeks+1)
                lamda[1] <- lamda.init
                for (i in 1:weeks) {
                        lamda[i+1] <- lamda[i] + d * lamda[1]
                }
                
                cues <- numeric(length = weeks+1)
                
                V <- numeric(length = weeks+1)
                V[1] <- min(c(1, max(c(0, (S.plus - Crav[1] - E[1]) )) ))
                
                A <- numeric(length = weeks+1)
                A[1] <- A.init
                
                addiction.all <- logical(no.simulations)
                
                return(list(Crav = Crav, S = S, E = E, lamda = lamda, cues = cues, V = V, A = A, addiction.end = addiction.end))
        }

# Initialize list for datastorage
# takes no arguments
# returns empty list
InitializeList <- function () {
        list.output <- list()
        return(list.output)
}


# Simulate C, S, V, and A - core process
# takes list "vectors" from InitializeVectors and vector "parameters" CalculateParameters, and input from GUI
# returns list with simulated parameters
SimulateAddictionComponents <- function (vectors, parameters, S.plus, q, weeks) {
        
        Crav <- vectors[[1]]
        S <- vectors[[2]]
        E <- vectors[[3]]
        lamda <- vectors[[4]]
        cues <- vectors[[5]]
        V <- vectors[[6]]
        A <- vectors [[7]]
        
        p <- parameters[1]
        k <- parameters[2]
        h <- parameters[3]
        b <- parameters[4]
        
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
        
        return(list(cues = cues, A = A, Crav = Crav, S = S, V = V, addiction = addiction))
        
}

# Build output dataframe
# takes list "simulation" from SimulateAddictionComponents and list "vectors"
# from InitializeVectorsand input from GUI
# returns dataframe
BuildOutputDataframe <- function (weeks, simulation, vectors) {
        
        cues <- simulation$cues
        A <- simulation$A
        Crav <- simulation$Crav
        S <- simulation$S
        V <- simulation$V
        E <- vectors$E
        lamda < vectors$lamda
        
        output <- data.frame("t" = (1: (weeks+1))-1,  "A" = A, "C" = Crav, "S" = S,
                             "E" = E, "lamda" = lamda, "cues" = cues, "V" = V)
        return(output)
}

# Build the list for final storage
# takes data.frame "df.output" from BuildOutputDataframe, list "simulation" from
# SimulateAddictionComponents, and list "list.output" from InitializeList
# returns list "output.addiction"
BuildOutputList <- function (df.output, simulation, list.output) {
        addiction <- simulation$addiction
        
        output.success <- list(df.output, addiction)  # includes success data
        output.addition <- c(list.output, list(output.success))  # adds the new data to the list
        return(output.addition)
}


# Looping function - calls functions SimulateAddictionComponents, BuildOutputDataframe, and
# BuildOutputList
# takes input from GUI, list "vectors" from InitializeVectors, vector "parameters" from
# CalculateParameters, and list "list.output" from InitializeList
# returns list "output.addiction"
SimulateMultiple <- function (no.simulations, vectors, parameters, S.plus, q, weeks, list.output) {
        
        for (i in 1:no.simulations) {
                SimulateAddictionComponents(vectors, parameters, S.plus, q, weeks)  # returns list "simulation"
                
                BuildOutputDataframe(weeks, simulation, vectors)  # returns "df.outout"
                
                BuildOutputList(df.output, simulation, list.output)  # returns "output.addiction"
                
                return(output.addiction)
        }
}


# Create a vector to be used in success calculations - Needs work
# takes arguments "loop" from current for loop and list "simulation" from SimulateAddictionComponents
# returns vector "addiction.all"
# BuildVectorSuccess <- function (loop, simulation) {
# addiction <- simulation$addiction

# addiction.all[loop] <- addiction
# return(addiction.all)
# }

# Calculate how often of the simulations patient is addicted after the time and get percentage
CalculateSuccess <- function (addiction.all, weeks) {
        success.percent <- (max(0, (sum(addiction.end == FALSE) - 1)) / weeks) * 100
        trials.success <- ((which(addiction.end == FALSE))[-1] - 1)  # discard first number for week 0
        # substract one to get actual week number
        trials.fail <- ((which(addiction.end == TRUE))[-1] - 1)
        
        return(list(success.percent = success.percent, trials.success = trials.success, trials.fail = trials.fail))
        
}


###############################################################################

# GRAPHS

# Graphs over time
# uses the output list to get data; x is always time t
MakeGraphs <- function (graph.type, successfull, list.output) {
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

MakeBifurcationDiagram <- function (bifurc, Y) {
        
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