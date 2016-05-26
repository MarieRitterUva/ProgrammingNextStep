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
                
                return(list(Crav = Crav, S = S, E = E, lamda = lamda, cues = cues, V = V, A = A, addiction.all = addiction.all))
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
SimulateAddictionComponents <- function (vectors, parameters, S.plus, q, weeks, d) {
        
        Crav <- vectors$Crav
        S <- vectors$S
        E <- vectors$E
        lamda <- vectors$lamda
        cues <- vectors$cues
        V <- vectors$V
        A <- vectors$A
        
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
        lamda <- vectors$lamda
        
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
        
        output.w.success <- list(df.output, addiction)  # includes success data
        list.output <- c(list.output, list(output.w.success))  # adds the new data to the list
        return(list.output)
}


# Looping function - calls functions SimulateAddictionComponents, BuildOutputDataframe, and
# BuildOutputList
# takes input from GUI, list "vectors" from InitializeVectors, vector "parameters" from
# CalculateParameters, and list "list.output" from InitializeList
# returns list "list.output"
SimulateMultiple <- function (no.simulations, vectors, parameters, S.plus, q, weeks, d, list.output) {
        
        for (i in 1:no.simulations) {
                thisi <- i
                
                simulation <- SimulateAddictionComponents(vectors, parameters, S.plus, q, weeks, d)  # returns list "simulation"
                
                df.output <- BuildOutputDataframe(weeks, simulation, vectors)  # returns "df.outout"
                
                list.output <-BuildOutputList(df.output, simulation, list.output)  # returns "output.addiction"
                
        }
        
        return(list.output)
}


# Calculate how often of the simulations patient is addicted after the time and get percentage
# takes list "output.addiction" from SimulateMultiple and input from GUI
# returns list "success.list"
CalculateSuccess <- function (output.addiction, no.simulations) {
        
        addiction.all <- logical()
        
        for (i in 1:length(output.addiction)) {
                addiction.all <- c(addiction.all, output.addiction[[i]][[2]])
        }
        
        success.percent <- (sum(addiction.all == FALSE) / no.simulations) * 100
        trials.success <- which(addiction.all == FALSE)
        trials.fail <- which(addiction.all == TRUE)
        
        return(list(success.percent = success.percent, trials.success = trials.success, trials.fail = trials.fail))
        
}


###############################################################################

# GRAPHS

# Graphs over time
# uses the output list to get data; x is always time t
MakeGraphs <- function (graph.type, graph.success, output.addiction, success.list, q, S.plus) {
        a <- numeric()
        
        if (graph.success == TRUE) {
                a <- as.numeric( success.list[[2]][1])
        } else if (graph.success == FALSE) {
                a <- as.numeric(success.list[[3]][1])
        }
        
        
        x <- output.addiction[[ a ]][[1]]$t  # time is always on x-axis
        y <- numeric()
        y1 <- numeric()
        y.lim <- numeric()
        g.title <- character()
        ytile <- character()
        
        
        # SINGLE PLOTS
        if (graph.type == 1) {
                y <- 100*(output.addiction[[a]][[1]]$A)
                ytitle <- "A(t) in alcoholic beverages"
                g.title <- "Addictive acts A(t) per week over time"
                y.lim <- c(0, 100*q)
        } else if (graph.type == 2) {
                y <- output.addiction[[a]][[1]]$C
                ytitle <- "C(t)"
                g.title <- "Craving over time"
                y.lim <- c(0, S.plus)
        } else if (graph.type == 3) {
                y <- output.addiction[[a]][[1]]$S
                ytitle <- "S(t)"
                g.title <- "Self-control over time"
                y.lim <- numeric()
        } else if (graph.type == 4) {
                y <- output.addiction[[a]][[1]]$V
                ytitle <- "V(t)"
                g.title <- "Vulnerability over time"
                y.lim <- c(0,1)
        }
        
        plot(x, y, bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
             type = "l", ylab = ytitle, ylim = y.lim, main = g.title)
        
        
        # DOUBLE PLOTS
        # S and V
        if (graph.type == 6) {
                plot(list.output[[a]][[1]]$t, list.output[[a]][[1]]$V,
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "V(t) and S(t)",
                     main = "Vulnerability V(t) and Self-Control S(t) over time")
                lines(list.output[[a]][[1]]$t, list.output[[a]][[1]]$S,
                      lty = 2, lwd = 2)
                legend("bottomright", legend = c("S(t)", "V(t"), lty = c(2, 1), lwd = 2)
        }
        
        # A and C
        if (graph.type == 5) {
                plot(list.output[[a]][[1]]$t, list.output[[a]][[1]]$A,
                     bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2,
                     type = "l", ylab = "A(t) and C(t)",
                     main = "Addictive acts A(t) and craving C(t) over time")
                lines(list.output[[a]][[1]]$t, list.output[[a]][[1]]$C,
                      lty = 2, lwd = 2)
                legend("bottomright", legend = c("C(t)", "A(t"), lty = c(2, 1), lwd = 2)
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