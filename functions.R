# Functions for sourcing

# Calculate parameters
# takes input from GUI
# returns vector with calculated parameters
CalculateParameters <- function (d, S.plus, q) {
        p <- 2*d  # resilience parameter
        k <- (p * S.plus)/q
        h <- p*S.plus
        b <- (2*d)/q
        
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
                Crav[i+1] <- ( Crav[i] + b * min(c(1, (1-Crav[i]))) * A[i] - d * Crav[i] )
                
                # calculate S(t+1)
                S[i+1] <- ( S[i] + p * max(c(0, (S.plus - S[i]))) - h * Crav[i] - k * A[i] )
                
                # calculate V(t+1)
                V[i+1] <- min(c(1, max(c(0, (Crav[i] - S[i] - E[i])))))
                
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
        x.lim <- c(0, length(x))
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
                y.lim <- c(-0.05, 100*q)
        } else if (graph.type == 2) {
                y <- output.addiction[[a]][[1]]$C
                ytitle <- "C(t)"
                g.title <- "Craving over time"
                y.lim <- c(-0.05, 1.05)
        } else if (graph.type == 3) {
                y <- output.addiction[[a]][[1]]$S
                ytitle <- "S(t)"
                g.title <- "Self-control over time"
                y.lim <- c(-0.05, S.plus)
        } else if (graph.type == 4) {
                y <- output.addiction[[a]][[1]]$V
                ytitle <- "V(t)"
                g.title <- "Vulnerability over time"
                y.lim <- c(-0.05, 1)
        } else if (graph.type == 6) {
                y <- output.addiction[[a]][[1]]$V
                ytitle <- "V(t) and S(t)"
                g.title <- "Vulnerability V(t) and Self-Control S(t) over time"
                y.lim <- c(-0.05, 1)
                y1 <- output.addiction[[a]][[1]]$S
                g.legend <- c("S(t)", "V(t)")
        } else if (graph.type == 5) {
                y <- output.addiction[[a]][[1]]$A
                ytitle <- "A(t) and C(t)"
                g.title <- "Addictive acts A(t) and craving C(t) over time"
                y.lim <- c(-0.25, 1)
                y1 <- output.addiction[[a]][[1]]$C
                g.legend <- c("C(t)", "A(t)")
        }
        
        
        
        plot(x, y, bty = "n", las = 1, xlab = "Time (in weeks)", lwd = 2, xlim = x.lim,
             type = "l", ylab = ytitle, ylim = y.lim, main = g.title, cex.lab = 1.5)
        
        if (graph.type == 5 | graph.type == 6) {
                lines(x, y1, lty = 2, lwd = 2)
                legend("bottomright", legend = g.legend, lty = c(2, 1), lwd = 2)
        }
        
        
        
}



# Bifurcation diagrams
# takes list "bifurc", Y ("C" or "S"), input from GUI
# renders a plot
MakeBifurcationDiagram <- function (bifurc, Y, S.plus, q, d, C.init, E.init, lamda.init, A.init) {
        
        weeks <- 300  # simulate total 500, later burn 250
        no.simulations <- 1  # 1 time for each bifurcation parameter step
        
        # Arguments to be called in CalculateParameters
        args1 <- list(d = d, S.plus = S.plus, q = q)
        
        # argument list to be used in InitializeVectors
        args2 <- list(C.init = C.init, S.plus = S.plus, E.init = E.init,
                      lamda.init = lamda.init, A.init = A.init, weeks = weeks,
                      no.simulations = no.simulations, d = d)
        
        # create lists to be used in plot creation
        if (bifurc == 1) {
                bifurc.list <- list(name = "E", min = -1, max = 1)
        } else if (bifurc == 2) {
                bifurc.list <- list(name = "S.plus", min = 0, max = 1)
        } else if (bifurc == 3) {
                bifurc.list <- list(name = "d", min = 0, max = 1)
        } else if (bifurc == 4) {
                bifurc.list <- list(name = "C.init", min = 0, max = 1)
        } else if (bifurc == 5) {
                bifurc.list <- list(name = "lamda", min = 0, max = 1)
        } else if (bifurc == 6) {
                bifurc.list <- list(name = "A.init", min = 0, max = 1)
        }
        
        
        # make sequence of bifurcation parameter
        min <- bifurc.list[[2]]
        max <- bifurc.list[[3]]
        
        if (bifurc == 6) {
                max <- q
        }
        
        bifurc.sequence <- seq(min, max, 0.05)  # sequence for bifurc
        
        
        
        # set to take either C or S from dataframe
        if (Y == "C") {
                y <- 3
                y.lim <- c(-0.25, 1.25)
        } else if (Y == "S") {
                y <- 4
                y.lim <- c(-0.25, (S.plus+0.25))
        }
        
        
        # create empty plot
        plot(c(min, max), c(0, 0), type = "n", pch = ".", xlab = bifurc.list[[1]], ylab = Y,
             bty = "n", las = 1, ylim = y.lim, cex.lab = 1.5, main = "Bifurcation Diagram", lwd = 2)
        
        
        # simulate the data
        for (i in bifurc.sequence) {
                
                thisi <- i  # circumvents some weird ShinyR behavior
                
                if (bifurc == 1) {
                        args2[[3]] <- thisi
                } else if (bifurc == 2) {
                        args1[[2]] <- thisi
                        args2[[2]] <- thisi
                } else if (bifurc == 3) {
                        args1[[1]] <- thisi
                        args2[[8]] <- thisi
                } else if (bifurc == 4) {
                        args2[[1]] <- thisi
                } else if (bifurc == 5) {
                        args2[[4]] <- thisi
                } else if (bifurc == 6) {
                        args2[[5]] <- thisi
                }
                
                for (k in 1:50) {
                         
                        thisk <- k
                        
                        parameters <- do.call(CalculateParameters, args1)  # returns "parameters"
                        
                        vectors <- do.call(InitializeVectors, args2)  # returns "vectors"
                        
                        #necessary once at this point to create argslist with "parameters" and "vectors"
                        # argument list to be used in SimulateAddictionComponents
                        args3 <- list(vectors = vectors, parameters = parameters, S.plus = S.plus, q = q, weeks = weeks, d = d)
                        
                        if (bifurc == 2) {
                                args3[[3]] <- thisi
                        } else if (bifurc == 3) {
                                args3[[6]] <- thisi
                        }
                        
                        
                        simulation <- do.call(SimulateAddictionComponents, args3)  # returns list "simulation"
                        
                        df.output <- BuildOutputDataframe(weeks, simulation, vectors)  # returns "df.outout"
                        
                        # burn first 250 of previously 500 created, plot last 250
                        for (j in 100:weeks) {  
                                points(i, df.output[j, y])
                        }
                        
                }
                
                
                
        }
}