## Programming: The next step - Simulating Addiction with Differential Equations
## Marie Ritter - UvA

# ToDo: GRAPHS!
# ToDo: Set default for init functions?
# ToDO: Calculation of parameters?!
# ToDo: Multiple simulations
# ToDo: Success output
# ToDo: Offer possibility of treatment weeks (if-statements, etc.)
# ToDo: Write input ifs
# ToDO: Simple GUI input and output
# ToDo: Display Logic GUI
# ToDo: Speed of Simulation
# ToDo: Beautify GUI
# ToDo: Manual and Instructions

#############################################################################

rm(list = ls())
setwd("/home/marie/Dokumente/Uni/Courses/Programming/Repo/ProgrammingNextStep")

#############################################################################

# input -> later from GUI

q <- 0.8  # maximum consumption

E.init <- 0  # external influences

S.plus <- 0.5  # maximum self-control

p <- 0.4  # resilience parameter

C.init <- 0  # craving at time 0

A.init <- 0.5 * q  # consumption at time 0

weeks <- 20  # number of weeks

lamda.init <- 0.5  # intensity of external influences

#############################################################################
# Functions

# calculate parameters
# - needs to be done in beginning and if input parameters change

CalculateParameters <- function (p = 0.4, S.plus = 0.5, q = 0.8) {
        d <<- p/2
        k <<- (p * S.plus) / q
        h <<- p * S.plus
        b <<- (2*d) / q
}


# initialize vectors
# - needs to be done in beginning and if input parameters change

InitializeVectors <- 
        function (C.init = 0, S.plus = 0.5, E.init = 0, lamda.init = 0.5, A.init = 0.4, weeks = 20) {
        
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
                
                # calculate A(t+1) with random component
                set.seed(1)  # for testing purposes - TAKE OUT AT THE END!!!
                cues[i] <- rpois(1, lamda[i])
                f <- cues[i] * (q/7)
                
                if (f <= (q * (1 - V[i])) ) {  # stay under max
                        
                        A[i+1] <- q * V[i] + f
                        
                } else {
                        A[i+1] <- q
                }
        }
        
        cues <<- cues
        A <<- A
        Crav <<- Crav
        S <<- S
        V <<- V
        
}


# build output dataframe

BuildOutputDataframe <- function (weeks, A, Crav, S, E, lamda, cues, V) {
        output <- data.frame("t" = (1: (weeks+1))-1,  "A" = A, "C" = Crav, "S" = S,
                             "E" = E, "lamda" = lamda, "cues" = cues, "V" = V)
        return(output)
}

###############################################################################
# initializing

CalculateParameters(p, S.plus, q)

InitializeVectors(C.init, S.plus, E.init, lamda.init, A.init, weeks)


# actual simulation

SimulateAddictionComponents(Crav, S, V, A, E, lamda, cues, weeks, b, d, p, S.plus, h, k, q)


# get output

BuildOutputDataframe(weeks, A, Crav, S, E, lamda, cues, V)


###############################################################################





