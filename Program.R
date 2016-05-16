## Programming: The next step - Simulating Addiction with Differential Equations
## Marie Ritter - UvA

# ToDo: GRAPHS!
# ToDO: Calculation of parameters?!
# ToDo: Multiple simulations
# ToDo: Success output
# ToDo: Offer possibility of treatment weeks (if-statements, etc.)
# ToDo: Write all in functions -> easy access
# ToDO: Simple GUI input and outpur
# ToDo: Display Logic GUI
# ToDo: Speed of Simulation
# ToDo: Beautify GUI
# ToDo: Manual and Instructions

#############################################################################

# input -> later from GUI

q <- 0.8  # maximum consumption

E.init <- 0  # external influences

S.plus <- 0.5  # maximum self-control

p <- 0.4  # resilience parameter

C.init <- 0  # craving at time 0

A.init <- 0.5 * q  # consumption at time 0

weeks <- 6  # number of weeks

lamda.init <- 0.5  # intensity of external influences

#############################################################################
# basic simulation


# calculate parameters

d <- p/2

k <- (p * S.plus) / q

h <- p * S.plus

b <- (2*d) / q


# initialize vectors

C <- numeric(length = weeks+1)
C[1] <- C.init

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
V[1] <- min(c(1, max(c(0, (S.plus - C[1] - E[1]) )) ))

A <- numeric(length = weeks+1)
A[1] <- A.init




# calculate C, S, V, and A

for (i in 1:weeks) {
        # calculate C(t+1)
        C[i+1] <- C[i] + b * min(c(1, 1-C[i])) * A[i] - d * C[i]
        
        # calculate S(t+1)
        S[i+1] <- S[i] + p * max(c(0, (S.plus - S[i])) ) - h * C[i] - k * A[i]
        
        # calculate V(t+1)
        V[i+1] <- min(c(1, max(c(0, C[i] - S[i] - E[i]))))
        
        # calculate A(t+1) with random component
        # cues
        cues[i] <- rpois(1, lamda[i])
        f <- cues[i] * (q/7)
        
        if (f <= (q * (1 - V[i])) ) {
                
        # calculate A(t+1)
        A[i+1] <- q * V[i] + f
        
        } else {
                A[i+1] <- q
        }
}


###############################################################################

# output
data <- data.frame("t" = (1: (weeks+1))-1,  "A" = A, "C" = C, "S" = S,
                   "E" = E, "lamda" = lamda, "cues" = cues, "V" = V)

data


