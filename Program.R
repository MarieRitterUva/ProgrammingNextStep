## Programming: The next step - Simulating Addiction with Differential Equations
## Marie Ritter - UvA

#############################################################################

# input -> later from GUI

q <- 0.8  # maximum consumption

E <- 0  # external influences

S.plus <- 0.5  # maximum self-control

d <- 0.2  # unlearning parameter

C.init <- 0  # craving at time 0

A.init <- 0.5 * q  # consumption at time 0

weeks <- 10  # number of weeks

#############################################################################
# basic simulation



# calculate parameters

p <- 2*d

k <- (p * S.plus) / q

h <- p * S.plus

b <- (2*d) / q


# initialize vectors

C <- numeric(length = weeks+1)
C[1] <- C.init

S <- numeric(length = weeks+1)
S[1] <- S.plus

E <-  numeric(length = weeks)
E <- rep(0, weeks+1)

V <- numeric(length = weeks+1)
V[1] <- S.plus - C[1] - E[1]

A <- numeric(length = weeks+1)
A[1] <- A.init

# calculate C, S, V, and A

for (i in 1:length(weeks)) {
        # calculate C(t+1)
        C[i+1] <- C[i] + b * min(c(1, 1-C[i])) * A[i] - d * C[i]
        
        # calculate S(t+1)
        S[i+1] <- S[i] + p * max(c(0, S.plus - S[i])) - h * C[i] - k * A[i]
        
        # calculate V(t+1)
        V[i+1] <- min(c(1, max(c(0, C[i] - S[i] - E[i]))))
        
        # calculate A(t+1)
        A[i+1] <- q * V[i]
}

data <- data.frame("t" = (1: (weeks+1))-1,  "A" = A, "C" = C, "S" = S, "E" = E, "V" = V)
data
