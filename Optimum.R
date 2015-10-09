Final <- read.csv("C:/Users/193344/Desktop/Lineups/Final _ 2015-10-09 .csv", stringsAsFactors=FALSE)

library("Rglpk")
library("slam")
num.players <- length(Final$Player)
var.types <- rep("B", num.players)

A <- rbind(as.numeric(Final$Position == "QB"), # num QB
           as.numeric(Final$Position == "RB"), # num RB
           as.numeric(Final$Position == "WR"), # num WR
           as.numeric(Final$Position == "TE"), # num TE
           as.numeric(Final$Position == "D"), # num D
           Final$Salary)                           # total cost


dir <- c("==",
         "==",
         "==",
         "==",
         "==",
         "<=")

b <- c(1,
       2,
       3,
       1,
       1,
       55000)

sol <- Rglpk_solve_LP(obj = Final$Predicted, mat = A, dir = dir, rhs = b,types = var.types, max = TRUE)

max <- cbind(Final,sol)
max <- max[max$solution==1,]
