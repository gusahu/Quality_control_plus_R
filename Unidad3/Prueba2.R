library(SixSigma)
n <- #tamaño de la muestra
d2 <- sapply(n, ss.cc.getd2)
d3 <- sapply(n, ss.cc.getd3)
c4 <- sapply(n, ss.cc.getc4)
A2 <- 3/(d2*sqrt(n))
D3 <- (1 - 3*(d3/d2))
D4 <- (1 + 3*(d3/d2))
B3 <- sapply(1:(n-1), function(x){
  max(0, 1 - 3*(sqrt(1-c4[x]^2)/c4[x]))})
B4 <- 1 + 3*(sqrt(1-c4^2)/c4)

