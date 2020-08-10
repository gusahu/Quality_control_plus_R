######################################
##### Solución Prueba parcial 2 ######
######################################

setwd("/Users/gusahu/Google Drive/Quality_control_plus_R/Unidad3")

library(SixSigma)
library(readxl)
library(qcc)

# Ejercicio 1

n <- 5
d2 <- sapply(n, ss.cc.getd2)
d3 <- sapply(n, ss.cc.getd3)
c4 <- sapply(n, ss.cc.getc4)
A2 <- 3/(d2*sqrt(n))
D3 <- (1 - 3*(d3/d2))
D4 <- (1 + 3*(d3/d2))
B3 <- sapply(1:(n-1), function(x){
  max(0, 1 - 3*(sqrt(1-c4[x]^2)/c4[x]))})
B4 <- 1 + 3*(sqrt(1-c4^2)/c4)


# a. Desviación estándar del proceo
R <- 0.6
desv <- R/d2

# b. Límites de control de X
R <- 0.6
CL <- 50
UCL <- CL + A2*R # 50.3
LCL <- CL - A2*R # 49.7

# c. Límites de control R
CL <- 0.6
UCL <- CL*D4 # 1.27
LCL <- CL*D3 # -0.0687

# d. Límites naturales
# Debe tomar en consideración las nuevas especificaciones para realizar el calculo
# de la desviación estándar

sigma <- sqrt(((51-50)^{2} + (49-50)^{2}) / n)
CL <- 50
UCL <- CL + 3*sigma # 51.897
LCL <- CL - 3*sigma # 48.103


# Ejercicio 2

n <- 5
d2 <- sapply(n, ss.cc.getd2)
d3 <- sapply(n, ss.cc.getd3)
c4 <- sapply(n, ss.cc.getc4)
A2 <- 3/(d2*sqrt(n))
D3 <- (1 - 3*(d3/d2))
D4 <- (1 + 3*(d3/d2))

# Gráfico de Media (X bar)
punteria_base <- read_excel("prueba_ejercicio2.xlsx")
xbar.punteria <- qcc(data = punteria_base, type = "xbar")

punteria_base$xj <- (rowSums(punteria_base[, 1:5]))/5
CL <- mean(punteria_base$xj)
bar.R <- 26.3
ULC <- CL + bar.R*A2
LCL <- CL - bar.R*A2

# Gráfico de Rango (R)
punteria_base <- read_excel("prueba_ejercicio2.xlsx")
R.punteria <- qcc(data = punteria_base, type = "R")
CL <- 26.3
UCL <- CL*D4
LCL <- CL*D3



