setwd("/Users/gusahu/Google Drive/Quality_control_plus_R/Unidad3")
install.packages("readxl")
install.packages("qcc")
library(readxl)
library(qcc)
library(memory.size)

datos_taller <- read_excel("datos_taller.xlsx")

valvulas_p <- qcc(data = datos_taller$decfectuosas,
              type = "p", # el tipo de gráfico es p
              sizes = 300)

names(valvulas_p)
valvulas_p$sizes

D <- sum(datos_taller$decfectuosas)
n <- 300
p <- D/(21*300)
UCL <- p + 3*(sqrt(p*(1-p)/n))
LCL <- p - 3*(sqrt(p*(1-p))/n)

## np chart
valvulas_p <- qcc(data = datos_taller$decfectuosas,
                  type = "np", # el tipo de gráfico es p
                  sizes = 300)


UCL2 <- n*p + 3*(sqrt((n*p)*(1-p)))
LCL2 <- n*p - 3*(sqrt((n*p)*(1-p)))
