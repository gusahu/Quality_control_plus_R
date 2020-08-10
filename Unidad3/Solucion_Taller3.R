# Establecer el directorio de trabajo
setwd("/Users/gusahu/Google Drive/Quality_control_plus_R/Unidad3")

# Si es neceario instalar los siguientes packages
# install.packages("readxl")
# install.packages("qcc")

# Cargar las siguientes librerias
library(readxl)
library(qcc)
library(memory.size)

###############
# Pregunta 1. #
###############

# 1.a

datos_taller <- read_excel("datos_taller.xlsx")

D <- sum(datos_taller$decfectuosas) # total defectuosos en la base de datos
nj <- 300
n <- 21*300
p <- D/n # linea central (CL)
UCL <- p + 3*(sqrt((p*(1-p))/(nj)))
LCL <- p - 3*(sqrt((p*(1-p))/(nj)))

# 1.b
# Los límites de control calculados muestran que de cada lote de 300 valvulas
# se espera que, de forma ordinaria, la proporción de valvulas con algún tipo
# de defecto varia entre 0.35% y un 6.8%, con un promedio de 3.55%.  

# 1.c

valvulas_p <- qcc(data = datos_taller$decfectuosas,
              type = "p", # el tipo de gráfico es p
              sizes = 300)

# Primero, corroboramos que los límites de control calculados en en inciso
# a) están bien calculados, ya que coincidos con los límites encontrados
# empleando la función qcc. Segundo, la proporción de valvulas defectuosas 
# se encuentran dos los límites de control, además no se identifica ningún 
# otro patrón especial que indique que el proceso se encuentre en una 
# situación bajo control estadístico

# 1.d

# De cuerdo a lo anterior, dado que no hay puntos fuera de los límites de 
# control o patrones especiales, podríamos decir que el proceso está bajo
# control estadítico (estable). Pero su desempeño se encuentra lejor de ser
# satisfactorio, ya que el porcentaje promedio de defectuosos es relativamen-
# te alto 3.55%. Por ello, es necesario generar un proyecto de mejora
# mediante la identificación de las causas comunes más importantes que están
# generando el problema.


###############
# Pregunta 2. #
###############

# 2.a
# Los límites de control variables están asociados con los tamaños muestrales
# de cada lote y no con el tamaño promedio de todos los lotes de la base de
# datos.

valvulas_p2 <- qcc(data = datos_taller$decfectuosas,
                  type = "p", # el tipo de gráfico es p
                  sizes = datos_taller$tamano_lote)


# 2.b

# En este caso los límites de control dependen del valor de n_i. En el caso
# en cuestión a la producción de las valvulas, se tiene que existen tamaños
# de lotes por encima y por debajo de 300, lo cual hace que se reduzcan o
# amplien los límites de control. En general, el límites de control superior
# tiende a reducirse, aunque la linea central pasa de un 3.55% a un 3.57%,
# levemente superior, ya que se debe a que varios de los tamaños de lotes
# son inferiores de 300. Por último, podemos indicar que utilizar límites
# de control variable no lleve a identificar puntos fuera de los límites
# o identificar patrones.


###############
# Pregunta 3. #
################ 

# 3.a

datos_taller <- read_excel("datos_taller.xlsx")

# Calculo manual de los límites de control
D <- sum(datos_taller$decfectuosas)
n <- 300
p <- D/(21*300)
np <- n*p
UCL2 <- n*p + 3*(sqrt((n*p)*(1-p)))
LCL2 <- n*p - 3*(sqrt((n*p)*(1-p)))

# Los límites del gráfico np indican qué tanto varía la cantidad de valvulas
# con algún tipo de defecto por cada lote de tamaño 300. Se espera que de
# cada de lote de 300 valvulas inspeccionadas, el número de rechazadas varíe
# entre 1.04 y 20.3, con un promedio de 10.7.

# 3.b

valvulas_np <- qcc(data = datos_taller$decfectuosas,
                  type = "np", # el tipo de gráfico es p
                  sizes = 300)
names(valvulas_np)
valvulas_np$sizes

# 3.c

# La diferencia más notoria entre ambos gráficos es el estadístico que está
# siendo monitoreado. Para el gráfico p el estadísitico bajo estudio es la
# proporción de valvulas defectuosos, mientras, el gráfico np toma en consi-
# deración el número de valvulas por lote con algún tipo de defecto.
# Ambos gráficos muestran que el proceso de producción de valvulas se 
# encuentra en bajo control estadístico.

# 3.d

# Dado que el tamaño de lotes de producción de valvulas es variables se 
# debería optar por el gráfico de control p.