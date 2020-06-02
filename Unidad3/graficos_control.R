###########################################################
# Ejercicios en clase: Gráficos de control X-R y X-S.
# Seguir los lineamientos especificados a largo del script.
###########################################################

                                ################
                                ## Practica 1 ##
                                ################

# Cargar los siguientes paquetes
library(SixSigma)
library(qcc)

data("pistonrings") # Cargamos los datos

# Exploración de los datos 
View(pistonrings)
attach(pistonrings) # attach permite acceder a las variables del data frame 
summary(pistonrings)
boxplot(diameter ~ sample)
plot(sample, diameter, cex=0.7)
lines(tapply(diameter,sample,mean))
detach(pistonrings)

# Crear una matriz de 40 muestras cada una con 5 observaciones
muestra.diametros <- qcc.groups(data = pistonrings$diameter,
                              sample = pistonrings$sample)
View(muestra.diametros)

# Gráfico de control X bar para el análsis de la fase I
xbar.diametros <- qcc(muestra.diametros[1:25,], type="xbar")
summary(xbar.diametros) # estadísticas de resumen 
names(xbar.diametros)
xbar.diametros$nsigmas
xbar.diametros$violations
xbar.diametros$limits

# Gráfico de control X bar para el análsis de la fase II
# se incluye una muestra adicional de diametros

xbar.diametros2 <- qcc(muestra.diametros[1:25,], type="xbar", newdata=muestra.diametros[26:40,])
summary(xbar.diametros) # estadísticas de resumen 
names(xbar.diametros2)
xbar.diametros2$nsigmas
xbar.diametros2$violations
xbar.diametros2$limits

diametro.betas <- oc.curves(xbar.diametros2)
1/(1 - diametro.betas[rownames(diametro.betas) == "1", 1])

xbar.diametros3 <- qcc(muestra.diametros[1:25,], type="xbar", newdata=muestra.diametros[26:40,], plot=FALSE)
plot(xbar.diametros3, chart.all=FALSE)

xbar.diametros4 <- qcc(muestra.diametros[1:25,], type="xbar", newdata=muestra.diametros[26:40,], nsigmas=2)
summary(xbar.diametros4) # estadísticas de resumen 

xbar.diametros5 <- qcc(muestra.diametros[1:25,], type="xbar", newdata=muestra.diametros[26:40,], confidence.level=0.99)
summary(xbar.diametros5)

# agregar límites de advertencia en 2 desviaciones estándar
xbar.diametros6 <- qcc(muestra.diametros[1:25,], type="xbar", newdata=muestra.diametros[26:40,], plot=FALSE)
(limite.adver <- limits.xbar(xbar.diametros6$center, xbar.diametros6$std.dev, xbar.diametros6$sizes, 2))
plot(xbar.diametros6, restore.par = FALSE)
abline(h = limite.adver, lty = 3, col = "chocolate")

# Gráfico de control R para el análsis de la fase I
r.diametros <- qcc(muestra.diametros[1:25,], type="R")
summary(r.diametros) # estadísticas de resumen 
names(r.diametros)
r.diametros$nsigmas
r.diametros$violations
r.diametros$limits

# Gráfico de control R para el análsis de la fase II
# se incluye una muestra adicional de diametros

r.diametros2 <- qcc(muestra.diametros[1:25,], type="R", newdata=muestra.diametros[26:40,])
summary(r.diametros2) # estadísticas de resumen 
names(r.diametros2)
r.diametros2$nsigmas
r.diametros2$violations
r.diametros2$limits

# Gráfico de control S para el análsis de la fase I
s.diametros <- qcc(muestra.diametros[1:25,], type="S")
summary(s.diametros) # estadísticas de resumen 
names(r.diametros)
s.diametros$nsigmas
s.diametros$violations
s.diametros$limits

# Gráfico de control S para el análsis de la fase II
# se incluye una muestra adicional de diametros
s.diametros2 <- qcc(muestra.diametros[1:25,], type="S", newdata=muestra.diametros[26:40,])
summary(s.diametros2) # estadísticas de resumen 
names(s.diametros2)
s.diametros2$nsigmas
s.diametros2$violations
s.diametros2$limits


                                  ################
                                  ## Practica 2 ##
                                  ################

data("orangejuice") # Cargamos los datos
help("orangejuice") # Para más información en relación a esta base de datos
View(orangejuice)

orangejuice$d <- orangejuice$D/orangejuice$size
attach(orangejuice)
summary(orangejuice)
boxplot(d ~ trial)
mark <- ifelse(trial, 1, 2)
plot(sample, d, type="b", col=mark, pch=mark)
detach(orangejuice)

# Gráfico de control X bar para el análsis de la fase I
xbar.lata <- qcc(orangejuice[1:15,], type="xbar")
xbar.lata <- qcc(orangejuice[1:15,], type="xbar", nsigmas=1)
summary(xbar.lata) # estadísticas de resumen 
names(xbar.lata)
xbar.lata$nsigmas
xbar.lata$violations
xbar.lata$limits


xbar.lata2 <- qcc(orangejuice[1:15,], type="xbar", newdata=orangejuice[16:22,], nsigmas=1)
summary(xbar.lata2) # estadísticas de resumen 
xbar.lata2$nsigmas
xbar.lata2$violations
xbar.lata2$limits


xbar.lata3 <- qcc(orangejuice[1:22,], type="xbar", newdata=orangejuice[23:30,], nsigmas=1)
summary(xbar.lata2) # estadísticas de resumen 
xbar.lata3$nsigmas
xbar.lata3$violations
xbar.lata3$limits

xbar.lata4 <- qcc(orangejuice[1:30,], type="xbar", newdata=orangejuice[31:54,], nsigmas=1)
xbar.lata5 <- qcc(orangejuice[31:54,], type="xbar", nsigmas = 1)


summary(xbar.lata2) # estadísticas de resumen 
xbar.lata3$nsigmas
xbar.lata3$violations
xbar.lata3$limits

