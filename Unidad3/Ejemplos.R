# Este script tiene como objetivo replicar los ejemplos del capitulo número 8 del
# libro Control Estadístico de Calidad Y Seis Sigma de Humberto Gutiérrez y Román
# de la Vara Salazar.


# Establecer el directorio de trabajo en cual se encuentra la base de datos

setwd("/Users/gusahu/Google Drive/Quality_control_plus_R/Unidad3")

# install.packages("readxl") # instalar readxl para cargar datos desde archivos Excel

# Replica ejemplo 8.1 libro Control Estadístico de Calidad y Seis Sigma Pagina 225

# Cargar las librerias 
library(readxl)
library(qcc)
salchicha_base <- read_excel("ejemplo8_1.xlsx")

aire_p <- qcc(data = salchicha_base$paquetes_con_aire,
              type = "p", # el tipo de gráfico es p
              sizes = salchicha_base$paquetes)

names(aire_p)
aire_p$center
aire_p$limits
aire_p$std.dev

# sizes hace referencia al tamaño de la muestras columnos paquetes, donde podemos ver
# que los tamaños muestrales no son iguales, por lo tanto, los límites de control 
# superior son variables, sin emabrgo, R realiza un promedio para calcular el límite
# superior como puede observar en el gráfico


# Replica ejemplo 8.2 libro Control Estadístico de Calidad y Seis Sigma Pagina 230

defectos_k12 <- read_excel("ejemplo8_2.xlsx")
defectos_np <- qcc(data = defectos_k12$componentes_defectuosos,
              type = "np", # el tipo de gráfico es p
              sizes = 120)

# como indica el libro los tamaños muestrales son igual n = 120
names(defectos_np)
defectos_np$center
defectos_np$limits

# Replica ejemplo 8.3 libro Control Estadístico de Calidad y Seis Sigma Pagina 234

mesa_defectos <- read_excel("ejemplo8_3.xlsx")
defectos_c <- qcc(data = mesa_defectos$defectos, type = "c")

# Replica ejemplo 8.4 libro Control Estadístico de Calidad y Seis Sigma Pagina 236

# Caso 1: se emplea el tamaño de subgrupo promedio n = 21.876 (ver pagina 237)
defectos_encontrado <- read_excel("ejemplo8_4.xlsx")
defectos_u <- qcc(data = defectos_encontrado$defectos_encontrados,
                  type = "u",
                  size = 21.875)

# Caso 2: límites variables (ver pagina 238 del libro)
defectos_u <- qcc(data = defectos_encontrado$defectos_encontrados,
                  type = "u",
                  size = defectos_encontrado$tamano_muestra)

names(defectos_u)
defectos_u$center
defectos_u$limits
