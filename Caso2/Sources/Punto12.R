# ====> ESTADISTICA BASEYIANA: CASO DE ESTUDIO #2

setwd("~/REPOS GIT/Estadistica_Bayesiana/Caso2")

# Cargar librerias

library(data.table)                            # Manejo de archivos grandes de datos más eficiente
library(metRology)                             # Distribución t escalada
library(progress)                              # Barras de progreso
library(cluster)                               # K-means
library(factoextra)                            # Visualización de K-means
library(sf)                                    # Manipulación de datos geoespaciales
library(dplyr)                                 # Manipulación de datos


# =====> PUNTO 12: Usando M4, hacer una segmentación de los departamentos usando las medias especificas
#               de los departamentos. por medio del método de agrupamientod de K-medias 
#               (Usar un método apropiado para seleccionar el número de grupos). 
#               Presentar los resultados obtenidos visualmente a través de una matriz de incidencia
#               organizada a partir del rankin Bayesiano obtenido en el punto 11.
#               y de un mapa que señale los departamentos que pertenecen a cada grupo.
#               



# Lectura y filtrado de datos
data <- fread('Data/Saber 11 2022-2.TXT', sep = ';')
data <- data[ESTU_NACIONALIDAD == 'COLOMBIA' & 
               ESTU_PAIS_RESIDE == 'COLOMBIA' & 
               ESTU_ESTADOINVESTIGACION == 'PUBLICAR' & 
               COLE_DEPTO_UBICACION != 'SAN ANDRES' & 
               !is.na(COLE_MCPIO_UBICACION) & 
               !is.na(COLE_DEPTO_UBICACION) & 
               !is.na(PUNT_GLOBAL) & 
               !is.na(COLE_COD_DEPTO_UBICACION)]

# Ordenar y agrupar
setorder(data, COLE_COD_MCPIO_UBICACION)
data[, group := .GRP, by = COLE_COD_DEPTO_UBICACION]
data[, groupM := .GRP, by = COLE_COD_MCPIO_UBICACION]

# CANTIDAD DE ESTUDIANTES:
nk <- data[, .N, by = group]$N                      # Por departamento
njk <- data[, .N, by = groupM]$N                    # Por municipio
N = nrow(data)                                      # Totales


r <- length(unique(data$COLE_COD_DEPTO_UBICACION))  # Número de departamentos
rn <- (unique(data$COLE_DEPTO_UBICACION))      # Nombre de departamentos

n = length(unique(data$COLE_COD_MCPIO_UBICACION))   # Número de municipios

group = data$group                                  # Grupos por municipios
groupM = data$groupM                                # Grupos por departamentos
groupMxD = c()                                      # Grupos de municipios por departamento
MxD = c()                                           # Municipios por departamento
for (i in unique(group)){
  groupMxD = c(groupMxD,rep(i,length(unique(groupM[group == i]))))
  MxD = c(MxD, length(unique(groupM[group == i])))
}
y = data$PUNT_GLOBAL

M4 = fread('Data/GibbsModelo4.txt') # Importe de Resultados del modelo 4

RankBay = matrix(NA, ncol = 3, nrow = r)

colnames(RankBay) = c("INF", "MEDIAS", "SUP")
rownames(RankBay) = rn

# Llenado de las medias por departamento

Col <- grep("^theta\\d+$", names(M4), value = TRUE)

for (i in 1:r){
  RankBay[i,2] = mean(M4[[Col[i]]])
  RankBay[i,1] = quantile(M4[[Col[i]]], 0.025)
  RankBay[i,3] = quantile(M4[[Col[i]]], 0.975)
}

# Ordenar la matriz 

RankBay = RankBay[order(RankBay[,2], decreasing = FALSE),]


# =====> PUNTO 12: Usando M4, hacer una segmentación de los departamentos usando las medias especificas
#               de los departamentos. por medio del método de agrupamientod de K-medias

# Seleccionar el número de grupos





























