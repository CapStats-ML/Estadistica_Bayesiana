# ====> ESTADISTICA BASEYIANA: CASO DE ESTUDIO #2

setwd("~/REPOS GIT/Estadistica_Bayesiana/Caso2")

# Cargar librerias

library(data.table)                            # Manejo de archivos grandes de datos más eficiente
library(metRology)                             # Distribución t escalada
library(progress)                              # Barras de progreso


# =====> PUNTO 11: Usando M4 hacer el ranking de los departamentos basado en las medias
#               especificas de los departamentos. Hacer una visualizacion del Ranking
#               Bayesiano. La visualización debe incluir simultaneamente las estimaciones
#               puntuales y los intervalos de credibiilidad del 95%.


# Cargar datos


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

# Llenado de las medias y cuantiles
RankBay[, "MEDIAS"] <- sapply(M4[, ..Col], mean)
RankBay[, c("INF", "SUP")] <- t(sapply(M4[, ..Col], quantile, probs = c(0.025, 0.975)))

# Ordenar la matriz 

RankBay = RankBay[order(RankBay[,2], decreasing = FALSE),]

# ranking basado en el promedio muestral
 
# Configuración de la gráfica
par(mfrow = c(1,1), mar = c(4,10,1.5,1), mgp = c(2.5,0.75,0))

# Crear el gráfico vacío
plot(x = c(min(RankBay[,1] - 75 ), max(RankBay[,3]) + 75), y = c(1, r), type = "n", 
     xlab = "Puntaje", ylab = "", 
     main = expression(bold("Ranking (promedio muestral)")), yaxt = "n")

# Líneas guía horizontales
abline(h = 1:r, col = "lightgray", lwd = 1)
abline(v = 250, col = "gray", lwd = 3)

# Dibujar los intervalos de credibilidad
colores = rep('black', r)
colores[which(RankBay[,"SUP"] < 250)] = 'red' 
colores[which(RankBay[,"INF"] > 250) ] = 'green'

for (j in 1:r) {
  segments(x0 = RankBay[j,'SUP'], y0 = j, lwd = 1, x1 = RankBay[j,'INF'], y1 = j, col = colores[j])
  lines(x = RankBay[j,'MEDIAS'], y = j, type = "p", pch = 16, cex = 1, col = colores[j])
}

# Etiquetas en el eje Y
axis(side = 2, at = 1:r, labels = rownames(RankBay), las = 2, cex.axis = 0.8)





