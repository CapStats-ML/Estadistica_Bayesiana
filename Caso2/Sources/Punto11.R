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
               !is.na(COLE_COD_DEPTO_UBICACION)&
               !is.na(COLE_COD_MCPIO_UBICACION)]

# Ordenar y agrupar
setorder(data, COLE_COD_MCPIO_UBICACION)
data[, group := .GRP, by = COLE_COD_DEPTO_UBICACION]
nj <- data[, .N, by = group]$N
r <- length(unique(data$COLE_COD_DEPTO_UBICACION))  # Número de departamentos
group = data$group
N = nrow(data)
y = data$PUNT_GLOBAL


Y = vector(mode = "list", length = r)              # Nombre Dep
g <- rep(NA,r)


for(j in 1:r){
  idx <- data$COLE_COD_DEPTO_UBICACION == sort(unique(data$COLE_COD_DEPTO_UBICACION))[j]
  g[idx] <- j
  Y[[j]] <- y[idx]
}


M4 = fread('Data/GibbsModelo4.txt')

# ranking basado en el promedio muestral
 
par(mfrow = c(1,1), mar = c(4,10,1.5,1), mgp = c(2.5,0.75,0))

plot(x = c(100,400), y = c(1,r), type = "n", xlab = "Puntaje", ylab = "", main = expression(bold("Ranking (promedio muestral)")), yaxt = "n")
abline(h = 1:m, col = "lightgray", lwd = 1)
abline(v = 250,col = "gray", lwd = 3)

for (l in 1:r) {
  j <- order(y)[l]
  points(x = Y[[r]], y = rep(l, nj[r]), pch = 16, cex = 0.4)
}

lines(x = y[order(y)], y = 1:r, type = "p", col = 4, pch = 16, cex = 1.1)
lines(x = y[order(y)], y = 1:r, type = "l", col = adjustcolor(4, 0.3))

axis(side = 2, at = 1:r, labels = estadisticos$nombre[order(y)], las = 2)

















