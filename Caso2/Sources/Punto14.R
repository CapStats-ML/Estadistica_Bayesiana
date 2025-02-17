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
               !is.na(COLE_COD_DEPTO_UBICACION) &
               !is.na(COLE_COD_MCPIO_UBICACION)]

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

n1 = length(unique(data$COLE_COD_MCPIO_UBICACION))   # Número de municipios
n2 = unique(data$COLE_MCPIO_UBICACION)       # Número de municipios

# Traer los nombres de los municipios

nn <- unique(data$COLE_MCPIO_UBICACION)

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

RankBay = matrix(NA, ncol = 3, nrow = n1)

colnames(RankBay) = c("INF", "MEDIAS", "SUP")
# rownames(RankBay) = n2

# Llenado de las medias por departamento

Col <- grep("^eps\\d+$", names(M4), value = TRUE)

# Llenado de las medias y cuantiles
RankBay[,"MEDIAS"] <- sapply(M4[, ..Col], mean)
RankBay[, c("INF", "SUP")] <- t(sapply(M4[, ..Col], quantile, probs = c(0.025, 0.975)))

# Ordenar la matriz 
RankBay = RankBay[order(RankBay[,2], decreasing = FALSE),]

# ranking basado en el promedio muestral

Cls <- matrix(NA, nrow = nrow(M4), ncol = n1)

# Inicializar la matriz de incidencia
MInci <- matrix(0, nrow = n1, ncol = n1)

# Iterar sobre cada fila de M4
for (i in 1:nrow(M4)) {
  # Aplicar k-means a las medias específicas de los departamentos
  km <- kmeans(as.numeric(M4[i, ..Col]), centers = 10, nstart = 25)
  Cls[i, ] <- km$cluster
  
  # Calcular la matriz de incidencia para esta iteración
  for (j in 1:5) {
    b <- (Cls[i, ] == j)  # Vector lógico para el grupo j en la iteración i
    MInci <- MInci + (b %*% t(b))  # Sumar la matriz de incidencia
  } 
}


rownames(MInci) = colnames(MInci)
MInci = MInci/10000

corrplot::corrplot(corr = MInci, 
                   is.corr = FALSE,
                   addgrid.col = NA, 
                   method = "color", 
                   tl.pos = "n",
                   title = 'Matriz de incidencia entre municipios')




































