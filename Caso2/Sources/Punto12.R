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

RankBay <- matrix(NA, ncol = 3, nrow = length(Col))
colnames(RankBay) <- c("INF", "MEDIAS", "SUP")
rownames(RankBay) = rn

# Llenado de las medias y cuantiles
RankBay[, "MEDIAS"] <- sapply(M4[, ..Col], mean)
RankBay[, c("INF", "SUP")] <- t(sapply(M4[, ..Col], quantile, probs = c(0.025, 0.975)))

# Ordenar la matriz 

RankBay = RankBay[order(RankBay[,2], decreasing = FALSE),]


# =====> PUNTO 12: Usando M4, hacer una segmentación de los departamentos usando las medias especificas
#               de los departamentos. por medio del método de agrupamientod de K-medias

# Seleccionar el número de grupos


set.seed(123)

data_matrix <- as.matrix(RankBay_df[, 2])

fviz_nbclust(data_matrix, kmeans, method = "wss")  +
  labs(title = "METODO DEL CODO PARA LA SELECCIÓN DEL NUMERO DE GRUPOS EN EL KMEANS",
       subtitle = "DEPARTAMENTOS EN COLOMBIA AGRUPADOS POR LA MEDIA DEL MODELO M4") +
  theme_minimal() + 
  theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(size = 9, face = "bold", hjust = 0.5, color = "grey50"),
        text = element_text(size = 10),
        legend.position = "right")

# fviz_nbclust(data_matrix, kmeans, method = "silhouette")  +
#   labs(title = "METODO DE SILUETA PARA LA SELECCIÓN DEL NUMERO DE GRUPOS EN EL KMEANS",
#        subtitle = "DEPARTAMENTOS EN COLOMBIA AGRUPADOS POR LA MEDIA DEL MODELO M4") +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5), 
#         plot.subtitle = element_text(size = 9, face = "bold", hjust = 0.5, color = "grey50"),
#         text = element_text(size = 10),
#         legend.position = "right")


# =====> A pesar que el método del codo sugiere de manera más clara 3 o 4 grupos, 
#        se procede a realizar el análisis con 5 grupos dado que el grafico muestra un aporte 
#        de la inercia en el quinto grupo


# Inicializar la matriz de clusters
Cls <- matrix(NA, nrow = nrow(M4), ncol = r)
colnames(Cls) <- rn

# Inicializar la matriz de incidencia
MInci <- matrix(0, nrow = r, ncol = r)

# Iterar sobre cada fila de M4
for (i in 1:nrow(M4)) {
  # Aplicar k-means a las medias específicas de los departamentos
  km <- kmeans(as.numeric(M4[i, ..Col]), centers = 5, nstart = 25)
  Cls[i, ] <- km$cluster
  
  # Calcular la matriz de incidencia para esta iteración
  for (j in 1:5) {
    b <- (Cls[i, ] == j)  # Vector lógico para el grupo j en la iteración i
    MInci <- MInci + (b %*% t(b))  # Sumar la matriz de incidencia
  } 
}

MInci <- MInci / nrow(M4)
rownames(MInci) <- rn

par(mfrow = c(1, 1))

corrplot::corrplot(corr = MInci, 
                   is.corr = FALSE,
                   addgrid.col = NA, 
                   method = "color", 
                   tl.pos = "lt",
                   tl.cex = 0.8,
                   tl.col = 'black')

title(main = 'Matriz de incidencia por departamentos', line = 1.8, cex.main = 1.6)


#Plot resultado Kmmeans 

plot(km$cluster)
abline(v = 1:32, col = "gray", lty = 2)
abline(h = 1:5, col = "gray", lty = 2)





