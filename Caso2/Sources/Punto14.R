# ====> ESTADISTICA BASEYIANA: CASO DE ESTUDIO #2

setwd("~/REPOS GIT/Estadistica_Bayesiana/Caso2")

# Cargar librerias

library(data.table)                            # Manejo de archivos grandes de datos más eficiente
library(metRology)                             # Distribución t escalada
library(progress)                              # Barras de progreso
library(tibble)
library(readxl)
library(readr)
library(mclust)

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
n2 = unique(cbind(data$COLE_MCPIO_UBICACION, data$COLE_COD_MCPIO_UBICACION))       # Número de municipios
n2[,2] <- ifelse(nchar(n2[,2]) == 4, paste0("0", n2[,2]), n2[,2])


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
rownames(RankBay) = n2[,2]

RankBay1 = RankBay
rownames(RankBay1) = n2[,2]

# Llenado de las medias por departamento

Col <- grep("^eps\\d+$", names(M4), value = TRUE)

# Llenado de las medias y cuantiles
RankBay[,"MEDIAS"] <- sapply(M4[, ..Col], mean)
RankBay[, c("INF", "SUP")] <- t(sapply(M4[, ..Col], quantile, probs = c(0.025, 0.975)))

RankBay1[, "MEDIAS"] <- sapply(M4[, ..Col], mean)
RankBay1[, c("INF", "SUP")] <- t(sapply(M4[, ..Col], quantile, probs = c(0.025, 0.975)))

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
                   tl.pos = "n",order = "AOE")

# Mapa de los departamentos 
## Mapa Colombia ----

MEN <- read_csv("Data/MEN.csv")

MEN <- MEN %>%
  filter(AÑO == 2022)


shp1 <- sf::st_read("Data/MGN2023_MCPIO_POLITICO/Municipios.shp", quiet = TRUE)

shp1$COD_MUN <- paste(shp1$DPTO_CCDGO, shp1$MPIO_CCDGO, sep = "")

shp1$COD_MUN <- as.character(shp1$COD_MUN)
MEN$CÓDIGO_MUNICIPIO <- as.character(MEN$CÓDIGO_MUNICIPIO)

shp1$COD_MUN <- ifelse(nchar(shp1$COD_MUN) == 4,
                       paste0("0", shp1$COD_MUN),
                       shp1$COD_MUN)

MEN$CÓDIGO_MUNICIPIO <- ifelse(nchar(MEN$CÓDIGO_MUNICIPIO) == 4,
                               paste0("0", MEN$CÓDIGO_MUNICIPIO),
                               MEN$CÓDIGO_MUNICIPIO)

# Ordenar por codigo de departamento y municipio 

shp1 <- shp1[order(shp1$COD_MUN),]
MEN <- MEN[order(MEN$CÓDIGO_MUNICIPIO),]

# Agrupamiento por MCLUST

set.seed(123)
Mod1 <- Mclust(MInci, modelNames = "VII", G = 9) #;Mod1$classification

plot(Mod1, what = "classification", dimens = c(1, 2),
     main = "Clasificación de los departamentos en 5 grupos")

###### MAPA CON EL KMEANS

Names <- rownames(RankBay1)

RankBay1 <- as.data.frame(RankBay1) %>%
  mutate(COD_MUN = rownames(.)) %>%  # Agregar código de departamento
  mutate(COD_MUN = as.character(n2[,2])) %>%  # Asegurar tipo correcto
  arrange(match(COD_MUN, Names))


set.seed(123)
km <- kmeans(as.numeric(RankBay1$MEDIAS), centers = 9, nstart = 25) #; km$cluster

RankBay1 <- RankBay1 %>%
  mutate(cluster = km$cluster) 

# Filtrar los departamentos que están en Names y ordenarlos

#shp1 <- shp1 %>%
#  filter(COD_MUN %in% Names) 


# Cuales son los diferentes en Names y COD_MUN

#setdiff(Names, shp1$COD_MUN)
#setdiff(shp1$COD_MUN, Names)

# Coincidencias entre dpto_ccdgo y Names

shp1 <- shp1 %>%
  left_join(RankBay1, by = "COD_MUN")

df_clust <- data.frame(
  COD_MUN = n2[,2], 
  Class = Mod1$classification
)

shp1 <- shp1 %>%
  left_join(df_clust, by = "COD_MUN")


colors <- c("#eeaf61", "#00afb9", "#ee5d6c", "#ff4d00", "#0081a7",
            "#ce4993", "#eef942", "#f44040", "#800080")

# Mapa con K-Means
ggplot(data = shp1) +
  geom_sf(aes(fill = factor(cluster)), color = "white", size = 0.2) +
  scale_fill_manual(values = colors,  name = "Cluster", na.value = "gray80") +  # Usar colores personalizados
  ggtitle("AGRUPAMIENTO POR KMEANS DE LA MEDIAS GLOBALES",
          subtitle = "CLUSTERS DE MUNICIPIOS EN COLOMBIA") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Mapa con MClust
ggplot(data = shp1) +
  geom_sf(aes(fill = factor(Class)), color = "white", size = 0.2) +
  scale_fill_manual(values = colors, name = "Cluster", na.value = "gray80") +  # Usar colores personalizados
  ggtitle("AGRUPAMIENTO POR MCLUST DE LA MEDIAS GLOBALES",
          subtitle = "CLUSTERS DE MUNICIPIOS EN COLOMBIA") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

























## MEDIA DE LOS CLUSTERS 

# Media de los clusters

shp1_df <- st_drop_geometry(shp1)  # Elimina la geometría

media_clusters <- shp1_df %>%
  group_by(cluster) %>%
  summarise(media = mean(MEDIAS, na.rm = TRUE))



extremos_clusters <- shp1_df %>%
  group_by(cluster) %>%
  summarise(
    max_mpio = MPIO_CNMBR[which.max(MEDIAS)],
    max_media = max(MEDIAS, na.rm = TRUE),
    min_mpio = MPIO_CNMBR[which.min(MEDIAS)],
    min_media = min(MEDIAS, na.rm = TRUE)
  )














