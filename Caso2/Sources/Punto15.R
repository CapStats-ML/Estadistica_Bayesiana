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
library(readxl)
library(readr)
library(patchwork)

# =====> PUNTO 13: Calcular la media posterior y un intervalo de credibilidad al 95%
#               de la incicendencia monetaria de los departamentos, para todos los departamentos
#               que no fueron medidos por el DANE, por medio de una regresion lineal simple de la IPM
#               frente a las medias especificas de los departamentos de M4. 
#               Presentar los resultados tabularmente y visualmente a través de un mapa


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
rc <- (unique(data$COLE_COD_DEPTO_UBICACION))      # Codigo de departamentos)
rc  <- ifelse(nchar(rc ) == 1, paste0("0", rc ), rc )

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


MEN <- read_csv("Data/MEN.csv")
MEN <- MEN %>%
  filter(AÑO == 2022)

#n2 <- MEN[, c('MUNICIPIO', 'CÓDIGO_MUNICIPIO')]

MEN$CÓDIGO_MUNICIPIO <- as.character(MEN$CÓDIGO_MUNICIPIO)
MEN$CÓDIGO_MUNICIPIO <- ifelse(nchar(MEN$CÓDIGO_MUNICIPIO) == 4,
                               paste0("0", MEN$CÓDIGO_MUNICIPIO),
                               MEN$CÓDIGO_MUNICIPIO)

MEN$CÓDIGO_DEPARTAMENTO <- as.character(MEN$CÓDIGO_DEPARTAMENTO)
MEN$CÓDIGO_DEPARTAMENTO <- ifelse(nchar(MEN$CÓDIGO_DEPARTAMENTO) == 1,
                               paste0("0", MEN$CÓDIGO_DEPARTAMENTO),
                               MEN$CÓDIGO_DEPARTAMENTO)


MEN <- as.data.table(MEN)
#MEN <- MEN[CÓDIGO_MUNICIPIO %in% n2[,2]]
MEN <- MEN[, c('CÓDIGO_MUNICIPIO', 'MUNICIPIO', 'CÓDIGO_DEPARTAMENTO','DEPARTAMENTO', 'COBERTURA_NETA_SECUNDARIA')]
#MEN <- MEN[CÓDIGO_MUNICIPIO %in% n2[,2]]

NAM <- c("27086", "94663")
NAMES <- n2[n2[,2] %in% NAM,1]
MEN$MUNICIPIO <- toupper(MEN$MUNICIPIO)


Rows <- cbind(NAM[1:2], NAMES[1:2], c("27", "94") , c("Chocó", "Guainía"), NA)
colnames(Rows) <- colnames(MEN)
MEN <- rbind(MEN, Rows)


MEN <- MEN[order(CÓDIGO_MUNICIPIO)] 
MEN$COBERTURA_NETA_SECUNDARIA <- as.numeric(MEN$COBERTURA_NETA_SECUNDARIA) 


Col <- grep("^eps\\d+$", names(M4), value = TRUE)

MENreg =  t(M4[, ..Col])
rownames(MENreg) = n2[,2]
MENreg = cbind('COBERTURA_NETA_SECUNDARIA' = MEN$COBERTURA_NETA_SECUNDARIA, MENreg)

MENpred = MENreg[is.na(MENreg[,'COBERTURA_NETA_SECUNDARIA']),]
MENreg = MENreg[!is.na(MENreg[,'COBERTURA_NETA_SECUNDARIA']),]

# Asegúrate de que todas las columnas sean numéricas
# MENreg <- lapply(MENreg, as.numeric)
# MENpred <- lapply(MENpred, as.numeric)

# Inicializar la matriz de predicciones
MENpredicciones = matrix(NA, ncol = nrow(MENpred), nrow = nrow(M4))
colnames(MENpredicciones) = rownames(MENpred)

# Barra de progreso
Progress = txtProgressBar(min = 2, max = nrow(M4) + 1, style = 3)

# Bucle de regresión
for (i in 2:(nrow(M4) + 1)){
  coeficientes = lm(MENreg[,'COBERTURA_NETA_SECUNDARIA'] ~ MENreg[,i])$coefficients
  MENpredicciones[i-1,] = coeficientes[1] + coeficientes[2] * MENpred[,i]
  setTxtProgressBar(Progress,i)
}


close(Progress)

ResultadosMEN = cbind('Municipio' = NAMES[1:2],
                      'Media Post' = colMeans(MENpredicciones),
                      '2.5%' = apply(MENpredicciones, 2, quantile, probs = 0.025),
                      '97.5%' = apply(MENpredicciones, 2, quantile, probs = 0.975))

row.names(ResultadosMEN) = NULL

xtable::xtable(ResultadosMEN, digits = 5)

################################################################################


# Buscar en la tabla de municipios los nombres de los municipos dado el codigo en NAM 


# Buscar los nombres de los municipios
sum(is.na(MEN$COBERTURA_NETA_SECUNDARIA))
MEN[MUNICIPIO %in% NAMES, 5] =  round(as.numeric(ResultadosMEN[,2]), digits = 4)


shp1 <- sf::st_read("Data/MGN2023_MCPIO_POLITICO/Municipios.shp", quiet = TRUE)


shp1$COD_MUN <- paste(shp1$DPTO_CCDGO, shp1$MPIO_CCDGO, sep = "")
shp1$COD_MUN <- as.character(shp1$COD_MUN)
shp1$COD_MUN <- ifelse(nchar(shp1$COD_MUN) == 4, paste0("0", shp1$COD_MUN), shp1$COD_MUN)


Choco <- shp1 %>%
  filter(DPTO_CCDGO == 27) %>%
  select(COD_MUN, DPTO_CCDGO, MPIO_CNMBR, geometry)

Guinia <- shp1 %>%
  filter(DPTO_CCDGO == 94) %>%
  select(COD_MUN, DPTO_CCDGO, MPIO_CNMBR, geometry)

MEN$CÓDIGO_DEPARTAMENTO <- as.character(MEN$CÓDIGO_DEPARTAMENTO)

MEN_Choco <- MEN[CÓDIGO_DEPARTAMENTO == 27]
MEN_Guinia <- MEN[CÓDIGO_DEPARTAMENTO == 94]

Map_Choco <- Choco %>%
  left_join(MEN_Choco, by = c("COD_MUN" = "CÓDIGO_MUNICIPIO")) 
sum(is.na(Map_Choco$COBERTURA_NETA_SECUNDARIA))

Map_Guinia <- Guinia %>%
  left_join(MEN_Guinia, by =  c("COD_MUN" = "CÓDIGO_MUNICIPIO"))
sum(is.na(Map_Guinia$COBERTURA_NETA_SECUNDARIA))


p1 <- ggplot(data = Map_Choco) +
  geom_sf(aes(fill = COBERTURA_NETA_SECUNDARIA), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = 30, name = "CNS", na.value = "gray90") +
  ggtitle("COBERTURA NETA SECUNDARIA EN LOS MUNICIPIOS DE CHOCÓ",
          subtitle = "CNS EN CHOCÓ COLOMBIA 2022") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )



p2 <- ggplot(data = Map_Guinia) +
  geom_sf(aes(fill = COBERTURA_NETA_SECUNDARIA), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = 30, name = "CNS", na.value = "gray90") +
  ggtitle("COBERTURA NETA SECUNDARIA EN LOS MUNICIPIOS DE GUAINÍA",
          subtitle = "CNS EN GUAINÍA COLOMBIA 2022") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

p1 + p2


p1 <- ggplot(data = Map_Choco) +
  geom_sf(aes(fill = COBERTURA_NETA_SECUNDARIA), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = 30, name = "CNS", na.value = "gray90") +
  ggtitle("COBERTURA NETA SECUNDARIA EN LOS MUNICIPIOS DE CHOCÓ",
          subtitle = "CNS EN CHOCÓ COLOMBIA 2022") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_sf(datum = NA)  # Esto evita que la proyección afecte el tamaño

p2 <- ggplot(data = Map_Guinia) +
  geom_sf(aes(fill = COBERTURA_NETA_SECUNDARIA), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = 30, name = "CNS", na.value = "gray90") +
  ggtitle("COBERTURA NETA SECUNDARIA EN LOS MUNICIPIOS DE GUAINÍA",
          subtitle = "CNS EN GUAINÍA COLOMBIA 2022") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_sf(datum = NA)

p1 + p2





p1 + p2 + plot_layout(widths = c(1.5, 1))



























