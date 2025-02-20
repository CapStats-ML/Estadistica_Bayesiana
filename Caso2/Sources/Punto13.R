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

IPM <- read_excel("Data/IPM.xlsx", col_types = c("text", "text", "numeric"))
names(IPM) <- c("dpto_nombre", "dpto_ccdgo", "IPM")
IPM <- as.data.table(IPM)
IPM <- IPM[dpto_ccdgo %in% rc]
IPM <- IPM[order(dpto_ccdgo)]  # Ordenar por el dpto_ccdgo


# Extraccion de los Thetas del M4

Col <- grep("^theta\\d+$", names(M4), value = TRUE)

##################################################################

IPMreg = t(M4[, ..Col])
rownames(IPMreg) = rn 
IPMreg = cbind('IPM' = IPM$IPM, IPMreg)

IPMpred = IPMreg[is.na(IPMreg[,'IPM']),]
IPMreg = IPMreg[!is.na(IPMreg[,'IPM']),]

IPMpredicciones = matrix(NA, ncol = nrow(IPMpred), nrow = nrow(M4))

colnames(IPMpredicciones) = rownames(IPMpred)

Progress = txtProgressBar(min = 2, max = nrow(M4) + 1, style = 3)

for (i in 2:(nrow(M4) + 1)){
  coeficientes = lm(IPMreg[,'IPM'] ~ IPMreg[,i])$coefficients
  IPMpredicciones[i-1,] = coeficientes[1] + coeficientes[2] * IPMpred[,i]
  setTxtProgressBar(Progress,i)
}

close(Progress)

ResultadosIPM = cbind('Media Post' = colMeans(IPMpredicciones),
                      '2.5%' = apply(IPMpredicciones, 2, quantile, probs = 0.025),
                      '97.5%' = apply(IPMpredicciones, 2, quantile, probs = 0.975))

ResultadosIPM

xtable::xtable(ResultadosIPM, digits = 5)
################################################################################

# Unir a la tabla de IPM las medias obtenidas por el modelo

IPM[25:32, 3] = ResultadosIPM[,1]
IPM$dpto_nombre <- toupper(IPM$dpto_nombre)

shp <- sf::st_read("Data/MGN2023_DPTO_POLITICO/MGN_ADM_DPTO_POLITICO.shp", quiet = TRUE)

map_data <- shp %>%
  left_join(IPM, by = c("dpto_ccdgo" = "dpto_ccdgo")) 


p1 <- ggplot(data = map_data) +
  geom_sf(aes(fill = IPM), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = 30, name = "IPM", na.value = "gray90") +
  ggtitle("MAPA COMPLETO INCIDENCIA DE POBREZA MONETARIA ",
          subtitle = "IPM POR DEPARTAMENTO EN COLOMBIA 2018") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )




# Lista de los nuevos departamentos
nuevos_dptos <- c("ARAUCA", "CASANARE", "PUTUMAYO", "AMAZONAS", 
                  "GUAINÍA", "GUAVIARE", "VAUPES", "VICHADA")

# Crear una nueva variable en el mapa para diferenciar los nuevos departamentos
map_data <- map_data %>%
  mutate(nuevo = ifelse(dpto_nombre %in% nuevos_dptos, "Nuevo", "Otros"))

p2 <- ggplot(data = map_data) +
  geom_sf(aes(fill = ifelse(nuevo == "Nuevo", IPM, NA)), color = "white", size = 0.2) +  # Color solo para nuevos
  geom_sf(data = map_data %>% filter(nuevo == "Otros"), fill = "gray90", color = "white", size = 0.2) +  # Color base para los demás
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = 30, name = "IPM", na.value = "gray90") +
  ggtitle("ESTIMACIÓN INCIDENCIA DE POBREZA MONETARIA ",
          subtitle = "IPM EPARTAMENTOS ESTIMADOS EN COLOMBIA 2018") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

p1 + p2




















































































