# =============================== MAPA DE COLOMBIA ======================================
# Este script carga y visualiza dos mapas de Colombia: uno con la media del puntaje global
# y otro con la incidencia de pobreza monetaria (IPM), colocados en una grilla.
# ======================================================================================

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(sf)
library(readxl)
library(data.table)
library(patchwork)  # Para combinar gráficos

# Leer los datos del examen Saber 11
data <- fread('Saber 11 2022-2.TXT', sep = ';')

data <- data[ESTU_NACIONALIDAD == 'COLOMBIA' & 
               ESTU_PAIS_RESIDE == 'COLOMBIA' & 
               ESTU_ESTADOINVESTIGACION == 'PUBLICAR' & 
               COLE_DEPTO_UBICACION != 'SAN ANDRES' & 
               !is.na(COLE_MCPIO_UBICACION) & 
               !is.na(COLE_DEPTO_UBICACION) & 
               !is.na(PUNT_GLOBAL) & 
               !is.na(COLE_COD_DEPTO_UBICACION)]

# Convertir COLE_COD_DEPTO_UBICACION a caracter para que coincida con dpto_ccdgo
data[, COLE_COD_DEPTO_UBICACION := as.character(COLE_COD_DEPTO_UBICACION)]

# Calcular la media del puntaje global por departamento
media_por_departamento <- data[, .(Media_Punt_Global = mean(PUNT_GLOBAL, na.rm = TRUE)),
                               by = COLE_COD_DEPTO_UBICACION]

media_por_departamento$COLE_COD_DEPTO_UBICACION  <- ifelse(nchar(media_por_departamento$COLE_COD_DEPTO_UBICACION ) == 1,
                                                           paste0("0", media_por_departamento$COLE_COD_DEPTO_UBICACION ),
                                                           media_por_departamento$COLE_COD_DEPTO_UBICACION )


# Leer los datos del Índice de Pobreza Monetaria (IPM)
IPM <- read_excel("IPM.xlsx", col_types = c("text", "text", "numeric"))
names(IPM) <- c("dpto_nombre", "dpto_ccdgo", "IPM")

# Leer el shapefile con los límites departamentales
shp <- sf::st_read("MGN2023_DPTO_POLITICO/MGN_ADM_DPTO_POLITICO.shp", quiet = TRUE)

# Convertir nombres de departamentos en mayúsculas para que coincidan en ambas bases de datos
IPM$dpto_nombre <- toupper(IPM$dpto_nombre)

# Unir los datos del IPM y del puntaje global con los datos espaciales
map_data <- shp %>%
  left_join(IPM, by = c("dpto_ccdgo" = "dpto_ccdgo")) %>%
  left_join(media_por_departamento, by = c("dpto_ccdgo" = "COLE_COD_DEPTO_UBICACION"))

# Convertir valores a numéricos
map_data$IPM <- as.numeric(map_data$IPM)
map_data$Media_Punt_Global <- as.numeric(map_data$Media_Punt_Global)

# Crear el mapa del puntaje global por departamento
map_puntaje <- ggplot(data = map_data) +
  geom_sf(aes(fill = Media_Punt_Global), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = mean(map_data$Media_Punt_Global, na.rm = TRUE) - 5,
                       name = "Media\nPuntaje Global", na.value = "gray90") +
  ggtitle("PUNTAJE GLOBAL PROMEDIO POR DEPARTAMENTO",
          subtitle = "PRUEBA SABER 11 EN COLOMBIA") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Crear el mapa del IPM por departamento
map_ipm <- ggplot(data = map_data) +
  geom_sf(aes(fill = IPM), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = 30, name = "IPM", na.value = "gray90") +
  ggtitle("INCIDENCIA DE POBREZA MONETARIA POR DEPARTAMENTO",
          subtitle = "IPM EN COLOMBIA 2018") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Mostrar los dos mapas en una grilla
map_puntaje + map_ipm
