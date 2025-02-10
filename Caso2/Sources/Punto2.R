# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(readr)
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
               !is.na(COLE_COD_DEPTO_UBICACION) &
               !is.na(COLE_COD_MCPIO_UBICACION)]

# Convertir COLE_COD_DEPTO_UBICACION a caracter para que coincida con dpto_ccdgo
data[, COLE_COD_MCPIO_UBICACION := as.character(COLE_COD_MCPIO_UBICACION)]

# Verificar existencia de algunos municipios 
#municipios_faltantes <- c("94884", "94885", "94886", "91430", "91530", "91536", "97161", "97666", "97777", "88001", "88564")
#v <- data[COLE_COD_MCPIO_UBICACION %in% municipios_faltantes]

# Calcular la media del puntaje global por departamento
media_mcpio <- data[, .(Media_Punt_Global = mean(PUNT_GLOBAL, na.rm = TRUE)),
                               by = COLE_COD_MCPIO_UBICACION]

media_mcpio$ESTU_COD_MCPIO_PRESENTACION <- as.character(media_mcpio$COLE_COD_MCPIO_UBICACION)

media_mcpio$COLE_COD_MCPIO_UBICACION  <- ifelse(nchar(media_mcpio$COLE_COD_MCPIO_UBICACION ) == 4,
                                                  paste0("0", media_mcpio$COLE_COD_MCPIO_UBICACION ),
                                                  media_mcpio$COLE_COD_MCPIO_UBICACION )


# Datos de cobertura neta secundaria 
MEN <- read_csv("MEN.csv")

#Filtrar MEN por MEN$AÑO

MEN <- MEN %>%
  filter(AÑO == 2022)


# Leer el shapefile con los límites departamentales
shp1 <- sf::st_read("MGN2023_MCPIO_POLITICO/Municipios.shp", quiet = TRUE)

#Concatenar DPTO_CCDGO y MPIO_CCDGO para obtener todo el codigo del departamente 

shp1$COD_MUN <- paste(shp1$DPTO_CCDGO, shp1$MPIO_CCDGO, sep = "")

shp1$COD_MUN <- as.character(shp1$COD_MUN)
MEN$CÓDIGO_MUNICIPIO <- as.character(MEN$CÓDIGO_MUNICIPIO)

shp1$COD_MUN <- ifelse(nchar(shp1$COD_MUN) == 4,
                       paste0("0", shp1$COD_MUN),
                       shp1$COD_MUN)

MEN$CÓDIGO_MUNICIPIO <- ifelse(nchar(MEN$CÓDIGO_MUNICIPIO) == 4,
                               paste0("0", MEN$CÓDIGO_MUNICIPIO),
                               MEN$CÓDIGO_MUNICIPIO)




# Unir los datos del MAN y del puntaje global con los datos espaciales
map_data1 <- shp1 %>%
  left_join(MEN, by = c("COD_MUN" = "CÓDIGO_MUNICIPIO")) %>% 
  left_join(media_mcpio, by = c("COD_MUN" = "COLE_COD_MCPIO_UBICACION"))


# Convertir valores a numéricos
map_data1$COBERTURA_NETA_SECUNDARIA <- as.numeric(map_data1$COBERTURA_NETA_SECUNDARIA)
map_data1$Media_Punt_Global <- as.numeric(map_data1$Media_Punt_Global)

# Crear el mapa del puntaje global por departamento
map_puntaje1 <- ggplot(data = map_data1) +
  geom_sf(aes(fill = Media_Punt_Global), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = mean(map_data1$Media_Punt_Global, na.rm = TRUE) - 25,
                       name = "Media\nPuntaje Global", na.value = "gray90") +
  ggtitle("PUNTAJE GLOBAL PROMEDIO POR MUNICIPIO",
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
map_MEN1 <- ggplot(data = map_data1) +
  geom_sf(aes(fill = COBERTURA_NETA_SECUNDARIA), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = 40, name = "CNS", na.value = "gray90") +
  ggtitle("COBERTURA NETA SECUNDARIA POR MUNICIPIO",
          subtitle = "CNS: COLOMBIA 2022") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Mostrar los dos mapas en una grilla
map_puntaje1 + map_MEN1




