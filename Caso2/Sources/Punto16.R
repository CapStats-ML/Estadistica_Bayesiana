# Como la base de datos se organiza, el orden de s_ijk es el mismo de la base de datos y por eso se puede
# realizar todo lo que sigue: 

library(data.table)
library(dplyr)


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


data = data |> 
  select(COLE_COD_DEPTO_UBICACION, COLE_DEPTO_UBICACION) |> 
  mutate('S posterior' = read.csv(file = 'Data/Sijk.txt')[,2])

umbral = quantile(x = data$`S posterior`, probs = 0.95)

# ===> ¿La media posterior se considera como atípica?
data$Atip = sapply(X = data$`S posterior`, FUN = function(x) x > umbral)

# ===> Obteniendo las proporciones y el top 5:
prop = data |>
  group_by(COLE_COD_DEPTO_UBICACION, COLE_DEPTO_UBICACION) |> 
  summarise('Proporción' = mean(Atip)) |> 
  arrange(-Proporción) 


prop$COLE_COD_DEPTO_UBICACION  <- ifelse(nchar(prop$COLE_COD_DEPTO_UBICACION ) == 1,
                                         paste0("0", prop$COLE_COD_DEPTO_UBICACION ),
                                         prop$COLE_COD_DEPTO_UBICACION )


shp <- sf::st_read("Data/MGN2023_DPTO_POLITICO/MGN_ADM_DPTO_POLITICO.shp", quiet = TRUE)


head(map_data$Proporción)

map_data <- shp %>%
  left_join(prop, by = c("dpto_ccdgo" = "COLE_COD_DEPTO_UBICACION")) 

# Ordenar por columna Proporcion
map_data <- map_data %>%
  arrange(-Proporción) %>%

head(map_data$Proporción)


map_data$Top <- ifelse(map_data$Proporción > 0.06, "Destacado", "No Destacado")


ggplot(data = map_data) +
  geom_sf(aes(fill = Proporción), color = "white", size = 0.2) +
  scale_fill_gradient2(low = "#eeaf61", mid = "#ff9818", high = "#6a0d83", 
                       midpoint = 0.045, name = "Prop Outliers", na.value = "gray90") +
  ggtitle("PROPORCIÓN DE OUTLIERS EN LAS ESTIMACIONES DEL PUNTAJE GLOBAL",
          subtitle = "OUTLIERS POR DEPARTAMENTO EN COLOMBIA") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, face = "bold", hjust = 0.5, color = "grey40"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


# ggplot(data = map_data) +
#  geom_sf(aes(fill = ifelse(Top == "Destacado", Proporción, NA)), 
#          color = "white", size = 0.2, na.rm = TRUE) + 
#  geom_sf(data = filter(map_data, Top == "No Destacado"), 
#          fill = "#eeaf61", color = "white", size = 0.2) +
#  scale_fill_gradient2(low = "#ff9818", mid = "#dc5858" ,high = "#6a0d83", midpoint = 0.065, name = "MEDIA\nPOSTERIOR") +
#  ggtitle(label = "MAPA DE COLOMBIA: DEPARTAMENTOS DESTACADOS",
#          subtitle = "CINCO MEJORES PUNTAJES DE MATEMATICAS EN 2023") +
#  theme_minimal() + 
#  theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5),text = element_text(size = 10),
#        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5, color = "grey40"),
#        axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())




# ===> Guardando los datos:
write.csv(prop, file = 'datos/Punto 16.csv')
