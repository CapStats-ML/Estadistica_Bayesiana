install.packages("circlize")
library(circlize)

#####

df <- do.call(rbind, lapply(seq_along(l), function(i) {
  cbind(Lista = i, l[[i]])
}))

df$Lista <- factor(df$Lista, levels = 1:9, labels = 2015:2023); df

departamentos <- unique(cbind(dat$ESTU_COD_RESIDE_DEPTO, dat$ESTU_DEPTO_RESIDE))
departamentos[,1] <- ifelse(nchar(departamentos[,1]) == 1,
                            paste0("0", departamentos[,1]), departamentos[,1])
departamentos <- as.data.frame(departamentos)
colnames(departamentos) <- c("codigo", "nombre")

# Incluir la abreviacion en departamentos 

abreviacion = c("LAG", "ANT", "CUN", "ATL", "NSA", "VAL", "BOG", "CAL", "COR", "HUI",
                "MET", "BOY", "TOL", "BOL", "ARA", "VIC", "MAG", "SUC", "CAQ", "CAU",
                "RIS", "QUI", "CHO", "CAS", "SAN", "CES", "STD", "NAR", "PUT", "GUA",
                "AMA", "GUV", "VAU")

departamentos$abreviacion = abreviacion; departamentos


unique_years <- unique(df$Lista); unique_years


# Definir colores fijos para departamentos y clusters
departamentos_colores <- setNames(rainbow(33), departamentos$abreviacion)  # Asociar colores con nombres
clusters_colores <- setNames(rainbow(5, start = 0.6), paste0("Clust-", 1:5))
colores <- c(departamentos_colores, clusters_colores)

# Crear el gráfico múltiple
par(mfrow = c(3, 3), bg = "white")

set.seed(123)

for (yr in unique_years) {
  # Filtrar datos para el año actual
  year_data <- subset(df, Lista == yr)
  
  # Crear enlaces entre departamentos y clusters
  links <- data.frame(
    source = year_data$dpto_ccdgo,  # Usar el código original
    target = paste("Clust", year_data$Cluster, sep = "-"),
    value = 1  # Puedes usar conteos u otros valores
  )
  
  # Sustituir códigos por nombres de departamentos
  links$source <- ifelse(
    links$source %in% departamentos$codigo,  # Verificar si el código está en la lista
    departamentos[match(links$source, departamentos$codigo), "abreviacion"],  # Sustituir por el nombre
    links$source
  )
  
  # Generar el diagrama
  circos.clear()
  chordDiagram(
    links,
    grid.col = colores,  # Aplicar colores unificados
    transparency = 0.5,
    annotationTrack = "grid",
    preAllocateTracks = 1
  )
  
  # Personalizar etiquetas
  circos.trackPlotRegion(
    track.index = 1,
    panel.fun = function(x, y) {
      circos.text(
        CELL_META$xcenter,
        CELL_META$ylim[1], 
        CELL_META$sector.index, 
        facing = "clockwise", 
        niceFacing = TRUE, 
        adj = c(0, 0.5),
        cex = 0.7  # Reducir tamaño de letra
      )
    }, bg.border = NA
  )
}


# Graficar solo 2023 

year_data <- subset(df, Lista == 2023)

# Crear enlaces entre departamentos y clusters

links <- data.frame(
  source = year_data$dpto_ccdgo,  # Usar el código original
  target = paste("Clust", year_data$Cluster, sep = "-"),
  value = 1  # Puedes usar conteos u otros valores
)

# Sustituir códigos por nombres de departamentos

links$source <- ifelse(
  links$source %in% departamentos$codigo,  # Verificar si el código está en la lista
  departamentos[match(links$source, departamentos$codigo), "abreviacion"],  # Sustituir por el nombre
  links$source
)










