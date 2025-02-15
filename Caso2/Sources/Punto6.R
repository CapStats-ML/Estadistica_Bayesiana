library(data.table)
library(parallel)
library(metRology)

# Configuración del directorio de trabajo

# Lectura y filtrado de datos
data = fread('Data/Saber 11 2022-2.TXT', sep = ';')
data = data[ESTU_NACIONALIDAD == 'COLOMBIA' & 
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


r = length(unique(data$COLE_COD_DEPTO_UBICACION))  # Número de departamentos
n = length(unique(data$COLE_COD_MCPIO_UBICACION))   # Número de municipios
group = data$group                                  # Grupos por municipios
groupM = data$groupM                                # Grupos por departamentos
y = data$PUNT_GLOBAL


logVerosimilitud = matrix(NA, ncol = 4, nrow = 10000)  # Se guardan todas las log verosimilitudes en una sola matriz 
colnames(logVerosimilitud) = c(paste('Modelo', 1:4))   # para facilitar el guardado y la graficación
n_cores = detectCores()                                # parallel: Usar todos los núcleos menos uno


# =========================> LOG VEROSIMILITUD [MODELO 1]: 


# ===> Parallel:
cl = makeCluster(n_cores)                             # Crear un clúster de procesos paralelos
clusterExport(cl, c("y", "group", "r", "dt.scaled"))  # Exportar variables necesarias

# ===> Lectura resultados muestreador de Gibbs:
Gibbs = fread('Data/GibbsModelo1.txt')

# ===> Cálculo de la log verosimilitud
tictoc::tic()
cat('\n\n====> Calculando para el modelo 1:\n\n')
logVerosimilitud[,'Modelo 1'] = parApply(cl, Gibbs, MARGIN = 1, FUN = function(x){
  sum(dt.scaled(x = y, df = 3, 
                mean = unlist(x)[paste0('theta', 1:r)][group], 
                sd = sqrt(x[['sigma2']]),
                log = T))
})
tictoc::toc()

# ===> Detener el cluster de parallel
stopCluster(cl)

# =========================> LOG VEROSIMILITUD [MODELO 2]: 

# ===> Parallel:
cl = makeCluster(n_cores)
clusterExport(cl, c('group', 'r', 'y', 'dt.scaled'))

# ===> Lectura de los datos del muestreador de Gibbs:
Gibbs = fread('Data/GibbsModelo2.txt')

# ===> Cálculo de la log verosimilitud:
tictoc::tic()
cat('\n\n====> Calculando para el modelo 2:\n\n')
logVerosimilitud[,"Modelo 2"] = parApply(cl, Gibbs, MARGIN = 1, FUN = function(x){
  sum(dt.scaled(x = y, df = 3, 
                mean = unlist(x)[paste0('theta',1:r)][group], 
                sd = sqrt(unlist(x)[paste0('sigma', 1:r)][group]),
                log = T))
})
tictoc::toc()

# ===> Detener el cluster de parallel
stopCluster(cl)

# =========================> LOG VEROSIMILITUD [MODELO 3]: 

# ===> Parallel:
cl = makeCluster(n_cores)                              # Crear un clúster de procesos paralelos
clusterExport(cl, c("y", "groupM", "n", "dt.scaled"))  # Exportar variables necesarias

# ===> Lectura de los datos del muestreador de Gibbs:
Gibbs = fread('Data/GibbsModelo3.txt')[1:10000,]

# ===> Calculando la log verosimilitud:
tictoc::tic()
cat('\n\n====> Calculando para el modelo 3:\n\n')
logVerosimilitud[,"Modelo 3"] = parApply(cl, Gibbs, MARGIN = 1, FUN = function(x){
  sum(dt.scaled(x = y, df = 3, 
                mean = unlist(x)[paste0('eps', 1:n)][groupM], 
                sd = sqrt(x[['kappa']]), 
                log = TRUE))
})
tictoc::toc()

# Detener el clúster
stopCluster(cl)

# =========================> LOG VEROSIMILITUD [MODELO 4]: 

# ===> Parallel:
cl = makeCluster(n_cores)                              # Crear un clúster de procesos paralelos
clusterExport(cl, c("y", "groupM", "n", "dt.scaled"))  # Exportar variables necesarias

# ===> Lectura de los datos del muestreador de Gibbs:
Gibbs = fread('Data/GibbsModelo4.txt')[1:10000,]

# ===> Calculando la log verosimilitud:
tictoc::tic()
cat('\n\n====> Calculando para el modelo 4:\n\n')
logVerosimilitud[,"Modelo 4"] = parApply(cl, Gibbs, MARGIN = 1, FUN = function(x){
  sum(dt.scaled(x = y, df = 3, 
                mean = unlist(x)[paste0('eps', 1:n)][groupM], 
                sd = sqrt(x[['kappa']]), 
                log = TRUE))
})
tictoc::toc()

# Detener el clúster
stopCluster(cl)


# =========================> GUARDAR LOS RESULTADOS: 
fwrite(x = logVerosimilitud, file = 'Data/logVerosimilitud.txt')

# Tabla para las medias de las log verosimilitudes

tabla = data.table(Modelo = c('Modelo 1', 'Modelo 2', 'Modelo 3', 'Modelo 4'),
                   Media = colMeans(logVerosimilitud))

xtable::xtable(tabla, digits = 5)

# =========================> GRAFICAR LOS RESULTADOS:

col <- c("#fb9062", "#ee5d6c", "#ce4993", "#6a0d83")

par(mfrow = c(2,2), tcl  = 0.5, mar = c(4, 4, 4, 4), mgp = c(1.5, 0.5, 0),
    mai = c(0.4, 0.4, 0.3, 0.2), oma = c(1, 0, 1, 0))

for(i in 1:4){
  plot(logVerosimilitud[,i], col = adjustcolor(col[i], alpha.f = 0.6), pch = 1, cex = 0.6,
       xlab = NA, ylab = "Log Verosimilitud",
       main = paste("Modelo", i), font.main = 2, cex.main = 0.9, family = "sans", 
       font.lab = 2, cex.lab = 0.8, cex.axis = 0.7)
  
  abline(h = mean(logVerosimilitud[,i]), col = 'white', lwd = 2)
  
  legend("topright", legend = paste("Media:", round(mean(logVerosimilitud[,i]), 2)), 
         bty = "n", text.col = "black", cex = 0.8, text.font = 2)
}

