# Anteriormente ya se desarrollaron los muestreadores de Gibbs y se calcularon las cadenas de log verosimilitud.

setwd('../Downloads/')                         # Seteando el directorio de trabajo
library(data.table)                            # Manejo de archivos grandes de datos más eficiente
library(metRology)                             # Distribución t escalada
library(progress)                              # Barras de progreso

# NOTA IMPORTANTE: La llamada a las columnas de un objeto de tipo data.table son diferentes a las de
#                  un objeto de tipo dataframe. Revisar bien cómo se llama a las columnas en cada
#                  caso. Cuando se quiere llamar una sola columna object[[columna]] es la forma correcta
#                  y cuando se quieren llamar varias object[, columnas] es la mejor forma


# ==========> PUNTO 9: CALCULAR EL DIC Y EL WAIC DE CADA MODELO:
#                      Para calcular estos dos criterios de información se necesitan tener las estimaciones
#                      de todos los modelos y recordar que: 
#                         - Modelo 1: Medias específicas por departamento.
#                         - Modelo 2: Medias y varianzas específicas por departamento.
#                         - Modelo 3: Medias específicas por municipio y departamento. 
#                         - Modelo 4: Medias específicas por municipio y departamento. 
#                      Además de que ahora se necesita información adicional del set de datos.

# ===> ESTIMACIONES:
est = list()
for (i in 1:4){
  est[[paste('Modelo',i)]] = colMeans(fread(paste0('Data/GibbsModelo',i,'.txt')))
}

# ===> LOG LIKELIHOOD CHAIN:
logLike = fread('Data/LogLike.txt')

# ===> INFORMACIÓN ADICIONAL DEL SET DE DATOS:
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


# ===> CÁLCULO DEL pDIC

information = matrix(NA, nrow = 4, ncol = 4)
colnames(information) = c('pDIC','DIC', 'pWAIC','WAIC')
rownames(information) = paste('Modelo', 1:4)

l1 = sum(dt.scaled(x = y, df = 3,
                   mean = est$`Modelo 1`[paste0('theta',1:32)][group],
                   sd = est$`Modelo 1`["sigma2"], 
                   log = T))
information["Modelo 1","pDIC"] = 2 * (l1 - mean(logLike$`Modelo 1`))

l2 = sum(dt.scaled(x = y, df = 3, 
                   mean = est$`Modelo 2`[paste0('theta',1:32)][group],
                   sd = est$`Modelo 2`[paste0('sigma',1:32)][group], 
                   log = T))
information["Modelo 2","pDIC"] = 2 * (l2 - mean(logLike$`Modelo 2`))

l3 = sum(dt.scaled(x = y, df = 3, 
                   mean = est$`Modelo 3`[paste0('eps',1:n)][groupM],
                   sd = est$`Modelo 3`["kappa"], 
                   log = T))

information["Modelo 3","pDIC"] = 2 * (l3 -  mean(logLike$`Modelo 3`))

l4 = sum(dt.scaled(x = y, df = 3, 
                   mean = est$`Modelo 4`[paste0('eps',1:1112)][groupM],
                   sd = est$`Modelo 4`["kappa"], 
                   log = T)) 
information["Modelo 4","pDIC"] = 2 * (l4 - mean(logLike$`Modelo 4`))

# ==> CÁLCULO DEL DIC: 
information[,"DIC"] = -2 * c(l1, l2, l3, l4) + 2 * information[,"pDIC"]

# ===> CÁLCULO DEL WAIC:
library(foreach)
library(doParallel)

# Configurar el cluster paralelo
cl <- makeCluster(detectCores())
registerDoParallel(cl)

medias = list(M1 = fread('Data/GibbsModelo1.txt')[, paste0('theta', 1:32)], 
              M2 = fread('Data/GibbsModelo2.txt')[, paste0('theta', 1:32)], 
              M3 = fread('Data/GibbsModelo3.txt')[, paste0('eps', 1:1112)], 
              M4 = fread('Data/GibbsModelo4.txt')[, paste0('eps', 1:1112)])

varianzas = list(M1 = fread('Data/GibbsModelo1.txt')[['sigma2']], 
                 M2 = fread('Data/GibbsModelo2.txt')[,paste0('sigma',1:32)], 
                 M3 = fread('Data/GibbsModelo3.txt')[['kappa']],
                 M4 = fread('Data/GibbsModelo4.txt')[['kappa']])


# Exportar variables y funciones necesarias a los workers
clusterExport(cl, varlist = c("y", "group", "groupM", "medias", "varianzas", "dt.scaled"))

# Calcular lppd y pWAIC en paralelo
resultados <- foreach(i = 1:N, .combine = function(a, b) {
  list(
    lppd = a$lppd + b$lppd,
    pWAIC = a$pWAIC + b$pWAIC
  )
}, .init = list(lppd = numeric(4), pWAIC = numeric(4)), .packages = c("data.table", "metRology")) %dopar% {
  
  # Cálculo de componentes para cada observación i
  tmp1 = metRology::dt.scaled(x = y[i], df = 3, 
                              mean = medias$M1[[group[i]]], 
                              sd = sqrt(varianzas$M1))
  tmp2 = metRology::dt.scaled(x = y[i], df = 3, 
                              mean = medias$M2[[group[i]]], 
                              sd = sqrt(varianzas$M2[[group[i]]]))
  tmp3 = metRology::dt.scaled(x = y[i], df = 3, 
                              mean = medias$M3[[groupM[i]]], 
                              sd = sqrt(varianzas$M3))
  tmp4 = metRology::dt.scaled(x = y[i], df = 3, 
                              mean = medias$M4[[groupM[i]]], 
                              sd = sqrt(varianzas$M4))
  
  # Contribución a lppd
  lppd_i = log(c(mean(tmp1), mean(tmp2), mean(tmp3), mean(tmp4)))
  
  # Cálculo de términos para pWAIC
  tmp12 = metRology::dt.scaled(x = y[i], df = 3, 
                               mean = medias$M1[[group[i]]], 
                               sd = sqrt(varianzas$M1), log = TRUE)
  tmp22 = metRology::dt.scaled(x = y[i], df = 3, 
                               mean = medias$M2[[group[i]]], 
                               sd = sqrt(varianzas$M2[[group[i]]]), log = TRUE)
  tmp32 = metRology::dt.scaled(x = y[i], df = 3, 
                               mean = medias$M3[[groupM[i]]], 
                               sd = sqrt(varianzas$M3), log = TRUE)
  tmp42 = metRology::dt.scaled(x = y[i], df = 3, 
                               mean = medias$M4[[groupM[i]]], 
                               sd = sqrt(varianzas$M4), log = TRUE)
  
  pWAIC_i = 2 * (log(c(mean(tmp1), mean(tmp2), mean(tmp3), mean(tmp4))) - 
                   c(mean(tmp12), mean(tmp22), mean(tmp32), mean(tmp42)))
  
  # Retornar resultados parciales
  list(lppd = lppd_i, pWAIC = pWAIC_i)
}

# Detener el cluster
stopCluster(cl)

# Asignar resultados a la matriz de información
information[, "pWAIC"] <- resultados$pWAIC
information[, "WAIC"] <- -2 * resultados$lppd + 2 * resultados$pWAIC

write.csv(information, file = 'Data/InfoCriteria.txt')
