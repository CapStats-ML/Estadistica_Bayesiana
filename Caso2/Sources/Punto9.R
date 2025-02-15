# Anteriormente ya se desarrollaron los muestreadores de Gibbs y se calcularon las cadenas de log verosimilitud.

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

# SE NECESITA INFORMACIÓN DE LA BASE DE DATOS:
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


# ============> CÁLCULO DIC: 

# ==> Medias posteriores para todos los parámetros: 
est = lapply(1:4, function(x){
  colMeans(fread(paste0('Data/GibbsModelo', x, '.txt')))
})

names(est) = paste('Modelo', 1:4)

# ==> Cálculo de las log verosimilitudes:
l1 = sum(dt.scaled(x = y, df = 3, log = T, 
                   mean = est$`Modelo 1`[paste0('theta', 1:32)[group]], 
                   sd = sqrt(est$`Modelo 1`["sigma2"])))
l2 = sum(dt.scaled(x = y, df = 3, log = T, 
                   mean = est$`Modelo 2`[paste0('theta', 1:32)[group]], 
                   sd = sqrt(est$`Modelo 2`[paste0('sigma', 1:32)[group]])))
l3 = sum(dt.scaled(x = y, df = 3, log = T, 
                   mean = est$`Modelo 3`[paste0('eps', 1:n)[groupM]], 
                   sd = sqrt(est$`Modelo 3`["kappa"])))
l4 = sum(dt.scaled(x = y, df = 3, log = T, 
                   mean = est$`Modelo 4`[paste0('eps', 1:n)[groupM]], 
                   sd = sqrt(est$`Modelo 4`["kappa"])))
l = c(l1,l2,l3,l4)

# =====> Cálculo del pDIC:
lp = colMeans(fread('Data/LogLike.txt'))
pDIC = 2 * (l - lp)

# =====> Cálculo del DIC:
DIC = -2 * l + 2 * pDIC

# ==> Presentación de los resultados: 

information = matrix(0, ncol = 4, nrow = 4)
colnames(information) = c('pDIC', 'DIC', 'pWAIC', "WAIC")
rownames(information) = paste('Modelo', 1:4)
information[,1] = pDIC
information[,2] = DIC

# ==========> CÁLCULO DEL WAIC:

# ======> MODELO 1:
modelo = fread('Data/GibbsModelo1.txt')
lppd = 0 
pWAIC = 0 
parametros = paste0('theta', group)
sigma = sqrt(modelo[["sigma2"]])

n_cores = detectCores() - 1

cl = makeCluster(n_cores)
clusterExport(cl, c('y', 'modelo', 'parametros', 'sigma', 'dt.scaled'))

tictoc::tic()
vector = parSapply(cl, 1:N, function(i){
  tmp = log(mean(dt.scaled(x = y[i], df = 3, 
                           mean =  modelo[[parametros[i]]], 
                           sd = sigma)))
  
  tmp2 = mean(dt.scaled(x = y[i], df = 3, 
                        mean =  modelo[[parametros[i]]], 
                        sd = sigma, 
                        log = T))
  
  return(c('tmp1' = tmp, 'tmp2' = tmp2))
})
tictoc::toc()
stopCluster(cl)

lppd = sum(vector["tmp1",])
pWAIC = sum(2 * (vector["tmp1",] - vector["tmp2",]))
WAIC = -2 * lppd + 2 * pWAIC

information[1,3:4] = c(pWAIC, WAIC)

# ======> MODELO 2:
modelo = fread('Data/GibbsModelo2.txt')
lppd = 0 
pWAIC = 0 
theta = paste0('theta', group)
sigma = paste0('sigma', group)

cl = makeCluster(n_cores)
clusterExport(cl, c('y', 'modelo', 'theta', 'sigma', 'dt.scaled'))

tictoc::tic()
vector = parSapply(cl, 1:N, function(i){
  tmp = log(mean(dt.scaled(x = y[i], df = 3, 
                           mean =  modelo[[theta[i]]], 
                           sd = sqrt(modelo[[sigma[i]]]))))
  
  tmp2 = mean(dt.scaled(x = y[i], df = 3, 
                        mean =  modelo[[theta[i]]], 
                        sd = sqrt(modelo[[sigma[i]]]), 
                        log = T))
  
  return(c('tmp1' = tmp, 'tmp2' = tmp2))
})
tictoc::toc()
stopCluster(cl)

lppd = sum(vector["tmp1",])
pWAIC = sum(2 * (vector["tmp1",] - vector["tmp2",]))
WAIC = -2 * lppd + 2 * pWAIC

information[2,3:4] = c(pWAIC, WAIC)


# =====> MODELO 3:
modelo = fread('Data/GibbsModelo3.txt')
lppd = 0 
pWAIC = 0 
theta = paste0('eps', groupM)
sigma = sqrt(modelo[['kappa']])

cl = makeCluster(n_cores)
clusterExport(cl, c('y', 'modelo', 'theta', 'sigma', 'dt.scaled'))

tictoc::tic()
vector = parSapply(cl, 1:N, function(i){
  tmp = log(mean(dt.scaled(x = y[i], df = 3, 
                           mean =  modelo[[theta[i]]], 
                           sd = sigma)))
  
  tmp2 = mean(dt.scaled(x = y[i], df = 3, 
                        mean =  modelo[[theta[i]]], 
                        sd = sigma, 
                        log = T))
  
  return(c('tmp1' = tmp, 'tmp2' = tmp2))
})
tictoc::toc()
stopCluster(cl)

lppd = sum(vector["tmp1",])
pWAIC = sum(2 * (vector["tmp1",] - vector["tmp2",]))
WAIC = -2 * lppd + 2 * pWAIC

information[3,3:4] = c(pWAIC, WAIC)

# ======> MODELO 4:
modelo = fread('Data/GibbsModelo4.txt')
lppd = 0 
pWAIC = 0 
theta = paste0('eps', groupM)
sigma = sqrt(modelo[['kappa']])

cl = makeCluster(n_cores)
clusterExport(cl, c('y', 'modelo', 'theta', 'sigma', 'dt.scaled'))

tictoc::tic()
vector = parSapply(cl, 1:N, function(i){
  tmp = log(mean(dt.scaled(x = y[i], df = 3, 
                           mean =  modelo[[theta[i]]], 
                           sd = sigma)))
  
  tmp2 = mean(dt.scaled(x = y[i], df = 3, 
                        mean =  modelo[[theta[i]]], 
                        sd = sigma, 
                        log = T))
  
  return(c('tmp1' = tmp, 'tmp2' = tmp2))
})
tictoc::toc()
stopCluster(cl)

lppd = sum(vector["tmp1",])
pWAIC = sum(2 * (vector["tmp1",] - vector["tmp2",]))
WAIC = -2 * lppd + 2 * pWAIC

information[4,3:4] = c(pWAIC, WAIC)
xtable::xtable(information, digits = 5)            # Salida a LaTeX       



write.csv(information, file = 'Data/Resultados/InfoCriteria.txt')
