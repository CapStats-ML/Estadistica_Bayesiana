# Anteriormente ya se desarrollaron los muestreadores de Gibbs y se calcularon las cadenas de log verosimilitud.

setwd("~/REPOS GIT/Estadistica_Bayesiana/Caso2")

library(data.table)                            # Manejo de archivos grandes de datos más eficiente
library(metRology)                             # Distribución t escalada
library(progress)                              # Barras de progreso

# NOTA IMPORTANTE: La llamada a las columnas de un objeto de tipo data.table son diferentes a las de
#                  un objeto de tipo dataframe. Revisar bien cómo se llama a las columnas en cada
#                  caso. Cuando se quiere llamar una sola columna object[[columna]] es la forma correcta
#                  y cuando se quieren llamar varias object[, columnas] es la mejor forma


# ==========> PUNTO 10: ESTIMACIÓN MU:
#                       Siendo mu la media general de la población. 

muEst = matrix(NA, ncol = 4, nrow = 4)
rownames(muEst) = paste('Modelos',1:4)
colnames(muEst) = c('Media posterior', '2.5%', '97.5%', 'CV')
for (i in 1:4){
  mu = fread(paste0('Data/GibbsModelo',i,'.txt'))[['mu']]
  muEst[i,] = c(mean(mu), quantile(mu, 0.025), quantile(mu, 0.975), sd(mu)/mean(mu))
}

xtable::xtable(muEst, digits = 5)            # Salida a LaTeX       

write.csv(muEst, file = 'Data/EstimacionMU.txt')
