# Anteriormente ya se desarrollaron los muestreadores de Gibbs y se calcularon las cadenas de log verosimilitud.

setwd('../Downloads/')                         # Seteando el directorio de trabajo
library(data.table)                            # Manejo de archivos grandes de datos más eficiente
library(metRology)                             # Distribución t escalada
library(progress)                              # Barras de progreso

# NOTA IMPORTANTE: La llamada a las columnas de un objeto de tipo data.table son diferentes a las de
#                  un objeto de tipo dataframe. Revisar bien cómo se llama a las columnas en cada
#                  caso. Cuando se quiere llamar una sola columna object[[columna]] es la forma correcta
#                  y cuando se quieren llamar varias object[, columnas] es la mejor forma


# ==========> PUNTO 8: ERROR ESTÁNDAR DE MONTECARLO:
#                      No estoy 100% seguro de que se deba incluir el error estándar de la log
#                      verosimilitud ni el tamaño efectivo de la log verosimilitud porque el profesor
#                      dice: "...De los parámetros..." Pero en la clase de modelos multinivel sí incluía
#                      la log verosimilitud, pero pues quitarla es sencillo.

eeLogLik = apply(X = fread('Data/LogLike.txt'), MARGIN = 2, sd)/neffLogLik
ee = list()
for (i in 1:4){
  ee[[paste('Modelo',i)]] = c(apply(X = fread(paste0('Data/GibbsModelo',i,'.txt')), 
                                    MARGIN = 2, 
                                    sd)/neff[[i]][1:(length(neff[[i]])-1)], 
                              'll' = eeLogLik[i])
}

tabla_ee = do.call(rbind, lapply(X = ee, function(x) summary(x)[1:5]))
xtable::xtable(tabla_ee, digits = 5)            # Salida a LaTeX       
write.csv(tabla_ee, 'Data/StandardError.txt')
