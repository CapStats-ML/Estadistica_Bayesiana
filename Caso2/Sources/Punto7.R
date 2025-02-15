# Anteriormente ya se desarrollaron los muestreadores de Gibbs y se calcularon las cadenas de log verosimilitud.

setwd('../Downloads/')                         # Seteando el directorio de trabajo
library(data.table)                            # Manejo de archivos grandes de datos más eficiente
library(metRology)                             # Distribución t escalada
library(progress)                              # Barras de progreso

# NOTA IMPORTANTE: La llamada a las columnas de un objeto de tipo data.table son diferentes a las de
#                  un objeto de tipo dataframe. Revisar bien cómo se llama a las columnas en cada
#                  caso. Cuando se quiere llamar una sola columna object[[columna]] es la forma correcta
#                  y cuando se quieren llamar varias object[, columnas] es la mejor forma

# ==========> PUNTO 7: TAMAÑO EFECTIVO DE MUESTRA:
#                      Como la log verosimilitud se calculó luego, se encuentra separada de los archivos
#                      que contienen la simulación de los parámetros mediante el muestreador de Gibbs. 
#                      Por esa razón el punto se desarrolla de la forma en que se desarrolla a continuación.

neffLogLik = coda::effectiveSize(fread('Data/LogLike.txt'))
neff = list()
for (i in 1:4){
  neff[[paste('Modelo',i)]] = c(coda::effectiveSize(fread(paste0('Data/GibbsModelo',i,'.txt'))), 'll' = neffLogLik[i])
}

tabla_neff = do.call(rbind, lapply(X = neff, summary))
xtable::xtable(tabla_neff)                      # Salida a LaTeX
write.csv(tabla_neff, 'Data/EffectSize.txt')
