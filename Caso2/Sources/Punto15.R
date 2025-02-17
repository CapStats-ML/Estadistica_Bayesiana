# ====> ESTADISTICA BASEYIANA: CASO DE ESTUDIO #2

setwd("~/REPOS GIT/Estadistica_Bayesiana/Caso2")

# Cargar librerias

library(data.table)                            # Manejo de archivos grandes de datos más eficiente
library(metRology)                             # Distribución t escalada
library(progress)                              # Barras de progreso
library(cluster)                               # K-means
library(factoextra)                            # Visualización de K-means
library(sf)                                    # Manipulación de datos geoespaciales
library(dplyr)                                 # Manipulación de datos
library(readxl)
library(patchwork)

# =====> PUNTO 13: Calcular la media posterior y un intervalo de credibilidad al 95%
#               de la incicendencia monetaria de los departamentos, para todos los departamentos
#               que no fueron medidos por el DANE, por medio de una regresion lineal simple de la IPM
#               frente a las medias especificas de los departamentos de M4. 
#               Presentar los resultados tabularmente y visualmente a través de un mapa


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
rn <- (unique(data$COLE_DEPTO_UBICACION))      # Nombre de departamentos
rc <- (unique(data$COLE_COD_DEPTO_UBICACION))      # Codigo de departamentos)
rc  <- ifelse(nchar(rc ) == 1, paste0("0", rc ), rc )

n1 = length(unique(data$COLE_COD_MCPIO_UBICACION))   # Número de municipios
n2 = unique(cbind(data$COLE_MCPIO_UBICACION, data$COLE_COD_MCPIO_UBICACION))       # Número de municipios
n2[,2] <- ifelse(nchar(n2[,2]) == 4, paste0("0", n2[,2]), n2[,2])

group = data$group                                  # Grupos por municipios
groupM = data$groupM                                # Grupos por departamentos
groupMxD = c()                                      # Grupos de municipios por departamento
MxD = c()                                           # Municipios por departamento
for (i in unique(group)){
  groupMxD = c(groupMxD,rep(i,length(unique(groupM[group == i]))))
  MxD = c(MxD, length(unique(groupM[group == i])))
}
y = data$PUNT_GLOBAL


M4 = fread('Data/GibbsModelo4.txt') # Importe de Resultados del modelo 4

MEN <- read_csv("Data/MEN.csv")
MEN <- MEN %>%
  filter(AÑO == 2022)


MEN$CÓDIGO_MUNICIPIO <- as.character(MEN$CÓDIGO_MUNICIPIO)
MEN$CÓDIGO_MUNICIPIO <- ifelse(nchar(MEN$CÓDIGO_MUNICIPIO) == 4,
                               paste0("0", MEN$CÓDIGO_MUNICIPIO),
                               MEN$CÓDIGO_MUNICIPIO)


MEN <- as.data.table(MEN)
#MEN <- MEN[CÓDIGO_MUNICIPIO %in% n2[,2]]
MEN <- MEN[order(CÓDIGO_MUNICIPIO)] 


Col <- grep("^eps\\d+$", names(M4), value = TRUE)


MENreg = cbind(n2[,2], t(M4[, ..Col]))

MENreg1 <- as.data.frame(MENreg) %>%
  left_join(MEN[, .(CÓDIGO_MUNICIPIO, COBERTURA_NETA_SECUNDARIA)], 
            by = c('V1' = 'CÓDIGO_MUNICIPIO'))

row.names(MENreg1) <- MENreg1$V1
MENreg1 <- MENreg1[,-1]

MENreg1 <- MENreg1[, c('COBERTURA_NETA_SECUNDARIA', setdiff(names(MENreg1), 'COBERTURA_NETA_SECUNDARIA'))]


MENpred = MENreg1[is.na(MENreg1[,'COBERTURA_NETA_SECUNDARIA']),]
MENpred <- MENpred %>% mutate(across(everything(), as.numeric))

MENreg = MENreg1[!is.na(MENreg1[,'COBERTURA_NETA_SECUNDARIA']),]
MENreg[, 'COBERTURA_NETA_SECUNDARIA'] <- as.numeric(MENreg[, 'COBERTURA_NETA_SECUNDARIA'])


# Inicializar la matriz de predicciones
MENpredicciones = matrix(NA, ncol = nrow(MENpred), nrow = nrow(M4))
colnames(MENpredicciones) = rownames(MENpred)

# Barra de progreso
Progress = txtProgressBar(min = 2, max = nrow(M4) + 1, style = 3)

# Bucle de regresión
for (i in 2:(nrow(M4) + 1)){
  coeficientes = lm(MENreg[,'COBERTURA_NETA_SECUNDARIA'] ~ MENreg[,i])$coefficients
  MENpredicciones[i-1,] = coeficientes[1] + coeficientes[2] * MENpred[,i]
  setTxtProgressBar(Progress,i)
}


close(Progress)

ResultadosIPM = cbind('Media Post' = colMeans(IPMpredicciones),
                      '2.5%' = apply(IPMpredicciones, 2, quantile, probs = 0.025),
                      '97.5%' = apply(IPMpredicciones, 2, quantile, probs = 0.975))

ResultadosIPM




































