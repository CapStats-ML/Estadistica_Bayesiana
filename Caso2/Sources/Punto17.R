library(data.table)
library(progress)
library(dplyr)

setwd('../Downloads/')

# Lectura y filtrado de datos
data <- fread('datos/Saber 11 2022-2.TXT', sep = ';')
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



# ==========> CARGANDO EL MODELO 4: Dentro de la matriz M4 las medias de los municipios, se encuentran
#                                   nombradas de la siguiente forma: eps1, eps2,..., eps1112 para los
#                                   municipios en el orden en que quedan en la base de datos después del
#                                   setorder de más arriba. Sólo se necesitan las medias.

# ====> Medias posteriores del modelo: 
mediasPosteriores = colMeans(fread('datos/GibbsModelo4'))

# ====> Medias globales por municipio:
medias = data |> 
  group_by(COLE_COD_MCPIO_UBICACION) |> 
  summarise(media = mean(PUNT_GLOBAL))

# ====> Se guardarán los ppps junto con las medias globales: 
medias$ppp = 0
estudiantesMun = table(groupM)

# ====> Calculando los ppps:
library(parallel)
n_cores = detectCores()
cl = makeCluster(n_cores)
clusterExport(cl, c('rt.scaled', 'estudiantesMun', 'mediasPosteriores', 'medias'))
medias$ppp = parSapply(cl, 1:1112, function(i){
  ppp = 0
  for(j in 1:50000){
    media = mean(rt.scaled(n = estudiantesMun[i], 
                           df = 3, 
                           mean = mediasPosteriores[paste0('eps',i)], 
                           sd = sqrt(mediasPosteriores["kappa"])))
    ppp = ppp + (media < medias[i,"media"])
  } 
  return(ppp/50000)
})
stopCluster(cl)

medias$`No. Estudiantes` = estudiantesMun
write.csv(x = medias, file = 'datos/Punto 17.csv')

# ==========> GRÁFICA 1: BOXPLOT. Se pretende revisar la distribución general de los ppps para las medias
#                                 de los municipios.

png('datos/plots/P17. Boxplot.png', width = 4, height = 6, res = 180, units = 'in')
par(mar = c(1,2,2.5,0.5))
colores <- c("#F4A582", "#FDD9A0", "#E7819E", "#C065D1", "#662282")
boxplot(medias$ppp, col = adjustcolor(colores[1], 0.3),
        border = colores[1], axes = F, pch = '*')
box(bty = 'l')
axis(2, cex.axis = 00.7)
abline(h = 0:10 * 0.1, lty = 'dashed', lwd = ifelse(0:10 * 0.1 == 0.5, 2, 1),
       col = ifelse(0:10 * 0.1 == 0.5, 'red', 'gray'))
boxplot(medias$ppp, col = adjustcolor(colores[1], 0.3),
        border = colores[1], axes = F, add = T, pch = '*')
title(main = 'Valores ppp para la\nmedia por departamento', cex.main = 1.2)
dev.off()

# ==========> GRÁFICA 2: VIOLIN. Se pretende observar más detalladamente la distribución de los valores 
#                                ppp.
#                                
png('datos/plots/P17. Violín.png', width = 6, height = 8, res = 180, units = 'in')
par(mar = c(3.5,3.5,3.5,1))
densidad = density(medias$ppp, cut = T, n = 600)
plot(x = NA, y = NA, xlim = c(-max(densidad$y) + 0.6, max(densidad$y) + 0.2) + 0.6,
     ylim = c(-0.05,1.05), axes = F, ylab = '', xlab = '')
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],           # Color de fondo
     col = adjustcolor("lightgray",0.5), border = NA)
axis(1, at = -3:5, labels = -4:4, tick = F, line = -0.7)
axis(2, tick = F, las = 2, line = -0.7)
title(xlab = 'Densidad', ylab = 'ppp', line = 1.8)
title(main = 'Valores ppp para la media por departamento')
grid(col = 'white', lty = 'solid', lwd = 1.5)
polygon(y = c(densidad$x, rev(densidad$x)), x = c(densidad$y,-rev(densidad$y)) + 1,
        border = colores[1], lwd = 1.2, col = adjustcolor(colores[1], 0.2))
boxplot(medias$ppp, axes = F, add = T, pch = '*', col = 'white')
dev.off()


# ==========> GRÁFICA 3: MEDIAS. Se comparan las medias globales a as medias posteriores.
#                                 de los municipios.

png('datos/plots/P17. Medias vs medias.png', width = 8, height = 8, res = 180, units = 'in')
par(mar = c(3.5,3.5,3.5,1))
plot(x = medias$media, y = mediasPosteriores[paste0('eps', 1:1112)], 
     axes = F, ylim = c(180,300), xlim = c(c(180,300)), type = 'n', 
     xlab = '', ylab = '')
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],           # Color de fondo
     col = adjustcolor("lightgray",0.5), border = NA)
axis(1, tick = F, line = -0.7)
axis(2, tick = F, las = 2, line = -0.7)
title(xlab = 'Medias globales', ylab = 'Medias posteriores', line = 1.8)
title(main = 'Comparación medias globales y posteriores\npor municipio')
grid(col = 'white', lty = 'solid', lwd = 1.5)

points(x = medias$media, y = mediasPosteriores[paste0('eps', 1:1112)], 
       pch = 21, col = colores[1], bg = adjustcolor(colores[1], 0.2), cex = 0.8)
abline(a = 0, b = 1, col = 'red', lwd = 1.2)
dev.off()

