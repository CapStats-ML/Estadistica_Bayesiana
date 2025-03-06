library(MASS)
library(dplyr)
library(tibble)
library(ggplot2)
library(caret)
library(mvtnorm)
library(progress)
library(tictoc)
library(data.table)
library(patchwork)
library(showtext)

# Agregar fuentes personalizadas
font_add(family = "Phudu-Light", regular = "C:/Users/capst/Downloads/TextSamu/Typo/Phudu-Light.ttf")
font_add(family = "Phudu-Regular", regular = "C:/Users/capst/Downloads/TextSamu/Typo/Phudu-Regular.ttf")
font_add(family = "Phudu-Medium", regular = "C:/Users/capst/Downloads/TextSamu/Typo/Phudu-Medium.ttf")
font_add(family = "Phudu-SemiBold", regular = "C:/Users/capst/Downloads/TextSamu/Typo/Phudu-SemiBold.ttf")
font_add(family = "Phudu-Bold", regular = "C:/Users/capst/Downloads/TextSamu/Typo/Phudu-Bold.ttf")
font_add(family = "Phudu-ExtraBold", regular = "C:/Users/capst/Downloads/TextSamu/Typo/Phudu-ExtraBold.ttf")
font_add(family = "Phudu-Black", regular = "C:/Users/capst/Downloads/TextSamu/Typo/Phudu-Black.ttf")

# Activar showtext
showtext_auto()

par(family = 'Phudu-Regular', font = 2) 

# Primera columna: altura de la planta
# Segunda columna: período de medición
# Tercera columna: nivel de pH

# Cargar los datos
Tplant <- read.table("https://www2.stat.duke.edu/~pdh10/FCBS/Exercises/tplant.dat",
                         header=FALSE); head(Tplant)

# ==========> Punto A

# Extraer las columnas relevantes
y <- Tplant[, 1]  # Altura de la planta
X <- Tplant[, 2]  # Período de medición
pH <- Tplant[, 3] # Nivel de pH

# Añadir intercepto (columna de unos) a X
X <- cbind(1, X)

# Crear un data frame
df <- data.frame(
  height = y,
  time = X[, 2],
  pH = pH
)

# Ajustar un modelo lineal
lmfit <- lm(height ~ time + pH, data = df);summary(lmfit)


# ==========> Punto B

# Obtener los residuales del modelo lineal
residuales <- residuals(lmfit)

# Definir intervalos para el nivel de pH
pH_intervals <- cut(df$pH, breaks = 7)  # Dividir el rango de pH en 5 intervalos

# Crear una paleta de colores para los intervalos
colores <- heat.colors(length(levels(pH_intervals)))  # Usar paleta heat.colors

# Crear el scatter plot usando plot base de R
plot(
  x = df$pH,                # Eje x: niveles de pH
  y = residuales,           # Eje y: residuales
  col = colores[as.numeric(pH_intervals)],  # Colores según los intervalos de pH
  pch = 19,                 # Tipo de punto (círculos sólidos)
  xlab = "Nivel de pH",     # Etiqueta del eje x
  ylab = "Residuales"      # Etiqueta del eje y
)

title(main = "Mapa de dispersión: Residuales vs Nivel de pH", family = 'Phudu-Medium', font = 2, cex.main = 1.5)
mtext("EXPLOTACION DE LOS RESICUALES EN FUNCION DEL PH", family = 'Phudu-Regular', font = 2, cex.main = 1.5)


# Añadir una leyenda mejorada
legend( 5.5, 1.5,               # Posición de la leyenda
  legend = levels(pH_intervals),  # Mostrar los intervalos de pH
  fill = colores,           # Colores correspondientes a los intervalos
  title = "Nivel de pH",    # Título de la leyenda
  cex = 1,                 # Tamaño de la leyenda
  bty = "n"                 # Sin caja alrededor de la leyenda
)

# ==========> Punto C

library(mvtnorm)                                               # Normal multivariada
library(progress)

Tplant <- read.table("https://www2.stat.duke.edu/~pdh10/FCBS/Exercises/tplant.dat",
                     header=FALSE); head(Tplant)
y <- Tplant[, 1]  # Altura de la planta
X <- Tplant[, 2]  # Período de medición
pH <- Tplant[, 3] # Nivel de pH


# Datos para la construcción del modelo
X = cbind(1,X, pH)
y = y



create_cor_matrix <- function(n, rho) {
  C <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        C[i, j] <- 1
      } else if ((i - j == 1 && i %% 2 == 0) || (j - i == 1 && j %% 2 == 0)) {
        C[i, j] <- rho
      }
    }
  }
  return(C)
}


# Hiperparámeros del algoritmo:
delta = 0.1

# Hiper parámetros del modelo
beta0 = c(0,0,0)
mu0 = diag(1/1000, nrow = length(beta0), ncol = length(beta0))
nu0 = 1
sigma0 = 1

# Parámetros iniciales del modelo: 
beta = rmvnorm(n = 1, mean = beta0, sigma = mu0)
sigma = 1/rgamma(n = 1, shape = nu0/2, rate = nu0 * sigma0/2)
p = runif(n = 1, min = 0, max = 1)
Cp = create_cor_matrix(n = 20, rho = p)
# Para evitar cálculos innecesarios: 
Xt = t(X)
smu0 = solve(mu0)

MCMC = matrix(NA, nrow = 10000, ncol = 5)


m = 110000
counter = 0


pb <- progress_bar$new(
  format = " Progreso [:bar] :percent | ETA: :eta", 
  total = m, 
  clear = FALSE, 
  width = 80
)

begin = sample(1:10, size = 1)

set.seed(1305)
for (j in 1:m){
  sCp = solve(Cp)
  # Actualizando beta:
  # muN = solve(Xt %% sCp %% X * 1/sigma + smu0)
  muN = solve(Xt %*% sCp %*% X * 1/sigma + mu0)
  betaN = muN %*% (Xt %*% sCp %*% y * 1/sigma + smu0 %*% beta0)
  
  beta = t(rmvnorm(n = 1, mean = betaN, sigma = muN))
  
  # Actualizando sigma^2:
  M = y - X %*% beta
  SSR = t(M) %*% sCp %*%  M
  
  sigma = 1/rgamma(n = 1, shape = 10.5, rate = 0.5 + 0.5 * SSR) 
  
  # Actualizando p:
  pEst = runif(n = 1, min = p - delta, max = p + delta)
  pEst = abs(pEst)
  pEst = min(pEst, 2 - pEst)
  
  CpEst = create_cor_matrix(n = 20, rho = pEst)
  
  # ACCEPTANCE RATIO MÁS SENCILLA:
  r = dmvnorm(x = y, mean = X %*% beta, sigma = sigma * CpEst)/
    dmvnorm(x = y, mean = X %*% beta, sigma = sigma * Cp)
  
  ## Acceptance ratio: 
  u = runif(n = 1, min = 0, max = 1)
  
  if (u < r) {
    p = pEst
    Cp = CpEst}
  
  # Guardado los datos
  # MCMC[j,] = c(beta, sigma, p)
  
  if (j > 10000 & (j - begin) %% 10 == 0) {
    counter = counter + 1 
    MCMC[counter,] = c(beta,sigma,p)
  }
  pb$tick()
}

colnames(MCMC) = c('Intercep', paste('beta',1:2), 'sigma', 'rho')

par(mfrow = c(2,3))
lapply(colnames(MCMC), function(x) plot(MCMC[,x], type = 'l', main = x))


par(mfrow = c(2,3))
lapply(colnames(MCMC), function(x) acf(MCMC[,x], main = x))


# ==========> Punto D

# Calcular estadísticas
stats <- data.frame(
  param = c("Intercep", "beta 1", "beta 2", "sigma", "rho"),
  mean = apply(MCMC, 2, mean),
  lower = apply(MCMC, 2, function(x) quantile(x, 0.025)),
  upper = apply(MCMC, 2, function(x) quantile(x, 0.975))
)

xtable::xtable(stats, digits = 3, caption = "Estadísticas de los parámetros del modelo")

# Configurar el diseño de la gráfica en 2x2
par(mfrow = c(1, 3))

# Histograma de beta_0
hist(MCMC[,1], breaks = 30, prob = TRUE, col = "gray85", border = "gray70",
     main = "Histograma de beta[0]", xlab = "beta[0]", ylab = "Densidad")
lines(density(MCMC[,1]), col = "black", lwd = 1.5)
abline(v = stats$mean[1], col = "red", lty = 2)
abline(v = stats$lower[1], col = "blue", lty = 3)
abline(v = stats$upper[1], col = "blue", lty = 3)

# Histograma de beta_1
hist(MCMC[,2], breaks = 30, prob = TRUE, col = "gray85", border = "gray70",
     main = "Histograma de beta[1]", xlab = "beta[1]", ylab = "Densidad")
lines(density(MCMC[,2]), col = "black", lwd = 1.5)
abline(v = stats$mean[2], col = "red", lty = 2)
abline(v = stats$lower[2], col = "blue", lty = 3)
abline(v = stats$upper[2], col = "blue", lty = 3)

# Histograma de beta_1
hist(MCMC[,3], breaks = 30, prob = TRUE, col = "gray85", border = "gray70",
     main = "Histograma de beta[2]", xlab = "beta[2]", ylab = "Densidad")
lines(density(MCMC[,3]), col = "black", lwd = 1.5)
abline(v = stats$mean[3], col = "red", lty = 2)
abline(v = stats$lower[3], col = "blue", lty = 3)
abline(v = stats$upper[3], col = "blue", lty = 3)





