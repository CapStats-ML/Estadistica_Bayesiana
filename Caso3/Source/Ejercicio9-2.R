# 9.2 Selección de modelos: 
# Como se describe en el Ejemplo 6 del Capítulo 7, el archivo `azdiabetes.dat` 
# contiene datos sobre variables relacionadas con la salud de una población de 
# 532 mujeres. En este ejercicio, se modelará la distribución condicional del 
# nivel de glucosa (`glu`) como una combinación lineal de las otras variables, 
# excluyendo la variable `diabetes`.

# Variables: 
# Glu : Nivel de glucosa en plasma sanguineo
# bp: Presión sanguínea
# skin: Grosor del pliegue cutaneo en triceps
# bmi: Indice de masa corporal
# age: Edad
# npreg: Número de embarazos
# ped: probabilidad de desarrollar diabetes en función del historial familiar y factores genéticos
# diabetes: Si la persona tiene diabetes o no


# a) Ajustar un modelo de regresión utilizando el prior g-prior con \( g = n \), 
# \( \nu_0 = 2 \) y \( \sigma_0^2 = 1 \). Obtener intervalos de confianza posteriores
# para todos los parámetros.

# b) Realizar el procedimiento de selección y promediado de modelos descrito en 
# la Sección 9.3. Obtener \( \Pr(\beta_j \neq 0|y) \), así como intervalos de 
# confianza posteriores para todos los parámetros. 
# Comparar con los resultados de la parte a).

library(dplyr)
library(readr)
library(readxl)
library(mvtnorm)
library(progress)
library(tictoc)
library(data.table)
library(patchwork)  # Para combinar gráficos
library(showtext)                               # Librería para agregar fuentes

# Agregar cada variante de la fuente con un nombre personalizado
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


datos <- read.table("https://www2.stat.duke.edu/~pdh10/FCBS/Exercises/azdiabetes.dat", 
                    header = TRUE); head(datos)

sum(is.na(datos))  # No hay valores faltantes)

Glu <- data.table(datos$glu); colnames(Glu) <- "glu"
X <- data.table(datos[, -c(2,8)]); head(X)



diabetes_col <- datos$diabetes
colores <- ifelse(diabetes_col == "Yes", "#6a0d83", "#eeaf61")

xl <- matrix(0, nrow = ncol(X), ncol = 2)

for(i in 1:length(X)){
  xl[i,1] <- ifelse(min(X[[i]]) - 5 > 0, min(X[[i]]) - 5, 0) 
  xl[i,2] <- max(X[[i]]) + 5
}


par(mfrow = c(3, 2), mar = c(4.1, 3, 4.1, 2), bg = "white", family = 'Phudu-Regular') 

# Loop para crear diagramas de dispersión
for (i in 1:(ncol(datos) - 2)) {  # Excluir la columna 'diabetes'
  if (colnames(datos)[i] != "diabetes") {  # No graficar 'diabetes' contra sí misma
    plot(datos$glu, X[[i]], ylim = xl[i,],
         #main = paste("Dispersión entre Glu vs", colnames(datos)[i]),  # Título del gráfico
         #family.main = 'Phudu-Bold',
         xlab = "Glu",  # Etiqueta del eje x
         ylab = colnames(datos)[i],  # Etiqueta del eje y
         col = colores,  # Color de los puntos basado en 'diabetes'
         pch = 19)  # Tipo de punto (círculos sólidos)
    
    title(paste("Dispersión entre Glu vs", colnames(datos)[i]), family = 'Phudu-Medium', cex = 1.5,  line = 1.8)
    mtext("Explorando Glu vs otras variables", side = 3, line = 0.5, adj = 0.5, cex = 0.7, family = 'Phudu-Regular')
    legend(60, xl[i,2] - 2 , title = "Diabetes", bty = "n", legend = c("No", "Yes"), fill = c("#eeaf61", "#6a0d83"))  # Leyenda
  }
}

cor(datos[,c(1:7)])

par(mfrow = c(1, 1), mar = c(4.1, 3, 4.1, 2), bg = "white", family = 'Phudu-Regular')
corrplot::corrplot(cor(datos[,c(1:7)]), method = "shade") # Parece Existe correalcion con GLu
#orrplot::corrplot(cor(X), method = "shade") # Para las otras variables no observamos corralcion distingible

# OLS 
X1 <- as.matrix(cbind(1, X)); head(X1)
X2 <- cbind(1, X); head(X2)
p <- ncol(X1); p
Y <- as.matrix(Glu); Y
(n <- dim(X1)[1])

bts_ols <- solve(t(X1) %*% X1) %*% t(X1) %*% Y ; bts_ols # Betas para el modelo de regresion lineal
sig2_ols <- sum((Y - X1 %*% bts_ols)^2) / (n - p); sig2_ols # Varianza del error
sqrt(sig2_ols)


lm(datos$glu ~ -1 + ., data = X2) %>% summary() 

xtable::xtable(lm(datos$glu ~ -1 + ., data = X2) %>% summary(), caption = "Resumen del modelo de regresión lineal",
       label = "tab:results", digits = 3)

# Un modelo de regresion lineal simple muestra que no todas las variables son significativas 
# Para el modelo de regresion, esto nos da un primer indicio de como se estan comportando 
# de esta muestra.


# a) Ajustar un modelo de regresión utilizando el prior g-prior con \( g = n \),
# \( \nu_0 = 2 \) y \( \sigma_0^2 = 1 \). Obtener intervalos de confianza posteriores


# hiperparametros (previa g)

nu0 <- 2
s20 <- 1
g <- n 

# Ajuste del modelo

Hg <- (g/(g + 1)) * (X1 %*% solve(t(X1) %*% X1) %*% t(X1))
SSRg <- as.numeric(t(Y) %*% (diag(1,n) - Hg) %*% Y)
Vbeta <- (g/(g + 1)) * (solve(t(X1) %*% X1))
Ebeta <- Vbeta %*% t(X1) %*% Y

# Monte Carlo

B <- 100000
sig2_mc <- matrix(0, B, 1)
betas_mc <- matrix(0, B, p)

set.seed(1998)

for (b in 1:B) {
  sig2_mc[b] <- 1/rgamma(1, (nu0 + n)/2, (nu0 * s20 + SSRg)/2)
  betas_mc[b,] <- c(mvtnorm::rmvnorm(1, Ebeta, sig2_mc[b] * Vbeta))
}

colnames(betas_mc) <- paste0("Beta_", colnames(X1))
head(betas_mc)

round(apply(X = sig2_mc, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.5, 0.975)), 3)
round(apply(X = betas_mc, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.5, 0.975)), 3)

round(colMeans(betas_mc > 0), 3) # Probabilidad de que los betas sean distintos de 0

# Make the table with the results

results <- data.frame(
  "Beta" = colnames(betas_mc),
  "2.5%" = apply(X = betas_mc, MARGIN = 2, FUN = quantile, probs = 0.025),
  "Mean" = apply(X = betas_mc, MARGIN = 2, FUN = mean),
  "97.5%" = apply(X = betas_mc, MARGIN = 2, FUN = quantile, probs = 0.975),
#  "Pr(Beta != 0)" = round(colMeans(betas_mc > 0), 3),
  check.names = FALSE
)

rownames(results) <- NULL
results[1,1] <- "Intercepto"; results

xtable::xtable(results, caption = "Intervalos de confianza para los parámetros del modelo de regresión lineal",
       label = "tab:results", digits = 3)

# b) Realizar el procedimiento de selección y promediado de modelos descrito en
# la Sección 9.3. Obtener \( \Pr(\beta_j \neq 0|y) \), así como intervalos de
# confianza posteriores para todos los parámetros.

ResidualSE <- function(y, X) {
  n <- nrow(X)
  p <- ncol(X)
  beta_hat <- solve(t(X) %*% X, t(X) %*% y)
  residuals <- y - X %*% beta_hat
  return(sum(residuals^2) / (n - p))
}

lmGprior <- function(y, X, g=length(y), nu0=1, sigma02=NULL, S=1000) {
  n <- nrow(X)
  p <- ncol(X)
  XtX_inv <- solve(t(X) %*% X)
  
  if (is.null(sigma02)) {
    sigma02 <- ResidualSE(y, X)
  }
  
  Hg <- (g / (g + 1)) * X %*% XtX_inv %*% t(X)
  SSRg <- crossprod(y, (diag(n) - Hg) %*% y)
  sigma2 <- 1 / rgamma(S, (nu0 + n) / 2, rate = (nu0 * sigma02 + SSRg) / 2)
  
  Vb <- g * XtX_inv / (g + 1)
  Eb <- Vb %*% t(X) %*% y
  
  beta_samples <- mvrnorm(S, mu=Eb, Sigma=Vb * sigma2)
  
  return(list(beta=beta_samples, sigma2=sigma2))
}

lpyX <- function(y, X, g=length(y), nu0=1) {
  n <- length(y)
  p <- ifelse(is.null(dim(X)), 0, ncol(X))
  
  if (p == 0) {
    sigma_hat2 <- mean(y^2)
    SS0 <- sum(y^2)
  } else {
    XtX_inv <- solve(t(X) %*% X)
    beta_hat <- XtX_inv %*% t(X) %*% y
    residuals <- y - X %*% beta_hat
    sigma_hat2 <- sum(residuals^2) / (n - p)
    H0 <- (g / (g + 1)) * X %*% XtX_inv %*% t(X)
    SS0 <- crossprod(y, (diag(n) - H0) %*% y)
  }
  
  return(
    -0.5 * n * log(2 * pi) + lgamma(0.5 * (nu0 + n)) - lgamma(0.5 * nu0) - 
      0.5 * p * log(1 + g) + 0.5 * nu0 * log(0.5 * nu0 * sigma_hat2) - 
      0.5 * (nu0 + n) * log(0.5 * (nu0 * sigma_hat2 + SS0))
  )
}

BayesianModelAveraging <- function(y, X, S=5000) {
  n <- nrow(X)
  p <- ncol(X)
  BETA <- matrix(NA, S, p)
  Z <- matrix(NA, S, p)
  z <- rep(1, p)
  lpy_c <- lpyX(y, X[, which(z == 1), drop=FALSE])
  
  pb <- progress_bar$new(total = S, format = "Simulación [:bar] :percent en :elapsed")
  tic()
  for (s in 1:S) {
    j_perm <- sample(1:p)  # Aleatoriza fuera del loop interno
    zp <- z
    lpy_p <- rep(NA, p)
    
    for (j in j_perm) {
      zp[j] <- 1 - zp[j]
      lpy_p[j] <- lpyX(y, X[, which(zp == 1), drop=FALSE])
      r <- (lpy_p[j] - lpy_c) * (-1)^(zp[j] == 0)
      z[j] <- rbinom(1, 1, 1 / (1 + exp(-r)))
      if (z[j] == zp[j]) lpy_c <- lpy_p[j]
    }
    
    beta <- z
    if (sum(z) > 0) {
      beta[which(z == 1)] <- lmGprior(y, X[, which(z == 1), drop=FALSE], S=1)$beta
    }
    
    Z[s, ] <- z
    BETA[s, ] <- beta
    pb$tick()
  }
  toc()
  return(list(BETA=BETA, Z=Z))
}


BMA_result <- BayesianModelAveraging(Y, X1, S=10000)
z_prob <- colMeans(BMA_result$Z)
ci_beta_bma <- apply(BMA_result$BETA, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

# Construcción del DataFrame con los resultados
coef_name <- c("intercept", colnames(X1)[-1])
df_ci_bma <- tibble(
  param = coef_name,
  prob = z_prob,
  ci_lower = ci_beta_bma[1, ],
  ci_upper = ci_beta_bma[2, ]
)

df_ci_bma
results

xtable::xtable(df_ci_bma, caption = "Probabilidad de inclusión y intervalos de confianza para los betas",
       label = "tab:results", digits = 3)


df_z <- data.frame(Variable = c(results$Beta, NA, NA), Probabilidad = c(z_prob, 0, 0))

par(mfrow = c(1, 1), mar = c(4.1, 3, 4.1, 2), bg = "white", family = 'Phudu-Regular')

colores <- sapply(df_z$Probabilidad, function(p) adjustcolor("steelblue", alpha.f = p + 0.2)) 

# Crear el gráfico de barras
barplot(df_z$Probabilidad, names.arg = df_z$Variable, col = colores, border = colores,
        xlab = "Índice de la variable", ylab = "Probabilidad de inclusión")
abline(h = 0.6, col = "red", lty = 2)  # Línea horizontal en 0.5

title("Probabilidad de inclusión de cada variable", family = 'Phudu-Medium', cex.main = 1.5, line = 1.8)
mtext("Metodos Bayasianos para los modelos de lm y glm", side = 3, line = 0.5, adj = 0.5, cex = 1, family = 'Phudu-Regular')

legend("topright", 
       legend = c("Probabilidad de inclusión\ndel Beta dentro del\nmodelo de regresión", "Línea al 0.6 como\nMargen de decisión"),
       fill = c("steelblue", NA),  border = c("steelblue", NA), lty = c(NA, 2),
       col = c(NA, "red"), bty = "n", pch = c(NA, NA), cex = 0.9)  

# Intervalos de confianza para los betas

ci_beta_bma <- apply(BMA_result$BETA, 2, function(x) quantile(x, probs = c(0.025, 0.975)))
df_beta <- data.frame(Variable = coef_name, `2.5%` = ci_beta_bma[1, ], `97.5%` = ci_beta_bma[2, ])
df_beta$Variable <- as.numeric(as.character(df_beta$Variable))

par(mar = c(4.1, 3, 4.1, 2), bg = "white", family = 'Phudu-Regular')
plot(1:nrow(df_beta), (ci_beta_bma[1, ] + ci_beta_bma[2, ]) / 2, 
     pch = 16, col = "black", 
     xlim = c(0, p + 1), 
     ylim = c(min(ci_beta_bma), max(ci_beta_bma)), 
     xlab = "Variable", ylab = "Intervalo de confianza",
     main = "Intervalos de confianza para los betas",
     xaxt = "n")  # Desactiva el eje x automático

axis(1, at = 1:nrow(df_beta), labels = df_beta$Variable, las = 2)  # Añade etiquetas personalizadas
segments(1:nrow(df_beta), ci_beta_bma[1, ], 1:nrow(df_beta), ci_beta_bma[2, ], col = "red", lwd = 2)
abline(h = 0, col = "black", lty = 2)

## Evolucion de la seleccion de Variables 

image(t(BMA_result$Z), col = c("white", "black"), axes = FALSE,
      main = "Evolución de la selección de variables",
      xlab = "Iteración", ylab = "Índice de la variable")
axis(1, at = seq(0, 1, length.out = 5), labels = seq(1, nrow(BMA_result$Z), length.out = 5))
axis(2, at = seq(0, 1, length.out = ncol(BMA_result$Z)), labels = 1:ncol(BMA_result$Z))


