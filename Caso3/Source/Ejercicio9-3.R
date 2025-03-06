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


# Cargar los datos
crime_data <- read.table("https://www2.stat.duke.edu/~pdh10/FCBS/Exercises/crime.dat",
                         header=TRUE); head(crime_data)

# Definir función para calcular el error estándar residual
ResidualSE <- function(y, X) {
  fit <- lm(y ~ X)
  return(sum(residuals(fit)^2) / (nrow(X) - ncol(X)))
}

# Definir función para el modelo bayesiano con G-prior
lmGprior <- function(y, X, g=length(y), nu0=1, sigma02=NULL, S=1000) {
  n <- nrow(X)
  p <- ncol(X)
  
  if (is.null(sigma02)) {
    sigma02 <- ResidualSE(y, X)
  }
  
  Hg <- (g / (g + 1)) * X %*% solve(t(X) %*% X) %*% t(X)
  SSRg <- t(y) %*% (diag(n) - Hg) %*% y
  sigma2 <- 1 / rgamma(S, (nu0 + n) / 2, rate = (nu0 * sigma02 + SSRg) / 2)
  
  Vb <- g * solve(t(X) %*% X) / (g + 1)
  Eb <- Vb %*% t(X) %*% y
  
  beta_samples <- matrix(NA, nrow=S, ncol=p)
  for (i in 1:S) {
    beta_samples[i, ] <- mvrnorm(1, mu=Eb, Sigma=sigma2[i] * Vb)
  }
  
  return(list(beta=beta_samples, sigma2=sigma2))
}

# Definir parámetros
y <- crime_data$y
X <- model.matrix(~ . -y, data=crime_data)
n <- nrow(X)
g <- n
nu0 <- 2
sigma02 <- 1
S <- 100000

# Ejecutar modelo bayesiano
result_gprior <- lmGprior(y, X, g=g, nu0=nu0, sigma02=sigma02, S=S)

# Media posterior de los coeficientes
beta_hat_gprior <- colMeans(result_gprior$beta)

# Intervalos de confianza posteriores para los coeficientes
ci_gprior <- apply(result_gprior$beta, 2, function(x) quantile(x, probs = c(0.025, 0.975)))

# Construcción del DataFrame con los resultados
coef_name <- c("intercept", colnames(X)[-1])
df_gprior <- tibble(
  param = coef_name,
  beta_hat = beta_hat_gprior,
  ci_lower = ci_gprior[1, ],
  ci_upper = ci_gprior[2, ],
  ci_length = ci_gprior[2, ] - ci_gprior[1, ]
)

print(df_gprior)

# Estimaciones de mínimos cuadrados ordinarios
ols_fit <- lm(y ~ X - 1)
ols_coef <- coef(ols_fit)
ols_pval <- summary(ols_fit)$coefficients[, 4]
ols_confint <- confint(ols_fit)

# Construcción del DataFrame con los resultados OLS
df_ols <- tibble(
  param = coef_name,
  beta_hat = ols_coef,
  pvalue = ols_pval,
  ci_lower = ols_confint[, 1],
  ci_upper = ols_confint[, 2],
  ci_length = ols_confint[, 2] - ols_confint[, 1]
)

print(df_ols)

xtable::xtable(df_ols, caption = "OLS Coefficients and Confidence Intervals", digits = 3)

df_plot <- tibble(
  param = coef_name,
  beta_hat_ols = ols_coef,
  pvalue_ols = ols_pval,
  ci_lower_ols = ols_confint[, 1],
  ci_upper_ols = ols_confint[, 2],
  beta_hat_bayes = beta_hat_gprior,
  ci_lower_bayes = ci_gprior[1, ],
  ci_upper_bayes = ci_gprior[2, ]
)

# Definir el orden de las variables
var_order <- c("intercept", "M", "So", "Ed", "Po1", "Po2", "LF", "M.F", "Pop", "NW", "U1", "U2", "GDP", "Ineq", "Prob", "Time")

# Crear un vector de posiciones para los puntos en el eje x
x_pos <- 1:length(var_order)

par(mfrow = c(1, 1), mar = c(4.1, 3, 4.1, 2), bg = "white", family = 'Phudu-Regular')

# Configurar el gráfico base
plot(
  x = NULL, y = NULL,
  xlim = c(0.5, length(var_order) + 0.5),  # Límites del eje x
  ylim = range(c(df_plot$ci_lower_ols, df_plot$ci_upper_ols, df_plot$ci_lower_bayes, df_plot$ci_upper_bayes)),  # Límites del eje y
  xlab = "Variable", ylab = "Value",
  xaxt = "n"  # No mostrar etiquetas del eje x por ahora
)

# Añadir etiquetas del eje x
axis(1, at = x_pos, labels = var_order, las = 2, cex.axis = 0.8)

# Dibujar puntos y barras de error para OLS
points(
  x = x_pos - 0.1,  # Desplazar ligeramente a la izquierda
  y = df_plot$beta_hat_ols,
  pch = 16,  # Forma del punto (círculo relleno)
  col = ifelse(df_plot$pvalue_ols < 0.05, "red", "pink")  # Color según significancia
)
arrows(
  x0 = x_pos - 0.1, y0 = df_plot$ci_lower_ols,
  x1 = x_pos - 0.1, y1 = df_plot$ci_upper_ols,
  length = 0.1, angle = 90, code = 3,
  col = ifelse(df_plot$pvalue_ols < 0.05, "red", "pink")
)

# Dibujar puntos y barras de error para Bayesiano
points(
  x = x_pos + 0.1,  # Desplazar ligeramente a la derecha
  y = df_plot$beta_hat_bayes,
  pch = 17,  # Forma del punto (triángulo relleno)
  col = "blue"  # Color azul para Bayesiano
)
arrows(
  x0 = x_pos + 0.1, y0 = df_plot$ci_lower_bayes,
  x1 = x_pos + 0.1, y1 = df_plot$ci_upper_bayes,
  length = 0.1, angle = 90, code = 3,
  col = "blue"
)

# Añadir una línea horizontal en y = 0 para referencia
abline(h = 0, lty = 2, col = "gray")

# Añadir una leyenda
legend(
  "topright",
  legend = c("OLS (significant)", "OLS (non-significant)", "Bayes"),
  pch = c(16, 16, 17),
  col = c("red", "pink", "blue"),
  bty = "n"
)

title("Comparación de Coeficientes estimados: OLS vs. Bayesiano", family = 'Phudu-Medium', cex.main = 1.5, line = 1.8)
mtext("Intervalos de confianza/credibilidad al 95% para los coeficientes", side = 3, line = 0.5, adj = 0.5, cex = 1, family = 'Phudu-Regular')


#####################

# Semilla para reproducibilidad
set.seed(1998)

# Dividir los datos en conjuntos de entrenamiento y prueba
n <- nrow(X)
i_te <- sample(1:n, size = floor(n / 4), replace = FALSE)
i_tr <- setdiff(1:n, i_te)

y_tr <- y[i_tr]
y_te <- y[i_te]
X_tr <- X[i_tr, ]
X_te <- X[i_te, ]

# Ajustar el modelo OLS
ols_fit <- lm(y_tr ~ X_tr - 1)  # Ajustar el modelo sin intercepto (ya que X incluye una columna de unos)
β_hat_ols <- coef(ols_fit)
y_hat_ols <- X_te %*% β_hat_ols
error_ols <- mean((y_te - y_hat_ols)^2)

# Ajustar el modelo Bayesiano con G-prior
result_gprior <- lmGprior(y_tr, X_tr, g = g, nu0 = nu0, sigma02 = sigma02, S = S)
β_hat_bayes <- colMeans(result_gprior$beta)
y_hat_bayes <- X_te %*% β_hat_bayes
error_bayes <- mean((y_te - y_hat_bayes)^2)

# Mostrar los errores de predicción
cat("Error de predicción (OLS):", error_ols, "\n")
cat("Error de predicción (Bayesiano):", error_bayes, "\n")


par(mfrow = c(1, 2), mar = c(4.1, 4, 4.1, 4), bg = "white", family = 'Phudu-Regular')

# Gráfico de dispersión de los valores reales vs. predichos (OLS)
plot(
  x = y_te, y = y_hat_ols,
  xlab = "Valor Real", ylab = "Valor Predicho",
  pch = 16, col = "red"
)
abline(a = 0, b = 1, lty = 2, col = "gray20")
mtext("Modelo OLS", side = 3, line = 1, adj = 0.5, cex = 1, col = "red", family = 'Phudu-Regular')


# Gráfico de dispersión de los valores reales vs. predichos (Bayesiano)
plot(
  x = y_te, y = y_hat_bayes,
  xlab = "Valor Real", ylab = "Valor Predicho",
  pch = 17, col = "blue"
)
abline(a = 0, b = 1, lty = 2, col = "gray20")
mtext("Modelo Bayesiano", side = 3, line = 1, adj = 0.5, cex = 1, col = "blue", family = 'Phudu-Regular')
mtext("Valores Reales vs. Predichos", family = 'Phudu-Medium', side = 1, cex.main = 1.5, line = 0, outer = TRUE)


#######################

# Cargar librerías necesarias
library(progress)

# Función para dividir los datos en conjuntos de entrenamiento y prueba
splitData <- function(y, X, test_size = 0.5) {
  n <- nrow(X)
  i_te <- sample(1:n, size = floor(n * test_size), replace = FALSE)
  i_tr <- setdiff(1:n, i_te)
  
  y_tr <- y[i_tr]
  y_te <- y[i_te]
  X_tr <- X[i_tr, ]
  X_te <- X[i_te, ]
  
  return(list(y_tr = y_tr, y_te = y_te, X_tr = X_tr, X_te = X_te))
}

# Función para calcular los MSEs de OLS y Bayesiano
computeMses <- function(y, X, g = g, nu0 = nu0, sigma02 = sigma02, S = S) {
  # Dividir los datos en conjuntos de entrenamiento y prueba
  split_data <- splitData(y, X)
  y_tr <- split_data$y_tr
  y_te <- split_data$y_te
  X_tr <- split_data$X_tr
  X_te <- split_data$X_te
  
  # Ajustar el modelo OLS
  ols_fit <- lm(y_tr ~ X_tr - 1)  # Ajustar el modelo sin intercepto
  β_hat_ols <- coef(ols_fit)
  y_hat_ols <- X_te %*% β_hat_ols
  error_ols <- mean((y_te - y_hat_ols)^2)
  
  # Ajustar el modelo Bayesiano con G-prior
  result_gprior <- lmGprior(y_tr, X_tr, g = g, nu0 = nu0, sigma02 = sigma02, S = S)
  β_hat_bayes <- colMeans(result_gprior$beta)
  y_hat_bayes <- X_te %*% β_hat_bayes
  error_bayes <- mean((y_te - y_hat_bayes)^2)
  
  return(list(error_ols = error_ols, error_bayes = error_bayes))
}

# Parámetros para la simulación
n_sim <- 1000
error_ols <- numeric(n_sim)
error_bayes <- numeric(n_sim)

# Semilla para reproducibilidad
set.seed(1234)

# Barra de progreso
pb <- progress_bar$new(
  format = "Simulando [:bar] :percent | Tiempo restante: :eta",
  total = n_sim, clear = FALSE, width = 60
)

# Simulación
for (i in 1:n_sim) {
  errors <- computeMses(y, X, g = g, nu0 = nu0, sigma02 = sigma02, S = S)
  error_ols[i] <- errors$error_ols
  error_bayes[i] <- errors$error_bayes
  pb$tick()  # Actualizar la barra de progreso
}


#Guardar objeto

save(error_ols, file = "EOLS.RData")
save(error_bayes, file = "EBAYES.RData")


# MSEs promedio
mean_error_ols <- mean(error_ols)
mean_error_bayes <- mean(error_bayes)

# Mostrar los resultados
cat("MSE promedio (OLS):", mean_error_ols, "\n")
cat("MSE promedio (Bayesiano):", mean_error_bayes, "\n")


# BoxPlot  MSE

par(mfrow = c(1, 1), mar = c(4.1, 4.1, 4.1, 2), bg = "white", family = 'Phudu-Regular')

# Crear el boxplot
boxplot(
  list(OLS = error_ols, Bayesiano = error_bayes, NA),
  names = c("OLS", "Bayesiano", NA),
  xlab = "Modelo",
  ylab = "Error Cuadrático Medio",
  col = c("red", "blue"),
  xlim = c(0.5, 2.8),
  border = "black",
  notch = TRUE
)

set.seed(1998)
# Agregar los puntos al lado derecho de cada boxplot
stripchart(
  list(OLS = error_ols, Bayesiano = error_bayes),
  method = "jitter",   # Desplazamiento para evitar solapamiento
  vertical = TRUE,     # Alinear con los boxplots
  add = TRUE,          # Agregar a la gráfica existente
  pch = 20,            # Tipo de punto
  col = c("darkred", "darkblue"),  # Color de los puntos
  at = c(1.2, 2.2)  # Desplazar los puntos a la derecha de cada boxplot
)

title("Comparación de Errores de Predicción: OLS vs. Bayesiano", family = 'Phudu-Medium', cex.main = 1.5, line = 1.8)
mtext("Distribución de los errores cuadráticos medios para OLS y Bayesiano", side = 3, line = 0.5, adj = 0.5, cex = 1, family = 'Phudu-Regular')

legend(2.5, 6, legend = c(paste("Mean Error\nOLS:", round(mean_error_ols, 3)), 
                              paste("Mean Error\nBAYES", round(mean_error_bayes, 3))),
       fill = c("red", "blue"), bty = "n", cex = 1, y.intersp= 1.5)





