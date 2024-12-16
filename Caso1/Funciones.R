bayesian_analysis <- function(dat, depto_res_col, depto_code, score_col, 
                              mu0 = 50, k0 = 1, s20 = 10^2, nu0 = 1, 
                              B = 10000, seed = 1234, save_samples = FALSE) {
  # Filtrar los datos para el departamento
  depto_data <- dat[dat[[depto_res_col]] == depto_code, score_col]
  
  # Tamaño de muestra
  n <- length(depto_data)
  
  # Estadísticos suficientes
  yb <- mean(depto_data)
  s2 <- var(depto_data)
  
  # Hiperparámetros actualizados
  kn <- k0 + n
  nun <- nu0 + n
  mun <- (k0 / kn) * mu0 + (n / kn) * yb
  s2n <- (nu0 * s20 + (n - 1) * s2 + k0 * n * (yb - mu0)^2 / kn) / nun
  
  # Muestras de la distribución posterior
  set.seed(seed)
  sigma2 <- 1 / rgamma(n = B, shape = nun / 2, rate = nun * s2n / 2)
  theta <- rnorm(n = B, mean = mun, sd = sqrt(sigma2 / kn))
  
  # Inferencias
  est_B <- mean(theta)
  cv_B <- sd(theta) / mean(theta)
  ic_B <- quantile(theta, probs = c(0.025, 0.975))
  
  desvi <- sqrt(sigma2)
  est_B2 <- mean(desvi)
  cv_B2 <- sd(desvi) / mean(desvi)
  ic_B2 <- quantile(desvi, probs = c(0.025, 0.975))
  
  CVar <- desvi / abs(theta)
  est_B3 <- mean(CVar)
  cv_B3 <- sd(CVar) / mean(CVar)
  ic_B3 <- quantile(CVar, probs = c(0.025, 0.975))
  
  # Tabulación de resultados
  tab <- rbind(
    c(est_B, cv_B, ic_B), 
    c(est_B2, cv_B2, ic_B2), 
    c(est_B3, cv_B3, ic_B3)
  )
  colnames(tab) <- c("Estimación", "CV", "L. Inf.", "L. Sup.")
  rownames(tab) <- c("Theta", "Desviación", "Coef. de Var")
  
  # Salida
  output <- list(
    resultados = knitr::kable(tab, digits = 3, align = "c", 
                              caption = "Inferencia Bayesiana sobre los parámetros"),
    parametros_posteriores = list(mun = mun, s2n = s2n, nun = nun, kn = kn)
  )
  
  # Opción para guardar muestras
  if (save_samples) {
    output$muestras <- list(theta = theta, sigma2 = sigma2, CVar = CVar)
  }
  
  return(output)
}



