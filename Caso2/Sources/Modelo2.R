library(data.table)
library(progress)

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
nj <- data[, .N, by = group]$N
r <- length(unique(data$COLE_COD_DEPTO_UBICACION))  # Número de departamentos
group = data$group
N = nrow(data)
y = data$PUNT_GLOBAL

# HIPERPARÁMETROS (xd):
# nuEst, mu0, gamma0, eta0, tau0, alpha0, beta0, lambda0

nuEst = 3; mu0 = 250; eta0 = 1; alpha0 = 1; alpha0 = 1; lambda0 = 1; beta0 = 1/2500
gamma0 = 50    # La raíz del valor que se tiene  como hiperparámetro
tau0 = 50      # La raíz del valor que se tiene  como hiperparámetro 


# PARÁMETROS:
# theta, s, sigmaj, nu, sigma, mu, tau

set.seed(1305)

# ===> Se muestrea de las distribuciones previas para inicializar el algoritmo:
# MEDIA DE Y:
mu = rnorm(n = 1, mean = mu0, sd = gamma0)
tau = 1/rgamma(n = 1, shape = eta0/2, rate = 1/2 * eta0 * tau0^2)
theta = rnorm(n = r, mean = mu, sd = sqrt(tau))

# VARIANZA DE Y:
sigma = rgamma(n = 1, shape = alpha0/2, rate = beta0/2)
nus0 = 1:50
nu = 1 
sigmaj = 1/rgamma(n = r, shape = nu/2, rate = nu * sigma/2)
s = 1/rgamma(n = N, shape = (nuEst + 1)/2, (nuEst*sigmaj[group] + (y - theta[group])^2)/2)

# ===> Muestreo sistemático:
begin <- sample(1:10, 1)

# Para guardar las simulaciones:
m = 110000 
Gibbs = matrix(NA, nrow = 10000, ncol = r + r + 4)
counter = 0
colnames(Gibbs) = c(paste0('theta', 1:r), paste0('sigma',1:r), 'mu', 'tau', 'nu', 'sigmaSq')

# ===> Barra de progreso:
pb <- progress_bar$new(
  format = " Progreso [:bar] :percent | ETA: :eta", 
  total = m, 
  clear = FALSE, 
  width = 80
)

for (i in 1:m){
  # ==========> RAMA IZQUIERDA (MEDIA):
  # ===> Actualizar mu
  Den = 1/(r/tau + 0.0004)
  #mu0/gamma0 = 6500
  mu = rnorm(n = 1, mean = (sum(theta)/tau + 0.1) * Den, sd = sqrt(Den)) 
  
  # ===> Actualizar tau^2
  # nu0 * tau0^2/2 = 1250
  vector = theta - mu
  tau = 1/rgamma(n = 1, shape = 0.5 * (eta0 + r), rate = 1250 + 0.5 * t(vector) %*% vector)
  
  # ===> Actualizar theta: 
  ys = rowsum(y/s, group)
  os = rowsum(1/s, group)
  Den = 1/(os + 1 /tau)
  theta = rnorm(n = r, mean = (ys + mu/tau) * Den, sd = sqrt(Den))
  
  # ==========> RAMA DERECHA (VARIANZA):
  lpnu = nus0 * r/2 * log( nus0 * sigma * 0.5) - r * lgamma(nus0 * 0.5) + nus0 * 0.5 * sum(log(sigmaj)) -
    nus0 * (lambda0  + sigma/2 * sum(sigmaj))
  nu = sample(x = nus0, size = 1, prob = exp(lpnu - max(lpnu)))

  # ===> Actualizando sigma^2 
  # alpha0/2 = 0.5
  sigma = rgamma(n = 1, shape = 0.5 + r * nu * 1/2, rate = 0.0002 + nu * sum(sigmaj) * 1/2)
  
  # ===> Actualizando sigma^2_j
  sigmaj = rgamma(n = r, shape = (nj * nuEst + nu)*0.5, rate = 0.5 * (nuEst * os + nu * sigma))
  
  # ===> Actualizando s (variable latente)
  # nuEst  + 1 /2 = 2, nuEst/2 = 1.5
  s = 1/rgamma(n = N, shape = 2, rate = (1.5 * sigmaj)[group] + 1/2 * (y - theta[group])^2)
  
  if (i > 10000 & (i - begin) %% 10 == 0) {
    counter = counter + 1 
    Gibbs[counter, ] <- c(theta, sigmaj, mu, tau, nu, sigma)
  }
  pb$tick()
}

fwrite(as.data.table(Gibbs), file = 'datos/GibbsModelo2.txt')