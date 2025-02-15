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


# Hiperparámetros
nuEst <- 3
mu0 <- 250
gamma0 <- tau0 <- sigma0 <- 50
nu0 <- eta0 <- 1

# Inicialización de parámetros
set.seed(1305)
mu <- rnorm(1, mean = mu0, sd = gamma0)
tau <- 1 / rgamma(1, shape = eta0/2, rate = eta0 * tau0^2 / 2)
theta <- rnorm(r, mean = mu, sd = tau)
sigma <- rgamma(1, shape = nu0/2, rate = nu0 * sigma0^2 / 2)
s <- 1 / rgamma(N, shape = nuEst/2, rate = nuEst * sigma / 2)

# Preparar matriz para almacenar resultados
m <- 110000
Gibbs <- matrix(0, nrow = 10000, ncol = 3 + r)
colnames(Gibbs) <- c('mu', 'tau', 'sigma2', paste0('theta', 1:r))
counter = 0      # Para llenar el Gibbs

# Barra de progreso
pb <- progress_bar$new(
  format = " Progreso [:bar] :percent | ETA: :eta", 
  total = m, 
  clear = FALSE, 
  width = 80
)

begin <- sample(1:10, 1)

# Bucle Gibbs
for (i in 1:m) {
  # Actualizar mu
  mu <- rnorm(1, 
              mean = (sum(theta) / tau + mu0 / gamma0^2) / (r / tau + 1 / gamma0^2), 
              sd = sqrt(1 / (r / tau + 1 / gamma0^2)))
  
  # Actualizar tau
  tau <- 1 / rgamma(1, 
                    shape = (eta0 + r) / 2, 
                    rate = (eta0 * tau0^2 + sum((theta - mu)^2)) / 2)
  
  # Actualizar sigma
  sigma <- rgamma(1, 
                  shape = (nu0 + nuEst * N) / 2, 
                  rate = (nu0 * sigma0^2 + nuEst * sum(1 / s)) / 2)
  
  # Actualizar s
  s = 1/rgamma(n = N, 
               shape = 2, 
               rate = (nuEst * sigma + (y - theta[group])^2)/2)
  
  # Actualizar theta:
  ys = rowsum(y/s, group)
  os = rowsum(1/s, group)
  over = 1/(os + 1/tau)
  theta = rnorm(n = r,
                mean = (ys + mu/tau) * over,
                sd = sqrt(over))
  
  # Guardar resultados después del burn-in y thinning
  if (i > 10000 & (i - begin) %% 10 == 0) {
    counter = counter + 1
    Gibbs[counter, ] <- c(mu, tau, sigma, theta)
  }
  
  # Actualizar barra de progreso
  pb$tick()
}

# Guardar resultados
# fwrite(as.data.table(Gibbs), file = 'datos/GibbsModelo1.txt') 
