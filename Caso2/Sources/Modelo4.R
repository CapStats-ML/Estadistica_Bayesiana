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

# HIPERPARÁMETROS: 
# nuEst
# mu0; gamma0
# eta0; tau0
# eps0; kappa0
# alpha0; beta
# lamba0

nuEst = 3
mu0 = 250; gamma0 = 50
eta0 = 1; tau0 = 50
eps0 = 1; kappa0 = 50
alpha0 = 1; beta = 1/2500
lambda0 = 1

# PARÁMETROS:
# - mu; tau
# - nu; sigma
# - thetak; sigmak
# - eps
# - kappa
# - s (Variable latente)

set.seed(1305)
# INICIALIZACIÓN DE LOS PARÁMETROS CON LA DISTRIBUCIÓN PREVIA:
mu = rnorm(n = 1, mean = mu0, sd = gamma0)
tau = 1/rgamma(n = 1, shape = eta0/2, rate = eta0 * (tau0^2)/2)

nu = 1; nus0 = 1:50
sigma = rgamma(n = 1, shape = alpha0/2, rate = beta/2)
  
thetak = rnorm(n = r, mean = mu, sd = sqrt(tau))
sigmak = 1/rgamma(n = r, shape = nu/2, rate = nu * sigma * 0.5)

eps = rnorm(n = n, mean = thetak[groupM], sd = sigmak[group])

kappa = 1/rgamma(n = 1, shape = eps0/2, rate =  0.5 * eps0 * kappa0^2) 

s = 1/rgamma(n = N, shape = 2, rate = 1.5 * kappa + 0.5 * (y - eps[groupM])^2)

m = 110000
counter = 0
Gibbs = matrix(NA, ncol = 1181, nrow = 10000)
colnames(Gibbs) = c(paste0('eps', 1:n), paste0('theta',1:r), paste0('sigma',1:r), 
                    'mu', 'tau', 'nu', 'sigma', 'kappa')
pb <- progress_bar$new(
  format = " Progreso [:bar] :percent | ETA: :eta", 
  total = m, 
  clear = FALSE, 
  width = 80
)
begin = sample(1:10, size = 1)

for (i in 1:m){
  # ===> Actualizando mu:
  Den = 1/(r/tau + 0.0004)
  mu = rnorm(n = 1, 
             mean = (sum(thetak)/tau + 0.1) * Den, 
             sd = Den^(1/2))
  
  # ===> Actualizando tau:
  # eta0 + m / 2 = 16.5
  vector = thetak - mu
  tau = 1/rgamma(n = 1, 
                 shape = 16.5,
                 rate = 1250 + 0.5 * t(vector) %*% vector)
  
  # ===> Actualizando nu:   (Checking)
  lpnu = nus0 * r/2 * log( nus0 * sigma * 0.5) - r * log(gamma(nus0 * 0.05)) - 
    nus0 * 0.5 * sum(log(sigmak)) - nus0 * (lambda0  + sigma/2 * sum(1/sigmak))
  nu = sample(x = nus0, size = 1, prob = exp(lpnu - max(lpnu)))
  
  # ===> Actualizando sigma:
  sigma = rgamma(n = 1,
                 shape = 0.5 + 16 * nu, 
                 rate = 0.0002 + nu * sum(1/sigmak) * 0.5)
  
  # ===> Actualizando thetak:
  Den = 1/(MxD/sigmak + 1/tau)
  thetak = rnorm(n = r, 
                 mean = Den * (rowsum(eps, groupMxD)/sigmak + mu/tau), 
                 sd = sqrt(Den))
  
  # ===> Actualizando sigmak:
  vector = eps - thetak[groupMxD]
  sigmak = 1/rgamma(n = r, 
                    shape = 0.5 * (nu + MxD), 
                    rate = 0.5* (nu * sigma + t(vector) %*% vector))
  
  # ===> Actulizando epsilon: 
  vector = thetak/sigmak
  Den = 1/(rowsum(1/s, groupM) + 1/sigma)
  eps = rnorm(n = n, 
              mean = ((thetak/sigmak)[groupMxD] + rowsum(y/s, groupM)) * Den, 
              sd = sqrt(Den))
  
  # ===> Actualizando kappa:
  kappa = rgamma(n = 1, 
                 shape = 787592,
                 rate = 1250 + 1.5 * sum(1/s))
  
  # ===> Actualizando s:
  s = 1/rgamma(n = N, 
               shape = 2, 
               rate = 1.5 * kappa + 0.5 * (y - eps[groupM])^2)
  
  
  # colnames(Gibbs) = c(paste0('eps', 1:n), paste0('theta',1:r), paste0('sigma',1:r), 
  #                     'mu', 'tau', 'nu', 'sigma', 'kappa')
  
  if (i > 10000 & (i - begin) %% 10 == 0) {
    counter = counter + 1 
    Gibbs[counter,] = c(eps, thetak, sigmak, mu, tau, nu, sigma, kappa)
  }
  pb$tick()
}

fwrite(Gibbs, file = 'datos/GibbsModelo4.txt')