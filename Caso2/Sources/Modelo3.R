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

group = data$group                                  # Grupos por departamento
groupM = data$groupM                                # Grupos por municipios
groupMxD = c()                                      # Grupos de municipios por departamento
MxD = c()                                           # Municipios por departamento
for (i in unique(group)){
  groupMxD = c(groupMxD,rep(i,length(unique(groupM[group == i]))))
  MxD = c(MxD, length(unique(groupM[group == i])))
}
y = data$PUNT_GLOBAL

# HIPERPARÁMETROS:
# - nuEst
# - mu0; gamma0
# - eta0; tau0
# - nu0; sigma0
# - eps0; kappa0

nuEst = 3
mu0 = 250; gamma0 = 50 
eta0 = 1; tau0 = 50
nu0 = 1; sigma0 = 50
eps0 = 1; kappa0 = 50 

# PARÁMETROS:
# - mu, tau
# - theta; sigma
# - eps; s
# - kappa
set.seed(1305)
# ======> INICIANDO EL ALGORITMO CON LAS DISTRIBUCIONES PREVIAS:
# MEDIA DE LAS OBSERVACIONES:
mu = rnorm(1, mean = mu0, sd = gamma0)
tau = 1/rgamma(n = 1, shape = 0.5, rate = 0.5 * tau0^2) 
theta = rnorm(n = r, mean = mu, sd = tau)
sigma = 1/rgamma(n = 1, shape = 0.5, rate = 0.5 * sigma0^2)
eps = rnorm(n = n,mean = theta[groupMxD], sd = sigma)

# VARIANZA DE LAS OBSERVACIONES:
kappa = rgamma(n = 1, shape = 0.5, rate = 0.5 * eps0 * kappa0^2)
s = 1/rgamma(n = N, shape = 2, rate = 1.5 * kappa + (y - eps[groupM])^2 * 0.5)

# ===> Para guardar las simulaciones:
m = 110000
Gibbs = matrix(NA, nrow = 10000, ncol = 1148)
colnames(Gibbs) = c(paste0('eps',1:n), 'kappa', 'sigma', paste0('theta',1:r), 'tau', 'mu')
begin = sample(1:10,1)
counter = 0

# ===> Barra de progreso:
pb <- progress_bar$new(
  format = " Progreso [:bar] :percent | ETA: :eta", 
  total = m, 
  clear = FALSE, 
  width = 80
)

tictoc::tic()
for (i in 1:m){
  # cat('====>Iteración',i,'\nLos parámetros simulados son:\n',
  #     'sigma:', sigma, '\n', 
  #     'media theta:', mean(theta), '\n', 
  #     'media eps:', mean(eps), '\n', 
  #     'kappa:', kappa, '\n', 
  #     'mu', mu, '\n',
  #     'tau', tau, '\n'
  # )
  # ==========> ACTUALIZANDO LA MEDIA DE LAS OBSERVACIONES:
  # ===> Actualizando mu:
  Den = 1/(r/tau + 1/2500)
  mu = rnorm(n = 1, mean = (sum(theta)/tau + 0.1) * Den, sd = sqrt(Den))
  
  # ===> Actualizando tau:
  # m + eta0 / 2 = 16.5; eta0 * tau0 / 2 = 1250
  tau = 1/rgamma(n = 1, shape = 16.5, rate = 1250 + sum((theta - mu)^2) * 0.5)
  
  # ===> Actualizando theta:
  Den = 1/(MxD/sigma + 1/tau)
  theta = rnorm(n = r, mean = Den * (rowsum(eps, groupMxD)/sigma + mu/tau) , sd = sqrt(Den))
  
  # ===> Actualizando sigma: 
  # No municipios + nu0 / 2 = 556.5; nu0*sigma0^2 = 2500 ###### ¿Doble suma?
  sigma = 1/rgamma(n = 1, shape = 556.5, rate = 1250 + 0.5 * (sum((eps - theta[groupMxD])^2)))
  
  # ===> Actualizando epsilon:
  Den = 1/(rowsum(1/s, groupM) + 1/sigma)
  eps = rnorm(n = n, mean = Den * (rowsum(y/s, groupM) + theta[groupMxD]/sigma), sd = sqrt(Den)) 
  
  # ==========> ACTUALIZANDO LA VARIANZA DE LAS OBSERVACIONES:
  # ===> Actualizando kappa:
  # eps0 + nuEst * r /2 = 485
  kappa = rgamma(n = 1, shape = 787592, rate = 1250 + sum(1/s) * 1.5)
  
  # ===> Actualizando s:
  s = 1/rgamma(n = N, shape = 2, rate = 1.5 * kappa + (y - eps[groupM])^2 * 0.5)
  
  if (i > 10000 & (i - begin) %% 10 == 0) {
    counter = counter + 1 
    Gibbs[counter,] = c(eps, kappa, sigma, theta, tau, mu)
  }
  pb$tick()
}
tictoc::toc()

fwrite(as.data.table(Gibbs), file = 'datos/GibbsModelo3.txt')