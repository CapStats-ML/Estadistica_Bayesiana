
data <- fread('Data/Saber 11 2022-2.TXT', sep = ';')
dat <- data
dat <- dat[order(dat$ESTU_COD_RESIDE_MCPIO), ]
# dimensión de la base
dim(dat)


table(dat$ESTU_DEPTO_RESIDE)


(m <- length(table(dat$ESTU_DEPTO_RESIDE)))
(n <- sum(table(dat$ESTU_DEPTO_RESIDE)))



y <- dat$PUNT_GLOBAL

Y <- vector(mode = "list", length = m)
g <- rep(NA, n)
for (j in 1:m) {
  idx <- dat$ESTU_COD_RESIDE_DEPTO == sort(unique(dat$ESTU_COD_RESIDE_DEPTO))[j]
  g[idx] <- j
  Y[[j]] <- y[idx]
}

library(dplyr)

# tabla
estadisticos <- dat %>% 
  group_by(ESTU_COD_RESIDE_DEPTO) %>% 
  summarise(
    codigo = first(ESTU_COD_RESIDE_DEPTO),
    nombre = first(ESTU_DEPTO_RESIDE),
    nj = n(), 
    yb = mean(PUNT_MATEMATICAS), 
    s2 = var(PUNT_MATEMATICAS)
  ) %>% 
  ungroup() %>% 
  select(-ESTU_COD_RESIDE_DEPTO)



#### 


# ranking Bayesiano
ids2 <- estadisticos$nombre

that <- colMeans(chain1$THETA[,1:m])

ic1  <- apply(X = chain1$THETA[,1:m], MARGIN = 2,
              FUN = function(x) quantile(x, c(0.025,0.975)))

ranking <- order(that) 
ids2 <- ids2[ ranking]
that <- that[ ranking]
ic1  <- ic1 [,ranking]

colo <- rep(2,m)
colo[which(ic1[1,]>50)] <- 1
colo[which(ic1[2,]<50)] <- 3

colo <- c("royalblue","black","red")[colo]

# gráfico
par(mfrow = c(1,1), mar = c(4,10,1.5,1), mgp = c(2.5,0.75,0))

plot(NA, NA, xlab = "Puntaje", ylab = "", main = expression(italic("Ranking Bayesisano")),
     xlim = c(0,100), ylim = c(1,m), cex.axis = 0.75, yaxt = "n")

axis(side = 2, at = 1:m, labels = ids2, las = 2)
abline(v = 50,  col = "gray", lwd = 3)
abline(h = 1:m, col = "lightgray", lwd = 1)

for (j in 1:m) {
  segments(x0 = ic1[1,j], y0 = j, x1 = ic1[2,j], y1 = j, col = colo[j])
  lines(x = that[j], y = j, type = "p", pch = 16, cex = 0.8, col = colo[j])
}


# ranking de los mejores

Thetas <- M4[,1113:1176]
ic1 <- apply(X = Thetas, MARGIN = 2, FUN = function(x) quantile(x, c(0.025,0.975)))
Med <- colMeans(Thetas)

ranking <- order(Med)
ids2 <- ids2[ranking]