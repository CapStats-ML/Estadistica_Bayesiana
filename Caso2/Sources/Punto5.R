# =============================== PUNTO 5 ======================================
# En un gráfico con cuatro paneles (2x2) hacer el DAG de los modelos M1, M2, M3 y M4.
# ==============================================================================

setwd("~/REPOS GIT/Estadistica_Bayesiana/Caso2")

library(intergraph)
library(igraph)
library(latex2exp)

# ================================ MODELO 1 ====================================
Modelo1 = graph(edges = c(1, 2, 
                          1, 3, 
                          2, 4,
                          2, 5,
                          4, 8,
                          4, 9,
                          5, 10,
                          5, 11,
                          3, 6,
                          3, 7,
                          7, 12,
                          7, 13), directed = TRUE)

V(Modelo1)$label = c(TeX("$y_{i,j}$"), 
                     TeX("$\\theta_{j}$"), TeX("$\\S^2$"),
                     TeX('$\\mu$'), TeX('$\\tau^2$'), TeX('$\\nu^2$'), TeX('$\\sigma^2$'),
                     TeX('$\\mu_0$'), TeX('$\\gamma^2_0$'), TeX('$\\eta_0$'), TeX('$\\tau^2_0$'), TeX('$\\nu_0$'),TeX('$\\sigma^2_0$'))

V(Modelo1)$label.color = "black"; V(Modelo1)$color = "white"
V(Modelo1)[1:5]$shape = 'circle'; V(Modelo1)[8:13]$shape = "square"
V(Modelo1)[7]$shape = 'circle'; V(Modelo1)[6]$shape = "square"
V(Modelo1)$size = 25
E(Modelo1)$arrow.size = 0.4; E(Modelo1)$color = 'black'

# ================================ MODELO 2 ====================================

Modelo2 = graph(edges = c(1, 2,
                          1, 3,
                          2, 4,
                          2, 5,
                          4, 6,
                          4, 7,
                          5, 8,
                          5, 9,
                          3, 10, 
                          3, 11,
                          11, 12,
                          11, 13,
                          12, 14, 
                          13, 15,
                          13, 16), directed = TRUE)

V(Modelo2)$label = c(TeX("$y_{i,j}$"), 
                     TeX("$\\theta_{j}$"), TeX("$\\S^2$"),
                     TeX('$\\mu$'), TeX('$\\tau^2$'),
                     TeX('$\\mu_0$'), TeX('$\\gamma^2_0$'),
                     TeX('$\\eta_0$'), TeX('$\\tau^2_0$'),
                     TeX('$\\nu^*$'), TeX('$\\sigma^2_j$'),
                     TeX('$\\nu$'),TeX('$\\sigma^2$'),
                     TeX('$\\lambda_0$'),TeX('$\\alpha_0$'), TeX('$\\beta_0$'))

V(Modelo2)$label.color = "black"; V(Modelo2)$color = "white"
V(Modelo2)[1:5]$shape = 'circle'
V(Modelo2)[11:13]$shape = 'circle'
V(Modelo2)[10]$shape = 'circle'
V(Modelo2)[6:9]$shape = "square"
V(Modelo2)[14:16]$shape = "square"
V(Modelo2)$size = 25
E(Modelo2)$arrow.size = 0.4; E(Modelo2)$color = 'black'

# ================================ MODELO 3 ====================================

Modelo3 = graph(edges = c(
  1, 2,
  1, 3,
  2, 4,
  2, 5,
  4, 6,
  4, 7,
  6, 8,
  6, 9,
  7, 10,
  7, 11,
  5, 12,
  5, 13,
  3, 14,
  3, 15,
  15, 16,
  15, 17
), directed = TRUE)

V(Modelo3)$label = c(TeX("$y_{i,j}$"), 
                     TeX("$\\xi_{jk}$"), TeX("$\\S^2_{ijk}$"),
                     TeX('$\\theta_k$'), TeX('$\\sigma^2$'),
                     TeX('$\\mu$'), TeX('$\\tau^2$'),
                     TeX('$\\mu_0$'), TeX('$\\gamma^2_0$'),
                     TeX('$\\eta_0$'), TeX('$\\tau^2_0$'),
                     TeX('$\\nu_0$'),TeX('$\\sigma^2_0$'),
                     TeX('$\\nu^{*}$'), TeX('$\\kappa^2$'),
                     TeX('$\\xi_0$'), TeX('$\\kappa_^2{0}$'))

V(Modelo3)$label.color = "black"; V(Modelo3)$color = "white"
V(Modelo3)[1:7]$shape = 'circle'
V(Modelo3)[15]$shape = 'circle'
V(Modelo3)[14]$shape = 'square'
V(Modelo3)[8:13]$shape = "square"
V(Modelo3)[16:17]$shape = "square"
V(Modelo3)$size = 25

E(Modelo3)$arrow.size = 0.4; E(Modelo3)$color = 'black'

# ================================ MODELO 4 ====================================

Modelo4 = graph(edges = c(
  1, 2,
  1, 3,
  2, 4,
  2, 5,
  4, 6,
  4, 7,
  6, 8,
  6, 9,
  7, 10,
  7, 11,
  5, 12,
  5, 13,
  12, 14,
  13, 15,
  13, 16,
  3, 17,
  3, 18,
  18, 19,
  18, 20
), directed = TRUE)


V(Modelo4)$label = c(TeX("$y_{i,j}$"), 
                     TeX("$\\epsilon_{jk}$"), TeX("$\\S^2_{ijk}$"),
                     TeX('$\\theta_k$'), TeX('$\\sigma^2_k$'),
                     TeX('$\\mu$'), TeX('$\\tau^2$'),
                     TeX('$\\mu_0$'), TeX('$\\gamma^2_0$'),
                     TeX('$\\eta_0$'), TeX('$\\tau_0$'),
                     TeX('$\\nu$'),TeX('$\\sigma^2$'),
                     TeX('$\\lambda_0$'), TeX('$\\alpha_0$'), TeX('$\\beta_0$'),
                     TeX('$\\nu^{*}$'), TeX('$\\kappa_^2$'),
                     TeX('$\\epsilon_0$'), TeX('$\\kappa^2_0$'))

V(Modelo4)$label.color = "black"; V(Modelo4)$color = "white"
V(Modelo4)$shape = 'circle'  # Primero, asignamos todos a círculo
V(Modelo4)[c(8,9,10,11,14,15,16,17,19,20)]$shape = "square"  # Luego, asignamos los cuadrados correctamente
V(Modelo4)$size = 25

E(Modelo4)$arrow.size = 0.4; E(Modelo4)$color = 'black'


# =============================== LA GRILLA ====================================

png("Punto5.png",res = 300, units = "px", width = 2000, height = 2000)
par(mfrow = c(2,2), tcl  = 0.5, mar = c(1, 1, 2, 1), mgp = c(1.5, 0.5, 0),
    mai = c(0.3, 0, 0.3, 0), oma = c(1, 0, 1, 0))

plot(Modelo1, layout = layout.reingold.tilford, vertex.label.cex = 1,
     vertex.label.font = 2, edge.arrow.mode = T, ylim = c(1.5, -1.5))
title("Modelo 1", line=-0.2, cex.main = 1.2)
mtext("a) Modelo t con medias\nespecíficas por departamento", side = 1,
      line = 0.7, at = 0.1, cex = 0.8)

plot(Modelo2, layout = layout.reingold.tilford, vertex.label.cex = 1,
     vertex.label.font = 2, edge.arrow.mode = T, ylim = c(1.5, -1.5))
title("Modelo 2", line=-0.2, cex.main = 1.2)
mtext("b) Modelo t con medias y\nvarianzas específicas por departamento",
      side = 1, line = 0.7, at = 0.1, cex = 0.8)

plot(Modelo3, layout = layout.reingold.tilford, vertex.label.cex = 1,
     vertex.label.font = 2, edge.arrow.mode = T, ylim = c(1.5, -1.5))
title("Modelo 3",line=-0.2, cex.main = 1.2)
mtext("c) Modelo t con medias\nespecíficas por municipio y departamento",
      side = 1, line = 0.7, at = 0.1, cex = 0.8)

plot(Modelo4, layout = layout.reingold.tilford, vertex.label.cex = 1,
     vertex.label.font = 2, edge.arrow.mode = T, ylim = c(1.5, -1.5))
title("Modelo 4", line=-0.2, cex.main = 1.2)
mtext("d) Modelo t con medias y varianzas\nespecíficas por municipio y departamento",
      side = 1, line = 0.7, at = 0.1, cex = 0.8)

par(cex.main = 1.2)
title(main = "DAG PARA LOS MODELOS BAYESIANOS PROPUESTOS", outer = TRUE,
      line = -0.5, font.main = 2)
dev.off()






