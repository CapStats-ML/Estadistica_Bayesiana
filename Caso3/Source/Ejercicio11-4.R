
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


# Leer el archivo línea por línea
file_url <- "https://www2.stat.duke.edu/~pdh10/FCBS/Exercises/mathstandard.dat"
lines <- readLines(file_url)

# Procesar las líneas para crear un dataframe
data_list <- lapply(lines, function(line) {
  # Dividir la línea en partes basadas en espacios
  parts <- unlist(strsplit(line, "\\s+"))
  
  # Verificar si la línea tiene 3 columnas (county, metstandard, percentms)
  if (length(parts) == 3) {
    return(parts)
  } else {
    # Si no tiene 3 columnas, devolver NA
    return(rep(NA, 3))
  }
})

# Convertir la lista en un dataframe
df <- do.call(rbind, data_list)
df <- as.data.frame(df, stringsAsFactors = FALSE)

# Asignar nombres a las columnas
colnames(df) <- c("county", "metstandard", "percentms")

# Convertir las columnas a los tipos correctos
df$metstandard <- as.numeric(df$metstandard)
df$percentms <- as.numeric(df$percentms)

# Eliminar filas con valores NA (filas problemáticas)
df <- na.omit(df)

# Mostrar el dataframe
head(df)

y <- df$metstandard
x <- df[,2]

str(df)


county_list <- unique(df$county)  
beta_estimates <- list()          

for (county in county_list) {
  county_data <- df[df$county == county, ]
  model <- glm(y ~ x, data = county_data, family = binomial)
  beta_estimates[[county]] <- coef(model)
}

# 2. Crear el gráfico base
plot(df[,2], df$metstandard, pch = 16, col = "gray", xlab = "Porcentaje de maestros con maestría",
     ylab = "Probabilidad de aprobación", main = "Funciones logísticas estimadas por condado")

# 3. Añadir las funciones logísticas estimadas para cada condado
x_values <- seq(min(df[,2]), max(df[,2]), length.out = 100)  # Valores de x para graficar

for (county in county_list) {
  # Extraer coeficientes para el condado actual
  beta0 <- beta_estimates[[county]][1]
  beta1 <- beta_estimates[[county]][2]
  
  # Calcular la función logística
  logistic_curve <- exp(beta0 + beta1 * x_values) / (1 + exp(beta0 + beta1 * x_values))
  
  # Añadir la curva al gráfico
  lines(x_values, logistic_curve, col = which(county_list == county), lwd = 2)
}

# 4. Añadir una leyenda
legend("bottomright", legend = county_list, col = 1:length(county_list), lwd = 2, title = "Condados")
