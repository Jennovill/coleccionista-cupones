
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(multcompView)
library(agricolae)

simular_una_coleccion <- function(n_cupones) {
  
  cupones_obtenidos <- c()
  intentos <- 0
  
  while (length(unique(cupones_obtenidos)) < n_cupones) {
    cupon <- sample(n_cupones, 1)
    cupones_obtenidos <- c(cupones_obtenidos, cupon)
    intentos <- intentos + 1
  }
  
  return(intentos)
}

ejecutar_simulacion <- function(n_cupones, n_simulaciones = 1000) {
  
  resultados <- numeric(n_simulaciones)
  
  for (i in 1:n_simulaciones) {
    resultados[i] <- simular_una_coleccion(n_cupones)
  }
  return(resultados)
}

comparar_diferentes_n <- function(lista_n, n_simulaciones = 1000) {
  
  resultados_comparacion <- list()
  
  for (n in lista_n) {
    resultados <- ejecutar_simulacion(n, n_simulaciones)
    
    promedio <- mean(resultados)
    p_teorico <- n * sum(1 / (1:n))
    varianza <- var(resultados)
    var_teorico <- sum(((1:n - 1) * n) / (n - (1:n) + 1)^2)
    
    resultados_comparacion[[as.character(n)]] <- list(
      n = n,
      promedio = promedio,
      p_teorico = p_teorico,
      varianza = varianza,
      var_teorico = var_teorico,
      resultados = resultados
    )
  }
  
  return(resultados_comparacion)
}


lista_n <- c(4, 5, 6)

set.seed(123)

resultados_comparacion <- comparar_diferentes_n(lista_n)

solo4 <- resultados_comparacion[[1]]$resultados

ggplot(x = solo4)

geom_density(alpha = 0.3) +
  labs(title = "Comparación de Densidades para Diferentes n",
       x = "Intentos para completar colección", y = "Densidad",
       color = "n", fill = "n") +
  theme_minimal()


df <- bind_rows(lapply(names(resultados_comparacion), function(n) {
  data.frame(
    n = as.integer(n),
    intentos = resultados_comparacion[[n]]$resultados
  )
}))

ggplot(df, aes(x = intentos, color = factor(n), fill = factor(n))) +
  geom_density(alpha = 0.3) +
  labs(title = "Comparación de Densidades para Diferentes n",
       x = "Intentos para completar colección", y = "Densidad",
       color = "n", fill = "n") +
  theme_minimal()

ggplot(df, aes(x = factor(n), y = intentos, fill = factor(n))) +
  geom_violin(trim = FALSE, alpha = 0.4) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Comparación de medias por gráfico de violín y boxplot",
       x = "Número de cupones (n)", y = "Número de intentos") +
  theme_minimal()

kruskal.test(df$intentos ~ as.factor(df$n), data = df)

pairwise.wilcox.test(df$intentos, df$n, p.adjust.method = "BH")



# ----------------------------------------------------------------------

simular_coleccion <- function(n_cupones, n_simulaciones = 1000) {
  
  resultados <- numeric(n_simulaciones)
  
  for (i in 1:n_simulaciones) {
    intentos <- 0
    for (k in 1:n_cupones) {
      p_nuevo <- (n_cupones - k + 1) / n_cupones
      intentos <- intentos + rgeom(1, prob = p_nuevo) + 1
    }
    resultados[i] <- intentos
  }
  
  return(resultados)
}

