# Generar simulación del problema del coleccionista de cupones
simular_coleccion <- function(n, repeticiones, tamano) {
  resultados <- numeric(repeticiones)
  
  for (i in 1:repeticiones) {
    cupones <- logical(n)
    contador <- 0
    while (sum(cupones) < n) {
      nuevos <- sample(1:n, tamano, replace = TRUE)  # Sobres con cupones aleatorios
      cupones[nuevos] <- TRUE
      contador <- contador + 1
    }
    resultados[i] <- contador
  }
  
  return(resultados)
}

# Parámetros de la simulación
n <- 25  # Número de tipos de cupones
reps <- 10000  # Número de simulaciones (aumentado a 10,000)
tamano <- 5  # Cupones por sobre

# Ejecutar la simulación
resultados_simulacion <- simular_coleccion(n, reps, tamano)

# Valor teórico (aproximación)
valor_teorico <- n * sum(1 / (1:n)) / tamano

# Calcular el valor esperado de la simulación (promedio de los intentos)
valor_esperado_simulacion <- mean(resultados_simulacion)

# Crear el histograma con la densidad
library(ggplot2)
df <- data.frame(intentos = resultados_simulacion)

ggplot(df, aes(x = intentos)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, 
                 fill = "#5B2C8F", color = "black", alpha = 0.6) +  # Transparente
  geom_density(color = "red", size = 1.2) +
  geom_vline(aes(xintercept = valor_teorico, color = "Teórico"), linetype = "dashed", size = 1.2) +
  geom_vline(aes(xintercept = valor_esperado_simulacion, color = "Simulado"), linetype = "dashed", size = 1.2) +  # Línea del valor esperado
  labs(
    title = "Histograma de Sobres Necesarios + Densidad",
    x = "Número de Intentos (Sobres)", y = "Densidad"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 18),
    plot.title = element_text(size = 20, face = "bold"),
    legend.position = c(0.75, 0.75),  # Posiciona la leyenda en la esquina superior derecha (relativo)
    legend.title = element_text(face = "bold", size = 14),  # Título en la leyenda
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Cuadro alrededor de la leyenda
    legend.key.size = unit(1.5, "cm"),  # Aumentar tamaño de la leyenda
    panel.grid = element_blank()
  ) +
  scale_color_manual(values = c("Teórico" = "blue", "Simulado" = "green")) +
  guides(color = guide_legend(title = "Valor Esperado"))


# Función de simulación con variantes
simular_coleccion <- function(n, repeticiones, tamano, probabilidades = rep(1/n, n), sin_repetidos = FALSE, meta_porcentaje = 100) {
  resultados <- numeric(repeticiones)
  
  for (i in seq_len(repeticiones)) {
    cupones <- integer(n)
    contador <- 0
    
    # Condición de salida cuando se obtiene el porcentaje de cupones deseado
    while (sum(cupones > 0) < n * meta_porcentaje / 100) {
      
      # Sobres con varios ítems
      if (sin_repetidos) {
        nuevos <- sample(1:n, tamano, replace = FALSE, prob = probabilidades)  # Sobres sin repetidos
      } else {
        nuevos <- sample(1:n, tamano, replace = TRUE, prob = probabilidades)  # Sobres con repetidos
      }
      
      cupones[nuevos] <- cupones[nuevos] + 1
      contador <- contador + 1
    }
    
    resultados[i] <- contador
  }
  
  return(resultados)
}

# Parámetros generales de la simulación
n <- 25  # Número de tipos de cupones
reps <- 10000  # Número de simulaciones
tamano <- 5  # Cupones por sobre

# Crear el vector de probabilidades desiguales
probabilidades <- rep(0.2, n)  # Inicializamos con 0.2 para todos
probabilidades[1:20] <- 0.8    # Los primeros 20 cupones tendrán probabilidad 0.8
probabilidades <- probabilidades / sum(probabilidades)  # Normalizamos para que la suma sea 1

# Realizar simulaciones con variantes
resultados_basico <- simular_coleccion(n, reps, tamano)
resultados_probabilidades_desiguales <- simular_coleccion(n, reps, tamano, probabilidades = probabilidades)
resultados_sin_repetidos <- simular_coleccion(n, reps, tamano, sin_repetidos = TRUE)
resultados_meta_parcial <- simular_coleccion(n, reps, tamano, meta_porcentaje = 80)

# Crear el DataFrame para graficar
df <- data.frame(
  intentos = c(resultados_basico, resultados_probabilidades_desiguales, resultados_sin_repetidos, resultados_meta_parcial),
  variante = factor(rep(c("Básico", "Probabilidades Desiguales", "Sin Repetidos", "Parcial"), each = reps))
)

ggplot(df, aes(x = intentos, color = variante, fill = variante)) +
  geom_density(size = 1.2, alpha = 0.3) +  # Ajustar transparencia para relleno
  labs(
    title = "Curvas de Densidad por Variante",
    x = "Número de Sobres Necesarios", y = "Densidad"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold"),
    legend.position = c(0.6, 0.65),  # Posiciona la leyenda en la esquina superior derecha (relativo)
    legend.title = element_text(face = "bold", size = 14),  # Título en la leyenda
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Cuadro alrededor de la leyenda
    legend.key.size = unit(1, "cm"),  # Aumentar tamaño de la leyenda
    panel.grid = element_blank()
  ) +
  scale_color_manual(values = c("red", "orange", "blue", "green")) +
  scale_fill_manual(values = c("red", "orange", "blue", "green")) +
  guides(
    color = guide_legend(title = "Variante"),
    fill = guide_legend(title = "Variante")
  )+
  xlim(c(0, 100))
