
set.seed(322)
library(mice)
library(rsample)

encuesta <- read.csv("FinalProject1.csv")

# Función para calcular la moda
calcular_moda <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Función de imputación por moda
imputar_moda <- function(x) {
  if (is.numeric(x)) {
    moda <- calcular_moda(x)
    x[is.na(x)] <- moda
  } else if (is.factor(x) || is.character(x)) {
    moda <- calcular_moda(x)
    x[is.na(x)] <- moda
  }
  return(x)
}

# Aplicar la imputación por moda a cada columna
encuesta_imputada <- as.data.frame(lapply(encuesta, imputar_moda))

# Verificar si quedan NA
sum(is.na(encuesta_imputada))

# Mostrar la estructura del dataset original y el imputado
str(encuesta)
str(encuesta_imputada)

encuesta <- encuesta_imputada

computos_boot <- bootstraps(encuesta, times = 10)
computos_boot

first_computos_boot <- computos_boot$splits[[1]]
first_computos_boot 
as.data.frame(first_computos_boot)

# Crear una lista para almacenar todas las muestras bootstrap
all_bootstrap_samples <- lapply(computos_boot$splits, function(split) {
  as.data.frame(split)
})

# Combinar todas las muestras bootstrap en un solo data frame
library(dplyr)

all_combined <- bind_rows(all_bootstrap_samples)

# summary(data)
# summary(all_combined)

# encuesta$tamano_empresa <- factor(encuesta$tamano_empresa, levels = c(1, 2, 3))
# encuesta$sector_empresa <- factor(encuesta$sector_empresa, levels = c(1, 2, 3, 4, 5))
# encuesta$rol_empresa <- factor(encuesta$rol_empresa, levels = c(1, 2, 3, 4, 5))
# encuesta$uso_actual_IA <- factor(encuesta$uso_actual_IA, levels = c(0, 1))
# encuesta$consideracion_futura_IA <- factor(encuesta$consideracion_futura_IA, levels = c(0, 1))
# encuesta$familiaridad_IA <- factor(encuesta$familiaridad_IA, levels = c(1, 2, 3, 4))
# encuesta$intencion_inversion_IA <- factor(encuesta$intencion_inversion_IA, levels = c(0, 1, 2))
# encuesta$presupuesto_IA <- factor(encuesta$presupuesto_IA, levels = c(1, 2, 3))
# encuesta$influencia_competencia <- factor(encuesta$influencia_competencia, levels = c(1, 2, 3, 4))
# encuesta$experiencia_previa_IA <- factor(encuesta$experiencia_previa_IA, levels = c(0, 1))
# encuesta$evaluacion_experiencia_IA <- factor(encuesta$evaluacion_experiencia_IA, levels = c(1, 2, 3, 4, 5))
# encuesta$preocupaciones_eticas <- factor(encuesta$preocupaciones_eticas, levels = c(1, 2, 3, 4))

encuesta <- all_combined

# DUMMY
encuesta <- cbind(encuesta, model.matrix(~ tipo_herramienta_IA - 1, data = encuesta))
encuesta <- cbind(encuesta, model.matrix(~ beneficios_IA - 1, data = encuesta))
encuesta <- cbind(encuesta, model.matrix(~ barreras_adopcion_IA - 1, data = encuesta))
encuesta <- cbind(encuesta, model.matrix(~ soporte_adopcion_IA - 1, data = encuesta))
encuesta <- cbind(encuesta, model.matrix(~ funcionalidades_interes_IA - 1, data = encuesta))
encuesta <- cbind(encuesta, model.matrix(~ fuente_informacion_IA - 1, data = encuesta))
encuesta <- cbind(encuesta, model.matrix(~ principales_preocupaciones_eticas - 1, data = encuesta))

str(encuesta)

# Análisis factorial
library(psych)
fa_result <- fa(all_combined, nfactors = 4, rotate = "varimax")
print(fa_result)

library(psych)
library(ggplot2)
library(corrplot)
library(reshape2)

# Matriz de Cargas Factoriales
loadings <- fa_result$loadings
loadings_df <- as.data.frame(loadings)

# Data frame de comunalidades y unicidades
communalities_df <- data.frame(Variable = rownames(fa_result$loadings), 
                               h2 = fa_result$communality,
                               u2 = fa_result$uniquenesses)

# Plot comunalidades
ggplot(communalities_df, aes(x = reorder(Variable, -h2), y = h2)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(x = "Variables", y = "Comunalidad (h2)") +
  theme_minimal()

# Plot unicidades
ggplot(communalities_df, aes(x = reorder(Variable, -u2), y = u2)) +
  geom_bar(stat = "identity", fill = "coral") +
  coord_flip() +
  labs(x = "Variables", y = "Unicidad (u2)") +
  theme_minimal()


library(factoextra)


# Convertir el resultado del análisis factorial a un objeto adecuado para factoextra
loadings <- fa_result$loadings
scores <- fa_result$scores

# # Crear un biplot de los resultados del análisis factorial
# fviz_pca_biplot(loadings, 
#                 label = "var", # Mostrar las etiquetas de las variables
#                 habillage = "none", # No usar color por grupos
#                 addEllipses = TRUE, # Añadir elipses de confianza
#                 col.var = "blue", # Color de las variables
#                 col.ind = "cos2", # Color de los individuos por la calidad de la representación
#                 gradient.cols = c("white", "blue", "red"), # Colores para el gradiente
#                 repel = TRUE) + # Evitar la superposición de etiquetas
#   theme_minimal()

# Otra forma es usar ggplot2 para crear un scatter plot de los scores
# Convertir scores a data frame
scores_df <- as.data.frame(scores)
scores_df$Group <- as.factor(rownames(scores_df))

# Graficar los grupos
ggplot(scores_df, aes(x = MR1, y = MR2, color = Group)) +
  geom_point(size = 3) +
  labs(x = "Factor 1 (MR1)", y = "Factor 2 (MR2)", title = "Grupos en el Espacio de los Factores") +
  theme_minimal()


library(ggforce)

ggplot(scores_df, aes(x = MR1, y = MR2, color = Group)) +
  geom_point(size = 3) +
  geom_mark_ellipse(aes(fill = Group), alpha = 0.2) +
  labs(x = "Factor 1 (MR1)", y = "Factor 2 (MR2)", title = "Grupos con Elipses de Confianza") +
  theme_minimal()


# Graficar la carga factorial
psych::fa.diagram(fa_result)

# Gráfico de cargas factoriales
fa.plot(fa_result)
