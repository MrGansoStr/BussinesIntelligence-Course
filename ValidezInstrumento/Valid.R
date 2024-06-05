library(car)

# Simulamos un conjunto de datos
set.seed(123)
data <- data.frame(
  group = factor(rep(c("A", "B", "C"), each = 90)),
  covariate = rnorm(90, mean = 50, sd = 10),
  item1 = sample(1:5, 90, replace = TRUE),
  item2 = sample(1:5, 90, replace = TRUE),
  item3 = sample(1:5, 90, replace = TRUE),
  item4 = sample(1:5, 90, replace = TRUE)
)

write.csv(data, "./simulated_data.csv", row.names = FALSE)

# Para simplificar, asumimos que item1 y item2 miden un constructo (dv1) y item3 y item4 miden otro constructo (dv2).
data$dv1 <- rowMeans(data[, c("item1", "item2")])
data$dv2 <- rowMeans(data[, c("item3", "item4")])

# Realizamos el MANCOVA
mancova_model <- manova(cbind(dv1, dv2) ~ group + covariate, data = data)

# Resumen del modelo MANCOVA
summary(mancova_model)

# Pruebas de efectos
summary.aov(mancova_model)
