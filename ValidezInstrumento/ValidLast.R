library(lavaan)
library(semPlot)
library(mice)
datos <- read.csv("DataKaggle.csv")

sum(is.na(datos))

imputed_data <-  mice(datos, method="sample")
full_data <- complete(imputed_data) 

sum(is.na(full_data))

modelo <- '
  FactorA =~ A1a + A1b + A2a + A2b + A2c + A2d + A2e + A2f + A2g + A2h + A3a + A3b + A3c + A3d + A3e + A3f + A3g + A3h + A3i + A3j + A3k + A3l + A3m
  FactorB =~ B1a + B1b + B2a + B2b + B2c + B2d + B3a + B3b + B3c + B3d + B4a + B4b + B4c + B4d + B5a + B5b + B5c + B5d
  FactorC =~ C1a + C1b + C1c + C1d + C1e + C1f + C1g
'

fit <- cfa(modelo, data = full_data)



summary(fit, fit.measures = TRUE, standardized = TRUE)

semPaths(fit, "std", layout = "tree", edge.label.cex = 0.8, sizeMan = 5, sizeLat = 7, residScale = 0.7)



