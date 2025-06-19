library(corrplot)
library(ggplot2)
library(reshape2)
library(patchwork)
library(stringr) # Útil para str_wrap si los títulos son muy largos
library(tidyverse) 
library(scales) 
library(dplyr)
library(caret)
library(pROC)

# Bosques Aleatorios -------------------------

set.seed(123)

# 1-Lectura datos -----------------
#Datos Originales (train)
setwd("C:/Users/edama/Desktop/Ucr_2025/Analisis_Datos/Bitacora_3")
datos <- read.csv("train.csv") 
#Quitar valores faltantes
# Crear un dataset limpio (solo filas sin NA)
datos <- na.omit(datos)

#Convertir a factor las variables categóricas
datos <- datos %>%
  mutate(across(c(Customer.Type, Inflight.wifi.service, Departure.Arrival.time.convenient,
                  Ease.of.Online.booking, Gate.location, Food.and.drink, Online.boarding, Seat.comfort,
                  Inflight.entertainment, On.board.service, Leg.room.service, Baggage.handling,
                  Checkin.service, Inflight.service, Cleanliness), as.factor))

#Datos test-------------------------------------------------------------------------
setwd("C:/Users/edama/Desktop/Ucr_2025/Analisis_Datos/Bitácora_1")
TEST <- read.csv("test.csv") 
TEST_MOD <- TEST[, c("Customer.Type", "Inflight.wifi.service", "Departure.Arrival.time.convenient",
                     "Ease.of.Online.booking", "Gate.location", "Food.and.drink", "Online.boarding", "Seat.comfort",
                     "Inflight.entertainment", "On.board.service", "Leg.room.service", "Baggage.handling",
                     "Checkin.service", "Inflight.service", "Cleanliness", "Departure.Delay.in.Minutes",
                     "Arrival.Delay.in.Minutes", "satisfaction")]
#Convertir a factor las variables categóricas
TEST_MOD <- TEST_MOD %>%
  mutate(across(c(Customer.Type, Inflight.wifi.service, Departure.Arrival.time.convenient,
                  Ease.of.Online.booking, Gate.location, Food.and.drink, Online.boarding, Seat.comfort,
                  Inflight.entertainment, On.board.service, Leg.room.service, Baggage.handling,
                  Checkin.service, Inflight.service, Cleanliness), as.factor))
# Crear un dataset limpio (solo filas sin NA)
TEST_MOD <- na.omit(TEST_MOD)
# Convertir a variable binaria
datos$satisfaction_bin <- ifelse(datos$satisfaction == "satisfied", 1, 0)
TEST_MOD$satisfaction_bin <- ifelse(TEST_MOD$satisfaction == "satisfied", 1, 0)

#2- Modelo Bosques Aleatorios----------------------
# Seleccionar variables predictoras
predictoras <- c("Inflight.wifi.service", "Online.boarding", "Seat.comfort",
                 "Inflight.entertainment", "On.board.service", "Leg.room.service", "Baggage.handling",
                 "Checkin.service")

# Filtrar solo columnas necesarias
datos_modelo <- datos[, c(predictoras, "satisfaction_bin")]

# Convertir satisfaction_bin a factor con etiquetas "No" y "Yes"
datos_modelo$satisfaction_bin <- factor(datos_modelo$satisfaction_bin, levels = c(0, 1), labels = c("No", "Yes"))

# Definir fórmula
formula_rf <- as.formula("satisfaction_bin ~ .")

# Definir control de entrenamiento (validación cruzada con AUC)
control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

#GRID HIPERPARAMETROS
grid <- expand.grid(
  mtry = c(3, 5, 7, 9, 11), # valores típicos entre 3 y sqrt(17)
  splitrule = c("gini", "extratrees"), # reglas comunes para clasificación
  min.node.size = c(1, 5, 10) # más pequeño = más complejidad del modelo
)

# Entrenar modelo con ranger
modelo_rf <- train(
  formula_rf,
  data = datos_modelo,
  method = "ranger",
  trControl = control,
  tuneGrid = grid,
  metric = "ROC",
  importance = 'impurity'
)

# Ver resultados
print(modelo_rf)

# Importancia de variables
varImp(modelo_rf)

# 3- Validacion--------------------------

# Asegurarse de que la variable satisfaction_bin es factor con mismos niveles
TEST_MOD$satisfaction_bin <- factor(TEST_MOD$satisfaction_bin, levels = c(0, 1), labels = c("No", "Yes"))

# Hacer predicciones con el modelo entrenado
predicciones <- predict(modelo_rf, newdata = TEST_MOD)

# Calcular la matriz de confusión
matriz_confusion <- confusionMatrix(predicciones, TEST_MOD$satisfaction_bin)

# Mostrar la matriz de confusión
print(matriz_confusion)

# 4- ROC y AUC ---------------------------------------

# Probabilidades predichas
probabilidades <- predict(modelo_rf, newdata = TEST_MOD, type = "prob")

# Crear el objeto ROC (recordando que la clase positiva es "Yes")
roc_obj <- roc(TEST_MOD$satisfaction_bin, probabilidades$Yes)

# Imprimir AUC
auc_valor <- auc(roc_obj)
print(auc_valor)

# Graficar la curva ROC
plot(roc_obj, col = "black", lwd = 3)
legend("bottomright", legend = paste("AUC =", round(auc_valor, 4)), col = "black", lwd = 3)

# Metricas---------------
# Obtener la importancia de variables
importancia <- varImp(modelo_rf)$importance

# Convertir a data frame y mover los nombres a una columna
importancia <- importancia %>%
  tibble::rownames_to_column(var = "Variable")

# Agrupar por el nombre base antes del número final
importancia$VariableBase <- gsub("[0-9]+$", "", importancia$Variable)

# Sumar importancia por variable base
importancia_agrupada <- importancia %>%
  group_by(VariableBase) %>%
  summarise(ImportanciaTotal = sum(Overall)) %>%
  arrange(desc(ImportanciaTotal)) %>%
  slice(1:20)  # Top 20

# Graficar
ggplot(importancia_agrupada, aes(x = reorder(VariableBase, ImportanciaTotal), y = ImportanciaTotal)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    #title = "Importancia Agrupada de Variables - Random Forest",
    x = "Variable",
    y = "Importancia Total"
  ) +
  theme_minimal()

# RFE
# Control del RFE con Random Forest y 5-fold cross-validation
control_rfe <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Ejecutar RFE
resultados_rfe <- rfe(
  x = datos_modelo[, predictoras],
  y = datos_modelo$satisfaction_bin,
  sizes = c(5, 10, 15, 17),  # Ajustar según el total de variables
  rfeControl = control_rfe
)

# Ver resultados
print(resultados_rfe)

# Variables seleccionadas como óptimas
predictors(resultados_rfe)

# Graficar rendimiento según número de variables
plot(resultados_rfe, type = c("g", "o"))

# Parece que el mejor caso es con 17 variables.

#AUC INDIVIDUAL-------------
library(pROC)

# AUC por variable predictora (una a una)
auc_individual <- sapply(predictoras, function(var) {
  roc_obj <- roc(datos_modelo$satisfaction_bin, as.numeric(datos_modelo[[var]]))
  auc(roc_obj)
})

# Ordenar y visualizar
sort(auc_individual, decreasing = TRUE)

#RANKING VARIABLES-------------
# Crear tabla con los datos de la imagen
tabla_metricas <- data.frame(
  Variable = c("Online.boarding", "Inflight.entertainment", "Seat.comfort", "Inflight.wifi.service",
               "Leg.room.service", "On.board.service", "Checkin.service", "Cleanliness",
               "Departure.Arrival.time.convenient", "Customer.Type", "Baggage.handling",
               "Inflight.service", "Food.and.drink", "Ease.of.Online.booking", "Gate.location",
               "Arrival.Delay.in.Minutes", "Departure.Delay.in.Minutes"),
  Importancia = c("Alta", "Media", "Baja", "Alta", "Media", "Baja", "Baja", "Baja", "Media",
                  "Baja", "Baja", "Baja", "Baja", "Baja", "Media", "Baja", "Baja"),
  RFE = c(3, 10, 6, 1, 5, 11, 4, 12, 2, 8, 7, 9, 15, 13, 16, 14, 17),
  AUC_Individual = c(0.8129593, 0.7269378, 0.7048787, 0.6636836, 0.6808150, 0.6861516,
                     0.6316279, 0.6724403, 0.4716030, 0.5731362, 0.6510238, 0.6486835,
                     0.6178719, 0.6010133, 0.4997807, 0.4468782, 0.4640737)
)
# Reescalar la importancia para que: Alta = 1, Media = 2, Baja = 3
tabla_metricas <- tabla_metricas %>%
  mutate(
    Ranking_Importancia = recode(Importancia, "Alta" = 1, "Media" = 2, "Baja" = 3),
    Ranking_AUC = rank(-AUC_Individual),  # Negativo para que el mayor AUC sea el mejor (1)
    Ranking_RFE = RFE,
    Ranking_Promedio = rowMeans(cbind(Ranking_Importancia, Ranking_AUC, Ranking_RFE))
  ) %>%
  arrange(Ranking_Promedio)
# Ver la tabla ordenada de mayor a menor relevancia
print(tabla_metricas[, c("Variable", "Importancia", "RFE", "AUC_Individual", "Ranking_Promedio")])

