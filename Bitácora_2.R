library(corrplot)
library(ggplot2)
library(reshape2)
library(patchwork)
library(stringr) # Útil para str_wrap si los títulos son muy largos
library(tidyverse) 
library(scales) 
library(dplyr)


#Datos Originales (train)-----------------------------------------------------------------------------------------------
setwd("C:/Users/edama/Desktop/Ucr_2025/Analisis_Datos/Bitácora_1")
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
                   "Arrival.Delay.in.Minutes")]
#Regresión Binaria-----------------------------------------------------------------------------------------------------
# Convertir a variable binaria
datos$satisfaction_bin <- ifelse(datos$satisfaction == "satisfied", 1, 0)

# Verificar la estructura
str(datos$satisfaction_bin)

modelo_reg_bin <- glm(satisfaction_bin ~ Customer.Type + Inflight.wifi.service + Departure.Arrival.time.convenient +
                Ease.of.Online.booking + Gate.location + Food.and.drink + Online.boarding + Seat.comfort +
                Inflight.entertainment + On.board.service + Leg.room.service + Baggage.handling +
                Checkin.service + Inflight.service + Cleanliness + Departure.Delay.in.Minutes +
                Arrival.Delay.in.Minutes,
              data = datos, family = "binomial")

##Resumen----------------------------------------------------------------------------------------------------------------
#summary(modelo_reg_bin)
#Un p-valor < 0.05 sugiere que esa variable es significativa para explicar la satisfacción.
#El signo del coeficiente indica si el efecto sobre la probabilidad de satisfacción es positivo o negativo.

#PRUEBA ANOVA
anova(modelo_reg_bin, test = "Chisq")


##Odds Ratios e intervalos de confianza----------------------------------------------------------------------------------
#exp(cbind(OddsRatio = coef(modelo_reg_bin), confint(modelo_reg_bin)))
#Un odds ratio > 1 indica un aumento en la probabilidad de estar satisfecho.
#Un odds ratio < 1 indica una disminución.
#También se calculan intervalos de confianza para los odds ratios.


#Evaluación del Modelo
##Matriz de confusión-----------------------------------------------------------------------------------------------------
predicciones <- ifelse(predict(modelo_reg_bin, type = "response") >= 0.5, 1, 0)
table(Predicho = predicciones, Real = datos$satisfaction_bin)

#Se generan predicciones de satisfacción:
#Si la probabilidad predicha ≥ 0.5, se clasifica como satisfecho (1),
#De lo contrario, como no satisfecho (0).
#La matriz de confusión muestra:
#Verdaderos positivos (TP): predijo 1 y era 1.
#Verdaderos negativos (TN): predijo 0 y era 0.
#Falsos positivos (FP): predijo 1 y era 0.
#Falsos negativos (FN): predijo 0 y era 1.

##Exactitud (accuracy)-----------------------------------------------------------------------------------------------------
mean(predicciones == datos$satisfaction_bin)
#Calcula el porcentaje de predicciones correctas.
#Es una medida global de desempeño del modelo.
#Sin embargo, puede ser engañosa si hay desbalance de clases (por ejemplo, muchos más 1 que 0).


##ROC y AUC----------------------------------------------------------------------------------------------------------------
library(pROC)
roc_obj <- roc(datos$satisfaction_bin, predict(modelo_reg_bin, type = "response"))
plot(roc_obj)
auc(roc_obj)
#ROC (Receiver Operating Characteristic):
#Grafica la tasa de verdaderos positivos vs. la tasa de falsos positivos.
#Una curva más cercana a la esquina superior izquierda indica mejor desempeño.

#AUC (Área Bajo la Curva):
#Mide el desempeño general del modelo (independiente del umbral).
#AUC ≈ 0.5 indica un modelo aleatorio, AUC ≈ 1 indica un modelo perfecto.

##Prueba Predicción:----------------------------------------------------------------------------------------------------------
nuevo <- data.frame(
  Customer.Type = "Loyal Customer",
  Inflight.wifi.service = 4,
  Departure.Arrival.time.convenient = 3,
  Ease.of.Online.booking = 4,
  Gate.location = 2,
  Food.and.drink = 3,
  Online.boarding = 4,
  Seat.comfort = 4,
  Inflight.entertainment = 5,
  On.board.service = 4,
  Leg.room.service = 3,
  Baggage.handling = 4,
  Checkin.service = 4,
  Inflight.service = 4,
  Cleanliness = 4,
  Departure.Delay.in.Minutes = 0,
  Arrival.Delay.in.Minutes = 0
)
predict(modelo_reg_bin, newdata = TEST_MOD[3,], type = "response")  # probabilidad de satisfacción

##Prueba TEST---------------------------------------------------------------------------------------------------

for (i in 1:nrow(TEST)) {
  TEST$prob[i] <- predict(modelo_reg_bin, newdata = TEST_MOD[i,], type = "response")  # probabilidad de satisfacción
}


#Arboles de Desición------------------------------------------------------------------------------------------------------------
library(rpart)         # Para construir el árbol
library(rpart.plot)    # Para visualizar el árbol
library(pROC)     

modelo_arbol <- rpart(satisfaction_bin ~ Customer.Type + Inflight.wifi.service + Departure.Arrival.time.convenient +
                        Ease.of.Online.booking + Gate.location + Food.and.drink + Online.boarding + Seat.comfort +
                        Inflight.entertainment + On.board.service + Leg.room.service + Baggage.handling +
                        Checkin.service + Inflight.service + Cleanliness + Departure.Delay.in.Minutes +
                        Arrival.Delay.in.Minutes,
                      data = datos, method = "class")
#method = "class" indica que es un modelo de clasificación (no regresión).

##Visualizar el árbol--------------------------------------------------------------------------------------------
rpart.plot(modelo_arbol, type = 2, extra = 104, fallen.leaves = F)
rpart.plot(modelo_arbol, digits = 2)

##Matriz de Confusión-------------------------------------------------------------------------------------------
pred_arbol_prob <- predict(modelo_arbol, type = "prob")[,2]  # Probabilidad de clase 1
pred_arbol_clase <- ifelse(pred_arbol_prob >= 0.5, 1, 0)
table(Predicho = pred_arbol_clase, Real = datos$satisfaction_bin)

##Exactitud-----------------------------------------------------------------------------------------------------
mean(pred_arbol_clase == datos$satisfaction_bin)

##ROC y AUC-------------------------------------------------------------------------------------------------------------
roc_arbol <- roc(datos$satisfaction_bin, pred_arbol_prob)
plot(roc_arbol)
auc(roc_arbol)

#PCA-----------------------------------------------------------------------------------------------------
##Variables Numericas-----------------------------------------
variables_numericas <- datos[, c("Inflight.wifi.service", "Departure.Arrival.time.convenient",
                                 "Ease.of.Online.booking", "Gate.location", "Food.and.drink", "Online.boarding",
                                 "Seat.comfort", "Inflight.entertainment", "On.board.service", "Leg.room.service",
                                 "Baggage.handling", "Checkin.service", "Inflight.service", "Cleanliness",
                                 "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes")]

pca_result <- prcomp(variables_numericas, center = TRUE, scale. = TRUE)
summary(pca_result) #Varianza de cada componente

##Primeros componentes---------------------------------------------------------------------------------
# Por ejemplo, usar los primeros 5 componentes
pca_data <- as.data.frame(pca_result$x[, 1:5])
pca_data$satisfaction_bin <- datos$satisfaction_bin #Agregar la variable respuesta


##Ajuste modelo (regresión logística con componentes)
modelo_pca <- glm(satisfaction_bin ~ ., data = pca_data, family = "binomial")
summary(modelo_pca)

# Predicciones
pred_pca <- ifelse(predict(modelo_pca, type = "response") >= 0.5, 1, 0)

##Matriz de confusión------------------------------------------------
table(Predicho = pred_pca, Real = pca_data$satisfaction_bin)

##Exactitud
mean(pred_pca == pca_data$satisfaction_bin)

##ROC y AUC--------------------------------------------
library(pROC)
roc_pca <- roc(pca_data$satisfaction_bin, predict(modelo_pca, type = "response"))
plot(roc_pca)
auc(roc_pca)


#GXBoost-------------------------------------------------------------------------------------------------
library(xgboost)
library(Matrix)
library(pROC)

datos_xgb <- datos  # Copia de seguridad

#Convertir factores a variables dummy (one-hot)
datos_matrix <- model.matrix(satisfaction_bin ~ . -1, data = datos_xgb)

#Crear el objeto DMatrix de XGBoost
dtrain <- xgb.DMatrix(data = datos_matrix, label = datos_xgb$satisfaction_bin)

#Ajuste del Modelo
modelo_xgb <- xgboost(data = dtrain, 
                      objective = "binary:logistic", 
                      nrounds = 100, 
                      verbose = 0)
#objective = "binary:logistic" indica clasificación binaria.
#nrounds = 100 es el número de iteraciones (puedes afinarlo luego).

##Matriz Confusión---------------------------------------------------------------------------------------------
pred_prob_xgb <- predict(modelo_xgb, newdata = dtrain)
pred_clase_xgb <- ifelse(pred_prob_xgb >= 0.5, 1, 0)
table(Predicho = pred_clase_xgb, Real = datos_xgb$satisfaction_bin)

##Exactitud------------------------------------------------------------------------------------------------------
mean(pred_clase_xgb == datos_xgb$satisfaction_bin)

##ROC y AUC-----------------------------------------------------------------------------------------------------
roc_xgb <- roc(datos_xgb$satisfaction_bin, pred_prob_xgb)
plot(roc_xgb)
auc(roc_xgb)

#Tabla Comparativa-------------------------------------------------------------------------------
# Para Regresión Logística
accuracy_log <- mean(predicciones == datos$satisfaction_bin)
auc_log <- auc(roc_obj)
conf_log <- table(Predicho = predicciones, Real = datos$satisfaction_bin)

# Para Árbol de Decisión
accuracy_tree <- mean(pred_arbol_clase == datos$satisfaction_bin)
auc_tree <- auc(roc_arbol)
conf_tree <- table(Predicho = pred_arbol_clase, Real = datos$satisfaction_bin)

# Para XGBoost
accuracy_xgb <- mean(pred_clase_xgb == datos$satisfaction_bin)
auc_xgb <- auc(roc_xgb)
conf_xgb <- table(Predicho = pred_clase_xgb, Real = datos$satisfaction_bin)

# Crear un dataframe con las métricas clave
comparacion_modelos <- data.frame(
  Modelo = c("Regresión Logística", "Árbol de Decisión", "XGBoost"),
  Exactitud = c(round(accuracy_log, 4), round(accuracy_tree, 4), round(accuracy_xgb, 4)),
  AUC = c(round(auc_log, 4), round(auc_tree, 4), round(auc_xgb, 4))
)

print(comparacion_modelos)




