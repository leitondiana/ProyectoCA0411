library(corrplot)
library(ggplot2)
library(reshape2)
library(patchwork)
library(stringr) # Útil para str_wrap si los títulos son muy largos
library(tidyverse) 
library(scales) 
library(dplyr)

#Guarda tu tema personalizado en un objeto llamado 'my_theme'
my_theme <- theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey70"),
    axis.line = element_line(color = "black")
  )

setwd("C:/Users/edama/Desktop/Ucr_2025/Analisis_Datos/Bitácora_1")
data <- read.csv("train.csv") 

#Gráfico Distribución Género
ggplot(data = data, aes(x = Gender)) +
  geom_bar(fill = "steelblue") + # fill="color" le da color a las barras
  labs(title = "Distribución por Género",
       x = "Género",
       y = "Cantidad de Pasajeros") +
  theme_minimal() # Un estilo visual limpio

#Gráfico Distribución Tipo Cliente
ggplot(data = data, aes(x = `Customer.Type`)) + 
  geom_bar(fill = "darkorange") +
  labs(title = "Distribución por Tipo de Cliente",
       x = "Tipo de Cliente",
       y = "Cantidad de Pasajeros") +
  theme_bw() # Otro estilo visual

#Gráfico Distribución Clase
ggplot(data = data, aes(x = Class)) +
  geom_bar(fill = "#69b3a2") + 
  labs(title = "Distribución por Clase de Vuelo",
       x = "Clase",
       y = "Cantidad de Pasajeros") +
  theme_light() 

#Gráfico Distribución Satisfacción General

valor_satisfecho <- "satisfied" 
valor_insatisfecho_neutral <- "neutral or dissatisfied"

data <- data %>%
  mutate(
    satisfaction_numeric = if_else(satisfaction == valor_satisfecho, 1, 0)
  )
ggplot(data = data, aes(x = satisfaction)) +
  geom_bar(fill = c("firebrick", "forestgreen")) +
  labs(title = "Distribución de Satisfacción General",
       x = "Satisfacción",
       y = "Cantidad de Pasajeros") +
  theme_classic()

#RELACIONES----------------
#Clase y Satisfacción (Proporciones)---------------
ggplot(data = data, aes(x = factor(Class), fill = factor(satisfaction_numeric))) + 
  geom_bar(position = "fill", color = "black") + 
  scale_y_continuous(labels = percent) + 
  scale_fill_manual(values = c("0" = "#FFF68F", "1" = "cadetblue2"), 
                    labels = c("0" = "Neutral / Insatisfecho", "1" = "Satisfecho"), 
                    name = "Satisfacción") + 
  labs(title = "Proporción de Satisfacción por Clase",
       x = "Clase", # Puedes hacer la etiqueta del eje X más explícita
       y = "Proporción de Pasajeros") +
  my_theme 

#Tipo de Viaje y Satisfacción (Proporciones)---------------
ggplot(data = data, aes(x = factor(Type.of.Travel), fill = factor(satisfaction_numeric))) + 
  # Ahora trata cada número (0-5) como una categoría separada en el eje X
  geom_bar(position = "fill", color = "black") + 
  scale_y_continuous(labels = percent) + 
  scale_fill_manual(values = c("0" = "#FFF68F", "1" = "cadetblue2"), 
                    labels = c("0" = "Neutral / Insatisfecho", "1" = "Satisfecho"), 
                    name = "Satisfacción") + 
  labs(title = "Proporción de Satisfacción por Tipo de Viaje",
       x = "Tipo de Viaje",
       y = "Proporción de Pasajeros") +
  my_theme 

#Tipo de Cliente y Satisfacción (Proporciones)---------------
ggplot(data = data, aes(x = factor(Customer.Type), fill = factor(satisfaction_numeric))) + 
  geom_bar(position = "fill", color = "black") + 
  scale_y_continuous(labels = percent) + 
  scale_fill_manual(values = c("0" = "#FFF68F", "1" = "cadetblue2"), 
                    labels = c("0" = "Neutral / Insatisfecho", "1" = "Satisfecho"), 
                    name = "Satisfacción") + 
  labs(title = "Proporción de Satisfacción por Tipo de Cliente",
       x = "Tipo de Cliente", # Puedes hacer la etiqueta del eje X más explícita
       y = "Proporción de Pasajeros") +
  my_theme 

#Grafico Calor---------------

# 1. Seleccionar las columnas 
columnas_numericas <- c(
  "Age", "Flight.Distance", "Inflight.wifi.service",
  "Departure.Arrival.time.convenient", "Ease.of.Online.booking", "Gate.location",
  "Food.and.drink", "Online.boarding", "Seat.comfort",
  "Inflight.entertainment", "On.board.service", "Leg.room.service",
  "Baggage.handling", "Checkin.service", "Inflight.service",
  "Cleanliness", "Departure.Delay.in.Minutes", "Arrival.Delay.in.Minutes",
  "satisfaction_numeric"
)

# Seleccionar las columnas inicialmente
data_to_convert <- data[, columnas_numericas]
names(data_to_convert)[19] <- "Satisfaction"

# --- PASO DE DEPURACIÓN: VER LOS TIPOS ORIGINALES ---
cat("Tipos de datos ANTES de la conversión:\n")
print(str(data_to_convert))
cat("\n") # Añadir un salto de línea

# --- CONVERTIR TODAS LAS COLUMNAS SELECCIONADAS A NUMÉRICAS ---
data_numeric <- data_to_convert %>%
  mutate(across(everything(), as.numeric))

# --- PASO DE VERIFICACIÓN: VER LOS TIPOS DESPUÉS DE LA CONVERSIÓN ---
# Ahora todas deberían ser 'num' o 'int' (o tener NAs si la conversión falló para algún valor)
cat("Tipos de datos DESPUÉS de la conversión:\n")
print(str(data_numeric))
cat("\n")

# 2. Calcular la matriz de correlación 
#    El argumento 'use = "pairwise.complete.obs"' manejará los NAs
matriz_cor <- cor(data_numeric, use = "pairwise.complete.obs")
# Verificar que la matriz se creó
# print(dim(matriz_cor))

# 1. Preparar los datos: Derretir la matriz de correlación
melted_cormat <- melt(matriz_cor)

# 2. Crear el mapa de calor con ggplot2 (Leyenda movida a la derecha)
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") + # Borde blanco para las celdas
  scale_fill_gradient2(low = "cadetblue2", high = "#FFF68F", mid = "white", # Escala de color
                       midpoint = 0,
                       limit = c(-1, 1),
                       space = "Lab",
                       name="Correlación\nPearson") + # Título de la leyenda
  theme_minimal() +
  coord_fixed() +
  geom_text(aes(label = round(value, 1)), color = "black", size = 2.5) + # Números en celdas (opcional)
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8), # Ajustes texto eje X
    axis.text.y = element_text(size = 8),                                   # Ajustes texto eje Y
    axis.title = element_blank(),                                           # Sin títulos en ejes
    panel.grid.major = element_blank(),                                     # Sin cuadrícula
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    # --- CORRECCIÓN POSICIÓN LEYENDA ---
    legend.position = "right", # Mueve la leyenda a la DERECHA del gráfico
    legend.direction = "vertical", # La orientación estándar a la derecha es vertical
  ) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10,                # Ajusta dimensiones barra de color vertical
                               title.position = "top", title.hjust = 0.5)) # Posición título leyenda
  # + labs(title = "Mapa de Calor de Correlaciones")  


#CATEGÓRICAS----------------------
#satisfaction_numeric---------------------------
data$satisfaction_numeric <- factor(data$satisfaction_numeric, levels = 0:5) # Ajusta los niveles si son diferentes

ggplot(data = data, aes(x = factor(satisfaction_numeric))) + 
  geom_bar(fill = "skyblue", color = "black") +
  # 2. Añadir texto con el conteo encima de las barras
  geom_text(
    stat = 'count', # Usar la misma estadística que geom_bar (contar)
    aes(label = after_stat(count)), # El texto a mostrar es el conteo calculado
    vjust = -0.5 # Ajuste vertical para ponerlo encima de la barra (valor negativo sube)
    # size = 3 # Puedes ajustar el tamaño del texto si es necesario
  ) +
  labs(
    title = "Distribución de Calificación de la Satisfacción",
    x = "Calificación (Satisfacción)", # La etiqueta del eje X sigue siendo válida
    y = "Cantidad de Pasajeros"
  ) +
  my_theme

#Inflight.wifi.service (p1)---------------------------
data$Inflight.wifi.service <- factor(data$Inflight.wifi.service, levels = 0:5) # Ajusta los niveles si son diferentes

p1 <- ggplot(data = data, aes(x = factor(Inflight.wifi.service))) +
  geom_bar(fill = "cadetblue2", color = "black") +
  geom_text(
    stat = 'count',
    aes(label = after_stat(count)),
    vjust = 1.5, color="black", # Puedes ajustar color para contraste
    size = 3 # Especifica un tamaño razonable
  ) +
  coord_cartesian(clip = 'off') +
  labs(
    title = "Calificación Servicio Wifi\na Bordo", # O usa str_wrap(long_title, width = 20)
    x = "Calificación (Servicio de Wifi)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    # Opcional: Ajustar tamaño título si aún se corta
    plot.title = element_text(size = 10),
    # Añadir margen superior si el texto aún se corta con clip='off'
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt") # Ajusta 't' (top)
  ) +
  my_theme

#Departure.Arrival.time.convenient (p2)---------------------------
data$Departure.Arrival.time.convenient <- factor(data$Departure.Arrival.time.convenient, levels = 0:5) # Ajusta los niveles si son diferentes

p2 <- ggplot(data = data, aes(x = factor(Departure.Arrival.time.convenient))) +
  geom_bar(fill = "cadetblue2", color = "black") +
  geom_text(
    stat = 'count',
    aes(label = after_stat(count)),
    vjust = 1.5, color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') +
  labs(
    title = "Calificación Tiempo de\nLlegada", 
    x = "Calificación (Tiempo de Llegada y Salida)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Ease.of.Online.booking (p3)---------------------------
data$Ease.of.Online.booking <- factor(data$Ease.of.Online.booking, levels = 0:5) # Ajusta los niveles si son diferentes

p3 <- ggplot(data = data, aes(x = factor(Ease.of.Online.booking))) +
  geom_bar(fill = "cadetblue2", color = "black") +
  geom_text(
    stat = 'count',
    aes(label = after_stat(count)),
    vjust = 1.5, color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') +
  labs(
    title = "Calificación Facilidad de\nReserva Online", 
    x = "Calificación (Facilidad de Reserva Online)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Gate.location (p4)---------------------------
data$Gate.location <- factor(data$Gate.location, levels = 0:5) # Ajusta los niveles si son diferentes

#Calcula las cuentas por categoría
data_countsp4 <- data %>%
  count(Gate.location, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(Gate.location = factor(Gate.location)) # Asegúrate que sea factor

p4 <- ggplot(data = data_countsp4, aes(x = Gate.location, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
      # Si n_count < 3000, vjust = -0.5 (arriba de la barra)
      # Si no, vjust = 1.5 (dentro de la barra, como antes)
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Ubicación de\nPuerta de Embarque",
    x = "Calificación (Ubicación)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Food.and.drink (p5)---------------------------
data$Food.and.drink <- factor(data$Food.and.drink, levels = 0:5) # Ajusta los niveles si son diferentes

#Calcula las cuentas por categoría
data_countsp5 <- data %>%
  count(Food.and.drink, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(Food.and.drink = factor(Food.and.drink)) # Asegúrate que sea factor

p5 <- ggplot(data = data_countsp5, aes(x = Food.and.drink, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Comida y\nBebida",
    x = "Calificación (Comida y Bebida)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Online.boarding (p6)---------------------------
data$Online.boarding <- factor(data$Online.boarding, levels = 0:5) # Ajusta los niveles si son diferentes

#Calcula las cuentas por categoría
data_countsp6 <- data %>%
  count(Online.boarding, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(Online.boarding = factor(Online.boarding)) # Asegúrate que sea factor

p6 <- ggplot(data = data_countsp6, aes(x = Online.boarding, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Proceso de\nAbordaje Online",
    x = "Calificación (Proceso de Abordaje)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Seat.comfort (p7)---------------------------
data$Seat.comfort <- factor(data$Seat.comfort, levels = 0:5) # Ajusta los niveles si son diferentes

#Calcula las cuentas por categoría
data_countsp7 <- data %>%
  count(Seat.comfort, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(Seat.comfort = factor(Seat.comfort)) # Asegúrate que sea factor

p7 <- ggplot(data = data_countsp7, aes(x = Seat.comfort, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Comodidad del\nAsiento",
    x = "Calificación (Comodidad del Asiento)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Inflight.entertainment (p8)---------------------------
data$Inflight.entertainment <- factor(data$Inflight.entertainment, levels = 0:5) # Ajusta los niveles si son diferentes

#Calcula las cuentas por categoría
data_countsp8 <- data %>%
  count(Inflight.entertainment, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(Inflight.entertainment = factor(Inflight.entertainment)) # Asegúrate que sea factor

p8 <- ggplot(data = data_countsp8, aes(x = Inflight.entertainment, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Entretenimiento en el\nVuelo",
    x = "Calificación (Entretenimiento en el Vuelo)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#On.board.service (p9)---------------------------
data$On.board.service <- factor(data$On.board.service, levels = 0:5) # Ajusta los niveles si son diferentes

#Calcula las cuentas por categoría
data_countsp9 <- data %>%
  count(On.board.service, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(On.board.service = factor(On.board.service)) # Asegúrate que sea factor

p9 <- ggplot(data = data_countsp9, aes(x = On.board.service, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Servicio a\nBordo",
    x = "Calificación (Servicio a Bordo)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Leg.room.service (p10) ------------------------------ 
data$Leg.room.service <- factor(data$Leg.room.service, levels = 0:5) # Ajusta los niveles si son diferentes

#Calcula las cuentas por categoría
data_countsp10 <- data %>%
  count(Leg.room.service, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(Leg.room.service = factor(Leg.room.service)) # Asegúrate que sea factor

p10 <- ggplot(data = data_countsp10, aes(x = Leg.room.service, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Espacio para\nlas Piernas",
    x = "Calificación (Espacio para las Piernas)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Baggage.handling (p11) ------------------------------
data$Baggage.handling <- factor(data$Baggage.handling, levels = 0:5) # Ajusta los niveles si son diferentes

data_countsp11 <- data %>%
  count(Baggage.handling, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(Baggage.handling = factor(Baggage.handling)) # Asegúrate que sea factor

p11 <- ggplot(data = data_countsp11, aes(x = Baggage.handling, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Manejo del\nEquipaje",
    x = "Calificación (Manejo del Equipaje)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Checkin.service (p12)---------------------------
data$Checkin.service <- factor(data$Checkin.service, levels = 0:5) # Ajusta los niveles si son diferentes

data_countsp12 <- data %>%
  count(Checkin.service, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(Checkin.service = factor(Checkin.service)) # Asegúrate que sea factor

p12 <- ggplot(data = data_countsp12, aes(x = Checkin.service, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Servicio de \nChek-In",
    x = "Calificación (Servicio de Check-In)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Inflight.service (p13)---------------------------
data$Inflight.service <- factor(data$Inflight.service, levels = 0:5) # Ajusta los niveles si son diferentes

data_countsp13 <- data %>%
  count(Inflight.service, name = "n_count") %>% # Calcula las cuentas y llámalo 'n_count'
  mutate(Inflight.service = factor(Inflight.service)) # Asegúrate que sea factor

p13 <- ggplot(data = data_countsp13, aes(x = Inflight.service, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' es útil para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Servicio durante\nel Vuelo",
    x = "Calificación (Servicio durante el Vuelo)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

#Cleanliness (p14)---------------------------
data$Cleanliness <- factor(data$Cleanliness, levels = 0:5) # Ajusta los niveles si son diferentes

data_countsp14 <- data %>%
  count(Cleanliness, name = "n_count") %>% # Calcula las cuentas
  mutate(Cleanliness = factor(Cleanliness)) # Asegúrate que sea factor

p14 <- ggplot(data = data_countsp14, aes(x = Cleanliness, y = n_count)) + # Usa data_counts y n_count para 'y'
  geom_bar(stat = "identity", fill = "cadetblue2", color = "black") + # Usa stat="identity" porque 'y' ya es la cuenta
  geom_text(
    aes(
      label = n_count, # Usa la columna de cuenta como etiqueta
      vjust = ifelse(n_count < 3000, -0.5, 1.5) # Ajuste vertical condicional
    ),
    color="black",
    size = 3
  ) +
  coord_cartesian(clip = 'off') + # clip='off' para que las etiquetas de arriba no se corten
  labs(
    title = "Calificación Servicio de\nLimpieza",
    x = "Calificación (Servicio de Limpieza)",
    y = "Cantidad de Pasajeros"
  ) +
  theme(
    plot.title = element_text(size = 10),
    plot.margin = margin(t = 15, r = 5, b = 5, l = 5, unit = "pt")
  ) +
  my_theme

# Gráficos Documento----------------------
image1 <- (p1 + p2) / (p3 + p4)
image2 <- (p5 + p6) / (p7 + p8)
image3 <- (p9 + p10) / (p11 + p12)
image4 <- (p13 + p14) / (plot_spacer() + plot_spacer())
# Muestra el gráfico combinado
print(image3)
