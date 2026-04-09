library(tidyverse)
library(palmerpenguins)

# TAREA 03 - Micaela Pagliuca
# Ejercicios Capitulo 1: Visualizacion de Datos

# 1. Exploracion inicial
# Filas: 344 | Columnas: 8
dim(penguins)

# 2. Grafico de dispersion (Aletas vs Peso)
# Se observa una relacion lineal positiva: a mayor aleta, mayor peso.
ggplot(data = penguins,
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(title = "Relacion entre aletas y peso por especie",
       subtitle = "Ejercicio Cap. 1 - R4DS",
       x = "Largo de aleta (mm)",
       y = "Masa corporal (g)",
       color = "Especie",
       shape = "Especie",
       caption = "Datos del paquete palmerpenguins") +
  theme_minimal()

# 3. Ejercicio adicional: Bill Depth vs Bill Length
ggplot(data = penguins, 
       mapping = aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species)) +
  labs(title = "Relacion Largo vs Profundidad del Pico")