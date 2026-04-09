library(tidyverse)
library(palmerpenguins)

# Tarea 03 - Micaela Pagliuca

# Ejercicio 1: Filas y columnas
dim(penguins)

# Ejercicio 2: Grafico de dispersion
ggplot(data = penguins, 
       mapping = aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point(aes(color = species)) + 
  geom_smooth(method = "lm") + 
  labs(title = "Grafico de Micaela - Relacion Aleta vs Peso")