# Tarea 03 - Visualización de Datos
# Alumna: Micaela Pagliuca

# 1. Cargar librerías necesarias
# install.packages("ggplot2")
library(ggplot2)

# 2. Crear un dataset de ejemplo (Ventas mensuales)
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun")
ventas <- c(150, 200, 180, 250, 300, 280)
datos <- data.frame(meses, ventas)

# 3. Gráfico de Barras Básico
ggplot(datos, aes(x = meses, y = ventas, fill = meses)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Ventas por Mes", x = "Mes", y = "Monto")

# 4. Gráfico de Dispersión (Scatter Plot)
x <- rnorm(100)
y <- x + rnorm(100)
df_scatter <- data.frame(x, y)

ggplot(df_scatter, aes(x = x, y = y)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación entre variables X e Y", subtitle = "Tarea 03 - Micaela Pagliuca")

# 5. Histograma
ggplot(df_scatter, aes(x = x)) +
  geom_histogram(binwidth = 0.5, fill = "forestgreen", color = "white") +
  labs(title = "Distribución de la variable X")




library(tidyverse)
install.packages("tidyverse")
library(tidyverse)
library(palmerpenguins)
install.packages("palmerpenguins")
library(palmerpenguins)
library(ggthemes)
penguins
install.packages("ggplot2")
ggplot(data = penguins)
library(ggplot2)
ggplot(data = penguins)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)
library(palmerpenguins)
data("penguins")
head(penguins)
library(ggplot2)
library(palmerpenguins)

ggplot(data = penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = "black")
) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
install.packages("ggthemes")
library(ggthemes)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
install.packages("ggplot2")
library(ggplot2)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
library(ggplot2)
library(ggthemes)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()
