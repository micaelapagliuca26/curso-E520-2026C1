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

# Fin del script de visualización
