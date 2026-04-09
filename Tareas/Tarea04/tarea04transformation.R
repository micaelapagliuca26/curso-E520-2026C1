# Tarea 04 - Transformación de Datos y Joins
# Alumna: Micaela Pagliuca

# --- PASO 0: Cargar las herramientas ---
# Cargamos tidyverse (dplyr) para manipular datos y nycflights13 para los datos de vuelos
library(nycflights13)
library(dplyr)

# --- PASO 1: Transformación de datos (Capítulo 3) ---

# 1.1. Filtrado: Buscamos solo los vuelos que llegaron con más de 2 horas de retraso
# Usamos filter() para seleccionar filas según una condición numérica
vuelos_retrasados <- flights %>% 
  filter(arr_delay >= 120)

# 1.2. Selección: Elegimos solo las columnas que nos interesan para no ver tanta info
# Usamos select() para limpiar el dataset y quedarnos con origen, destino y retrasos
vuelos_compactos <- flights %>% 
  select(origin, dest, dep_delay, arr_delay)

# --- PASO 2: Identificación de Claves (Sección 19.2.4) ---

# Para unir tablas, primero debemos saber qué columna identifica a cada fila (Key)
# Aquí verificamos si 'tailnum' (patente del avión) es una clave primaria en la tabla planes
# Si el conteo de duplicados es cero, significa que es una clave válida
verificacion_keys <- planes %>% 
  count(tailnum) %>% 
  filter(n > 1)

# --- PASO 3: Uniones de tablas / Joins (Sección 19.3.4) ---

# 3.1. Unir Vuelos con Aeropuertos (Left Join)
# Queremos que en lugar del código "JFK", aparezca el nombre completo del aeropuerto
# Unimos la tabla 'flights' con 'airports' haciendo coincidir 'dest' con 'faa'
vuelos_con_nombres <- flights %>%
  left_join(airports, by = c("dest" = "faa"))

# 3.2. Unir Vuelos con Clima (Inner Join)
# Queremos saber cómo estaba el tiempo al momento de cada salida
# Unimos por múltiples columnas para que coincida exactamente el año, mes, día y hora
vuelos_clima <- flights %>%
  inner_join(weather, by = c("origin", "year", "month", "day", "hour"))

# Fin del script detallado
