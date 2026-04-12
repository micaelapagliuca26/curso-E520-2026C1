install.packages("nycflights13")
library(nycflights13
      )

library(tidyverse)
install.packages("nycflights13")
library(nycflights13
)
library(tidyverse)
library(nycflights13)
flights
glimpse(flights)
flights$year
flights[[1]]

duplicated(flights)
flights[duplicated(flights)]

airports
planes
airlines
weather


# buscamos los vuelos en flights cuyo destino es IAH
flights |>
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

# Flights that departed on January 1
flights |> 
  filter(month == 1 & day == 1)

#Lo que falta hcerlo de tarea:

# A shorter way to select flights that departed in January or February
flights |> 
  filter(month %in% c(1, 2))
jan1 <- flights |> 
  filter(month == 1 & day == 1)
flights |> 
  filter(month = 1)
flights |> 
  filter(month == 1 | 2)

flights |> 
  arrange(year, month, day, dep_time)

flights |> 
  arrange(desc(dep_delay))

# Remove duplicate rows, if any
flights |> 
  distinct()
# Find all unique origin and destination pairs
flights |> 
  distinct(origin, dest)

flights |> 
  distiflights |>
  count(origin, dest, sort = TRUE)
flights |>
  count(origin, dest, sort = TRUE)

#EJERCICIOS
#1)

library(nycflights13)
library(dplyr)

# 1. Retraso de llegada de dos o más horas (120 min)
cond1 <- flights |> filter(arr_delay >= 120)

# 2. Volaron a Houston (IAH o HOU)
cond2 <- flights |> filter(dest %in% c("IAH", "HOU"))

# 3. Operados por United (UA), American (AA) o Delta (DL)
cond3 <- flights |> filter(carrier %in% c("UA", "AA", "DL"))

# 4. Salieron en verano (Julio, Agosto, Septiembre)
cond4 <- flights |> filter(month %in% 7:9)

# 5. Llegaron más de dos horas tarde pero no salieron tarde
cond5 <- flights |> filter(arr_delay > 120 & dep_delay <= 0)

# 6. Retraso de al menos una hora, pero recuperaron más de 30 min en vuelo
cond6 <- flights |> filter(dep_delay >= 60 & (dep_delay - arr_delay > 30))

#2)

flights |> arrange(desc(dep_delay))

#3)

flights |> 
  arrange(desc(distance / air_time)) |> 
  select(carrier, flight, origin, dest, distance, air_time)

#4)

flights |> 
  distinct(year, month, day) |> 
  nrow()

#5)

flights |> arrange(desc(distance))
flights |> arrange(distance)

#TAREA CAP 19.2.4 EXERCISE 

#la hora duplicada
library(nycflights13)
library(tidyverse)

# Buscamos la fila duplicada
weather %>%
  group_by(year, month, day, hour, origin) %>%
  filter(n() > 1)
#el duplicado ocurre el 3 de noviembre a la 1:00AM. Esto sucede por un cambio de horario de invierno en EEUU.


library(tidyverse)
library(nycflights13)
# install.packages("Lahman") # Correr si no lo tenés instalado
library(Lahman)

# Ejercicio 1: Relación weather y airports
# La relación es a través de las columnas 'origin' (en weather) y 'faa' (en airports).
# Es una relación de Muchos a Uno (N:1).
# Visualmente: weather -> airports [origin == faa]

# Ejercicio 2: Weather en todo USA ---
# Si weather tuviera datos nacionales, se conectaría con flights mediante:
# flights$dest == weather$dest
# Esto permitiría analizar el clima en el aeropuerto de llegada.

# Ejercicio 3:La hora duplicada
weather %>%
  group_by(year, month, day, hour, origin) %>%
  filter(n() > 1)

# Explicación: El duplicado ocurre el 3 de noviembre a la 1:00 AM. 
# Esto se debe al cambio de horario, donde el reloj 
# se atrasa una hora y esa hora "se vive dos veces".

# Ejercicio 4: Días especiales (Feriados)
# Representación de data frame:
special_days <- tibble(
  year = 2013,
  month = 12,
  day = c(24, 25),
  event = c("Nochebuena", "Navidad")
)

# La Primary Key es la combinación de (year, month, day).
# Se conectaría a flights mediante un left_join:
# flights %>% left_join(special_days, by = c("year", "month", "day"))

# Ejercicio 5: Paquete Lahman ---

# A) Batting, People y Salaries:
# Se conectan por 'playerID'. People es la tabla maestra.

# B) People, Managers, AwardsManagers:
# People (playerID) -> Managers (playerID)
# Managers (playerID, yearID) -> AwardsManagers (playerID, yearID)

# C) Relación Batting, Pitching y Fielding:
# Es una relación 1:1 por cada "stint" de jugador. 
# Se vinculan por una llave compuesta: (playerID, yearID, stint).




#TAREA CAP 19.3.4 

library(tidyverse)
library(nycflights13)

# Cargo datos
flights <- nycflights13::flights
weather <- nycflights13::weather
planes <- nycflights13::planes
airports <- nycflights13::airports

#promedio de retrasos
Peores_48h <- flights |>
  group_by(year, month, day, hour) |>
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) |>
  ungroup() |>
  mutate(rolling_delay = slider::slide_dbl(dep_delay, mean, .before = 48, .complete = TRUE)) |>
  arrange(desc(rolling_delay)) |>
  head(1)

# Paso 1: Identificar los 10 destinos top
top_dest <- flights |> 
  count(dest, sort = TRUE) |> 
  head(10)

# Paso 2: Filtrar todos los vuelos a esos destinos
vuelos_a_destinos_top <- flights |> 
  semi_join(top_dest, by = "dest")

vuelos_sin_info_avion <- flights |> 
  anti_join(planes, by = "tailnum") |> 
  count(carrier, sort = TRUE) |> 
  mutate(p = n / sum(n))

# Agregar columna con los carriers por avión
planes_con_carriers <- flights |>
  group_by(tailnum) |>
  summarize(carriers = paste(unique(carrier), collapse = ", ")) |>
  left_join(planes, by = "tailnum")

# ¿Hay aviones con más de un carrier?
aviones_compartidos <- flights |>
  group_by(tailnum) |>
  summarize(n_carriers = n_distinct(carrier)) |>
  filter(n_carriers > 1)

flights_con_coordenadas <- flights |>
  # Unir origen
  inner_join(airports |> select(faa, lat_orig = lat, lon_orig = lon), by = c("origin" = "faa")) |>
  # Unir destino
  inner_join(airports |> select(faa, lat_dest = lat, lon_dest = lon), by = c("dest" = "faa"))

# Paso 1: Promedio de retraso por destino ese día
retrasos_13_junio <- flights |>
  filter(year == 2013, month == 6, day == 13) |>
  group_by(dest) |>
  summarize(delay = mean(arr_delay, na.rm = TRUE))

# Paso 2: Unir con aeropuertos y graficar
airports |> 
  inner_join(retrasos_13_junio, by = c("faa" = "dest")) |> 
  ggplot(aes(x = lon, y = lat, size = delay, color = delay)) +
  borders("state") +
  geom_point() +
  coord_quickmap() +
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Retrasos en USA - 13 de Junio 2013",
       subtitle = "Efecto de tormentas severas en el noreste")


#CONCLUSION: El análisis del 13 de junio de 2013 demuestra que los retrasos masivos fueron causados por tormentas severas en el noreste de EE. UU. Además, se identificó que el 90% de los aviones sin datos técnicos pertenecen a American Airlines y Envoy Air, y se confirmó que un mismo avión puede ser operado por múltiples aerolíneas. Finalmente, se determinó que es mejor renombrar las coordenadas antes de unirlas para evitar errores en el código.

