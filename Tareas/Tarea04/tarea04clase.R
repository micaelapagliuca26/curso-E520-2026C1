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