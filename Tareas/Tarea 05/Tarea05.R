# cargar librerias --------------------------------------------------------
library(tidyverse)


# cargar datos ------------------------------------------------------------

anac_2025 <- read.csv(file = "Desktop/curso-E520-2026C1/Tareas/Tarea04/202512-informe-ministerio-actualizado-dic-final.csv")

#analisis de datos 

glimpse(anac_2025)

library(tidyverse)
anac_2025 <- read_csv(file = "Desktop/curso-E520-2026C1/Tareas/202512-informe-ministerio-actualizado-dic-final.csv")
glimpse(anac_2025)

library(tidyverse)

#vuelos 2025
anac_2025 <- read_csv2(file = "Desktop/curso-E520-2026C1/Tareas/202512-informe-ministerio-actualizado-dic-final.csv")
glimpse(anac_2025)


#Aeropuertos 

aeropuertos <- read_csv(file = "Desktop/curso-E520-2026C1/archivos:datos/iata-icao.csv")

#Clima.
##revisar si podemos encontrar una relacion de nombre de estacion meteorologica y codigo de aeropuerto. 
clima <-
  read_csv(file ="Desktop/curso-E520-2026C1/archivos:datos/registro_temperatura365d_smn.txt")

clima <-
  read_fwf(file = "Desktop/curso-E520-2026C1/archivos:datos/registro_temperatura365d_smn.txt" ,
           col_positions = fwf_widths(c(8,6,6,200),c("fecha","tmax",
                                                "tmin", "nombre"))
           )


clima[1:2,]


#Clima 2
clima <-
  read_table(file ="Desktop/curso-E520-2026C1/archivos:datos/registro_temperatura365d_smn.txt")

clima <-
  read_fwf(file = "Desktop/curso-E520-2026C1/archivos:datos/registro_temperatura365d_smn.txt" ,
           
  )



glimpse(anac_2025)




anac_2025 <- anac_2025 |>
  mutate(
    tipo_vuelo      = factor(`Clase de Vuelo (todos los vuelos)`),
    clasif_vuelo    = factor(`Clasificación Vuelo`), 
    tipo_movimiento = factor(`Tipo de Movimiento`),
    aeropuertos     = factor(Aeropuerto),           
    origen_destino  = factor(`Origen / Destino`),   
    aerolinea       = factor(`Aerolinea Nombre`),
    aeronave        = factor(Aeronave),
    calidad_dato    = factor(`Calidad dato`)        
  )

glimpse(anac_2025)

summary(anac_2025)
glimpse(anac_2025)


glimpse(aeropuertos)

#TAREA: COMO HACER PARA ELIMINAR LAS COLUMNAS NUEVAS (ADENTRO DE ANAC)
#TAREAS: EXPLORAR CLIMAS (glimpse clima)
## EXPLORAR AEROPUERTOS 

#Borrar columnas nuevas dentro de anac
anac_2025 <- anac_2025 |> 
  select(-tipo_vuelo, -clasif_vuelo, -aeropuertos, -origen_destino)

anac_2025 <- anac_2025 |> 
  select(-tipo_vuelo, -clasif_vuelo, -tipo_movimiento, 
         -aeropuertos, -origen_destino, -aerolinea, 
         -aeronave, -calidad_dato)
colnames(anac_2025)
anac_2025 <- anac_2025 |> 
  select(-tipo_movimiento, -aerolinea, -aeronave, -calidad_dato)


#Explorar climas
glimpse(clima)

clima_limpio <- clima |>
  # 1. Filtramos para quitar las filas que tienen guiones o la palabra FECHA
  filter(fecha != "FECHA", fecha != "--------") |>
  mutate(
    tmax = as.numeric(tmax),
    tmin = as.numeric(tmin)
  )
glimpse(clima_limpio)

##Temperatura maxima 
max(clima_limpio$tmax, na.rm = TRUE)

##Mediciones por estacion
clima_limpio |> count(nombre, sort = TRUE)


#Explorar aeropuertos

glimpse(aeropuertos)

##Distribucion geografica

aeropuertos |> 
  select(latitude, longitude) |> 
  summary()


#TAREA PANDEMIA

library(tidyverse)

anac_2019 <- read_csv2(file ="Desktop/curso-E520-2026C1/archivos:datos/201912-informe-ministerio-actualizado-dic-final.csv")
anac_2020 <- read_csv2(file = "Desktop/curso-E520-2026C1/archivos:datos/202012-informe-ministerio-actualizado-dic-final.csv")
anac_2021 <- read_csv2(file = "Desktop/curso-E520-2026C1/archivos:datos/202112-informe-ministerio-actualizado-dic-final.csv")
anac_2022 <- read_csv2(file = "Desktop/curso-E520-2026C1/archivos:datos/202212-informe-ministerio-actualizado-dic-final.csv")
anac_2023 <- read_csv2(file = "Desktop/curso-E520-2026C1/archivos:datos/202312-informe-ministerio-actualizado-dic.csv")
anac_2024 <- read_csv2(file = "Desktop/curso-E520-2026C1/archivos:datos/202412-informe-ministerio-actualizado-dic-final.csv")

# 1. Aseguramos que la columna conflictiva sea texto en todos los años
anac_2019$Aeronave <- as.character(anac_2019$Aeronave)
anac_2020$Aeronave <- as.character(anac_2020$Aeronave)
anac_2021$Aeronave <- as.character(anac_2021$Aeronave)
anac_2022$Aeronave <- as.character(anac_2022$Aeronave)
anac_2023$Aeronave <- as.character(anac_2023$Aeronave)
anac_2024$Aeronave <- as.character(anac_2024$Aeronave)

# 2. Unimos todo
anac_total <- bind_rows(anac_2019, anac_2020, anac_2021, 
                        anac_2022, anac_2023, anac_2024, anac_2025)

# 3. Limpiamos nombres manualmente (convertimos a minúsculas y cambiamos espacios por _)
# Esta línea reemplaza a janitor:
names(anac_total) <- gsub(" ", "_", tolower(names(anac_total)))


#QUE PASO EN LA PANDEMIA
library(lubridate)

anac_total |> 
  mutate(anio = year(dmy(fecha_utc))) |> 
  count(anio)

#Bajan mucho la cantidad de vuelos a partr del 2020 hasta el 2021, que comienza a recuperarse.

names(anac_total)

# 1. ¿Qué se observa EN LA PANDEMIA?
# Un desplome total. En 2020 los vuelos cayeron a menos de la mitad 
# comparado con el año anterior. Hubo meses donde casi no hubo 
# movimiento por las restricciones.

# 2. ¿Cuánto tardó en recuperarse?
# Un montón. Los años siguientes (2021, 2022) siguieron flojos. 
# Recién ahora, en 2024, estamos viendo la misma cantidad de 
# aviones en el aire que teníamos antes de que empezara todo.

# 3. ¿Qué cambió?
# Se viaja distinto. Los vuelos dentro del país volvieron rápido, 
# pero los viajes al exterior tardaron mucho más en arrancar. 
# También se nota que cambiaron las empresas que más vuelan.


