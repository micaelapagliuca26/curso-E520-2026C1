# Tarea 02 - R Base
# Alumna: Micaela Pagliuca

# Operaciones básicas
a <- 10
b <- 20
suma <- a + b
print(suma)

# Ejemplo de vector y promedio
notas <- c(8, 9, 7, 10, 6)
promedio <- mean(notas)
print(promedio)

datos_estudiantes <- data.frame(
  Nombre = c("Micaela", "Juan", "Lucas"),
  Edad = c(22, 25, 21),
  Nota = c(10, 8, 9)
)

dim(datos_estudiantes)
summary(datos_estudiantes)

desempeno <- factor(c("Alto", "Medio", "Alto", "Bajo"))
# Ver los niveles
levels(desempeno)

# Gráfico de barras simple con las notas
barplot(datos_estudiantes$Nota, 
        names.arg = datos_estudiantes$Nombre, 
        col = "blue", 
        main = "Notas por Estudiante",
        xlab = "Alumnos", 
        ylab = "Calificación")

# Crear una lista con diferentes tipos de objetos
mi_resumen <- list(
  titulo = "Informe de Clase",
  cantidad_alumnos = nrow(estudiantes),
  promedio_final = promedio
)

# Acceder a un elemento de la lista
print(mi_resumen$titulo)
# 1. Primero definimos el Data Frame
estudiantes <- data.frame(
  nombre = c("Mica", "Juan", "Poli", "Leo", "Santi"),
  nota = c(10, 8, 9, 7, 6)
)

# 2.calculamos el promedio
promedio <- mean(estudiantes$nota)

# 3.crear la lista
mi_resumen <- list(
  titulo = "Informe de Clase",
  cantidad_alumnos = nrow(estudiantes),
  promedio_final = promedio
)

# 4. 
print(mi_resumen)

estudiantes <- data.frame(
  nombre = c("Mica", "Juan", "Poli", "Leo", "Santi"),
  nota = c(10, 8, 9, 7, 6)
)

promedio <- mean(estudiantes$nota)

mi_resumen <- list(
  titulo = "Informe de Clase",
  cantidad_alumnos = nrow(estudiantes),
  promedio_final = promedio
)

print(mi_resumen$titulo)
print(mi_resumen)