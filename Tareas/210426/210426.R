install.packages("janeaustenr")
library(janeaustenr)
library(dplyr)
library(stringr)

 #chapter:detecta de la columna text exprecciones reg.
 #mutate: crea columnas 
original_books <- austen_books() |>
  group_by(book) |>
  mutate(
    line = row_number(),
    chapter = cumsum(str_detect(
      text,
      regex("^chapter [\\divxlc]", ignore_case = TRUE)
    ))
  ) |>
  ungroup()

original_books





install.packages(c("tidyverse", "tidytext", "gutenbergr", "rvest"))
library(tidyverse)
library(tidytext)
library(gutenbergr)
library(rvest)

# ----------------------------------------------------------------words
# 2. ANALIZAR TEXTO DESDE URL DIRECTA (Biblia - Project Gutenberg)
# ----------------------------------------------------------------
url_directa <- "https://mirrors.xmission.com/gutenberg/1/10/10-0.txt"
texto_biblia <- readLines(url_directa, encoding = "UTF-8")

# Convertimos a formato tidy (una palabra por fila)
biblia_tidy <- tibble(text = texto_biblia) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) # Quitamos conectores en inglés

# Resultado: Top 10 palabras más frecuentes
frecuencia_biblia <- biblia_tidy %>%
  count(word, sort = TRUE) %>%
  head(10)

print("Top 10 palabras en el primer link:")
print(frecuencia_biblia)

# ----------------------------------------------------------------
# 3. USO DEL PAQUETE 'gutenbergr'
# ----------------------------------------------------------------
# Buscamos un libro clásico en español, por ejemplo "Don Quijote" (ID 2000)
# Puedes buscar IDs con: gutenberg_works(languages = "es")
quijote_raw <- gutenberg_download(2000)

quijote_tidy <- quijote_raw %>%
  unnest_tokens(word, text)

# Cargamos stop words en español para un análisis real
stop_words_es <- get_stopwords("es")

quijote_analisis <- quijote_tidy %>%
  anti_join(stop_words_es, by = c("word" = "word")) %>%
  count(word, sort = TRUE)

print("Palabras más frecuentes en el Quijote:")
print(head(quijote_analisis, 10))

# ----------------------------------------------------------------
# 4. WEB SCRAPING (Ciudad Seva - Julio Cortázar)
# ----------------------------------------------------------------
url_cortazar <- "https://ciudadseva.com/autor/julio-cortazar/cuentos/"
pagina <- read_html(url_cortazar)

# Extraemos los títulos de los cuentos que están en la página
titulos_cortazar <- pagina %>%
  html_nodes("article h2 a") %>%
  html_text()

# Creamos un dataframe con los títulos encontrados
df_cortazar <- tibble(titulo = titulos_cortazar)

print("Primeros cuentos detectados de Cortázar:")
print(head(df_cortazar))

# ----------------------------------------------------------------
# 5. CORPUS DE INTERÉS (Propuesta: Educación)
# ----------------------------------------------------------------
# Como trabajas con datos educativos, analicemos conceptos clave 
# que podrías encontrar en textos sobre aprendizaje.
corpus_educacion <- tibble(
  texto = c("La asistencia escolar es fundamental para el aprendizaje.",
            "La brecha entre el sector estatal y privado es notable.",
            "Los resultados de las pruebas aprender muestran disparidad.")
)

analisis_educ <- corpus_educacion %>%
  unnest_tokens(word, texto) %>%
  anti_join(stop_words_es, by = c("word" = "word")) %>%
  count(word, sort = TRUE)

print("Análisis de mini-corpus educativo:")
print(analisis_educ)

# ----------------------------------------------------------------
# 6. VISUALIZACIÓN 
# ----------------------------------------------------------------
# Gráfico de las palabras del Quijote
quijote_analisis %>%
  head(15) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Palabras más frecuentes - Don Quijote",
       x = "Palabras", y = "Cantidad") +
  theme_minimal()

install.packages(c("tidyverse", "tidytext", "gutenbergr", "rvest"), type = "binary")



install.packages("stopwords")
if (!require("stopwords")) install.packages("stopwords")
library(tidyverse)
library(tidytext)
library(rvest)

# 1. ANALIZAR BIBLIA 
url_biblia <- "https://mirrors.xmission.com/gutenberg/1/10/10-0.txt"
biblia <- readLines(url_biblia, encoding = "UTF-8") %>%
  tibble(text = .) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# 2. ANALIZAR QUIJOTE 
url_quijote <- "https://www.gutenberg.org/cache/epub/2000/pg2000.txt"
quijote_raw <- readLines(url_quijote, encoding = "UTF-8")
stop_words_es <- tibble(word = stopwords::stopwords("es"))

quijote <- tibble(text = quijote_raw) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words_es) %>%
  count(word, sort = TRUE)

# 3. SCRAPING CORTÁZAR (Link 2)
url_cortazar <- "https://ciudadseva.com/autor/julio-cortazar/cuentos/"
titulos_cortazar <- read_html(url_cortazar) %>%
  html_nodes("article h2 a") %>%
  html_text() %>%
  tibble(cuento = .)

# 4. CORPUS EDUCACIÓN ARGENTINA (tema de interés)
corpus_educ <- tibble(texto = c("Pruebas Aprender 2023", "Asistencia escolar", 
                                "Sector estatal y privado", "Brecha educativa")) %>%
  unnest_tokens(word, texto) %>%
  count(word, sort = TRUE)

# --- MOSTRAR RESULTADOS ---
print("Primeras palabras Biblia:")
print(head(biblia, 5))
print("Cuentos de Cortázar encontrados:")
print(head(titulos_cortazar, 5))
print("Análisis de Educación:")
print(corpus_educ)

# --- GRÁFICO ---
quijote %>%
  head(15) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(title = "Frecuencia de palabras: Don Quijote", x = "", y = "Frecuencia") +
  theme_minimal()
