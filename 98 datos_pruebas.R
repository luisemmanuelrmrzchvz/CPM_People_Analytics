# Cargar librerías necesarias
library(readxl)
library(DBI)
library(RSQLite)

# Ruta del archivo de entrada y base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
archivo_excel <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/catalog_tickets.xlsx"

# Leer el archivo XLSX ignorando la primera fila (títulos de columnas)
datos <- read_excel(archivo_excel, skip = 1, col_names = FALSE)

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Definir las columnas de la tabla de base de datos
columnas_db <- c("id_ticket", "tipo_atencion", "prioridad", "tipo_ticket", 
                 "nivel_atencion", "categoria", "subcategoria")

# Renombrar las columnas de los datos para que coincidan con la tabla de SQLite
colnames(datos) <- columnas_db

# Insertar los datos en la tabla datos_colaboradores
dbWriteTable(conn, "catalog_tickets", datos, append = TRUE, row.names = FALSE)

# Cerrar la conexión
dbDisconnect(conn)

print("Datos insertados en la base de datos correctamente.")


####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################
####################################################################################



# Cargar las librerías necesarias
library(readxl)
library(tidyverse)
library(tm)
library(tidytext)
library(syuzhet)
library(topicmodels)
library(ggplot2)
library(textstem)
library(widyr)
library(writexl)
library(igraph)
library(ggraph)
library(udpipe)
library(spacyr)

# 1. Cargar el archivo Excel y renombrar la columna
ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas abiertas.xlsx"
df <- read_excel(ruta_archivo, col_names = FALSE)
colnames(df) <- c("Respuesta_Abierta")

df <- df %>% 
  filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "") %>%
  mutate(doc_id = row_number())

# 2. Limpieza de datos y tokenización
df_limpio <- df %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  unnest_tokens(word, Respuesta_Abierta) %>%
  filter(!word %in% stopwords("es")) %>%
  mutate(word = lemmatize_strings(word, language = "es"))

# 3. Análisis exploratorio: Palabras más comunes
word_counts <- df_limpio %>% count(word, sort = TRUE)
print(head(word_counts, 10))

# 4. Análisis de Emociones Específicas
lexicon_es <- get_sentiment_dictionary("nrc", language = "spanish")

df_polaridad <- df_limpio %>%
  inner_join(lexicon_es, by = c("word" = "word"), relationship = "many-to-many") %>%
  group_by(doc_id) %>%
  summarise(polaridad = sum(value, na.rm = TRUE))

df <- df %>%
  left_join(df_polaridad, by = "doc_id")

df_negativos <- df %>%
  filter(polaridad < 0) %>%
  select(doc_id, Respuesta_Abierta, polaridad)

write_xlsx(df_negativos, path = "Respuestas_Negativas.xlsx")

# 5. Análisis de Redes Semánticas
trigramas <- df %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  unnest_tokens(trigrama, Respuesta_Abierta, token = "ngrams", n = 3) %>%
  separate(trigrama, into = c("palabra1", "palabra2", "palabra3"), sep = " ") %>%
  filter(!is.na(palabra1), !is.na(palabra2), !is.na(palabra3)) %>%
  filter(!palabra1 %in% stopwords("es"),
         !palabra2 %in% stopwords("es"),
         !palabra3 %in% stopwords("es")) %>%
  unite(trigrama, palabra1, palabra2, palabra3, sep = " ") %>%
  count(trigrama, sort = TRUE)

# 6. Uso de Modelos de Lenguaje Avanzados
# Cargar el modelo de lenguaje udpipe manualmente
modelo_udpipe <- udpipe_load_model(file = "C:/Users/racl26345/Documents/DataBases/udpipe/spanish-gsd-ud-2.5-191206.udpipe")

# Anotar el texto con el modelo udpipe
df_anotado <- udpipe_annotate(modelo_udpipe, x = df$Respuesta_Abierta)
df_anotado <- as.data.frame(df_anotado)

# Crear la columna `cluster` basada en la polaridad
df <- df %>%
  mutate(cluster = ifelse(polaridad < 0, "Negativo", "Positivo"))

# Análisis de palabras por cluster
palabras_por_cluster <- df_limpio %>%
  left_join(df %>% select(doc_id, cluster), by = "doc_id") %>%
  group_by(cluster, word) %>%
  summarise(n = n(), .groups = 'drop') %>%
  top_n(10, n)

print(palabras_por_cluster)


####################################################################################
####################################################################################
####################################################################################
####################################################################################


# Cargar las librerías necesarias
library(readxl)
library(tidyverse)
library(tm)
library(tidytext)
library(syuzhet)
library(caret)
library(udpipe)
library(writexl)
library(ggplot2)

# 1. Cargar el archivo Excel, omitiendo la primera fila (título) y solo considerando los registros (no la pregunta)
ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas abiertas.xlsx"
df <- read_excel(ruta_archivo, col_names = FALSE)

# Filtrar la primera fila que es la pregunta
df <- df[-1, ]
colnames(df) <- c("Respuesta_Abierta")

# 2. Filtrar respuestas con menos de 4 palabras y categorizar como "Neutro_Directo"
df <- df %>%
  filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "") %>%
  mutate(doc_id = row_number(),
         num_palabras = str_count(Respuesta_Abierta, "\\w+"),
         sentimiento = if_else(num_palabras < 4, "Neutro_Directo", NA_character_))

# 3. Filtrar solo las respuestas con más de 3 palabras (para análisis de sentimientos)
df_modelo <- df %>%
  filter(num_palabras >= 4)

# 4. Limpieza de datos y tokenización (solo para respuestas de más de 4 palabras)
df_limpio <- df_modelo %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  unnest_tokens(word, Respuesta_Abierta) %>%
  filter(!word %in% stopwords("es")) %>%
  mutate(word = lemmatize_strings(word, language = "es"),
         word = stem_strings(word, language = "es"))

# 5. Agregar la columna original `Respuesta_Abierta` a `df_limpio` para análisis de sentimientos
df_limpio <- df_limpio %>%
  left_join(df %>% select(doc_id, Respuesta_Abierta), by = "doc_id")

# 6. Análisis de sentimientos utilizando `syuzhet` (diccionario NRC) para las respuestas mayores a 3 palabras
sentimientos <- get_nrc_sentiment(df_limpio$Respuesta_Abierta)

# 7. Clasificación de sentimientos (Positivo, Negativo, Neutro, con categorías más detalladas)
if (ncol(sentimientos) > 0) {
  df_limpio$sentimiento_valor <- sentimientos$positive - sentimientos$negative  # Puntaje de sentimiento
  
  # Ajustar las categorías de sentimiento según el puntaje
  df_limpio$sentimiento <- case_when(
    df_limpio$sentimiento_valor <= -2 ~ "Muy Negativo",
    df_limpio$sentimiento_valor == -1 ~ "Negativo",
    df_limpio$sentimiento_valor == 0 ~ "Neutro",
    df_limpio$sentimiento_valor == 1 ~ "Positivo",
    df_limpio$sentimiento_valor >= 2 ~ "Muy Positivo",
    TRUE ~ "Neutro"
  )
} else {
  warning("El análisis de sentimientos no produjo resultados válidos.")
}

# 8. Filtrar respuestas "Neutro_Directo" y respuestas que entran al modelo
df_neutro_directo <- df %>% filter(sentimiento == "Neutro_Directo")
df_entrar_modelo <- df %>% filter(sentimiento != "Neutro_Directo")

# 9. Calcular la proporción y volumen de "Neutro_Directo" vs "Entran a Modelo"
proporcion <- df %>%
  mutate(categoria = if_else(sentimiento == "Neutro_Directo", "Neutro_Directo", "Entran a Modelo")) %>%
  group_by(categoria) %>%
  summarise(Conteo = n()) %>%
  mutate(Porcentaje = Conteo / sum(Conteo) * 100)

# Ver el resultado de la proporción
print(proporcion)

# Gráfico de la proporción "Neutro_Directo" vs "Entran a Modelo"
ggplot(proporcion, aes(x = categoria, y = Porcentaje, fill = categoria)) +
  geom_bar(stat = "identity") +
  labs(title = "Proporción de Neutro_Directo vs Entran a Modelo", x = "Categoría", y = "Porcentaje") +
  scale_fill_manual(values = c("Neutro_Directo" = "lightblue", "Entran a Modelo" = "lightgreen")) +
  theme_minimal()

# 10. Análisis de bigramas y trigramas (para obtener las relaciones entre palabras)
# Extraemos los bigramas y trigramas para observar las relaciones
df_bigrams <- df_limpio %>%
  unnest_tokens(bigram, Respuesta_Abierta, token = "ngrams", n = 2)

# Filtramos los bigramas más frecuentes
bigrams_freq <- df_bigrams %>%
  count(bigram, sort = TRUE) %>%
  filter(n > 1)

# Graficar bigramas más frecuentes
ggplot(bigrams_freq, aes(x = reorder(bigram, n), y = n, fill = bigram)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Frecuencia de Bigramas", x = "Bigramas", y = "Frecuencia") +
  theme_minimal()

# 11. Ajuste del archivo de Excel: obtener clasificación promedio por cada registro
df_promedio <- df_limpio %>%
  group_by(doc_id) %>%
  summarise(sentimiento_promedio = mean(sentimiento_valor, na.rm = TRUE)) %>%
  left_join(df %>% select(doc_id, Respuesta_Abierta), by = "doc_id")

# Guardar el archivo con la clasificación promedio de cada registro
write_xlsx(df_promedio, "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas_Clasificadas.xlsx")




############################################################

> # Cargar las librerías necesarias
  > library(readxl)
Aviso:
  package ‘readxl’ was built under R version 4.4.2 
> library(tidyverse)
── Attaching core tidyverse packages ──────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.5
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ ggplot2   3.5.1     ✔ tibble    3.2.1
✔ lubridate 1.9.4     ✔ tidyr     1.3.1
✔ purrr     1.0.2     
── Conflicts ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package to force all conflicts to become errors
Avisos:
  1: package ‘tidyverse’ was built under R version 4.4.2 
2: package ‘ggplot2’ was built under R version 4.4.2 
3: package ‘tidyr’ was built under R version 4.4.2 
4: package ‘readr’ was built under R version 4.4.2 
5: package ‘purrr’ was built under R version 4.4.2 
6: package ‘dplyr’ was built under R version 4.4.2 
7: package ‘forcats’ was built under R version 4.4.2 
8: package ‘lubridate’ was built under R version 4.4.2 
> library(tm)
Cargando paquete requerido: NLP

Adjuntando el paquete: ‘NLP’

The following object is masked from ‘package:ggplot2’:
  
  annotate

Avisos:
  1: package ‘tm’ was built under R version 4.4.2 
2: package ‘NLP’ was built under R version 4.4.2 
> library(tidytext)
Aviso:
  package ‘tidytext’ was built under R version 4.4.2 
> library(syuzhet)
Aviso:
  package ‘syuzhet’ was built under R version 4.4.2 
> library(caret)
Cargando paquete requerido: lattice

Adjuntando el paquete: ‘caret’

The following object is masked from ‘package:purrr’:
  
  lift

Aviso:
  package ‘caret’ was built under R version 4.4.2 
> library(udpipe)
Aviso:
  package ‘udpipe’ was built under R version 4.4.2 
> library(writexl)
Aviso:
  package ‘writexl’ was built under R version 4.4.2 
> library(ggplot2)
> 
  > # 1. Cargar el archivo Excel, omitiendo la primera fila (título) y solo considerando los registros (no la pregunta)
  > ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas abiertas.xlsx"
> df <- read_excel(ruta_archivo, col_names = FALSE)
New names:
  • `` -> `...1`
> 
  > # Filtrar la primera fila que es la pregunta
  > df <- df[-1, ]
> colnames(df) <- c("Respuesta_Abierta")
> 
  > # 2. Filtrar respuestas con menos de 4 palabras y categorizar como "Neutro_Directo"
  > df <- df %>%
  +   filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "") %>%
  +   mutate(doc_id = row_number(),
             +          num_palabras = str_count(Respuesta_Abierta, "\\w+"),
             +          sentimiento = if_else(num_palabras < 4, "Neutro_Directo", NA_character_))
> 
  > # 3. Filtrar solo las respuestas con más de 3 palabras (para análisis de sentimientos)
  > df_modelo <- df %>%
  +   filter(num_palabras >= 4)
> 
  > # 4. Limpieza de datos y tokenización (solo para respuestas de más de 4 palabras)
  > df_limpio <- df_modelo %>%
  +   mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
             +          Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
             +          Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
             +          Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  +   unnest_tokens(word, Respuesta_Abierta) %>%
  +   filter(!word %in% stopwords("es")) %>%
  +   mutate(word = lemmatize_strings(word, language = "es"),
             +          word = stem_strings(word, language = "es"))
Error in `mutate()`:
  ℹ In argument: `word = lemmatize_strings(word, language = "es")`.
Caused by error in `lemmatize_strings()`:
  ! no se pudo encontrar la función "lemmatize_strings"
Run `rlang::last_trace()` to see where the error occurred.
> rlang::last_trace()
<error/dplyr:::mutate_error>
  Error in `mutate()`:
  ℹ In argument: `word = lemmatize_strings(word, language = "es")`.
Caused by error in `lemmatize_strings()`:
  ! no se pudo encontrar la función "lemmatize_strings"
---
  Backtrace:
  ▆
1. ├─... %>% ...
2. ├─dplyr::mutate(...)
3. └─dplyr:::mutate.data.frame(...)
4.   └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
5.     ├─base::withCallingHandlers(...)
6.     └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
7.       └─mask$eval_all_mutate(quo)
8.         └─dplyr (local) eval()
Run rlang::last_trace(drop = FALSE) to see 3 hidden frames.
> rlang::last_trace(drop = FALSE)
<error/dplyr:::mutate_error>
  Error in `mutate()`:
  ℹ In argument: `word = lemmatize_strings(word, language = "es")`.
Caused by error in `lemmatize_strings()`:
  ! no se pudo encontrar la función "lemmatize_strings"
---
  Backtrace:
  ▆
1. ├─... %>% ...
2. ├─dplyr::mutate(...)
3. ├─dplyr:::mutate.data.frame(...)
4. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
5. │   ├─base::withCallingHandlers(...)
6. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
7. │     └─mask$eval_all_mutate(quo)
8. │       └─dplyr (local) eval()
9. └─base::.handleSimpleError(...)
10.   └─dplyr (local) h(simpleError(msg, call))
11.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)