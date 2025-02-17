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
  mutate(word = lemmatize_strings(word, language = "es"),
         word = stem_strings(word, language = "es"))

# 3. Análisis de sentimientos utilizando `syuzhet` (diccionario NRC)
sentimientos <- get_sentiment(df_limpio$Respuesta_Abierta, method = "nrc", language = "spanish")

df$sentimiento <- sentimientos$sentiment

# 4. Clasificación de Sentimientos (Machine Learning) con `caret`
# Primero etiquetamos los sentimientos en positivo/negativo para el modelo
df$sentimiento_categoria <- ifelse(df$sentimiento == "positive", "Positivo", 
                                   ifelse(df$sentimiento == "negative", "Negativo", "Neutral"))

# 5. Preprocesamiento para Machine Learning
# Convertimos las respuestas en un formato adecuado para ML
df_ml <- df %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta)) %>%
  select(doc_id, Respuesta_Abierta, sentimiento_categoria)

# Crear un "corpus" para el análisis de texto
corpus <- Corpus(VectorSource(df_ml$Respuesta_Abierta))

# Preprocesamiento de texto
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("es"))
corpus <- tm_map(corpus, stripWhitespace)

# Crear una matriz de términos (DTM)
dtm <- DocumentTermMatrix(corpus)

# Convertir la DTM en un data.frame
dtm_df <- as.data.frame(as.matrix(dtm))
colnames(dtm_df) <- make.names(colnames(dtm_df))

# Agregar la variable de etiqueta (sentimiento) al data.frame
dtm_df$sentimiento_categoria <- df_ml$sentimiento_categoria

# 6. Entrenamiento de modelo con `caret` (Random Forest)
set.seed(123)

# Entrenar el modelo usando Random Forest
modelo_rf <- train(sentimiento_categoria ~ ., data = dtm_df, method = "rf", trControl = trainControl(method = "cv"))

# Predicción con el modelo entrenado
predicciones <- predict(modelo_rf, dtm_df)

# Agregar predicciones al dataframe original
df$sentimiento_predicho <- predicciones

# 7. Ver los resultados
head(df)

# Guardar las respuestas negativas en la nueva ubicación especificada
df_negativos <- df %>%
  filter(sentimiento_predicho == "Negativo") %>%
  select(doc_id, Respuesta_Abierta, sentimiento_predicho)

# Especificar la nueva ruta para guardar el archivo
write_xlsx(df_negativos, path = "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas_Negativas_ML.xlsx")

############################################################



> # Cargar las librerías necesarias
  > library(readxl)
> library(tidyverse)
> library(tm)
> library(tidytext)
> library(syuzhet)
> library(caret)
Cargando paquete requerido: lattice

Adjuntando el paquete: ‘caret’

The following object is masked from ‘package:purrr’:
  
  lift

Aviso:
  package ‘caret’ was built under R version 4.4.2 
> library(udpipe)
> 
  > # 1. Cargar el archivo Excel y renombrar la columna
  > ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas abiertas.xlsx"
> df <- read_excel(ruta_archivo, col_names = FALSE)
New names:
  • `` -> `...1`
> colnames(df) <- c("Respuesta_Abierta")
> 
  > df <- df %>% 
  +   filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "") %>% 
  +   mutate(doc_id = row_number())
> 
  > # 2. Limpieza de datos y tokenización
  > df_limpio <- df %>%
  +   mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
             +          Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
             +          Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
             +          Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  +   unnest_tokens(word, Respuesta_Abierta) %>%
  +   filter(!word %in% stopwords("es")) %>%
  +   mutate(word = lemmatize_strings(word, language = "es"),
             +          word = stem_strings(word, language = "es"))
> 
  > # 3. Análisis de sentimientos utilizando `syuzhet` (diccionario NRC)
  > sentimientos <- get_sentiment(df_limpio$Respuesta_Abierta, method = "nrc", language = "spanish")
Aviso:
  Unknown or uninitialised column: `Respuesta_Abierta`. 
> 
  > df$sentimiento <- sentimientos$sentiment
> 
  > # 4. Clasificación de Sentimientos (Machine Learning) con `caret`
  > # Primero etiquetamos los sentimientos en positivo/negativo para el modelo
  > df$sentimiento_categoria <- ifelse(df$sentimiento == "positive", "Positivo", 
                                       +                                    ifelse(df$sentimiento == "negative", "Negativo", "Neutral"))
Error in `$<-`:
  ! Assigned data `ifelse(...)` must be compatible with existing data.
✖ Existing data has 2564 rows.
✖ Assigned data has 0 rows.
ℹ Only vectors of size 1 are recycled.
Caused by error in `vectbl_recycle_rhs_rows()`:
  ! Can't recycle input of size 0 to size 2564.
Run `rlang::last_trace()` to see where the error occurred.
Aviso:
Unknown or uninitialised column: `sentimiento`. 
> rlang::last_trace()
<error/tibble_error_assign_incompatible_size>
Error in `$<-`:
! Assigned data `ifelse(...)` must be compatible with existing data.
✖ Existing data has 2564 rows.
✖ Assigned data has 0 rows.
ℹ Only vectors of size 1 are recycled.
Caused by error in `vectbl_recycle_rhs_rows()`:
! Can't recycle input of size 0 to size 2564.
---
  Backtrace:
  ▆
1. ├─base::`$<-`(`*tmp*`, sentimiento_categoria, value = `<lgl>`)
2. └─tibble:::`$<-.tbl_df`(`*tmp*`, sentimiento_categoria, value = `<lgl>`)
3.   └─tibble:::tbl_subassign(...)
4.     └─tibble:::vectbl_recycle_rhs_rows(value, fast_nrow(xo), i_arg = NULL, value_arg, call)