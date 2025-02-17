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

# 1. Cargar el archivo Excel, omitiendo la primera fila (título)
ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas abiertas.xlsx"
df <- read_excel(ruta_archivo, col_names = FALSE)
# Renombrar la columna
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

# Verificar las primeras filas de los resultados de sentimiento para asegurar que los datos son válidos
head(sentimientos)

# 7. Clasificación de sentimientos (Positivo, Negativo, Neutro, con categorías más detalladas)
if (ncol(sentimientos) > 0) {
  df_limpio$sentimiento <- case_when(
    sentimientos$positive > 0.75 ~ "Muy Positivo",
    sentimientos$positive > 0 ~ "Positivo",
    sentimientos$negative > 0.75 ~ "Muy Negativo",
    sentimientos$negative > 0 ~ "Negativo",
    TRUE ~ "Neutro"
  )
  
  # Añadir columna de valor de sentimiento (puntaje)
  # Asegurarse de que 'sentimientos' tiene las columnas adecuadas para el cálculo
  if ("positive" %in% colnames(sentimientos) && "negative" %in% colnames(sentimientos)) {
    df_limpio$sentimiento_valor <- sentimientos$positive - sentimientos$negative  # Puntaje de sentimiento
  } else {
    warning("Las columnas 'positive' y 'negative' no están presentes en el análisis de sentimientos.")
  }
} else {
  warning("El análisis de sentimientos no produjo resultados válidos.")
}

# 8. Filtrar respuestas "Neutro_Directo" y respuestas que entran al modelo
df_neutro_directo <- df %>% filter(sentimiento == "Neutro_Directo")
df_entrar_modelo <- df %>% filter(sentimiento != "Neutro_Directo")

# 9. Visualización de la proporción de "Neutro_Directo" vs "Entran a Modelo"
# Gráfico de pastel para mostrar proporción de "Neutro_Directo" y registros "Entran a Modelo"
ggplot(df, aes(x = "", fill = sentimiento)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Proporción de Neutro Directo vs Entran a Modelo")

# 10. Frecuencias de palabras para respuestas "Neutro_Directo"
# Top 15 palabras más comunes
top_palabras <- df_limpio %>%
  count(word, sort = TRUE) %>%
  top_n(15)

# Frecuencia de palabras fuera del top 15
otros_palabras <- df_limpio %>%
  count(word) %>%
  filter(!word %in% top_palabras$word) %>%
  summarise(otros = sum(n))

# Combina el top 15 y el volumen de palabras fuera del top 15
top_palabras <- bind_rows(top_palabras, tibble(word = "Otros", n = otros_palabras$otros))

# Gráfico de frecuencias de palabras (Top 15 más frecuentes)
ggplot(top_palabras, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip() +
  labs(title = "Frecuencia de Palabras (Top 15)") +
  theme_minimal()

# 11. Análisis de Bigramas
# Análisis de bigramas, evitando el valor "N/A"
bigrama <- df_limpio %>%
  unnest_tokens(bigram, word, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

# Excluir bigramas "N/A"
bigrama <- bigrama %>%
  filter(bigram != "NA NA")

# Gráfico de bigramas
ggplot(bigrama, aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  coord_flip() +
  labs(title = "Frecuencia de Bigramas") +
  theme_minimal()

# 12. Gráfico de distribución de sentimientos (Negativo, Neutro, Positivo, Muy Positivo, Muy Negativo)
ggplot(df_limpio, aes(x = sentimiento)) +
  geom_bar(fill = c("red", "grey", "green", "blue", "darkred")) +
  labs(title = "Distribución de Sentimientos") +
  theme_minimal()

# 13. Guardar los resultados en un archivo de Excel
write_xlsx(df_limpio, "Respuestas_Clasificadas.xlsx")






############################################################




