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

library(readxl)       # Para leer archivos Excel
library(tidyverse)    # Para manipulación de datos
library(tm)           # Para procesamiento de texto
library(tidytext)     # Para tokenización y análisis de texto
library(syuzhet)      # Para análisis de sentimientos
library(topicmodels)  # Para modelado de tópicos
library(ggplot2)      # Para visualización
library(textstem)     # Para lematización
library(widyr)        # Para n-gramas

# 1. Cargar el archivo Excel y renombrar la columna
ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas_abiertas.xlsx"
df <- read_excel(ruta_archivo, col_names = FALSE)  # Leer sin nombres de columna
colnames(df) <- c("Respuesta_Abierta")  # Asignar el nombre "Respuesta_Abierta"

# Eliminar filas vacías
df <- df %>% filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "")

# 2. Limpieza de datos
df_limpio <- df %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta), # Convertir a minúsculas
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta), # Eliminar puntuación
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta), # Eliminar números
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>% # Eliminar espacios extra
  unnest_tokens(word, Respuesta_Abierta) %>%
  filter(!word %in% stopwords("es")) # Eliminar stopwords en español

# 3. Lematización
df_limpio <- df_limpio %>%
  mutate(word = lemmatize_strings(word, language = "es"))

# 4. Análisis exploratorio: Palabras más comunes
word_counts <- df_limpio %>%
  count(word, sort = TRUE)

# Ver las 10 palabras más comunes
print(head(word_counts, 10))

# Visualización de palabras más comunes
word_counts %>%
  filter(n > 50) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Palabras más comunes en respuestas abiertas", x = "Palabra", y = "Frecuencia")

# 5. Bigramas
bigramas <- df %>%
  unnest_tokens(bigrama, Respuesta_Abierta, token = "ngrams", n = 2) %>%
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>%
  filter(!palabra1 %in% stopwords("es"),
         !palabra2 %in% stopwords("es")) %>%
  unite(bigrama, palabra1, palabra2, sep = " ") %>%
  count(bigrama, sort = TRUE)

# Visualización de bigramas
bigramas %>%
  filter(n > 20) %>%
  ggplot(aes(x = reorder(bigrama, n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Bigramas más comunes en respuestas abiertas", x = "Bigrama", y = "Frecuencia")

# 6. Análisis de sentimientos
sentimientos <- get_nrc_sentiment(df$Respuesta_Abierta, language = "spanish")

# Resumir sentimientos
summary_sentimientos <- colSums(sentimientos)
print(summary_sentimientos)

# Visualización de sentimientos
tibble(sentimiento = names(summary_sentimientos), total = summary_sentimientos) %>%
  ggplot(aes(x = reorder(sentimiento, total), y = total, fill = sentimiento)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Análisis de sentimientos", x = "Sentimiento", y = "Total")

# 7. Modelado de tópicos (LDA)
# Crear matriz de términos-documento (DTM)
dtm <- df_limpio %>%
  count(word, name = "freq") %>%
  cast_dtm(document = 1, term = word, value = freq)

# Ajustar modelo LDA con 5 tópicos
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

# Ver los tópicos y palabras clave
topics <- tidy(lda_model, matrix = "beta")
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)


####################################################################################
####################################################################################
####################################################################################
####################################################################################

