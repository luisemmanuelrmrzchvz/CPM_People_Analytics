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

library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm)
library(textclean)
library(textstem)

# Cargar datos
df <- read.csv("Encuesta.csv", stringsAsFactors = FALSE)

# Verificar nombres de las columnas
print(names(df))

# Convertir texto a minúsculas y eliminar caracteres especiales
df_limpio <- df %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta)) %>%
  mutate(Respuesta_Abierta = replace_non_ascii(Respuesta_Abierta)) %>%
  mutate(Respuesta_Abierta = gsub("[^a-z ]", "", Respuesta_Abierta))

# Eliminar filas con respuestas vacías
df_limpio <- df_limpio %>% filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "")

# Tokenización
df_tokens <- df_limpio %>%
  unnest_tokens(word, Respuesta_Abierta)

# Eliminar stopwords
data("stop_words")
df_tokens <- df_tokens %>% anti_join(stop_words, by = "word")

# Aplicar lematización
df_tokens <- df_tokens %>% mutate(word = lemmatize_words(word))

# Contar palabras más comunes
top_words <- df_tokens %>%
  count(word, sort = TRUE)
print(head(top_words, 20))

# Crear Document-Term Matrix (DTM)
dtm <- df_tokens %>%
  count(Respuesta_Abierta, word) %>%
  cast_dtm(document = Respuesta_Abierta, term = word, value = n)

# Ajustar modelo LDA con 3 tópicos (ajustable según necesidad)
k <- 3
lda_model <- LDA(dtm, k = k, control = list(seed = 1234))

# Extraer términos por tópico
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

