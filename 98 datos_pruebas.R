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
library(readxl)       # Para leer archivos Excel
library(tidyverse)    # Para manipulación de datos
library(tm)           # Para procesamiento de texto
library(tidytext)     # Para tokenización y análisis de texto
library(syuzhet)      # Para análisis de sentimientos
library(topicmodels)  # Para modelado de tópicos
library(ggplot2)      # Para visualización
library(textstem)     # Para lematización
library(widyr)        # Para n-gramas
library(writexl)      # Para exportar a Excel
library(igraph)       # Para redes de bigramas
library(ggraph)       # Para visualizar redes
library(reticulate)   # Para integrar modelos de Python (BERT)

# Configurar entorno de Python para usar BERT
# 1. Verificar si Conda está instalado
if (!reticulate::conda_binary() == "") {
  # 2. Crear un entorno de Conda si no existe
  if (!"r-tensorflow" %in% reticulate::conda_list()$name) {
    reticulate::conda_create("r-tensorflow")
    reticulate::conda_install("r-tensorflow", "tensorflow")
    reticulate::conda_install("r-tensorflow", "transformers")
  }
  
  # 3. Usar el entorno
  use_condaenv("r-tensorflow")
} else {
  stop("Conda no está instalado. Por favor, instala Anaconda o Miniconda.")
}

# Verificar la configuración de Python
print(py_config())

# 1. Cargar el archivo Excel y renombrar la columna
ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas abiertas.xlsx"
df <- read_excel(ruta_archivo, col_names = FALSE)  # Leer sin nombres de columna
colnames(df) <- c("Respuesta_Abierta")             # Asignar el nombre "Respuesta_Abierta"

# Eliminar filas vacías y asignar un identificador de documento
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

# Gráfico de palabras más comunes
word_counts %>%
  filter(n > 50) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Palabras más comunes en respuestas abiertas",
       subtitle = "Frecuencia de palabras",
       x = "Palabra", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# 4. Análisis de Emociones Específicas
# Cargar el léxico de sentimientos en español
lexicon_es <- get_sentiment_dictionary("nrc", language = "spanish")

# Asignar polaridad (positiva/negativa) a cada palabra
df_polaridad <- df_limpio %>%
  inner_join(lexicon_es, by = c("word" = "word"), relationship = "many-to-many") %>%
  group_by(doc_id) %>%
  summarise(polaridad = sum(value, na.rm = TRUE))

# Unir la polaridad al dataframe original
df <- df %>%
  left_join(df_polaridad, by = "doc_id")

# Visualizar la distribución de polaridad (excluyendo NA)
df %>%
  filter(!is.na(polaridad)) %>%
  ggplot(aes(x = polaridad)) +
  geom_histogram(binwidth = 1, fill = "steelblue") +
  labs(title = "Distribución de Polaridad en las Respuestas",
       x = "Polaridad", y = "Frecuencia") +
  theme_minimal()

# Filtrar respuestas con polaridad negativa para revisión manual
df_negativos <- df %>%
  filter(polaridad < 0) %>%
  select(doc_id, Respuesta_Abierta, polaridad)

write_xlsx(df_negativos, path = "Respuestas_Negativas.xlsx")

# 5. Análisis de Redes Semánticas
# Extraer trigramas
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

# Visualizar trigramas más comunes
trigramas %>%
  filter(n > 10) %>%
  ggplot(aes(x = reorder(trigrama, n), y = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Trigramas más comunes en respuestas abiertas",
       x = "Trigrama", y = "Frecuencia") +
  theme_minimal()

# Análisis de co-ocurrencia de palabras
co_ocurrencia <- df_limpio %>%
  pairwise_count(word, doc_id, sort = TRUE)

# Filtrar las aristas con mayor peso (n) para simplificar el grafo
co_ocurrencia_filtrado <- co_ocurrencia %>%
  filter(n > 10)  # Ajusta este umbral según sea necesario

# Crear el grafo a partir del dataframe filtrado
co_ocurrencia_graph <- graph_from_data_frame(co_ocurrencia_filtrado, directed = FALSE)

# Verificar el grafo
print(co_ocurrencia_graph)

# Visualizar la red de co-ocurrencia
ggraph(co_ocurrencia_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, color = "gray") +
  geom_node_point(color = "darkred", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.5, size = 3, check_overlap = TRUE) +
  theme_minimal() +
  labs(title = "Red de Co-Ocurrencia de Palabras",
       subtitle = "Relaciones entre palabras que aparecen juntas")

# 6. Uso de Modelos de Lenguaje Avanzados (BERT)
# Cargar tokenizador y modelo BERT en español
transformers <- import("transformers")
tokenizer <- transformers$BertTokenizer$from_pretrained("dccuchile/bert-base-spanish-wwm-cased")
model <- transformers$TFAutoModel$from_pretrained("dccuchile/bert-base-spanish-wwm-cased")

# Función para obtener embeddings de un texto
get_embeddings <- function(texto) {
  inputs <- tokenizer$encode_plus(texto, return_tensors = "tf", max_length = 128, truncation = TRUE, padding = "max_length")
  outputs <- model(inputs)
  embeddings <- outputs$last_hidden_state[, 1, ]  # Usamos el embedding del token [CLS]
  return(embeddings)
}

# Aplicar la función a todas las respuestas
embeddings <- t(sapply(df$Respuesta_Abierta, get_embeddings))

# Realizar clustering en los embeddings
kmeans_result <- kmeans(embeddings, centers = 5)  # Ajusta el número de clusters según necesidad
df$cluster <- kmeans_result$cluster

# Visualizar la distribución de clusters
ggplot(df, aes(x = as.factor(cluster), fill = as.factor(cluster))) +
  geom_bar() +
  labs(title = "Distribución de Clusters", x = "Cluster", y = "Frecuencia") +
  theme_minimal()

# Mostrar palabras clave por cluster
palabras_por_cluster <- df_limpio %>%
  left_join(df %>% select(doc_id, cluster), by = "doc_id") %>%
  group_by(cluster, word) %>%
  summarise(n = n()) %>%
  top_n(10, n)

print(palabras_por_cluster)


####################################################################################
####################################################################################
####################################################################################
####################################################################################

