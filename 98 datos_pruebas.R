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
library(udpipe)       # Para lematización y análisis de dependencias
library(spacyr)       # Para análisis avanzado de texto (opcional)

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

# 6. Uso de Modelos de Lenguaje Avanzados (Alternativa a BERT en R)
# Usar udpipe para lematización y análisis de dependencias

# Intentar descargar el modelo de nuevo
modelo_udpipe <- udpipe_download_model(language = "spanish", model_dir = "C:/Users/racl26345/Documents/DataBases/")

# Si la descarga manual fue necesaria, carga el modelo desde la ruta especificada
modelo_udpipe <- udpipe_load_model(file = "C:/Users/racl26345/Documents/DataBases/udpipe/spanish-gsd-ud-2.5-191206.udpipe")

# Anotar el texto con udpipe
df_anotado <- udpipe_annotate(modelo_udpipe, x = df$Respuesta_Abierta)
df_anotado <- as.data.frame(df_anotado)

# Realizar clustering basado en las anotaciones
# (Aquí puedes agregar tu lógica de clustering basada en las anotaciones)

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

> # 6. Uso de Modelos de Lenguaje Avanzados (Alternativa a BERT en R)
  > # Usar udpipe para lematización y análisis de dependencias
  > 
  > # Intentar descargar el modelo de nuevo
  > modelo_udpipe <- udpipe_download_model(language = "spanish", model_dir = "C:/Users/racl26345/Documents/DataBases/")
Downloading udpipe model from https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.5/master/inst/udpipe-ud-2.5-191206/spanish-gsd-ud-2.5-191206.udpipe to C:/Users/racl26345/Documents/DataBases//spanish-gsd-ud-2.5-191206.udpipe
- This model has been trained on version 2.5 of data from https://universaldependencies.org
- The model is distributed under the CC-BY-SA-NC license: https://creativecommons.org/licenses/by-nc-sa/4.0
- Visit https://github.com/jwijffels/udpipe.models.ud.2.5 for model license details.
- For a list of all models and their licenses (most models you can download with this package have either a CC-BY-SA or a CC-BY-SA-NC license) read the documentation at ?udpipe_download_model. For building your own models: visit the documentation by typing vignette('udpipe-train', package = 'udpipe')
probando la URL 'https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.5/master/inst/udpipe-ud-2.5-191206/spanish-gsd-ud-2.5-191206.udpipe'
Something went wrong
Error in utils::download.file(url = url, destfile = to, mode = "wb") : 
  no fue posible abrir la URL 'https://raw.githubusercontent.com/jwijffels/udpipe.models.ud.2.5/master/inst/udpipe-ud-2.5-191206/spanish-gsd-ud-2.5-191206.udpipe'

> 
  > # Si la descarga manual fue necesaria, carga el modelo desde la ruta especificada
  > modelo_udpipe <- udpipe_load_model(file = "C:/Users/racl26345/Documents/DataBases/udpipe/spanish-gsd-ud-2.5-191206.udpipe")
> 
  > # Anotar el texto con udpipe
  > df_anotado <- udpipe_annotate(modelo_udpipe, x = df$Respuesta_Abierta)
> df_anotado <- as.data.frame(df_anotado)
> 
  > # Realizar clustering basado en las anotaciones
  > # (Aquí puedes agregar tu lógica de clustering basada en las anotaciones)
  > 
  > # Visualizar la distribución de clusters
  > ggplot(df, aes(x = as.factor(cluster), fill = as.factor(cluster))) +
  +   geom_bar() +
  +   labs(title = "Distribución de Clusters", x = "Cluster", y = "Frecuencia") +
  +   theme_minimal()
Error in `geom_bar()`:
  ! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error:
  ! objeto 'cluster' no encontrado
Run `rlang::last_trace()` to see where the error occurred.
> rlang::last_trace()
<error/rlang_error>
  Error in `geom_bar()`:
  ! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error:
  ! objeto 'cluster' no encontrado
---
  Backtrace:
  ▆
1. ├─base (local) `<fn>`(x)
2. ├─ggplot2:::print.ggplot(x)
3. │ ├─ggplot2::ggplot_build(x)
4. │ └─ggplot2:::ggplot_build.ggplot(x)
5. │   └─ggplot2:::by_layer(...)
6. │     ├─rlang::try_fetch(...)
7. │     │ ├─base::tryCatch(...)
8. │     │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
9. │     │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
10. │     │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
11. │     │ └─base::withCallingHandlers(...)
12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
13. │       └─l$compute_aesthetics(d, plot)
14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
15. │           └─base::lapply(aesthetics, eval_tidy, data = data, env = env)
16. │             └─rlang (local) FUN(X[[i]], ...)
17. └─base::as.factor(cluster)
18.   └─base::is.factor(x)
Run rlang::last_trace(drop = FALSE) to see 5 hidden frames.
> rlang::last_trace(drop = FALSE)
<error/rlang_error>
  Error in `geom_bar()`:
  ! Problem while computing aesthetics.
ℹ Error occurred in the 1st layer.
Caused by error:
  ! objeto 'cluster' no encontrado
---
  Backtrace:
  ▆
1. ├─base (local) `<fn>`(x)
2. ├─ggplot2:::print.ggplot(x)
3. │ ├─ggplot2::ggplot_build(x)
4. │ └─ggplot2:::ggplot_build.ggplot(x)
5. │   └─ggplot2:::by_layer(...)
6. │     ├─rlang::try_fetch(...)
7. │     │ ├─base::tryCatch(...)
8. │     │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
9. │     │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
10. │     │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
11. │     │ └─base::withCallingHandlers(...)
12. │     └─ggplot2 (local) f(l = layers[[i]], d = data[[i]])
13. │       └─l$compute_aesthetics(d, plot)
14. │         └─ggplot2 (local) compute_aesthetics(..., self = self)
15. │           └─base::lapply(aesthetics, eval_tidy, data = data, env = env)
16. │             └─rlang (local) FUN(X[[i]], ...)
17. ├─base::as.factor(cluster)
18. │ └─base::is.factor(x)
19. └─base::.handleSimpleError(...)
20.   └─rlang (local) h(simpleError(msg, call))
21.     └─handlers[[1L]](cnd)
22.       └─cli::cli_abort(...)
23.         └─rlang::abort(...)
