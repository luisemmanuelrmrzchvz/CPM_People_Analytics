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
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),                  # Convertir a minúsculas
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),          # Eliminar puntuación
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),              # Eliminar números
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%        # Eliminar espacios extra
  unnest_tokens(word, Respuesta_Abierta) %>%                                # Tokenizar
  filter(!word %in% stopwords("es")) %>%                                    # Eliminar stopwords en español
  mutate(word = lemmatize_strings(word, language = "es"))                   # Lematización

# 3. Análisis exploratorio: Palabras más comunes
word_counts <- df_limpio %>% count(word, sort = TRUE)
print(head(word_counts, 10))

# Visualización de palabras más comunes (gráfico mejorado)
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

# 4. Bigramas (frases de 2 palabras)
bigramas <- df %>%
  # Se aplica una limpieza previa para formar correctamente los n-gramas
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  unnest_tokens(bigrama, Respuesta_Abierta, token = "ngrams", n = 2) %>%
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>%
  # Eliminar casos donde una de las dos palabras sea NA
  filter(!is.na(palabra1), !is.na(palabra2)) %>%
  filter(!palabra1 %in% stopwords("es"),
         !palabra2 %in% stopwords("es")) %>%
  unite(bigrama, palabra1, palabra2, sep = " ") %>%
  count(bigrama, sort = TRUE)

# Visualización de bigramas (gráfico mejorado)
bigramas %>%
  filter(n > 20) %>%
  ggplot(aes(x = reorder(bigrama, n), y = n)) +
  geom_bar(stat = "identity", fill = "tomato") +
  coord_flip() +
  labs(title = "Bigramas más comunes en respuestas abiertas",
       subtitle = "Frecuencia de combinaciones de dos palabras",
       x = "Bigrama", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# 5. Análisis de sentimientos
sentimientos <- get_nrc_sentiment(df$Respuesta_Abierta, language = "spanish")
summary_sentimientos <- colSums(sentimientos)
print(summary_sentimientos)

# Visualización de sentimientos (gráfico mejorado)
tibble(sentimiento = names(summary_sentimientos), total = summary_sentimientos) %>%
  ggplot(aes(x = reorder(sentimiento, total), y = total, fill = sentimiento)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Análisis de Sentimientos",
       subtitle = "Distribución de emociones en las respuestas",
       x = "Sentimiento", y = "Total") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none")

# 6. Modelado de tópicos (LDA)
# Crear la matriz documento-término (DTM) usando el identificador 'doc_id'
dtm <- df_limpio %>%
  count(doc_id, word, sort = TRUE) %>%
  cast_dtm(document = doc_id, term = word, value = n)

# Ajustar un modelo LDA con 5 tópicos (puedes ajustar este número según sea necesario)
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

# Extraer los tópicos y las palabras clave
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


> # Cargar las librerías necesarias
  > library(readxl)       # Para leer archivos Excel
> library(tidyverse)    # Para manipulación de datos
> library(tm)           # Para procesamiento de texto
> library(tidytext)     # Para tokenización y análisis de texto
> library(syuzhet)      # Para análisis de sentimientos
> library(topicmodels)  # Para modelado de tópicos
> library(ggplot2)      # Para visualización
> library(textstem)     # Para lematización
> library(widyr)        # Para n-gramas
> 
  > # 1. Cargar el archivo Excel y renombrar la columna
  > ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas abiertas.xlsx"
> df <- read_excel(ruta_archivo, col_names = FALSE)  # Leer sin nombres de columna
New names:
  • `` -> `...1`
> colnames(df) <- c("Respuesta_Abierta")             # Asignar el nombre "Respuesta_Abierta"
> 
  > # Eliminar filas vacías y asignar un identificador de documento
  > df <- df %>% 
  +   filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "") %>%
  +   mutate(doc_id = row_number())
> 
  > # 2. Limpieza de datos y tokenización
  > df_limpio <- df %>%
  +   mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),                  # Convertir a minúsculas
             +          Respuesta_Abierta = removePunctuation(Respuesta_Abierta),          # Eliminar puntuación
             +          Respuesta_Abierta = removeNumbers(Respuesta_Abierta),              # Eliminar números
             +          Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%        # Eliminar espacios extra
  +   unnest_tokens(word, Respuesta_Abierta) %>%                                # Tokenizar
  +   filter(!word %in% stopwords("es")) %>%                                    # Eliminar stopwords en español
  +   mutate(word = lemmatize_strings(word, language = "es"))                   # Lematización
> 
  > # 3. Análisis exploratorio: Palabras más comunes
  > word_counts <- df_limpio %>% count(word, sort = TRUE)
> print(head(word_counts, 10))
# A tibble: 10 × 2
word            n
<chr>       <int>
  1 bien          279
2 gracias       206
3 informacion   198
4 ninguna       189
5 socio         173
6 curso         152
7 momento       141
8 excelente     123
9 comentarios   103
10 ninguno       103
> 
  > # Visualización de palabras más comunes (gráfico mejorado)
  > word_counts %>%
  +   filter(n > 50) %>%
  +   ggplot(aes(x = reorder(word, n), y = n)) +
  +   geom_bar(stat = "identity", fill = "steelblue") +
  +   coord_flip() +
  +   labs(title = "Palabras más comunes en respuestas abiertas",
           +        subtitle = "Frecuencia de palabras",
           +        x = "Palabra", y = "Frecuencia") +
  +   theme_minimal() +
  +   theme(plot.title = element_text(face = "bold", size = 16),
            +         axis.text = element_text(size = 12),
            +         axis.title = element_text(size = 14))
> 
  > # 4. Bigramas (frases de 2 palabras)
  > bigramas <- df %>%
  +   # Se aplica una limpieza previa para formar correctamente los n-gramas
  +   mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
             +          Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
             +          Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
             +          Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  +   unnest_tokens(bigrama, Respuesta_Abierta, token = "ngrams", n = 2) %>%
  +   separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>%
  +   # Eliminar casos donde una de las dos palabras sea NA
  +   filter(!is.na(palabra1), !is.na(palabra2)) %>%
  +   filter(!palabra1 %in% stopwords("es"),
             +          !palabra2 %in% stopwords("es")) %>%
  +   unite(bigrama, palabra1, palabra2, sep = " ") %>%
  +   count(bigrama, sort = TRUE)
> 
  > # Visualización de bigramas (gráfico mejorado)
  > bigramas %>%
  +   filter(n > 20) %>%
  +   ggplot(aes(x = reorder(bigrama, n), y = n)) +
  +   geom_bar(stat = "identity", fill = "tomato") +
  +   coord_flip() +
  +   labs(title = "Bigramas más comunes en respuestas abiertas",
           +        subtitle = "Frecuencia de combinaciones de dos palabras",
           +        x = "Bigrama", y = "Frecuencia") +
  +   theme_minimal() +
  +   theme(plot.title = element_text(face = "bold", size = 16),
            +         axis.text = element_text(size = 12),
            +         axis.title = element_text(size = 14))
> 
  > # 5. Análisis de sentimientos
  > sentimientos <- get_nrc_sentiment(df$Respuesta_Abierta, language = "spanish")
> summary_sentimientos <- colSums(sentimientos)
> print(summary_sentimientos)
anger anticipation      disgust         fear          joy      sadness     surprise        trust     negative     positive 
40          246           29          108          313          361           80          593          450         1243 
> 
  > # Visualización de sentimientos (gráfico mejorado)
  > tibble(sentimiento = names(summary_sentimientos), total = summary_sentimientos) %>%
  +   ggplot(aes(x = reorder(sentimiento, total), y = total, fill = sentimiento)) +
  +   geom_bar(stat = "identity") +
  +   coord_flip() +
  +   labs(title = "Análisis de Sentimientos",
           +        subtitle = "Distribución de emociones en las respuestas",
           +        x = "Sentimiento", y = "Total") +
  +   theme_minimal() +
  +   theme(plot.title = element_text(face = "bold", size = 16),
            +         axis.text = element_text(size = 12),
            +         axis.title = element_text(size = 14),
            +         legend.position = "none")
> 
  > # 6. Modelado de tópicos (LDA)
  > # Crear la matriz documento-término (DTM) usando el identificador 'doc_id'
  > dtm <- df_limpio %>%
  +   count(doc_id, word, sort = TRUE) %>%
  +   cast_dtm(document = doc_id, term = word, value = n)
> 
  > # Ajustar un modelo LDA con 5 tópicos (puedes ajustar este número según sea necesario)
  > lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))
> 
  > # Extraer los tópicos y las palabras clave
  > topics <- tidy(lda_model, matrix = "beta")
> top_terms <- topics %>%
  +   group_by(topic) %>%
  +   top_n(10, beta) %>%
  +   ungroup() %>%
  +   arrange(topic, -beta)
> 
  > print(top_terms)
# A tibble: 50 × 3
topic term          beta
<int> <chr>        <dbl>
  1     1 gracias     0.113 
2     1 momento     0.0774
3     1 ma          0.0409
4     1 información 0.0373
5     1 considero   0.0285
6     1 credito     0.0235
7     1 proceso     0.0181
8     1 dudas       0.0148
9     1 iniciativa  0.0143
10     1 socio       0.0138
# ℹ 40 more rows
# ℹ Use `print(n = ...)` to see more rows
