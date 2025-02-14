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
colnames(df) <- c("Respuesta_Abierta")  # Asignar el nombre "Respuesta_Abierta" (sin espacios)

# Verificar la estructura del archivo
print(head(df))
print(colnames(df))

# 2. Limpieza de datos
df_limpio <- df %>%
  mutate(Respuesta_Abierta = as.character(Respuesta_Abierta),
         Respuesta_Abierta = tolower(Respuesta_Abierta), # Convertir a minúsculas
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta), # Eliminar puntuación
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta), # Eliminar números
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta), # Eliminar espacios extra
         Respuesta_Abierta = removeWords(Respuesta_Abierta, stopwords("es"))) # Eliminar stopwords en español

# 3. Lematización (en lugar de stemming)
df_limpio <- df_limpio %>%
  mutate(Respuesta_Abierta = lemmatize_strings(Respuesta_Abierta, language = "es"))

# 4. Análisis exploratorio: Palabras más comunes
tokenized_responses <- df_limpio %>%
  unnest_tokens(word, Respuesta_Abierta)

word_counts <- tokenized_responses %>%
  count(word, sort = TRUE)

# Ver las 10 palabras más comunes
print(head(word_counts, 10))

# Visualización de palabras más comunes
word_counts %>%
  filter(n > 50) %>% # Filtrar palabras que aparecen más de 50 veces
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Palabras más comunes en respuestas abiertas", x = "Palabra", y = "Frecuencia")

# 5. Bigramas (para capturar frases o ideas compuestas)
bigramas <- df_limpio %>%
  unnest_tokens(bigrama, Respuesta_Abierta, token = "ngrams", n = 2) %>%
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>%
  filter(!palabra1 %in% stopwords("es"),
         !palabra2 %in% stopwords("es")) %>%
  unite(bigrama, palabra1, palabra2, sep = " ") %>%
  count(bigrama, sort = TRUE)

# Ver los 10 bigramas más comunes
print(head(bigramas, 10))

# Visualización de bigramas
bigramas %>%
  filter(n > 20) %>% # Filtrar bigramas que aparecen más de 20 veces
  ggplot(aes(x = reorder(bigrama, n), y = n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Bigramas más comunes en respuestas abiertas", x = "Bigrama", y = "Frecuencia")

# 6. Análisis de sentimientos
sentimientos <- get_nrc_sentiment(df_limpio$Respuesta_Abierta, language = "spanish")

# Unir sentimientos al data frame original
df_limpio <- cbind(df_limpio, sentimientos)

# Resumir sentimientos
summary_sentimientos <- colSums(sentimientos)
print(summary_sentimientos)

# Visualización de sentimientos
df_limpio %>%
  gather(key = "sentimiento", value = "valor", -Respuesta_Abierta) %>%
  group_by(sentimiento) %>%
  summarise(total = sum(valor)) %>%
  ggplot(aes(x = reorder(sentimiento, total), y = total)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Análisis de sentimientos", x = "Sentimiento", y = "Total")

# 7. Modelado de tópicos (Topic Modeling)
# Crear una matriz de términos de documento (DTM)
dtm <- df_limpio %>%
  select(Respuesta_Abierta) %>%  # Seleccionar solo la columna de respuestas
  unnest_tokens(word, Respuesta_Abierta) %>%
  count(Respuesta_Abierta, word) %>%
  cast_dtm(Respuesta_Abierta, word, n)

# Ajustar un modelo LDA con 5 tópicos (puedes ajustar el número de tópicos)
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))

# Ver los tópicos y las palabras más relevantes
topics <- tidy(lda_model, matrix = "beta")
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

print(top_terms)

# Visualización de los tópicos
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(title = "Tópicos más relevantes", x = "Palabra", y = "Beta")


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
> colnames(df) <- c("Respuesta_Abierta")  # Asignar el nombre "Respuesta_Abierta" (sin espacios)
> 
  > # Verificar la estructura del archivo
  > print(head(df))
# A tibble: 6 × 1
Respuesta_Abierta                                                                                                                                             
<chr>                                                                                                                                                         
  1 ¿Qué necesidades de conocimiento o información acerca del programa SMART CORE, específicamente sobre la iniciativa “Disminuir el número de cancelaciones en s…
2 SE EVITA CON UNA BUENA ASESORIA AL SOCIO                                                                                                                      
3 todo bien                                                                                                                                                     
4 CONSIDERO QUE CON LA INFORMACIÓN PROPORCIONADA ES SUFICIENTE                                                                                                  
5 IMPORTANTE QUE SE ENVIE RECORDATORIO MEDIANTE PLATAFORMAS ACTUALES A LOS SOCIOS                                                                               
6 Considero que se nos ha proporcionado toda la informacion necesaria para conocer y saber el porque de las cancelaciones de las solicitudes, situaciones que d…
> print(colnames(df))
[1] "Respuesta_Abierta"
> 
  > # 2. Limpieza de datos
  > df_limpio <- df %>%
  +   mutate(Respuesta_Abierta = as.character(Respuesta_Abierta),
             +          Respuesta_Abierta = tolower(Respuesta_Abierta), # Convertir a minúsculas
             +          Respuesta_Abierta = removePunctuation(Respuesta_Abierta), # Eliminar puntuación
             +          Respuesta_Abierta = removeNumbers(Respuesta_Abierta), # Eliminar números
             +          Respuesta_Abierta = stripWhitespace(Respuesta_Abierta), # Eliminar espacios extra
             +          Respuesta_Abierta = removeWords(Respuesta_Abierta, stopwords("es"))) # Eliminar stopwords en español
> 
  > # 3. Lematización (en lugar de stemming)
  > df_limpio <- df_limpio %>%
  +   mutate(Respuesta_Abierta = lemmatize_strings(Respuesta_Abierta, language = "es"))
> 
  > # 4. Análisis exploratorio: Palabras más comunes
  > tokenized_responses <- df_limpio %>%
  +   unnest_tokens(word, Respuesta_Abierta)
> 
  > word_counts <- tokenized_responses %>%
  +   count(word, sort = TRUE)
> 
  > # Ver las 10 palabras más comunes
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
  > # Visualización de palabras más comunes
  > word_counts %>%
  +   filter(n > 50) %>% # Filtrar palabras que aparecen más de 50 veces
  +   ggplot(aes(x = reorder(word, n), y = n)) +
  +   geom_bar(stat = "identity") +
  +   coord_flip() +
  +   labs(title = "Palabras más comunes en respuestas abiertas", x = "Palabra", y = "Frecuencia")
> 
  > # 5. Bigramas (para capturar frases o ideas compuestas)
  > bigramas <- df_limpio %>%
  +   unnest_tokens(bigrama, Respuesta_Abierta, token = "ngrams", n = 2) %>%
  +   separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>%
  +   filter(!palabra1 %in% stopwords("es"),
             +          !palabra2 %in% stopwords("es")) %>%
  +   unite(bigrama, palabra1, palabra2, sep = " ") %>%
  +   count(bigrama, sort = TRUE)
> 
  > # Ver los 10 bigramas más comunes
  > print(head(bigramas, 10))
# A tibble: 10 × 2
bigrama                       n
<chr>                     <int>
  1 NA NA                      1427
2 buen curso                   43
3 excelente curso              26
4 excelente informacion        24
5 momento ninguna              21
6 informacion clara            17
7 servicio socio               16
8 buena informacion            15
9 cancelaciones solicitudes    14
10 toda informacion             12
> 
  > # Visualización de bigramas
  > bigramas %>%
  +   filter(n > 20) %>% # Filtrar bigramas que aparecen más de 20 veces
  +   ggplot(aes(x = reorder(bigrama, n), y = n)) +
  +   geom_bar(stat = "identity") +
  +   coord_flip() +
  +   labs(title = "Bigramas más comunes en respuestas abiertas", x = "Bigrama", y = "Frecuencia")
> 
  > # 6. Análisis de sentimientos
  > sentimientos <- get_nrc_sentiment(df_limpio$Respuesta_Abierta, language = "spanish")
> 
  > # Unir sentimientos al data frame original
  > df_limpio <- cbind(df_limpio, sentimientos)
> 
  > # Resumir sentimientos
  > summary_sentimientos <- colSums(sentimientos)
> print(summary_sentimientos)
anger anticipation      disgust         fear          joy      sadness     surprise        trust     negative     positive 
40          245           29          108          313          134           82          594          223         1241 
> 
  > # Visualización de sentimientos
  > df_limpio %>%
  +   gather(key = "sentimiento", value = "valor", -Respuesta_Abierta) %>%
  +   group_by(sentimiento) %>%
  +   summarise(total = sum(valor)) %>%
  +   ggplot(aes(x = reorder(sentimiento, total), y = total)) +
  +   geom_bar(stat = "identity") +
  +   coord_flip() +
  +   labs(title = "Análisis de sentimientos", x = "Sentimiento", y = "Total")
> 
  > # 7. Modelado de tópicos (Topic Modeling)
  > # Crear una matriz de términos de documento (DTM)
  > dtm <- df_limpio %>%
  +   select(Respuesta_Abierta) %>%  # Seleccionar solo la columna de respuestas
  +   unnest_tokens(word, Respuesta_Abierta) %>%
  +   count(Respuesta_Abierta, word) %>%
  +   cast_dtm(Respuesta_Abierta, word, n)
Error in `count()`:
  ! Must group by variables found in `.data`.
✖ Column `Respuesta_Abierta` is not found.
Run `rlang::last_trace()` to see where the error occurred.
> rlang::last_trace()
<error/rlang_error>
  Error in `count()`:
  ! Must group by variables found in `.data`.
✖ Column `Respuesta_Abierta` is not found.
---
  Backtrace:
  ▆
1. ├─... %>% cast_dtm(Respuesta_Abierta, word, n)
2. ├─tidytext::cast_dtm(., Respuesta_Abierta, word, n)
3. │ └─tidytext::cast_sparse(data, !!document, !!term, !!value, ...)
4. │   └─dplyr::ungroup(data)
5. ├─dplyr::count(., Respuesta_Abierta, word)
6. └─dplyr:::count.data.frame(., Respuesta_Abierta, word)
Run rlang::last_trace(drop = FALSE) to see 4 hidden frames.