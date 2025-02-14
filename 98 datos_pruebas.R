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
# Generamos bigramas a partir del df original aplicando una limpieza previa
bigramas <- df %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  unnest_tokens(bigrama, Respuesta_Abierta, token = "ngrams", n = 2) %>%
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>%
  filter(!is.na(palabra1), !is.na(palabra2)) %>%
  filter(!palabra1 %in% stopwords("es"),
         !palabra2 %in% stopwords("es")) %>%
  unite(bigrama, palabra1, palabra2, sep = " ") %>%
  count(bigrama, sort = TRUE)

# Visualización de bigramas generales (gráfico mejorado)
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

# Bigramas informativos: red de bigramas
# Separamos nuevamente los bigramas sin unir para crear la red
palabras_genericas <- c("bien", "gracias", "informacion", "ninguna", "socio", "curso", "momento", "excelente", "comentarios", "ninguno")

bigram_separados <- df %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  unnest_tokens(bigrama, Respuesta_Abierta, token = "ngrams", n = 2) %>%
  separate(bigrama, into = c("palabra1", "palabra2"), sep = " ") %>%
  filter(!is.na(palabra1), !is.na(palabra2)) %>%
  filter(!palabra1 %in% stopwords("es"),
         !palabra2 %in% stopwords("es")) %>%
  filter(!palabra1 %in% palabras_genericas,
         !palabra2 %in% palabras_genericas) %>%
  count(palabra1, palabra2, sort = TRUE) %>%
  filter(n > 5)  # Ajusta este umbral según convenga

# Crear y visualizar la red de bigramas informativos
bigram_graph <- graph_from_data_frame(bigram_separados)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = arrow(type = "closed", length = unit(3, "mm"))) +
  geom_node_point(color = "darkblue", size = 4) +
  geom_node_text(aes(label = name), vjust = 1.5, size = 4) +
  theme_minimal() +
  labs(title = "Red de Bigramas Informativos",
       subtitle = "Conexiones entre palabras significativas")

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

# Zoom en sentimientos negativos: filtrar respuestas con emociones negativas
negativos <- c("anger", "disgust", "fear", "sadness", "negative")
df_sentimientos <- cbind(df, sentimientos)
df_negativos <- df_sentimientos %>%
  filter(anger > 0 | disgust > 0 | fear > 0 | sadness > 0 | negative > 0)

# Visualización de los sentimientos negativos en general (opcional)
tibble(sentimiento = negativos, total = summary_sentimientos[negativos]) %>%
  ggplot(aes(x = reorder(sentimiento, total), y = total, fill = sentimiento)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Zoom en Sentimientos Negativos",
       subtitle = "Enojo, disgusto, miedo, tristeza y negativo global",
       x = "Sentimiento negativo", y = "Total") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        legend.position = "none")

# Guardar respuestas con percepciones negativas para revisión manual
ruta_export <- file.path(dirname(ruta_archivo), "Revisión Manual Sentimientos Negativos.xlsx")
write_xlsx(df_negativos, path = ruta_export)

# 6. Modelado de tópicos (LDA)
# Crear la matriz documento-término (DTM) usando el identificador 'doc_id'
dtm <- df_limpio %>%
  count(doc_id, word, sort = TRUE) %>%
  cast_dtm(document = doc_id, term = word, value = n)

# Ajustar un modelo LDA con 5 tópicos (ajustable según necesidad)
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


Respuesta_Abierta	doc_id	anger	anticipation	disgust	fear	joy	sadness	surprise	trust	negative	positive
la capacitación e información ha sido esencial y buena para mejorar el servicio, el tema toca temas importantes como " el socio como centro de nuestro servicio" para conocer la experiencia de servicio del socio y que esta mejore también se deberá cuidar la parte laboral o talento humano, mediante estos activos de capacitación manteniendo un personal informado, capacitado, con plantillas completas y sobre todo con satisfacción de pertenecer a CPM, es por ello que es importante el cliente interno o la fuerza laboral esperadando que se mantenga así este año mediante incrementos salariales competitivos al mercado y acordes las nuevas metas y NO con las diferencias de los últimos años entre personal de confianza y sindicalizados.	62	1	1	0	2	2	0	0	7	1	15
Existe al día de hoy una gran área de oportunidad para validar los teléfonos en sucursal, por servicio y practicidad es necesario cuenten con un equipo móvil para validar los números de teléfono de los socios, incluso para apoyar a los socios con consultas sobre las apps. Me parece un acierto que se retome el tema del porque se cancelan las solicitudes, me parece también que es necesario se identifiquen puntualmente por plaza las principales incidencias que se dan en lo particular y poder trabajarlas. También necesario prepara al personal porque el servicio es bien importante, en este mundo de tecnologia el diferenciador será el trato con el socio.	2131	0	1	1	1	0	1	2	3	2	3	656
VIVIMOS  EN UN MUNDO  DE PRISAS Y EL SOCIO  NO TIENE  EL TIEMPO  SUFICENTE  PARA DURAR EN ESPERA PORQUE  SUS COMPROMISIS  LABORALES , PROFESIONALES  NO SE  LO PERMITEN  Y CPM NO LES  ESTA HACIENDO  NINGUN FAVOR ENTIENDANLO BIEN, LA  BUROCRACIA EN TRAMITES , PROCESOS  REPETITIVOS  Y POCO PERSONAL  EN ATENCION NO ES  SUFICIENTE NESECITAMOS  MAS  ANALISTAS  DE  CREDITO ,DE QUE  SIRVE TENER MUCHO MERCADO Y  NO TENEMOS  EL PERSONAL SUFICENTE  PARA  REALIZARLO ,  Y OJO NO ES PRESIONANDO AL PERSONAL A  DAR MAS , ES  NADA  MAS  SER CONGRUENTES	1950	0	1	0	0	0	1	0	2	2	1	541
