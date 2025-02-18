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
library(caret)
library(udpipe)
library(writexl)
library(ggplot2)
library(textstem)
library(ggrepel)  # Para evitar superposición de etiquetas

# 1. Definir paleta de colores corporativa
colores_corporativos <- c(
  "Neutro_Directo" = "#66C2A5",  # Verde pastel
  "Entran_a_Modelo" = "#FC8D62",  # Naranja pastel
  "Muy Negativo" = "#D53E4F",  # Rojo oscuro
  "Negativo" = "#F46D43",  # Naranja oscuro
  "Neutro" = "#FFFFBF",  # Amarillo pastel
  "Positivo" = "#ABDDA4",  # Verde claro
  "Muy Positivo" = "#3288BD",  # Azul claro
  "Frustración" = "#8B0000",  # Rojo oscuro
  "Ansiedad" = "#FF4500",  # Naranja rojizo
  "Desesperación" = "#4B0082",  # Índigo
  "Sorpresa_Positiva" = "#32CD32",  # Verde lima
  "Sorpresa_Negativa" = "#8A2BE2",  # Azul violeta
  "Confianza_Optimismo" = "#00CED1",  # Turquesa
  "Resentimiento" = "#B22222",  # Rojo fuego
  "Nostalgia" = "#FF69B4",  # Rosa caliente
  "Euforia" = "#FFD700",  # Oro
  "Desilusión" = "#800080",  # Púrpura
  "Indignación" = "#DC143C",  # Carmesí
  "Gratitud" = "#228B22",  # Verde forestal
  "Miedo_al_Fracaso" = "#2F4F4F",  # Gris oscuro
  "Orgullo" = "#FF8C00",  # Naranja oscuro
  "Culpa" = "#8B4513"  # Marrón
)

# 2. Cargar el archivo Excel, omitiendo la primera fila (título) y solo considerando los registros (no la pregunta)
ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas abiertas.xlsx"
df <- read_excel(ruta_archivo, col_names = FALSE)

# Filtrar la primera fila que es la pregunta
df <- df[-1, ]
colnames(df) <- c("Respuesta_Abierta")

# 3. Filtrar respuestas con 5 o menos palabras y categorizar como "Neutro_Directo"
df <- df %>%
  filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "") %>%
  mutate(doc_id = row_number(),
         num_palabras = str_count(Respuesta_Abierta, "\\w+"),
         sentimiento = if_else(num_palabras < 6, "Neutro_Directo", NA_character_))

# 4. Filtrar solo las respuestas con más de 5 palabras (para análisis de sentimientos)
df_modelo <- df %>%
  filter(num_palabras >= 6)

# 5. Limpieza de datos y tokenización (solo para respuestas de más de 5 palabras)
df_limpio <- df_modelo %>%
  mutate(Respuesta_Abierta = tolower(Respuesta_Abierta),
         Respuesta_Abierta = removePunctuation(Respuesta_Abierta),
         Respuesta_Abierta = removeNumbers(Respuesta_Abierta),
         Respuesta_Abierta = stripWhitespace(Respuesta_Abierta)) %>%
  unnest_tokens(word, Respuesta_Abierta) %>%
  filter(!word %in% stopwords("es")) %>%
  mutate(word = ifelse(is.na(lemmatize_strings(word, language = "es")), word, lemmatize_strings(word, language = "es")))

# 6. Agregar la columna original `Respuesta_Abierta` a `df_limpio` para análisis de sentimientos
df_limpio <- df_limpio %>%
  left_join(df %>% select(doc_id, Respuesta_Abierta), by = "doc_id")

# 7. Análisis de sentimientos utilizando `syuzhet` (diccionario NRC) para las respuestas mayores a 5 palabras
sentimientos <- get_nrc_sentiment(df_limpio$Respuesta_Abierta)

# 8. Agregar las emociones básicas al dataframe
df_limpio <- cbind(df_limpio, sentimientos)

# 9. Definir umbrales para emociones básicas
umbral <- 1  # Puedes ajustar este valor según tus datos

# 10. Crear columnas para emociones complejas
df_limpio <- df_limpio %>%
  mutate(
    Frustración = ifelse(anger > umbral & sadness > umbral & disgust > umbral, 1, 0),
    Ansiedad = ifelse(fear > umbral & anticipation > umbral, 1, 0),
    Desesperación = ifelse(fear > umbral & sadness > umbral & negative > umbral, 1, 0),
    Sorpresa_Positiva = ifelse(surprise > umbral & joy > umbral & positive > umbral, 1, 0),
    Sorpresa_Negativa = ifelse(surprise > umbral & fear > umbral & negative > umbral, 1, 0),
    Confianza_Optimismo = ifelse(trust > umbral & anticipation > umbral & positive > umbral, 1, 0),
    Resentimiento = ifelse(anger > umbral & disgust > umbral & negative > umbral, 1, 0),
    Nostalgia = ifelse(sadness > umbral & joy > umbral & trust > umbral, 1, 0),
    Euforia = ifelse(joy > umbral & surprise > umbral & positive > umbral, 1, 0),
    Desilusión = ifelse(sadness > umbral & disgust > umbral & negative > umbral, 1, 0),
    Indignación = ifelse(anger > umbral & disgust > umbral & negative > umbral, 1, 0),
    Gratitud = ifelse(joy > umbral & trust > umbral & positive > umbral, 1, 0),
    Miedo_al_Fracaso = ifelse(fear > umbral & sadness > umbral & negative > umbral, 1, 0),
    Orgullo = ifelse(joy > umbral & trust > umbral & positive > umbral, 1, 0),
    Culpa = ifelse(sadness > umbral & fear > umbral & negative > umbral, 1, 0)
  )

# 11. Definir prioridad de emociones complejas (las negativas primero)
prioridad_emociones <- c(
  "Frustración", "Ansiedad", "Desesperación", "Sorpresa_Negativa", "Resentimiento",
  "Desilusión", "Indignación", "Miedo_al_Fracaso", "Culpa", "Sorpresa_Positiva",
  "Confianza_Optimismo", "Nostalgia", "Euforia", "Gratitud", "Orgullo"
)

# 12. Clasificar respuestas según prioridad
df_limpio <- df_limpio %>%
  rowwise() %>%
  mutate(
    Emoción_Compleja = {
      emociones <- c(
        Frustración, Ansiedad, Desesperación, Sorpresa_Negativa, Resentimiento,
        Desilusión, Indignación, Miedo_al_Fracaso, Culpa, Sorpresa_Positiva,
        Confianza_Optimismo, Nostalgia, Euforia, Gratitud, Orgullo
      )
      nombres_emociones <- prioridad_emociones
      emocion <- nombres_emociones[which.max(emociones)]
      if (sum(emociones) == 0) "Neutro" else emocion
    }
  ) %>%
  ungroup()

# 13. Gráfico de distribución de emociones complejas
ggplot(df_limpio, aes(x = Emoción_Compleja, fill = Emoción_Compleja)) +
  geom_bar(color = "white", linewidth = 0.5) +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4, color = "black") +
  scale_fill_manual(values = colores_corporativos) +
  labs(title = "Distribución de Emociones Complejas",
       x = "Emoción Compleja",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# 14. Guardar los resultados en un archivo Excel
write_xlsx(df_limpio, "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Resultados_Emociones_Complejas.xlsx")



############################################################

