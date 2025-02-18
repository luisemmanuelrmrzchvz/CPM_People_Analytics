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
  "Culpa" = "#8B4513",  # Marrón
  "Ironía" = "#FF1493",  # Rosa profundo
  "Preocupación" = "#8A2BE2",  # Azul violeta
  "Descontento" = "#A52A2A"  # Marrón oscuro
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
umbral <- 0.5  # Umbral reducido para capturar emociones con valores bajos

# 10. Crear columnas para emociones complejas
df_limpio <- df_limpio %>%
  mutate(
    Frustración = ifelse(anger > umbral | sadness > umbral | disgust > umbral, 1, 0),
    Ansiedad = ifelse(fear > umbral | anticipation > umbral, 1, 0),
    Desesperación = ifelse(fear > umbral | sadness > umbral | negative > umbral, 1, 0),
    Sorpresa_Positiva = ifelse(surprise > umbral | joy > umbral | positive > umbral, 1, 0),
    Sorpresa_Negativa = ifelse(surprise > umbral | fear > umbral | negative > umbral, 1, 0),
    Confianza_Optimismo = ifelse(trust > umbral | anticipation > umbral | positive > umbral, 1, 0),
    Resentimiento = ifelse(anger > umbral | disgust > umbral | negative > umbral, 1, 0),
    Nostalgia = ifelse(sadness > umbral | joy > umbral | trust > umbral, 1, 0),
    Euforia = ifelse(joy > umbral | surprise > umbral | positive > umbral, 1, 0),
    Desilusión = ifelse(sadness > umbral | disgust > umbral | negative > umbral, 1, 0),
    Indignación = ifelse(anger > umbral | disgust > umbral | negative > umbral, 1, 0),
    Gratitud = ifelse(joy > umbral | trust > umbral | positive > umbral, 1, 0),
    Miedo_al_Fracaso = ifelse(fear > umbral | sadness > umbral | negative > umbral, 1, 0),
    Orgullo = ifelse(joy > umbral | trust > umbral | positive > umbral, 1, 0),
    Culpa = ifelse(sadness > umbral | fear > umbral | negative > umbral, 1, 0),
    Ironía = ifelse((anger > umbral & joy > umbral) | (disgust > umbral & positive > umbral), 1, 0),
    Preocupación = ifelse(fear > umbral | anticipation > umbral | sadness > umbral, 1, 0),
    Descontento = ifelse(anger > umbral | disgust > umbral | sadness > umbral, 1, 0)
  )

# 11. Definir prioridad de emociones complejas (las negativas primero)
prioridad_emociones <- c(
  "Frustración", "Ansiedad", "Desesperación", "Resentimiento", "Desilusión", 
  "Indignación", "Miedo_al_Fracaso", "Sorpresa_Negativa", "Culpa", 
  "Sorpresa_Positiva", "Confianza_Optimismo", "Nostalgia", "Euforia", 
  "Gratitud", "Orgullo", "Ironía", "Preocupación", "Descontento", "Neutro"
)

# 12. Clasificar respuestas según prioridad
df_limpio <- df_limpio %>%
  rowwise() %>%
  mutate(
    Emoción_Compleja = {
      emociones <- c(
        Frustración, Ansiedad, Desesperación, Resentimiento, Desilusión,
        Indignación, Miedo_al_Fracaso, Sorpresa_Negativa, Culpa,
        Sorpresa_Positiva, Confianza_Optimismo, Nostalgia, Euforia,
        Gratitud, Orgullo, Ironía, Preocupación, Descontento
      )
      nombres_emociones <- prioridad_emociones
      emocion <- nombres_emociones[which.max(emociones)]
      if (sum(emociones) == 0) "Neutro" else emocion
    }
  ) %>%
  ungroup()

# 13. Calcular la proporción y volumen de "Neutro_Directo" vs "Entran a Modelo"
proporcion <- df %>%
  summarise(Neutro_Directo = sum(sentimiento == "Neutro_Directo", na.rm = TRUE),
            Entran_a_Modelo = sum(is.na(sentimiento) | sentimiento != "Neutro_Directo", na.rm = TRUE)) %>%
  mutate(Total = Neutro_Directo + Entran_a_Modelo,
         Porcentaje_Neutro_Directo = Neutro_Directo / Total * 100,
         Porcentaje_Entran_a_Modelo = Entran_a_Modelo / Total * 100)

# 14. Gráfico de proporciones con estilo corporativo y volumen de registros
ggplot(proporcion, aes(x = "", y = Porcentaje_Neutro_Directo, fill = "Neutro_Directo")) +
  geom_bar(stat = "identity", width = 1, color = "white", linewidth = 0.5) +
  geom_bar(aes(y = Porcentaje_Entran_a_Modelo, fill = "Entran_a_Modelo"), stat = "identity", width = 1, color = "white", linewidth = 0.5) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Proporción de Neutro Directo vs Entran a Modelo",
       fill = "Categoría") +
  scale_fill_manual(values = colores_corporativos) +
  geom_label_repel(aes(label = paste0(round(Porcentaje_Neutro_Directo, 1), "%\n(", Neutro_Directo, " registros)")), 
                   position = position_stack(vjust = 0.5), size = 5, color = "white", fill = colores_corporativos["Neutro_Directo"], 
                   label.padding = unit(0.4, "lines"), show.legend = FALSE, box.padding = 0.5, max.overlaps = Inf) +
  geom_label_repel(aes(label = paste0(round(Porcentaje_Entran_a_Modelo, 1), "%\n(", Entran_a_Modelo, " registros)")), 
                   position = position_stack(vjust = 0.5), size = 5, color = "black", fill = colores_corporativos["Entran_a_Modelo"], 
                   label.padding = unit(0.4, "lines"), show.legend = FALSE, box.padding = 0.5, max.overlaps = Inf) +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# 15. Frecuencias de palabras para respuestas "Neutro_Directo"
# Top 15 palabras más comunes
top_palabras <- df_limpio %>%
  count(word, sort = TRUE) %>%
  top_n(15)

# Gráfico de frecuencias de palabras (Top 15 más frecuentes) con estilo corporativo
ggplot(top_palabras, aes(x = reorder(word, n), y = n)) +
  geom_bar(stat = "identity", fill = colores_corporativos["Neutro_Directo"], color = "white", linewidth = 0.5) +
  geom_text(aes(label = n), hjust = -0.2, size = 4, color = "black") +  # Agregar etiquetas
  coord_flip() +
  labs(title = "Frecuencia de Palabras (Top 15)",
       x = "Palabra",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# 16. Análisis de Bigramas
df_limpio <- df_limpio %>%
  mutate(word = ifelse(is.na(lemmatize_strings(word, language = "es")), word, lemmatize_strings(word, language = "es")))

# Generar bigramas
bigrama <- df_limpio %>%
  filter(!is.na(word)) %>%  # Eliminar palabras NA
  unnest_tokens(bigram, word, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE)

# Filtrar bigramas vacíos o inválidos
bigrama <- bigrama %>%
  filter(!is.na(bigram) & bigram != "NA NA")  # Eliminar bigramas NA o vacíos

# Verificar los bigramas generados
print(head(bigrama))

# Gráfico de bigramas (si hay datos válidos) con estilo corporativo
if (nrow(bigrama) > 0) {
  ggplot(bigrama, aes(x = reorder(bigram, n), y = n)) +
    geom_bar(stat = "identity", fill = colores_corporativos["Entran_a_Modelo"], color = "white", linewidth = 0.5) +
    geom_text(aes(label = n), hjust = -0.2, size = 4, color = "black") +  # Agregar etiquetas
    coord_flip() +
    labs(title = "Frecuencia de Bigramas",
         x = "Bigrama",
         y = "Frecuencia") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 14))
} else {
  print("No se encontraron bigramas válidos.")
}

# 17. Gráfico de distribución de emociones complejas (reemplaza el gráfico original de sentimientos)
df_limpio$Emoción_Compleja <- factor(df_limpio$Emoción_Compleja,
                                     levels = c("Frustración", "Ansiedad", "Desesperación", "Resentimiento", "Desilusión", 
                                                "Indignación", "Miedo_al_Fracaso", "Sorpresa_Negativa", "Culpa", 
                                                "Sorpresa_Positiva", "Confianza_Optimismo", "Nostalgia", "Euforia", 
                                                "Gratitud", "Orgullo", "Ironía", "Preocupación", "Descontento", "Neutro"))

# Gráfico de distribución de emociones complejas con estilo corporativo
ggplot(df_limpio, aes(x = Emoción_Compleja, fill = Emoción_Compleja)) +
  geom_bar(color = "white", linewidth = 0.5) +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4, color = "black") +  # Usar after_stat(count)
  scale_fill_manual(values = colores_corporativos) +
  labs(title = "Distribución de Emociones Complejas",
       x = "Emoción Compleja",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# 18. Guardar los resultados en un archivo Excel
write_xlsx(df_limpio, "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Resultados_Clasificacion_Sentimientos.xlsx")



############################################################

