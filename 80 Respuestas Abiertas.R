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
  "Muy Positivo" = "#3288BD"  # Azul claro
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

# 8. Clasificación de sentimientos (Positivo, Negativo, Neutro, con categorías más detalladas)
if (ncol(sentimientos) > 0) {
  df_limpio$sentimiento_valor <- sentimientos$positive - sentimientos$negative  # Puntaje de sentimiento
  
  # Ajustar las categorías de sentimiento según el puntaje
  df_limpio$sentimiento <- case_when(
    df_limpio$sentimiento_valor <= -2 ~ "Muy Negativo",
    df_limpio$sentimiento_valor == -1 ~ "Negativo",
    df_limpio$sentimiento_valor == 0 ~ "Neutro",
    df_limpio$sentimiento_valor == 1 ~ "Positivo",
    df_limpio$sentimiento_valor >= 2 ~ "Muy Positivo",
    TRUE ~ "Neutro"
  )
} else {
  warning("El análisis de sentimientos no produjo resultados válidos.")
}

# 9. Filtrar respuestas "Neutro_Directo" y respuestas que entran al modelo
df_neutro_directo <- df %>% filter(sentimiento == "Neutro_Directo")
df_entrar_modelo <- df %>% filter(sentimiento != "Neutro_Directo")

# 10. Calcular la proporción y volumen de "Neutro_Directo" vs "Entran a Modelo"
proporcion <- df %>%
  summarise(Neutro_Directo = sum(sentimiento == "Neutro_Directo", na.rm = TRUE),
            Entran_a_Modelo = sum(is.na(sentimiento) | sentimiento != "Neutro_Directo", na.rm = TRUE)) %>%
  mutate(Total = Neutro_Directo + Entran_a_Modelo,
         Porcentaje_Neutro_Directo = Neutro_Directo / Total * 100,
         Porcentaje_Entran_a_Modelo = Entran_a_Modelo / Total * 100)

print(proporcion)

# 11. Gráfico de proporciones con estilo corporativo y volumen de registros
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

# 12. Frecuencias de palabras para respuestas "Neutro_Directo"
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

# 13. Análisis de Bigramas
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

# 14. Gráfico de distribución de sentimientos (Negativo, Neutro, Positivo, Muy Positivo, Muy Negativo)
df_limpio$sentimiento <- factor(df_limpio$sentimiento,
                                levels = c("Muy Negativo", "Negativo", "Neutro", "Positivo", "Muy Positivo"))

# Gráfico de distribución de sentimientos con estilo corporativo
ggplot(df_limpio, aes(x = sentimiento, fill = sentimiento)) +
  geom_bar(color = "white", linewidth = 0.5) +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4, color = "black") +  # Usar after_stat(count)
  scale_fill_manual(values = colores_corporativos) +
  labs(title = "Distribución de Sentimientos",
       x = "Sentimiento",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# 15. Guardar los resultados en un archivo Excel
write_xlsx(df_limpio, "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Resultados_Clasificacion_Sentimientos.xlsx")






################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################




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
library(ggrepel)
library(wordcloud)
library(igraph)
library(ggraph)
library(topicmodels)
library(reticulate)  # Para integración con Python
library(data.table)  # Para manejo eficiente de datos grandes

# 1. Definir paleta de colores corporativa
colores_corporativos <- c(
  "Neutro_Directo" = "#66C2A5",  # Verde pastel
  "Entran_a_Modelo" = "#FC8D62",  # Naranja pastel
  "Muy Negativo" = "#D53E4F",  # Rojo oscuro
  "Negativo" = "#F46D43",  # Naranja oscuro
  "Neutro" = "#FFFFBF",  # Amarillo pastel
  "Positivo" = "#ABDDA4",  # Verde claro
  "Muy Positivo" = "#3288BD"  # Azul claro
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
sentimientos <- get_nrc_sentiment(df_limpio$Respuesta_Abierta, language = "spanish")

# 8. Clasificación de sentimientos (Positivo, Negativo, Neutro, con categorías más detalladas)
if (ncol(sentimientos) > 0) {
  df_limpio$sentimiento_valor <- sentimientos$positive - sentimientos$negative  # Puntaje de sentimiento
  
  # Ajustar las categorías de sentimiento según el puntaje
  df_limpio$sentimiento <- case_when(
    df_limpio$sentimiento_valor <= -2 ~ "Muy Negativo",
    df_limpio$sentimiento_valor == -1 ~ "Negativo",
    df_limpio$sentimiento_valor == 0 ~ "Neutro",
    df_limpio$sentimiento_valor == 1 ~ "Positivo",
    df_limpio$sentimiento_valor >= 2 ~ "Muy Positivo",
    TRUE ~ "Neutro"
  )
} else {
  warning("El análisis de sentimientos no produjo resultados válidos.")
}

# 9. Filtrar respuestas "Neutro_Directo" y respuestas que entran al modelo
df_neutro_directo <- df %>% filter(sentimiento == "Neutro_Directo")
df_entrar_modelo <- df %>% filter(sentimiento != "Neutro_Directo")

# 10. Calcular la proporción y volumen de "Neutro_Directo" vs "Entran a Modelo"
proporcion <- df %>%
  summarise(Neutro_Directo = sum(sentimiento == "Neutro_Directo", na.rm = TRUE),
            Entran_a_Modelo = sum(is.na(sentimiento) | sentimiento != "Neutro_Directo", na.rm = TRUE)) %>%
  mutate(Total = Neutro_Directo + Entran_a_Modelo,
         Porcentaje_Neutro_Directo = Neutro_Directo / Total * 100,
         Porcentaje_Entran_a_Modelo = Entran_a_Modelo / Total * 100)

print(proporcion)

# 11. Gráfico de proporciones con estilo corporativo y volumen de registros
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

# 12. Frecuencias de palabras para respuestas "Neutro_Directo"
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

# 13. Análisis de Bigramas
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

# 14. Gráfico de distribución de sentimientos (Negativo, Neutro, Positivo, Muy Positivo, Muy Negativo)
df_limpio$sentimiento <- factor(df_limpio$sentimiento,
                                levels = c("Muy Negativo", "Negativo", "Neutro", "Positivo", "Muy Positivo"))

# Gráfico de distribución de sentimientos con estilo corporativo
ggplot(df_limpio, aes(x = sentimiento, fill = sentimiento)) +
  geom_bar(color = "white", linewidth = 0.5) +
  geom_text(aes(label = after_stat(count)), stat = "count", vjust = -0.5, size = 4, color = "black") +  # Usar after_stat(count)
  scale_fill_manual(values = colores_corporativos) +
  labs(title = "Distribución de Sentimientos",
       x = "Sentimiento",
       y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

# 15. Análisis de Temas (Topic Modeling)
# Crear una matriz de términos de documento (DTM)
dtm <- df_limpio %>%
  count(doc_id, word) %>%
  cast_dtm(doc_id, word, n)

# Aplicar LDA para identificar 5 temas
lda_model <- LDA(dtm, k = 5, control = list(seed = 1234))
topics <- tidy(lda_model, matrix = "beta")

# Obtener los términos más importantes por tema
top_terms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Gráfico de los términos más importantes por tema
ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Términos más importantes por Tema",
       x = "Término",
       y = "Beta") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

# 16. Guardar los resultados en un archivo Excel
write_xlsx(df_limpio, "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Resultados_Clasificacion_Sentimientos.xlsx")