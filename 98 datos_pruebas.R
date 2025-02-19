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

# 1. Definir paleta de colores corporativa
colores_corporativos <- c(
  "Neutro_Directo" = "#66C2A5",
  "Entran_a_Modelo" = "#FC8D62",
  "Muy Negativo" = "#D53E4F",
  "Negativo" = "#F46D43",
  "Neutro" = "#FFFFBF",
  "Positivo" = "#ABDDA4",
  "Muy Positivo" = "#3288BD",
  "Frustración" = "#8B0000",
  "Ansiedad" = "#FF4500",
  "Desesperación" = "#4B0082",
  "Sorpresa_Positiva" = "#32CD32",
  "Sorpresa_Negativa" = "#8A2BE2",
  "Confianza_Optimismo" = "#00CED1",
  "Resentimiento" = "#B22222",
  "Nostalgia" = "#FF69B4",
  "Euforia" = "#FFD700",
  "Desilusión" = "#800080",
  "Indignación" = "#DC143C",
  "Gratitud" = "#228B22",
  "Miedo_al_Fracaso" = "#2F4F4F",
  "Orgullo" = "#FF8C00",
  "Culpa" = "#8B4513"
)

# 2. Cargar y preparar datos
ruta_archivo <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Respuestas Abiertas C4C.xlsx"
df <- read_excel(ruta_archivo, col_names = FALSE) %>% 
  slice(-1) %>% 
  rename(Respuesta_Abierta = "...1") %>% 
  filter(!is.na(Respuesta_Abierta) & Respuesta_Abierta != "")

# 3. Categorizar respuestas cortas
df <- df %>%
  mutate(
    doc_id = row_number(),
    num_palabras = str_count(Respuesta_Abierta, "\\w+"),
    sentimiento = if_else(num_palabras < 6, "Neutro_Directo", NA_character_)
  )

# 4. Filtrar respuestas largas
df_modelo <- df %>% filter(num_palabras >= 6)

# 5. Calcular proporciones para gráfico
proporcion <- data.frame(
  Neutro_Directo = sum(df$sentimiento == "Neutro_Directo", na.rm = TRUE),
  Entran_a_Modelo = nrow(df_modelo)
) %>% 
  mutate(
    Total = Neutro_Directo + Entran_a_Modelo,
    Porcentaje_Neutro_Directo = (Neutro_Directo / Total) * 100,
    Porcentaje_Entran_a_Modelo = (Entran_a_Modelo / Total) * 100
  )

# 6. Preprocesamiento de texto
df_limpio <- df_modelo %>%
  mutate(
    Respuesta_Abierta = tolower(Respuesta_Abierta) %>% 
      removePunctuation() %>% 
      removeNumbers() %>% 
      stripWhitespace()
  ) %>%
  unnest_tokens(word, Respuesta_Abierta) %>%
  filter(!word %in% stopwords("es")) %>%
  mutate(word = lemmatize_strings(word, language = "es"))

# 7. Análisis de sentimientos
sentimientos <- get_nrc_sentiment(df_limpio$word, language = "spanish")
df_limpio <- cbind(df_limpio, sentimientos)

# 8. Detección de emociones complejas
umbral <- 0.5
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

# 9. Clasificación final de emociones
prioridad_emociones <- c(
  "Frustración", "Ansiedad", "Desesperación", "Sorpresa_Negativa", "Resentimiento",
  "Desilusión", "Indignación", "Miedo_al_Fracaso", "Culpa", "Sorpresa_Positiva",
  "Confianza_Optimismo", "Nostalgia", "Euforia", "Gratitud", "Orgullo"
)

df_limpio <- df_limpio %>%
  rowwise() %>%
  mutate(
    Emoción_Compleja = {
      emociones <- c_across(all_of(prioridad_emociones))
      ifelse(sum(emociones) == 0, "Neutro", prioridad_emociones[which.max(emociones)])
    }
  ) %>%
  ungroup()

# 10. Agregar el texto original al análisis
df_final <- df_limpio %>%
  left_join(df %>% select(doc_id, Comentario_Original = Respuesta_Abierta), by = "doc_id") %>%
  group_by(doc_id) %>%
  mutate(Comentario_Original = first(Comentario_Original)) %>%  # Mantener una sola copia por documento
  ungroup()

# 11.1 Visualizaciones ------------------------------------------------------
# Versión mejorada del gráfico de proporciones
g_prop <- proporcion %>%
  pivot_longer(cols = c(Neutro_Directo, Entran_a_Modelo)) %>%
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(
    name = "Categorías",
    values = colores_corporativos,
    labels = c(
      "Entran_a_Modelo" = paste0("Entran al Modelo: ", proporcion$Entran_a_Modelo,
                                 " (", round(proporcion$Porcentaje_Entran_a_Modelo, 1), "%)"),
      "Neutro_Directo" = paste0("Neutro Directo: ", proporcion$Neutro_Directo,
                                " (", round(proporcion$Porcentaje_Neutro_Directo, 1), "%)")
    )
  ) +
  labs(title = "Distribución de Respuestas") +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )

print(g_prop)

# 11.2 Resto de gráficos (Frecuencias, Bigramas, Emociones) -----------------
# Frecuencias de palabras para Neutro_Directo
df_neutros <- df %>% 
  filter(sentimiento == "Neutro_Directo") %>%
  unnest_tokens(word, Respuesta_Abierta) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  top_n(15, n)

g_neutros <- ggplot(df_neutros, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = colores_corporativos["Neutro_Directo"]) +
  geom_text(aes(label = n), hjust = -0.3, color = "black") +
  coord_flip() +
  labs(title = "Top 15 Palabras en Respuestas Neutro Directo",
       x = "Palabra",
       y = "Frecuencia") +
  theme_minimal()

print(g_neutros)

# Análisis de Bigramas
bigramas <- df_limpio %>%
  select(doc_id, word) %>%
  group_by(doc_id) %>%
  summarise(texto = paste(word, collapse = " ")) %>%
  unnest_tokens(bigrama, texto, token = "ngrams", n = 2) %>%
  separate(bigrama, c("palabra1", "palabra2"), sep = " ") %>%
  filter(!palabra1 %in% stopwords("es"),
         !palabra2 %in% stopwords("es")) %>%
  unite(bigrama, palabra1, palabra2, sep = " ") %>%
  count(bigrama, sort = TRUE) %>%
  top_n(15, n)

g_bigramas <- ggplot(bigramas, aes(x = reorder(bigrama, n), y = n)) +
  geom_col(fill = colores_corporativos["Entran_a_Modelo"]) +
  geom_text(aes(label = n), hjust = -0.3, color = "black") +
  coord_flip() +
  labs(title = "Bigramas más frecuentes",
       x = "Bigrama",
       y = "Frecuencia") +
  theme_minimal()

print(g_bigramas)

# Distribución de emociones complejas (excluyendo Neutro)
g_emociones <- df_limpio %>%
  filter(Emoción_Compleja != "Neutro") %>%  # Filtramos para excluir Neutro
  count(Emoción_Compleja) %>%
  ggplot(aes(x = reorder(Emoción_Compleja, n), y = n, fill = Emoción_Compleja)) +
  geom_col() +
  scale_fill_manual(
    values = colores_corporativos,
    name = "Emociones",
    guide = guide_legend(reverse = TRUE)
  ) +
  coord_flip() +
  labs(title = "Distribución de Emociones Detectadas (excluyendo Neutro)",
       x = "Emoción",
       y = "Frecuencia") +
  theme_minimal() +
  geom_text(aes(label = n), hjust = -0.3, color = "black", size = 3.5) +  # Añadir etiquetas numéricas
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.y = element_text(size = 10))

print(g_emociones)

# 12. Exportar resultados con comentario original
write_xlsx(list(
  Proporciones = proporcion,
  Analisis_Completo = df_final,
  Frecuencias_Neutros = df_neutros,
  Bigramas = bigramas
), "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Resultados_Finales_C4C.xlsx")
