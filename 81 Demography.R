# --------------------------------------
# ANÁLISIS DEMOGRÁFICO - COOPERATIVA FINANCIERA
# --------------------------------------

# 1. Cargar librerías ----
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)

# 2. Cargar y preparar datos ----
file_path <- "C:/Users/racl26345/Documents/Gestión de Indicadores/Indicadores de RH (2015-2024)/Indicadores 2025/5. Mayo/02 Management/32_Datos_demograficos_2025_05_31.xlsx"

data <- read_xlsx(file_path) %>%
  mutate(
    fecha_contratacion = as.Date(fecha_contratacion),
    fecha_nacimiento = as.Date(fecha_nacimiento),
    edad = as.numeric(difftime(as.Date("2025-03-01"), fecha_nacimiento, units = "days"))/365.25, # Paréntesis corregido
    antiguedad = as.numeric(difftime(as.Date("2025-03-01"), fecha_contratacion, units = "days"))/365.25, # Paréntesis corregido
    generacion = case_when(
      edad >= 65 ~ "Tradicionalistas (65+)",
      edad >= 57 & edad < 65 ~ "Baby Boomers (57-64)",
      edad >= 41 & edad < 57 ~ "Generación X (41-56)",
      edad >= 25 & edad < 41 ~ "Millennials (25-40)",
      edad < 25 ~ "Gen Z (18-24)",
      TRUE ~ "Fuera de rango"
    )
  )

# 3. Función para guardar gráficos ----
guardar_grafico <- function(nombre_archivo, ancho = 10, alto = 6) {
  ggsave(
    paste0("plots/", nombre_archivo, ".png"),
    width = ancho,
    height = alto,
    dpi = 300
  )
}

# Crear directorio para plots
if(!dir.exists("plots")) dir.create("plots")

# 4. Visualizaciones clave ----

# Gráfico A. Equilibrio de género en roles de liderazgo ----
data %>%
  filter(clasificacion_liderazgo != "SIN PERSONAL A CARGO") %>%
  mutate(clasificacion_liderazgo = fct_relevel(
    clasificacion_liderazgo,
    "LIDER CON PERSONAL A CARGO",
    "LIDER SIN PERSONAL A CARGO"
  )) %>%
  ggplot(aes(x = clasificacion_liderazgo, fill = genero)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("#FF69B4", "#4682B4")) +
  labs(
    title = "Equilibrio de Género en Roles de Liderazgo",
    subtitle = "Roles con responsabilidad de personal o liderazgo funcional",
    x = "Tipo de rol",
    y = "Proporción",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("01_liderazgo_genero")

# Gráfico B. Brecha de género en puestos estratégicos ----
data %>%
  filter(segmento_puesto %in% c("Estrategicos", "Tácticos")) %>%
  group_by(segmento_puesto, genero) %>%
  summarise(Total = n(), .groups = "drop") %>%
  ggplot(aes(x = segmento_puesto, y = Total, fill = genero)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = Total),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    color = "black"
  ) +
  scale_fill_manual(values = c("#FF69B4", "#4682B4")) +
  labs(
    title = "Distribución de Género en Roles Estratégicos",
    subtitle = "Puestos de alto impacto en la organización",
    x = "Segmento de puesto",
    y = "Total de colaboradores",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("02_estrategicos_genero")

# Gráfico C. Top 10 puestos con mayor brecha de género ----
# Primero creamos overview_list con los datos necesarios
overview_list <- list()

overview_list$puesto_generico <- data %>%
  group_by(puesto_generico) %>%
  summarise(
    Total = n(),
    "% Femenino" = mean(genero == "F", na.rm = TRUE) * 100,
    "% Masculino" = mean(genero == "M", na.rm = TRUE) * 100
  ) %>%
  arrange(desc(Total))

# Luego generamos el gráfico
overview_list$puesto_generico %>%
  filter(Total >= 50) %>%
  mutate(
    Diferencia = abs(`% Femenino` - `% Masculino`),
    Etiqueta = paste0(round(Diferencia, 1), "%")
  ) %>%
  arrange(desc(Diferencia)) %>%
  head(10) %>%
  ggplot(aes(
    x = reorder(puesto_generico, Diferencia),
    y = Diferencia,
    fill = ifelse(`% Femenino` > 50, "F", "M")
  )) +
  geom_col() +
  geom_text(
    aes(label = Etiqueta),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("F" = "#FF69B4", "M" = "#4682B4"),
    name = "Mayoría"
  ) +
  expand_limits(y = max(overview_list$puesto_generico$Diferencia) * 1.1) +
  labs(
    title = "Top 10 Puestos con Mayor Brecha de Género",
    subtitle = "Diferencia porcentual entre géneros (muestras >50 empleados)",
    x = "",
    y = "Diferencia porcentual"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("03_top_brechas", ancho = 9, alto = 6)

# Gráfico D. Distribución generacional en liderazgo ----
data %>%
  filter(
    segmento_puesto %in% c("Estrategicos", "Tácticos") |
      str_detect(clasificacion_liderazgo, "LIDERAZGO")
  ) %>%
  count(generacion, genero) %>%
  ggplot(aes(x = generacion, y = n, fill = genero)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  scale_fill_manual(values = c("#FF69B4", "#4682B4")) +
  labs(
    title = "Distribución Generacional en Liderazgo y Estratégicos",
    subtitle = "Combinación de roles de alto impacto",
    x = "Generación",
    y = "Total de colaboradores",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("04_generaciones_liderazgo")

# Gráfico E. Densidad de edad y antigüedad ----
data %>%
  ggplot(aes(x = edad, y = antiguedad)) +
  geom_bin2d(bins = 20) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_fill_gradient(low = "#E0F3F8", high = "#4575B4") +  # Paréntesis corregido
  labs(
    title = "Relación Edad-Antigüedad",
    subtitle = "Distribución conjunta y tendencia lineal",
    x = "Edad",
    y = "Antigüedad (años)",
    fill = "Cantidad"
  ) +
  theme_minimal()

guardar_grafico("05_edad_antiguedad")

# --------------------------------------
# VISUALIZACIÓN: EDADES Y LIDERAZGO 
# --------------------------------------

# 6. Nueva visualización: Pirámide poblacional en liderazgo ----
data_liderazgo <- data %>%
  filter(clasificacion_liderazgo %in% c("CON PERSONAL A CARGO", "SIN PERSONAL A CARGO (ROL DE LIDERAZGO)")) %>%
  mutate(
    grupo_edad = cut(edad,
                     breaks = c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf),
                     labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+"),
                     right = FALSE
    )
  )

# Gráfico F. Pirámide de edades por género en liderazgo
ggplot(data_liderazgo, aes(x = grupo_edad, fill = genero)) +
  geom_bar(data = subset(data_liderazgo, genero == "F"), aes(y = ..count..), stat = "count") +
  geom_bar(data = subset(data_liderazgo, genero == "M"), aes(y = -..count..), stat = "count") +
  scale_fill_manual(values = c("F" = "#FF69B4", "M" = "#4682B4")) +
  scale_y_continuous(
    labels = abs,
    breaks = seq(-200, 200, 50),
    name = "Cantidad de Colaboradores"
  ) +
  coord_flip() +
  labs(
    title = "Distribución de Edades en Posiciones de Liderazgo",
    subtitle = "Estructura poblacional por género y grupos quinquenales",
    x = "Grupo de Edad",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("06_piramide_liderazgo")

# Gráfico G. Heatmap: Edad vs. Antigüedad en Liderazgo ----
data_liderazgo %>%
  mutate(
    antiguedad_grupo = cut(antiguedad,
                           breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
                           labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "30+"),
                           right = FALSE
    )
  ) %>%
  count(grupo_edad, antiguedad_grupo) %>%
  ggplot(aes(x = grupo_edad, y = antiguedad_grupo, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 3) +
  scale_fill_gradient(low = "#E0F3F8", high = "#4575B4", name = "Empleados") +
  labs(
    title = "Relación Edad-Antigüedad en Liderazgo",
    subtitle = "Concentración de experiencia en posiciones clave",
    x = "Grupo de Edad",
    y = "Antigüedad (años)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("07_heatmap_experiencia")

# Gráfico H. Boxplot: Antigüedad en Liderazgo por Género ----
data_liderazgo %>%
  ggplot(aes(x = genero, y = antiguedad, fill = genero)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("F" = "#FF69B4", "M" = "#4682B4")) +
  labs(
    title = "Distribución de Antigüedad en Liderazgo por Género",
    subtitle = "Comparación de la experiencia acumulada",
    x = "Género",
    y = "Antigüedad (años)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("08_boxplot_antiguedad")

# --------------------------------------
# ANÁLISIS: DISPARIDAD DE GÉNERO EN CORPORATIVO (ODG)
# --------------------------------------

# 9. Filtrar datos para posiciones ODG ----
data_odg <- data %>%
  filter(nivel_gestion == "ODG") %>%
  group_by(departamento) %>%
  summarise(
    Total = n(),
    "% Femenino" = mean(genero == "F", na.rm = TRUE) * 100,
    "% Masculino" = mean(genero == "M", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    Diferencia = abs(`% Femenino` - `% Masculino`)
  )

# 10. Top 10 departamentos con mayor disparidad de género ----
top_10_disparidad <- data_odg %>%
  filter(Total >= 20) %>%
  arrange(desc(Diferencia)) %>%
  head(10)

# Gráfico I: Top 10 departamentos con mayor disparidad
ggplot(top_10_disparidad, aes(
  x = reorder(departamento, Diferencia),
  y = Diferencia,
  fill = ifelse(`% Femenino` > `% Masculino`, "F", "M")
)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(Diferencia, 1), "%")),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("F" = "#FF69B4", "M" = "#4682B4"),
    name = "Mayoría"
  ) +
  labs(
    title = "Top 10 Familias de Puestos con Mayor Disparidad de Género en Corporativo",
    subtitle = "Posiciones ODG: Corporativo (muestras > 20 colaboradores)",
    x = "Familia de Puestos",
    y = "Diferencia porcentual entre géneros"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("09_top10_disparidad_odg", ancho = 9, alto = 6)

# 11. Tabla de resumen para pegar en el chat ----
cat("=== RESUMEN: DISPARIDAD DE GÉNERO EN CORPORATIVO (ODG) ===\n\n")
print(top_10_disparidad, n = Inf)
cat("\n----------------------------------------\n")





################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################
















# --------------------------------------
# ANÁLISIS DEMOGRÁFICO - COOPERATIVA FINANCIERA
# --------------------------------------

# 1. Cargar librerías ----
library(tidyverse)
library(lubridate)
library(readxl)
library(scales)
library(glue)

# 2. Cargar y preparar datos ----
file_path <- "C:\\Users\\racl26345\\Documents\\Gestión de Indicadores\\Indicadores de RH (2015-2024)\\Indicadores 2025\\05. Mayo\\02 Management\\32_Datos_demograficos_2025_05_31.xlsx"

data <- read_xlsx(file_path) %>%
  mutate(
    fecha_contratacion = as.Date(fecha_contratacion),
    fecha_nacimiento = as.Date(fecha_nacimiento),
    edad = as.numeric(difftime(as.Date("2025-03-01"), fecha_nacimiento, units = "days"))/365.25,
    antiguedad = as.numeric(difftime(as.Date("2025-03-01"), fecha_contratacion, units = "days"))/365.25,
    generacion = case_when(
      edad >= 65 ~ "Tradicionalistas (65+)",
      edad >= 57 & edad < 65 ~ "Baby Boomers (57-64)",
      edad >= 41 & edad < 57 ~ "Generación X (41-56)",
      edad >= 25 & edad < 41 ~ "Millennials (25-40)",
      edad < 25 ~ "Gen Z (18-24)",
      TRUE ~ "Fuera de rango"
    ),
    generacion = factor(generacion, levels = c("Gen Z (18-24)", "Millennials (25-40)", 
                                               "Generación X (41-56)", "Baby Boomers (57-64)", 
                                               "Tradicionalistas (65+)", "Fuera de rango"))
  )

# 3. Función para guardar gráficos ----
guardar_grafico <- function(nombre_archivo, ancho = 10, alto = 6) {
  ggsave(
    paste0("plots/", nombre_archivo, ".png"),
    width = ancho,
    height = alto,
    dpi = 300
  )
}

# Crear directorio para plots
if(!dir.exists("plots")) dir.create("plots")

# 4. Visualizaciones clave con mejoras ----

# Gráfico A. Equilibrio de género en roles de liderazgo ----
data %>%
  filter(clasificacion_liderazgo != "SIN PERSONAL A CARGO") %>%
  mutate(
    clasificacion_liderazgo = fct_relevel(
      clasificacion_liderazgo,
      "LIDER CON PERSONAL A CARGO",
      "LIDER SIN PERSONAL A CARGO"
    ),
    # Acortar los nombres para el eje X
    clasificacion_liderazgo_short = case_when(
      clasificacion_liderazgo == "LIDER CON PERSONAL A CARGO" ~ "Líder c/personal",
      clasificacion_liderazgo == "LIDER SIN PERSONAL A CARGO" ~ "Líder s/personal",
      TRUE ~ as.character(clasificacion_liderazgo)
    )
  ) %>%
  count(clasificacion_liderazgo_short, genero) %>%
  group_by(clasificacion_liderazgo_short) %>%
  mutate(
    porcentaje = n/sum(n),
    etiqueta = glue("{round(porcentaje*100)}% ({n})")
  ) %>%
  ggplot(aes(x = clasificacion_liderazgo_short, y = porcentaje, fill = genero)) +
  geom_col() +
  geom_text(
    aes(label = etiqueta),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 6
  ) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = c("#FF69B4", "#4682B4")) +
  labs(
    title = "Equilibrio de Género en Roles de Liderazgo",
    subtitle = "Roles con responsabilidad de personal o liderazgo funcional",
    x = "Tipo de rol",
    y = "Proporción",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.x = element_text(angle = 0, hjust = 0.5) # Texto horizontal
  )

guardar_grafico("01_liderazgo_genero")

# Gráfico B. Brecha de género en puestos estratégicos ----
data %>%
  filter(segmento_puesto %in% c("Estrategicos", "Tácticos")) %>%
  group_by(segmento_puesto, genero) %>%
  summarise(Total = n(), .groups = "drop") %>%
  ggplot(aes(x = segmento_puesto, y = Total, fill = genero)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = Total),
    position = position_dodge(width = 0.9),
    vjust = -0.5,
    color = "black",
    size = 4.5
  ) +
  scale_fill_manual(values = c("#FF69B4", "#4682B4")) +
  labs(
    title = "Distribución de Género en Roles Estratégicos",
    subtitle = "Puestos de alto impacto en la organización",
    x = "Segmento de puesto",
    y = "Total de colaboradores",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("02_estrategicos_genero")

# Gráfico C. Top 10 puestos con mayor brecha de género ----
overview_list <- list()

overview_list$puesto_generico <- data %>%
  group_by(puesto_generico) %>%
  summarise(
    Total = n(),
    Femenino = sum(genero == "F", na.rm = TRUE),
    Masculino = sum(genero == "M", na.rm = TRUE),
    Porc_Femenino = mean(genero == "F", na.rm = TRUE) * 100,
    Porc_Masculino = mean(genero == "M", na.rm = TRUE) * 100
  ) %>%
  arrange(desc(Total))

overview_list$puesto_generico %>%
  filter(Total >= 50) %>%
  mutate(
    Diferencia = abs(Porc_Femenino - Porc_Masculino),
    Etiqueta_dif = paste0(round(Diferencia, 1), "%"),
    Etiqueta_generos = paste0("F=", Femenino, " / M=", Masculino)  # Formato simplificado
  ) %>%
  arrange(desc(Diferencia)) %>%
  head(10) %>%
  ggplot(aes(
    x = reorder(puesto_generico, Diferencia),
    y = Diferencia,
    fill = ifelse(Porc_Femenino > 50, "F", "M")
  )) +
  geom_col() +
  # Etiqueta de brecha (exterior - derecha)
  geom_text(
    aes(label = Etiqueta_dif),
    hjust = -0.1,
    size = 4,
    color = "black"
  ) +
  # Etiqueta simplificada (interior - izquierda)
  geom_text(
    aes(label = Etiqueta_generos),
    y = 1,  # Posición fija cerca del inicio de la barra
    hjust = 0,
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("F" = "#FF69B4", "M" = "#4682B4"),
    name = "Mayoría"
  ) +
  expand_limits(y = max(overview_list$puesto_generico$Diferencia) * 1.1) +
  labs(
    title = "Top 10 Puestos con Mayor Brecha de Género",
    subtitle = "Diferencia porcentual entre géneros (muestras >50 empleados)\nTotal de colaboradores por género (F=Femenino, M=Masculino)",
    x = "",
    y = "Diferencia porcentual"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("03_top_brechas", ancho = 9, alto = 6)

# Gráfico D. Distribución generacional en liderazgo ----
data %>%
  filter(
    segmento_puesto %in% c("Estrategicos", "Tácticos") |
      str_detect(clasificacion_liderazgo, "LIDERAZGO")
  ) %>%
  count(generacion, genero) %>%
  filter(generacion != "Fuera de rango") %>%
  ggplot(aes(x = generacion, y = n, fill = genero)) +
  geom_col(position = "dodge") +
  geom_text(
    aes(label = n),
    position = position_dodge(width = 0.9),
    vjust = -0.5
  ) +
  scale_fill_manual(values = c("#FF69B4", "#4682B4")) +
  labs(
    title = "Distribución Generacional en Liderazgo y Estratégicos",
    subtitle = "Combinación de roles de alto impacto (ordenado de menor edad a mayor edad)",
    x = "Generación",
    y = "Total de colaboradores",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("04_generaciones_liderazgo")

# Gráfico E. Densidad de edad y antigüedad ----
data %>%
  ggplot(aes(x = edad, y = antiguedad)) +
  geom_bin2d(bins = 20) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  scale_fill_gradient(low = "#E0F3F8", high = "#4575B4") +
  labs(
    title = "Relación Edad-Antigüedad",
    subtitle = "Distribución conjunta y tendencia lineal",
    x = "Edad",
    y = "Antigüedad (años)",
    fill = "Cantidad"
  ) +
  theme_minimal()

guardar_grafico("05_edad_antiguedad")

# --------------------------------------
# VISUALIZACIÓN: EDADES Y LIDERAZGO 
# --------------------------------------

# 6. Nueva visualización: Pirámide poblacional en liderazgo ----
data_liderazgo <- data %>%
  filter(clasificacion_liderazgo %in% c("CON PERSONAL A CARGO", "SIN PERSONAL A CARGO (ROL DE LIDERAZGO)")) %>%
  mutate(
    grupo_edad = cut(edad,
                     breaks = c(18, 25, 30, 35, 40, 45, 50, 55, 60, 65, Inf),
                     labels = c("18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+"),
                     right = FALSE
    )
  )

# Gráfico F. Pirámide de edades por género en liderazgo
data_piramide <- data_liderazgo %>%
  count(grupo_edad, genero) %>%
  mutate(
    n = ifelse(genero == "M", -n, n),
    etiqueta_pos = ifelse(genero == "F", n + 5, n - 5)
  )

ggplot(data_piramide, aes(x = grupo_edad, y = n, fill = genero)) +
  geom_col() +
  geom_text(
    aes(y = etiqueta_pos, label = abs(n)),
    color = "black",
    size = 4.5
  ) +
  scale_fill_manual(values = c("F" = "#FF69B4", "M" = "#4682B4")) +
  scale_y_continuous(
    labels = abs,
    breaks = seq(-200, 200, 50),
    name = "Cantidad de Colaboradores"
  ) +
  coord_flip() +
  labs(
    title = "Distribución de Edades en Posiciones de Liderazgo",
    subtitle = "Estructura poblacional por género y grupos quinquenales",
    x = "Grupo de Edad",
    fill = "Género"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("06_piramide_liderazgo")

# Gráfico G. Heatmap: Edad vs. Antigüedad en Liderazgo ----
data_liderazgo %>%
  mutate(
    antiguedad_grupo = cut(antiguedad,
                           breaks = c(0, 5, 10, 15, 20, 25, 30, Inf),
                           labels = c("0-5", "6-10", "11-15", "16-20", "21-25", "26-30", "30+"),
                           right = FALSE
    )
  ) %>%
  count(grupo_edad, antiguedad_grupo) %>%
  ggplot(aes(x = grupo_edad, y = antiguedad_grupo, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black", size = 4.5) +
  scale_fill_gradient(low = "#E0F3F8", high = "#4575B4", name = "Empleados") +
  labs(
    title = "Relación Edad-Antigüedad en Liderazgo",
    subtitle = "Concentración de experiencia en posiciones clave",
    x = "Grupo de Edad",
    y = "Antigüedad (años)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("07_heatmap_experiencia")

# Gráfico H. Boxplot: Antigüedad en Liderazgo por Género ----
data_liderazgo %>%
  ggplot(aes(x = genero, y = antiguedad, fill = genero)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("F" = "#FF69B4", "M" = "#4682B4")) +
  labs(
    title = "Distribución de Antigüedad en Liderazgo por Género",
    subtitle = "Comparación de la experiencia acumulada",
    x = "Género",
    y = "Antigüedad (años)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("08_boxplot_antiguedad")

# --------------------------------------
# ANÁLISIS: DISPARIDAD DE GÉNERO EN CORPORATIVO (ODG)
# --------------------------------------

# 9. Filtrar datos para posiciones ODG ----
data_odg <- data %>%
  filter(nivel_gestion == "ODG") %>%
  group_by(departamento) %>%
  summarise(
    Total = n(),
    Femenino = sum(genero == "F", na.rm = TRUE),
    Masculino = sum(genero == "M", na.rm = TRUE),
    Porc_Femenino = mean(genero == "F", na.rm = TRUE) * 100,
    Porc_Masculino = mean(genero == "M", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    Diferencia = abs(Porc_Femenino - Porc_Masculino)
  )

# Gráfico I: Top 10 departamentos con mayor disparidad --------
top_10_disparidad <- data_odg %>%
  filter(Total >= 20) %>%
  arrange(desc(Diferencia)) %>%
  head(10) %>%
  mutate(
    Etiqueta_dif = paste0(round(Diferencia, 1), "%"),
    Etiqueta_generos = paste0("F=", Femenino, " / M=", Masculino)  # Formato simplificado
  )

ggplot(top_10_disparidad, aes(
  x = reorder(departamento, Diferencia),
  y = Diferencia,
  fill = ifelse(Porc_Femenino > Porc_Masculino, "F", "M")
)) +
  geom_col() +
  # Etiqueta de brecha (exterior - derecha)
  geom_text(
    aes(label = Etiqueta_dif),
    hjust = -0.1,
    size = 4,
    color = "black"
  ) +
  # Etiqueta simplificada (interior - izquierda)
  geom_text(
    aes(label = Etiqueta_generos),
    y = 1,  # Posición fija cerca del inicio de la barra
    hjust = 0,
    size = 4,
    color = "white",
    fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("F" = "#FF69B4", "M" = "#4682B4"),
    name = "Mayoría"
  ) +
  labs(
    title = "Top 10 Familias de Puestos con Mayor Disparidad de Género en Corporativo",
    subtitle = "Posiciones ODG: Corporativo (muestras > 20 colaboradores)\nTotal de colaboradores por género (F=Femenino, M=Masculino)",
    x = "Familia de Puestos",
    y = "Diferencia porcentual entre géneros"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 14)
  )

guardar_grafico("09_top10_disparidad_odg", ancho = 9, alto = 6)

# 11. Tabla de resumen para pegar en el chat ----
cat("=== RESUMEN: DISPARIDAD DE GÉNERO EN CORPORATIVO (ODG) ===\n\n")
print(top_10_disparidad, n = Inf)
cat("\n----------------------------------------\n")