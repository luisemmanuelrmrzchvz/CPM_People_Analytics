library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)
library(grid)
library(gtable)

# Configurar estilo minimalista para videowall con fondo oscuro
theme_videowall <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#1E1E1E", color = NA),
      panel.background = element_rect(fill = "#2D2D2D", color = NA),
      panel.grid.major = element_line(color = "#404040", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      text = element_text(family = "sans-serif", color = "#E8E8E8"),
      plot.title = element_text(size = 32, face = "bold", hjust = 0.5, color = "#F5F5F5"),
      plot.subtitle = element_text(size = 24, hjust = 0.5, color = "#B0B0B0"),
      axis.title = element_text(size = 20, color = "#B0B0B0"),
      axis.text = element_text(size = 18, color = "#B0B0B0"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      legend.text = element_text(size = 16, color = "#E8E8E8"),
      legend.title = element_text(size = 18, color = "#E8E8E8"),
      legend.background = element_rect(fill = "#2D2D2D", color = NA),
      legend.key = element_rect(fill = "#2D2D2D", color = NA),
      strip.text = element_text(size = 16, face = "bold", color = "#E8E8E8"),
      strip.background = element_rect(fill = "#3D3D3D", color = NA)
    )
}

# Paleta de colores para fondo oscuro
colors <- c(
  "F" = "#FF6B6B",    # Rojo coral para mujeres
  "M" = "#4ECDC4",    # Verde azulado para hombres
  "primary" = "#4A7AFF",     # Azul más brillante
  "secondary" = "#00E5D4",   # Verde azulado más vibrante
  "accent" = "#FF5252",      # Rojo más brillante
  "success" = "#4CD964"      # Verde más vibrante
)

# Paleta para niveles de gestión
nivel_colors <- c(
  "ODG" = "#4A7AFF",     # Azul
  "PLAZA" = "#00E5D4",   # Verde azulado
  "SUCURSAL" = "#FF5252" # Rojo
)

# Conectar a la base de datos
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Query corregido con NUEVA clasificación de generaciones
query <- "WITH colaboradores_activos AS (
    SELECT 
        id_colaborador,
        area_de_cobranza,
        nombre_posicion,
        nivel_gestion,
        regional,
        estado,
        municipio,
        genero,
        fecha_nacimiento,
        fecha_ingreso
    FROM hist_posiciones
    WHERE fecha_daily = DATE('now', 'start of month', '-1 day')
        AND status = 'A'
        AND vacante = 'False'
),
inegi_unicos AS (
    SELECT 
        estado_rhenueva,
        municipio_rhenueva,
        estado_inegi,
        municipio_inegi,
        cve_ent,
        cve_mun,
        latitud,
        longitud,
        lat_decimal,
        lon_decimal,
        cve_carta
    FROM inegi_stds_mncps
    GROUP BY estado_rhenueva, municipio_rhenueva
)
SELECT
    CASE WHEN ca.nombre_posicion = 'COMODIN' THEN 'SUCURSAL'
        WHEN ca.nombre_posicion = 'ANALISTA DE CREDITO PCR' THEN 'SUCURSAL'
        ELSE ca.nivel_gestion END AS nivel_gestion,
    ca.regional,
    iu.estado_inegi,
    iu.municipio_inegi,
    iu.cve_ent,
    iu.cve_mun,
    iu.latitud,
    iu.longitud,
    iu.lat_decimal,
    iu.lon_decimal,
    iu.cve_carta,
    ca.genero,
    CASE WHEN ca.fecha_nacimiento <= '1945-12-31' THEN 'Tradicionalistas'
        WHEN ca.fecha_nacimiento <= '1955-12-31' THEN 'Boomers - Tempranos'
        WHEN ca.fecha_nacimiento <= '1964-12-31' THEN 'Boomers - Tardíos'
        WHEN ca.fecha_nacimiento <= '1973-12-31' THEN 'Gen X - Tempranos'
        WHEN ca.fecha_nacimiento <= '1980-12-31' THEN 'Gen X - Tardíos'
        WHEN ca.fecha_nacimiento <= '1988-12-31' THEN 'Millennials - Tempranos'
        WHEN ca.fecha_nacimiento <= '1996-12-31' THEN 'Millennials - Tardíos'
        WHEN ca.fecha_nacimiento <= '2004-12-31' THEN 'Gen Z - Tempranos'
        WHEN ca.fecha_nacimiento <= '2012-12-31' THEN 'Gen Z - Tardíos'
        WHEN ca.fecha_nacimiento <= '2018-12-31' THEN 'Gen Alfa - Tempranos'
        WHEN ca.fecha_nacimiento <= '2025-12-31' THEN 'Gen Alfa - Tardíos'
        ELSE 'Gen Beta' END AS generacion,
    CASE WHEN ((JULIANDAY(DATE('now', 'start of month', '-1 day')) - JULIANDAY(ca.fecha_ingreso)) / 365) < 1 THEN '< 1 Años'
        WHEN ((JULIANDAY(DATE('now', 'start of month', '-1 day')) - JULIANDAY(ca.fecha_ingreso)) / 365) < 5 THEN '1-5 Años'
        WHEN ((JULIANDAY(DATE('now', 'start of month', '-1 day')) - JULIANDAY(ca.fecha_ingreso)) / 365) < 10 THEN '5-9 Años'
        WHEN ((JULIANDAY(DATE('now', 'start of month', '-1 day')) - JULIANDAY(ca.fecha_ingreso)) / 365) < 15 THEN '10-14 Años'
        WHEN ((JULIANDAY(DATE('now', 'start of month', '-1 day')) - JULIANDAY(ca.fecha_ingreso)) / 365) < 20 THEN '15-19 Años'
        WHEN ((JULIANDAY(DATE('now', 'start of month', '-1 day')) - JULIANDAY(ca.fecha_ingreso)) / 365) < 25 THEN '20-24 Años'
        ELSE '> 25 Años' END AS tenure_group,
    COUNT(DISTINCT ca.id_colaborador) AS colaboradores_activos,
    SUM(JULIANDAY(DATE('now', 'start of month', '-1 day')) - JULIANDAY(ca.fecha_ingreso)) AS tenure_days
FROM colaboradores_activos ca
LEFT JOIN inegi_unicos iu
    ON (ca.estado || '_' || ca.municipio) = (iu.estado_rhenueva || '_' || iu.municipio_rhenueva)
GROUP BY nivel_gestion, regional, estado_inegi, municipio_inegi, cve_ent, cve_mun, 
         latitud, longitud, lat_decimal, lon_decimal, cve_carta, genero, generacion, tenure_group
ORDER BY nivel_gestion, regional, estado_inegi, municipio_inegi, cve_ent, cve_mun, 
         latitud, longitud, lat_decimal, lon_decimal, cve_carta, genero, generacion, tenure_group
;"

datos <- dbGetQuery(con, query)
dbDisconnect(con)

# Calcular indicadores principales
total_activos <- sum(datos$colaboradores_activos)
total_mujeres <- sum(datos$colaboradores_activos[datos$genero == 'F'])
total_hombres <- sum(datos$colaboradores_activos[datos$genero == 'M'])

# Función para crear recuadros de indicadores con fondo oscuro
crear_indicador <- function(valor, titulo, color) {
  ggplot() +
    annotate("rect", xmin = -1, xmax = 1, ymin = -1, ymax = 1, 
             fill = color, alpha = 0.9, color = NA) +
    annotate("text", x = 0, y = 0.3, label = format(valor, big.mark = ","), 
             size = 12, fontface = "bold", color = "white") +
    annotate("text", x = 0, y = -0.3, label = titulo, 
             size = 8, color = "white", fontface = "bold") +
    theme_void() +
    coord_fixed() +
    theme(plot.background = element_rect(fill = "#1E1E1E", color = NA))
}

# Crear recuadros de indicadores
indicador_total <- crear_indicador(total_activos, "TOTAL ACTIVOS", colors["primary"])
indicador_mujeres <- crear_indicador(total_mujeres, "MUJERES", colors["F"])
indicador_hombres <- crear_indicador(total_hombres, "HOMBRES", colors["M"])

# FUNCIONES PARA LOS GRÁFICOS

# 1. Pirámide poblacional por Generación y Género vs Nivel de Gestión (CON NUEVAS GENERACIONES)
crear_piramide_generacion <- function(data) {
  # Preparar datos para la pirámide
  datos_piramide <- data %>%
    filter(!is.na(genero), !is.na(generacion), !is.na(nivel_gestion)) %>%
    group_by(generacion, genero, nivel_gestion) %>%
    summarise(total = sum(colaboradores_activos), .groups = 'drop') %>%
    group_by(generacion, genero) %>%
    mutate(porcentaje_genero = total / sum(total) * 100) %>%
    ungroup()
  
  # Convertir a negativo los valores para mujeres (lado izquierdo)
  datos_piramide <- datos_piramide %>%
    mutate(total_ajustado = ifelse(genero == "F", -total, total))
  
  # Ordenar generaciones (de mayor a menor edad)
  niveles_generacion <- c(
    "Tradicionalistas",
    "Boomers - Tempranos", "Boomers - Tardíos",
    "Gen X - Tempranos", "Gen X - Tardíos",
    "Millennials - Tempranos", "Millennials - Tardíos",
    "Gen Z - Tempranos", "Gen Z - Tardíos",
    "Gen Alfa - Tempranos", "Gen Alfa - Tardíos",
    "Gen Beta"
  )
  
  datos_piramide$generacion <- factor(datos_piramide$generacion, levels = niveles_generacion)
  
  # Filtrar solo las generaciones que tienen datos significativos (opcional)
  datos_piramide <- datos_piramide %>%
    filter(!is.na(generacion))
  
  # Crear pirámide
  piramide <- ggplot(datos_piramide, aes(x = generacion, y = total_ajustado, fill = nivel_gestion)) +
    geom_bar(stat = "identity", position = "stack", alpha = 0.9, width = 0.7) +
    geom_text(aes(label = ifelse(abs(total_ajustado) > max(abs(total_ajustado))*0.02, 
                                 format(abs(total), big.mark = ","), "")),
              position = position_stack(vjust = 0.5), 
              color = "white", size = 5, fontface = "bold") + # Reducido tamaño de texto por más categorías
    coord_flip() +
    scale_fill_manual(values = nivel_colors, name = "Nivel de Gestión") +
    scale_y_continuous(labels = function(x) format(abs(x), big.mark = ","),
                       breaks = scales::pretty_breaks(n = 8)) +
    labs(title = "PIRÁMIDE GENERACIONAL Y GÉNERO VS NIVEL DE GESTIÓN",
         x = "Generación",
         y = "Número de Colaboradores",
         subtitle = "Mujeres ← | → Hombres") +
    theme_videowall() +
    theme(legend.position = "bottom",
          plot.subtitle = element_text(hjust = 0.5, size = 14, color = "#B0B0B0", face = "bold"),
          axis.text.y = element_text(size = 12, face = "bold"), # Reducido para más categorías
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  
  return(piramide)
}

# 2. Gráfico de barras para Antigüedad y Género vs Nivel de Gestión (BARRAS AGRUPADAS)
crear_grafico_antiguedad <- function(data) {
  # Preparar datos para antigüedad
  datos_antiguedad <- data %>%
    filter(!is.na(genero), !is.na(tenure_group), !is.na(nivel_gestion)) %>%
    group_by(tenure_group, genero, nivel_gestion) %>%
    summarise(total = sum(colaboradores_activos), .groups = 'drop') %>%
    group_by(tenure_group, genero) %>%
    mutate(porcentaje_genero = total / sum(total) * 100) %>%
    ungroup()
  
  # Ordenar rangos de antigüedad
  niveles_antiguedad <- c("< 1 Años", "1-5 Años", "5-9 Años", "10-14 Años",
                          "15-19 Años", "20-24 Años", "> 25 Años")
  datos_antiguedad$tenure_group <- factor(datos_antiguedad$tenure_group, levels = niveles_antiguedad)
  
  # Crear gráfico de barras agrupadas (géneros juntos)
  grafico_antiguedad <- ggplot(datos_antiguedad, aes(x = tenure_group, y = total, fill = nivel_gestion, alpha = genero)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(aes(label = ifelse(total > max(total)*0.01, format(total, big.mark = ","), "")), 
              position = position_dodge(width = 0.8),
              vjust = -0.3, color = "white", size = 4.5, fontface = "bold") +
    scale_fill_manual(values = nivel_colors, name = "Nivel de Gestión") +
    scale_alpha_manual(values = c("F" = 0.9, "M" = 0.6), name = "Género", 
                       labels = c("F" = "Femenino", "M" = "Masculino")) +
    scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.15))) +
    labs(title = "ANTIGÜEDAD Y GÉNERO VS NIVEL DE GESTIÓN",
         x = "Rango de Antigüedad",
         y = "Número de Colaboradores") +
    theme_videowall() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
          plot.title = element_text(size = 18, hjust = 0.5),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.box = "vertical")
  
  return(grafico_antiguedad)
}

# 3. Función para crear tabla de datos de la pirámide (CON NUEVAS GENERACIONES)
crear_tabla_piramide_compacta <- function(data) {
  datos_tabla <- data %>%
    filter(!is.na(genero), !is.na(generacion), !is.na(nivel_gestion)) %>%
    group_by(generacion, genero, nivel_gestion) %>%
    summarise(total = sum(colaboradores_activos), .groups = 'drop') %>%
    pivot_wider(names_from = nivel_gestion, values_from = total, values_fill = 0) %>%
    mutate(Total = ODG + PLAZA + SUCURSAL) %>%
    arrange(generacion, genero)
  
  # Ordenar generaciones (igual que en la pirámide)
  niveles_generacion <- c(
    "Tradicionalistas",
    "Boomers - Tempranos", "Boomers - Tardíos",
    "Gen X - Tempranos", "Gen X - Tardíos",
    "Millennials - Tempranos", "Millennials - Tardíos",
    "Gen Z - Tempranos", "Gen Z - Tardíos",
    "Gen Alfa - Tempranos", "Gen Alfa - Tardíos",
    "Gen Beta"
  )
  
  datos_tabla$generacion <- factor(datos_tabla$generacion, levels = niveles_generacion)
  datos_tabla <- datos_tabla %>% arrange(generacion, genero)
  
  # Renombrar columnas en español y mayúsculas
  datos_tabla <- datos_tabla %>%
    rename(
      "GENERACIÓN" = generacion,
      "GÉNERO" = genero,
      "TOTAL" = Total
    )
  
  # Crear tabla visual compacta con texto más grande
  tabla_grob <- tableGrob(
    datos_tabla,
    rows = NULL,
    theme = ttheme_default(
      core = list(
        bg_params = list(fill = "#2D2D2D", col = "#404040"),
        fg_params = list(col = "#E8E8E8", fontsize = 16) # Ajustado por más categorías
      ),
      colhead = list(
        bg_params = list(fill = "#4A7AFF", col = "#404040"),
        fg_params = list(col = "white", fontsize = 18, fontface = "bold")
      ),
      rowhead = list(
        bg_params = list(fill = "#3D3D3D", col = "#404040"),
        fg_params = list(col = "#E8E8E8", fontsize = 16) # Ajustado por más categorías
      )
    )
  )
  
  return(tabla_grob)
}

# 4. Función para crear tabla de datos de antigüedad
crear_tabla_antiguedad_compacta <- function(data) {
  datos_tabla <- data %>%
    filter(!is.na(genero), !is.na(tenure_group), !is.na(nivel_gestion)) %>%
    group_by(tenure_group, genero, nivel_gestion) %>%
    summarise(total = sum(colaboradores_activos), .groups = 'drop') %>%
    pivot_wider(names_from = nivel_gestion, values_from = total, values_fill = 0) %>%
    mutate(Total = ODG + PLAZA + SUCURSAL) %>%
    arrange(tenure_group, genero)
  
  # Ordenar antigüedad
  niveles_antiguedad <- c("< 1 Años", "1-5 Años", "5-9 Años", "10-14 Años",
                          "15-19 Años", "20-24 Años", "> 25 Años")
  datos_tabla$tenure_group <- factor(datos_tabla$tenure_group, levels = niveles_antiguedad)
  datos_tabla <- datos_tabla %>% arrange(tenure_group, genero)
  
  # Renombrar columnas en español y mayúsculas, reemplazando guiones bajos por espacios
  datos_tabla <- datos_tabla %>%
    rename(
      "RANGO ANTIGÜEDAD" = tenure_group,
      "GÉNERO" = genero,
      "TOTAL" = Total
    )
  
  # Crear tabla visual compacta con texto más grande
  tabla_grob <- tableGrob(
    datos_tabla,
    rows = NULL,
    theme = ttheme_default(
      core = list(
        bg_params = list(fill = "#2D2D2D", col = "#404040"),
        fg_params = list(col = "#E8E8E8", fontsize = 16)
      ),
      colhead = list(
        bg_params = list(fill = "#00E5D4", col = "#404040"),
        fg_params = list(col = "white", fontsize = 18, fontface = "bold")
      ),
      rowhead = list(
        bg_params = list(fill = "#3D3D3D", col = "#404040"),
        fg_params = list(col = "#E8E8E8", fontsize = 16)
      )
    )
  )
  
  return(tabla_grob)
}

# 5. NUEVA FUNCIÓN: Crear tabla de rangos y eventos definitorios
crear_tabla_rangos_eventos <- function() {
  # Crear data frame con la información proporcionada
  datos_rangos <- data.frame(
    GENERACIÓN = c("Tradicionalistas", "Boomers - Tempranos", "Boomers - Tardíos",
                   "Gen X - Tempranos", "Gen X - Tardíos",
                   "Millennials - Tempranos", "Millennials - Tardíos",
                   "Gen Z - Tempranos", "Gen Z - Tardíos",
                   "Gen Alfa - Tempranos", "Gen Alfa - Tardíos"),
    RANGO_AÑOS = c("Hasta 1945", "1946-1955", "1956-1964",
                   "1965-1973", "1974-1980",
                   "1981-1988", "1989-1996",
                   "1997-2004", "2005-2012",
                   "2013-2018", "2019-2024"),
    EVENTO_DEFINITORIO = c("Fin 2da Guerra Mundial | Inicio Milagro Mexicano", "Milagro Mexicano | TV en México", "Movimiento del 68 | Olimpiadas del 68",
                           "Crisis 1982 | Mundial 86", "Terremoto 85 | Muro de Berlín (1989)",
                           "Crisis 1994 | TLCAN y EZLN (1994)", "Elecciones 2000 | 11-Sep (2001) | YouTube (2005)",
                           "iPhone (2007) | Crisis 2008 | Crisis Seguridad (2006)", "#MeToo (2017) | Pandemia COVID'19 | Boom TikTok",
                           "Aprendizaje Remoto (2020) | Boom Twitch (2020)", "Masificación IA | ChatGPT (2022)"),
    stringsAsFactors = FALSE
  )
  
  # Renombrar columnas para mejor presentación
  datos_rangos <- datos_rangos %>%
    rename(
      "GENERACIÓN" = GENERACIÓN,
      "RANGO AÑOS" = RANGO_AÑOS,
      "EVENTO DEFINITORIO" = EVENTO_DEFINITORIO
    )
  
  # Crear tabla visual con el mismo estilo
  tabla_grob <- tableGrob(
    datos_rangos,
    rows = NULL,
    theme = ttheme_default(
      core = list(
        bg_params = list(fill = "#2D2D2D", col = "#404040"),
        fg_params = list(col = "#E8E8E8", fontsize = 16) # Tamaño ajustado para la información
      ),
      colhead = list(
        bg_params = list(fill = "#FF5252", col = "#404040"), # Color diferente para distinguir
        fg_params = list(col = "white", fontsize = 18, fontface = "bold")
      ),
      rowhead = list(
        bg_params = list(fill = "#3D3D3D", col = "#404040"),
        fg_params = list(col = "#E8E8E8", fontsize = 16)
      )
    )
  )
  
  return(tabla_grob)
}

# Función para crear títulos con fondo oscuro
crear_titulo <- function(texto) {
  textGrob(texto, 
           gp = gpar(fontsize = 36, fontface = "bold", col = "#F5F5F5"))
}

# Función para crear subtítulos
crear_subtitulo <- function(texto) {
  textGrob(texto, 
           gp = gpar(fontsize = 20, fontface = "bold", col = "#B0B0B0"))
}

# Crear los nuevos gráficos y tablas compactas
cat("Creando nuevos gráficos visuales y tablas con subgeneraciones...\n")
piramide_generacion <- crear_piramide_generacion(datos)
grafico_antiguedad <- crear_grafico_antiguedad(datos)
tabla_piramide <- crear_tabla_piramide_compacta(datos)
tabla_antiguedad <- crear_tabla_antiguedad_compacta(datos)
tabla_rangos_eventos <- crear_tabla_rangos_eventos()

# Crear dashboard completo MODIFICADO - ahora con 4 elementos en la fila de generación
dashboard_completo <- grid.arrange(
  # Título principal (ocupando toda la fila superior)
  crear_titulo("DASHBOARD DE COLABORADORES - RESUMEN GENERAL"),
  
  # Segunda fila: Indicadores principales (ocupando toda la fila)
  arrangeGrob(
    indicador_total, indicador_mujeres, indicador_hombres,
    nrow = 1
  ),
  
  # Tercera fila: CUATRO ELEMENTOS - Gráfico de generación + Tabla rangos + Tabla desglose
  arrangeGrob(
    # Cuadrante 1: Gráfico de generación (40% de ancho)
    piramide_generacion,
    # Cuadrante 2: Tabla de rangos y eventos (30% de ancho)
    tabla_rangos_eventos,
    # Cuadrante 3: Tabla de desglose numérico (30% de ancho)
    tabla_piramide,
    nrow = 1,
    widths = c(4, 3, 3)  # Ajuste de proporciones
  ),
  
  # Cuarta fila: Gráfico de antigüedad y su tabla
  arrangeGrob(
    # Cuadrante 4: Gráfico de antigüedad (2 columnas de ancho)
    grafico_antiguedad,
    # Cuadrante 5: Tabla de antigüedad (1 columna de ancho)
    tabla_antiguedad,
    nrow = 1,
    widths = c(2, 1)  # Gráfico más ancho, tabla más estrecha
  ),
  
  ncol = 1,
  heights = c(0.08, 0.12, 0.45, 0.35),  # Ajustado para nueva distribución
  padding = unit(0.5, "cm")
)

# Guardar imagen en alta resolución para videowall con fondo oscuro
cat("Guardando dashboard completo con tabla de rangos y eventos...\n")
ggsave("C:/Users/racl26345/Documents/Reportes Automatizados/Observatorio Digital/dashboard_colaboradores_con_rangos.png",
       dashboard_completo,
       width = 38.4, height = 21.6, dpi = 100,
       bg = "#1E1E1E")  # Fondo oscuro

# También crear imágenes individuales para cada gráfico
cat("Guardando imágenes individuales con subgeneraciones...\n")

# Pirámide de generación individual
ggsave("C:/Users/racl26345/Documents/Reportes Automatizados/Observatorio Digital/piramide_generacion_subgeneraciones.png",
       piramide_generacion,
       width = 19.2, height = 10, dpi = 100, bg = "#1E1E1E")

# Gráfico de antigüedad individual
ggsave("C:/Users/racl26345/Documents/Reportes Automatizados/Observatorio Digital/grafico_antiguedad_subgeneraciones.png",
       grafico_antiguedad,
       width = 19.2, height = 10, dpi = 100, bg = "#1E1E1E")

# Guardar tabla de rangos individualmente
cat("Guardando tabla de rangos y eventos individual...\n")
grid.newpage()
grid.draw(tabla_rangos_eventos)
ggsave("C:/Users/racl26345/Documents/Reportes Automatizados/Observatorio Digital/tabla_rangos_eventos.png",
       width = 16, height = 10, dpi = 100, bg = "#1E1E1E")

cat("\n=== PROCESO COMPLETADO ===\n")
cat("Total de colaboradores activos:", format(total_activos, big.mark = ","), "\n")
cat("Total de mujeres:", format(total_mujeres, big.mark = ","), "\n")
cat("Total de hombres:", format(total_hombres, big.mark = ","), "\n")
cat("Porcentaje de mujeres:", round(total_mujeres/total_activos*100, 1), "%\n")
cat("Porcentaje de hombres:", round(total_hombres/total_activos*100, 1), "%\n")
cat("Archivos guardados en: C:/Users/racl26345/Documents/Reportes Automatizados/Observatorio Digital/\n")
cat("MEJORAS IMPLEMENTADAS:\n")
cat("1. Nueva clasificación de generaciones con subgeneraciones (Early/Late)\n")
cat("2. Tabla de rangos y eventos definitorios incluida en el dashboard\n")
cat("3. Distribución modificada: Gráfico + Tabla Rangos + Tabla Desglose\n")
cat("4. Estilos consistentes aplicados a todas las tablas\n")
cat("5. Tabla de rangos con color distintivo para fácil identificación\n")