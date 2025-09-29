library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)
library(grid)

# Configurar estilo minimalista para videowall (tamaños originales)
theme_videowall <- function() {
  theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#F0F2F6", color = NA),
      panel.background = element_rect(fill = "#F0F2F6", color = NA),
      panel.grid.major = element_line(color = "#D1D9E6", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      text = element_text(family = "sans-serif", color = "#2E3440"),
      plot.title = element_text(size = 32, face = "bold", hjust = 0.5, color = "#2E3440"),
      plot.subtitle = element_text(size = 24, hjust = 0.5, color = "#4C566A"),
      axis.title = element_text(size = 20, color = "#4C566A"),
      axis.text = element_text(size = 18, color = "#4C566A"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 18),
      strip.text = element_text(size = 16, face = "bold")
    )
}

# Paleta de colores corporativa
colors <- c(
  "F" = "#FF6B6B",    # Rojo coral para mujeres
  "M" = "#4ECDC4",    # Verde azulado para hombres
  "primary" = "#2E5BFF",
  "secondary" = "#00C7BE",
  "accent" = "#FF3B30",
  "success" = "#34C759"
)

# Conectar a la base de datos
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)

# Query corregido
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
    CASE WHEN ca.area_de_cobranza <> 'No Aplica' THEN 'COBRANZA'
        WHEN ca.nombre_posicion = 'COMODIN' THEN 'SUCURSAL'
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
        WHEN ca.fecha_nacimiento <= '1964-12-31' THEN 'Baby Boomers'
        WHEN ca.fecha_nacimiento <= '1980-12-31' THEN 'Generación X'
        WHEN ca.fecha_nacimiento <= '1996-12-31' THEN 'Millennials'
        WHEN ca.fecha_nacimiento <= '2012-12-31' THEN 'Generación Z'
        ELSE 'Generación Alfa' END AS generacion,
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

# Función para crear recuadros de indicadores (tamaños originales)
crear_indicador <- function(valor, titulo, color) {
  ggplot() +
    annotate("rect", xmin = -1, xmax = 1, ymin = -1, ymax = 1, 
             fill = color, alpha = 0.8, color = NA) +
    annotate("text", x = 0, y = 0.3, label = format(valor, big.mark = ","), 
             size = 12, fontface = "bold", color = "white") +
    annotate("text", x = 0, y = -0.3, label = titulo, 
             size = 8, color = "white", fontface = "bold") +
    theme_void() +
    coord_fixed()
}

# Crear recuadros de indicadores
indicador_total <- crear_indicador(total_activos, "TOTAL ACTIVOS", colors["primary"])
indicador_mujeres <- crear_indicador(total_mujeres, "MUJERES", colors["F"])
indicador_hombres <- crear_indicador(total_hombres, "HOMBRES", colors["M"])

# Función para crear tablas estéticas con facetas - AUMENTAR SOLO LOS NÚMEROS EN 20%
crear_tabla_estetica_facetas <- function(data, x_var, y_var) {
  data_summary <- data %>%
    group_by(!!sym(x_var), !!sym(y_var), genero) %>%
    summarise(total = sum(colaboradores_activos), .groups = 'drop') %>%
    group_by(!!sym(x_var), !!sym(y_var)) %>%
    mutate(porcentaje = total / sum(total) * 100) %>%
    ungroup()
  
  # Ordenar factores según especificaciones
  if (x_var == "nivel_gestion") {
    data_summary[[x_var]] <- factor(data_summary[[x_var]], 
                                    levels = c("ODG", "PLAZA", "SUCURSAL", "COBRANZA"))
  } else if (x_var == "regional") {
    regionales_orden <- c("ODG", sort(unique(data_summary$regional[data_summary$regional != "ODG"])))
    data_summary[[x_var]] <- factor(data_summary[[x_var]], levels = regionales_orden)
  }
  
  if (y_var == "generacion") {
    data_summary[[y_var]] <- factor(data_summary[[y_var]],
                                    levels = c("Tradicionalistas", "Baby Boomers", "Generación X", 
                                               "Millennials", "Generación Z", "Generación Alfa"))
  } else if (y_var == "tenure_group") {
    data_summary[[y_var]] <- factor(data_summary[[y_var]],
                                    levels = c("< 1 Años", "1-5 Años", "5-9 Años", "10-14 Años",
                                               "15-19 Años", "20-24 Años", "> 25 Años"))
  }
  
  ggplot(data_summary, aes(x = !!sym(x_var), y = !!sym(y_var), fill = genero)) +
    geom_tile(color = "white", linewidth = 1, alpha = 0.9) +
    # AUMENTAR TAMAÑO DE NÚMEROS EN 20%: de 5 a 6
    geom_text(aes(label = paste0(format(total, big.mark = ","), "\n(", round(porcentaje, 1), "%)")),
              color = "white", size = 7, fontface = "bold", lineheight = 0.8) + # Cambiado de 5 a 6
    scale_fill_manual(values = colors) +
    labs(x = NULL, y = NULL, fill = "Género") +
    facet_wrap(~genero, nrow = 1) +
    theme_videowall() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
}

# Crear las tablas solicitadas con números más grandes
cat("Creando tablas con números aumentados en 20%...\n")
tabla1 <- crear_tabla_estetica_facetas(datos, "nivel_gestion", "generacion") + 
  labs(title = "Distribución por Nivel de Gestión y Generación")

tabla2 <- crear_tabla_estetica_facetas(datos, "regional", "generacion") + 
  labs(title = "Distribución por Regional y Generación")

tabla3 <- crear_tabla_estetica_facetas(datos, "nivel_gestion", "tenure_group") + 
  labs(title = "Distribución por Nivel de Gestión y Antigüedad")

tabla4 <- crear_tabla_estetica_facetas(datos, "regional", "tenure_group") + 
  labs(title = "Distribución por Regional y Antigüedad")

# Crear dashboard completo
dashboard_completo <- grid.arrange(
  # Título principal
  textGrob("DASHBOARD DE COLABORADORES - RESUMEN GENERAL", 
           gp = gpar(fontsize = 36, fontface = "bold", col = "#2E3440")),
  
  # Primera fila: Indicadores principales
  arrangeGrob(
    indicador_total, indicador_mujeres, indicador_hombres,
    nrow = 1
  ),
  
  # Segunda fila: Tablas de generación
  arrangeGrob(
    tabla1,
    tabla2,
    nrow = 1
  ),
  
  # Tercera fila: Tablas de antigüedad
  arrangeGrob(
    tabla3,
    tabla4,
    nrow = 1
  ),
  
  ncol = 1,
  heights = c(0.08, 0.15, 0.38, 0.38),
  padding = unit(1, "cm")
)

# Guardar imagen en alta resolución para videowall
cat("Guardando dashboard completo...\n")
ggsave("C:/Users/racl26345/Documents/Reportes Automatizados/dashboard_colaboradores_4K.png",
       dashboard_completo,
       width = 38.4, height = 21.6, dpi = 100,
       bg = "#F0F2F6")

# También crear imágenes individuales para cada sección
cat("Guardando imágenes individuales...\n")

# Indicadores principales
indicadores_principales <- arrangeGrob(
  textGrob("INDICADORES PRINCIPALES", 
           gp = gpar(fontsize = 28, fontface = "bold", col = "#2E3440")),
  arrangeGrob(indicador_total, indicador_mujeres, indicador_hombres, nrow = 1),
  ncol = 1, heights = c(0.1, 0.9)
)

ggsave("C:/Users/racl26345/Documents/Reportes Automatizados/indicadores_principales.png",
       indicadores_principales,
       width = 38.4, height = 5, dpi = 100, bg = "#F0F2F6")

# Distribución por generación
distribucion_generacion <- arrangeGrob(
  textGrob("DISTRIBUCIÓN POR GENERACIÓN", 
           gp = gpar(fontsize = 28, fontface = "bold", col = "#2E3440")),
  arrangeGrob(tabla1, tabla2, nrow = 1),
  ncol = 1, heights = c(0.1, 0.9)
)

ggsave("C:/Users/racl26345/Documents/Reportes Automatizados/distribucion_generacion.png",
       distribucion_generacion,
       width = 38.4, height = 12, dpi = 100, bg = "#F0F2F6")

# Distribución por antigüedad
distribucion_antiguedad <- arrangeGrob(
  textGrob("DISTRIBUCIÓN POR ANTIGÜEDAD", 
           gp = gpar(fontsize = 28, fontface = "bold", col = "#2E3440")),
  arrangeGrob(tabla3, tabla4, nrow = 1),
  ncol = 1, heights = c(0.1, 0.9)
)

ggsave("C:/Users/racl26345/Documents/Reportes Automatizados/distribucion_antiguedad.png",
       distribucion_antiguedad,
       width = 38.4, height = 12, dpi = 100, bg = "#F0F2F6")

cat("\n=== PROCESO COMPLETADO ===\n")
cat("Total de colaboradores activos:", format(total_activos, big.mark = ","), "\n")
cat("Total de mujeres:", format(total_mujeres, big.mark = ","), "\n")
cat("Total de hombres:", format(total_hombres, big.mark = ","), "\n")
cat("Porcentaje de mujeres:", round(total_mujeres/total_activos*100, 1), "%\n")
cat("Porcentaje de hombres:", round(total_hombres/total_activos*100, 1), "%\n")
cat("Archivos guardados en: C:/Users/racl26345/Documents/Reportes Automatizados/\n")