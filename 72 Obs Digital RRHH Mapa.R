# SOLUCIÓN CON MAPA REAL DE ESTADOS DE MÉXICO
library(RSQLite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthhires) # instalar con: remotes::install_github("ropensci/rnaturalearthhires")

# CONFIGURACIÓN INICIAL
ruta_db <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
ruta_salida <- "C:/Users/racl26345/Documents/Reportes Automatizados/"

# PALETA DE COLORES CORPORATIVA
colores_regionales <- c(
  "CENTRO" = "#1f77b4",
  "NORESTE" = "#ff7f0e", 
  "NORTE" = "#2ca02c",
  "OCCIDENTE" = "#d62728",
  "ODG" = "#9467bd",
  "SUR" = "#8c564b",
  "SURESTE" = "#e377c2"
)

# ESTILO PARA MAPA
theme_mapa_real <- function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill = "#F0F2F6", color = NA),
      panel.background = element_rect(fill = "#F0F2F6", color = NA),
      text = element_text(color = "#2E3440", family = "sans"),
      plot.title = element_text(face = "bold", size = 45, hjust = 0.5, margin = margin(b = 15)), # Original size = 36
      plot.subtitle = element_text(size = 36, hjust = 0.5, margin = margin(b = 20)), # Original size = 24
      legend.text = element_text(size = 20), # Original size = 14
      legend.title = element_text(face = "bold", size = 24), # Original size = 16
      plot.margin = margin(30, 30, 30, 30)
    )
}

# EXTRAER DATOS
con <- dbConnect(RSQLite::SQLite(), ruta_db)

query <- "
WITH colaboradores_activos AS (
    SELECT 
        id_colaborador,
        regional,
        estado,
        municipio
    FROM hist_posiciones
    WHERE fecha_daily = DATE('now', 'start of month', '-1 day')
        AND status = 'A'
        AND vacante = 'False'
),
inegi_unicos AS (
    SELECT 
        estado_rhenueva,
        municipio_rhenueva,
        cve_ent,
        cve_mun,
        lat_decimal,
        lon_decimal
    FROM inegi_stds_mncps
    GROUP BY estado_rhenueva, municipio_rhenueva
)
SELECT
    ca.regional,
    iu.cve_ent,
    iu.cve_mun,
    iu.lat_decimal,
    iu.lon_decimal,
    COUNT(DISTINCT ca.id_colaborador) AS colaboradores_activos
FROM colaboradores_activos ca
LEFT JOIN inegi_unicos iu
    ON (ca.estado || '_' || ca.municipio) = (iu.estado_rhenueva || '_' || iu.municipio_rhenueva)
WHERE iu.cve_ent IS NOT NULL AND iu.cve_mun IS NOT NULL
    AND iu.lat_decimal IS NOT NULL AND iu.lon_decimal IS NOT NULL
GROUP BY ca.regional, iu.cve_ent, iu.cve_mun, iu.lat_decimal, iu.lon_decimal
"

datos <- dbGetQuery(con, query)
dbDisconnect(con)

# PREPARAR DATOS
datos <- datos %>%
  mutate(
    cve_ent = str_pad(as.character(cve_ent), 2, "left", "0"),
    cve_mun = str_pad(as.character(cve_mun), 3, "left", "0"),
    CVEGEO = paste0(cve_ent, cve_mun),
    lat = as.numeric(lat_decimal),
    lon = as.numeric(lon_decimal)
  ) %>%
  filter(!is.na(lat) & !is.na(lon))

print(paste("Municipios con coordenadas:", nrow(datos)))

# CREAR PUNTOS PARA LOS MUNICIPIOS
puntos_municipios <- st_as_sf(datos, coords = c("lon", "lat"), crs = 4326)

# IDENTIFICAR LEÓN
leon_gto <- puntos_municipios %>% filter(CVEGEO == "11020")

# === DESCARGAR MAPA DE ESTADOS DE MÉXICO ===
print("Cargando shapefile de estados de México...")
estados_mexico <- ne_states(country = "Mexico", returnclass = "sf")
print(paste("Estados de México cargados:", nrow(estados_mexico)))

# CREAR EL MAPA CON CONTORNOS DE ESTADOS
mapa_con_estados <- ggplot() +
  geom_sf(data = estados_mexico, 
          fill = "#F8F9FA", 
          color = "#B0BEC5", 
          size = 0.6,
          alpha = 0.8) +
  geom_sf(data = puntos_municipios, 
          aes(color = regional, size = colaboradores_activos),
          alpha = 0.8, shape = 19, stroke = 0.5) +
  geom_sf(data = leon_gto, 
          color = "#FFD700", size = 8, shape = 1, stroke = 2.5, alpha = 0.9) +
  geom_sf_text(data = leon_gto,
               aes(label = "CORPORATIVO\nLEÓN"),
               size = 4.5, fontface = "bold", color = "#2E3440",
               lineheight = 0.8, nudge_y = 0.5) +
  scale_color_manual(values = colores_regionales, name = "Regional") +
  scale_size_continuous(
    range = c(2, 10),
    name = "Colaboradores",
    breaks = c(1, 10, 50, 100),
    labels = c("1-9", "10-49", "50-99", "100+")
  ) +
  theme_mapa_real() +
  labs(
    title = "PRESENCIA DE COLABORADORES EN MÉXICO",
    subtitle = "Distribución Municipal por Regional"
  ) +
  coord_sf(xlim = c(-118, -86), ylim = c(14, 33), expand = FALSE) +
  theme(legend.position = "right", legend.box = "vertical")

# PANEL DE ESTADÍSTICAS
resumen_regional <- datos %>%
  group_by(regional) %>%
  summarise(
    total_colaboradores = sum(colaboradores_activos),
    total_municipios = n_distinct(CVEGEO),
    .groups = 'drop'
  ) %>%
  mutate(
    porcentaje = round(total_colaboradores / sum(total_colaboradores) * 100, 1),
    etiqueta = sprintf("%s\n%s colaboradores\n%s municipios",
                       regional,
                       format(total_colaboradores, big.mark = ","),
                       total_municipios)
  )

panel_estadisticas <- ggplot(resumen_regional, aes(x = 1, y = reorder(regional, total_colaboradores))) +
  geom_tile(aes(fill = regional), width = 0.25, height = 0.7, alpha = 0.9) +
  geom_text(aes(label = etiqueta), hjust = 0, nudge_x = 0.3, size = 11,# Original 4.5
            lineheight = 0.8, color = "#2E3440", fontface = "bold") +
  scale_fill_manual(values = colores_regionales) +
  scale_x_continuous(limits = c(1, 8)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F0F2F6", color = NA),
    plot.title = element_text(face = "bold", size = 40, hjust = 0.5, margin = margin(b = 10)), # Original size = 18
    legend.text = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 26, face = "bold")
  ) +
  labs(title = "RESUMEN POR REGIONAL")

# COMBINAR MAPA Y PANEL
layout_final <- mapa_con_estados + panel_estadisticas + plot_layout(widths = c(2, 1))

# GUARDAR MAPA
ggsave(
  filename = paste0(ruta_salida, "mapa_estados_mexico_completo.png"),
  plot = layout_final,
  width = 3840/100,
  height = 2160/100,
  dpi = 300,
  bg = "#F0F2F6"
)

print("✅ MAPA CON ESTADOS CREADO EXITOSAMENTE!")
print(paste("📍 Municipios mapeados:", nrow(datos)))
print(paste("👥 Total colaboradores:", sum(resumen_regional$total_colaboradores)))



####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################
####################################################################################################################################












# ==============================
# MAPA MUNICIPAL (MGI 2024) POR REGIONAL + PUNTOS DE PRESENCIA
# Lee automáticamente 00mun.shp dentro de la carpeta Shapes (subcarpetas incluidas)
# ==============================

# Paquetes
library(RSQLite)
library(dplyr)
library(stringr)
library(sf)
library(ggplot2)
library(patchwork)
library(rnaturalearth)
library(scales) # alpha()
# library(rnaturalearthhires) # si lo usas localmente

# -----------------------------
# CONFIGURACIÓN (ajusta si quieres)
# -----------------------------
ruta_db <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
ruta_salida <- "C:/Users/racl26345/Documents/Reportes Automatizados/"
ruta_shapes_base <- "C:/Users/racl26345/Documents/Shapes"  # carpeta donde descomprimiste el ZIP del MGI 2024
pattern_shp <- "00mun.*\\.shp$"  # patrón esperado del shapefile municipal en MGI 2024

# control de performance: si el shapefile es muy grande, puedes simplificar las geometrías
usar_simplify <- TRUE
simplify_tol <- 0.001  # aumenta si quieres más simplificación (en grados) -> menor detalle y más velocidad

# Colores (intensos para puntos; fills con alpha suave)
colores_regionales <- c(
  "CENTRO"   = "#1f77b4",
  "NORESTE"  = "#ff7f0e",
  "NORTE"    = "#2ca02c",
  "OCCIDENTE"= "#d62728",
  "ODG"      = "#9467bd",
  "SUR"      = "#8c564b",
  "SURESTE"  = "#e377c2"
)
colores_fill <- alpha(colores_regionales, 0.45)

# Tema del mapa (lo puedes ajustar)
theme_mapa_real <- function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill = "#F0F2F6", color = NA),
      panel.background = element_rect(fill = "#F0F2F6", color = NA),
      text = element_text(color = "#2E3440", family = "sans"),
      plot.title = element_text(face = "bold", size = 40, hjust = 0.5, margin = margin(b = 12)),
      plot.subtitle = element_text(size = 28, hjust = 0.5, margin = margin(b = 10)),
      legend.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 16),
      plot.margin = margin(20, 20, 20, 20)
    )
}

# -----------------------------
# 1) Buscar shapefile municipal en la carpeta Shapes (recursivo)
# -----------------------------
shp_files <- list.files(path = ruta_shapes_base, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)

# tratar de encontrar "00mun" (nombre esperado en MGI 2024) u otros que contengan "mun"
shp_target <- shp_files[grepl(pattern_shp, basename(shp_files), ignore.case = TRUE)]
if (length(shp_target) == 0) {
  # fallback: cualquier shp que contenga "mun" en su nombre
  shp_target <- shp_files[grepl("mun", basename(shp_files), ignore.case = TRUE)]
}
if (length(shp_target) == 0) {
  stop("No encontré ningún shapefile municipal en la carpeta Shapes. Verifica que descomprimiste el MGI2024 y que existen archivos .shp en subcarpetas (ej. 'conjunto_de_datos').")
}
# si hay varios resultados, prioriza el que tenga '00mun' en el nombre, sino toma el primero
shp_target <- shp_target[order(!grepl("00mun", basename(shp_target), ignore.case = TRUE))]  # pone los que sí tienen 00mun primero
shp_path <- shp_target[1]
message("Shapefile municipal detectado: ", shp_path)

# verificar que existan los archivos auxiliares (.dbf y .shx)
base_no_ext <- tools::file_path_sans_ext(shp_path)
if (!file.exists(paste0(base_no_ext, ".dbf")) || !file.exists(paste0(base_no_ext, ".shx"))) {
  stop("Faltan archivos auxiliares del shapefile (.dbf o .shx). Asegúrate de copiar todos los archivos del .shp (.shx, .dbf, .prj, etc.) en la misma carpeta.")
}

# -----------------------------
# 2) Extraer datos desde la base (tu consulta original)
# -----------------------------
con <- dbConnect(RSQLite::SQLite(), ruta_db)

query <- "
WITH colaboradores_activos AS (
    SELECT 
        id_colaborador,
        regional,
        estado,
        municipio
    FROM hist_posiciones
    WHERE fecha_daily = DATE('now', 'start of month', '-1 day')
        AND status = 'A'
        AND vacante = 'False'
),
inegi_unicos AS (
    SELECT 
        estado_rhenueva,
        municipio_rhenueva,
        cve_ent,
        cve_mun,
        lat_decimal,
        lon_decimal
    FROM inegi_stds_mncps
    GROUP BY estado_rhenueva, municipio_rhenueva
)
SELECT
    ca.regional,
    iu.cve_ent,
    iu.cve_mun,
    iu.lat_decimal,
    iu.lon_decimal,
    COUNT(DISTINCT ca.id_colaborador) AS colaboradores_activos
FROM colaboradores_activos ca
LEFT JOIN inegi_unicos iu
    ON (ca.estado || '_' || ca.municipio) = (iu.estado_rhenueva || '_' || iu.municipio_rhenueva)
WHERE iu.cve_ent IS NOT NULL AND iu.cve_mun IS NOT NULL
    AND iu.lat_decimal IS NOT NULL AND iu.lon_decimal IS NOT NULL
GROUP BY ca.regional, iu.cve_ent, iu.cve_mun, iu.lat_decimal, iu.lon_decimal
"

datos <- dbGetQuery(con, query)
dbDisconnect(con)

# preparar datos: claves y coordenadas
datos <- datos %>%
  mutate(
    cve_ent = str_pad(as.character(cve_ent), 2, "left", "0"),
    cve_mun = str_pad(as.character(cve_mun), 3, "left", "0"),
    CVEGEO = paste0(cve_ent, cve_mun),
    lat = as.numeric(lat_decimal),
    lon = as.numeric(lon_decimal)
  ) %>%
  filter(!is.na(lat) & !is.na(lon))

message("Filas en 'datos' (municipios con presencia y coordenadas): ", nrow(datos))

# convertir puntos a sf
puntos_municipios <- st_as_sf(datos, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# detectar si existe León (ejemplo CVEGEO "11020")
leon_gto <- puntos_municipios %>% filter(CVEGEO == "11020")

# -----------------------------
# 3) Leer shapefile municipal y detectar campos (CVE_ENT, CVE_MUN, CVEGEO)
# -----------------------------
municipios_sf <- st_read(shp_path, quiet = TRUE)
message("Shapefile leído. Columnas detectadas: ", paste(names(municipios_sf)[1: min(20, length(names(municipios_sf)))], collapse = ", "))

# detectar nombres de columnas posibles (case-insensitive)
cols_lower <- tolower(names(municipios_sf))

found_cvegeo <- names(municipios_sf)[cols_lower %in% "cvegeo" | grepl("^cvegeo$", cols_lower)]
found_cve_ent <- names(municipios_sf)[grepl("cve.*ent|cve_ent|cveent|clave_ent", cols_lower)]
found_cve_mun <- names(municipios_sf)[grepl("cve.*mun|cve_mun|cvemun|clave_mun", cols_lower)]

# si no los encontró exactamente, intentar buscar por substring "ent" y "mun"
if (length(found_cve_ent) == 0) found_cve_ent <- names(municipios_sf)[grepl("ent", cols_lower) & grepl("cve|clave|id", cols_lower)]
if (length(found_cve_mun) == 0) found_cve_mun <- names(municipios_sf)[grepl("mun", cols_lower) & grepl("cve|clave|id", cols_lower)]

# fallback: si aparece "cve_geo" u otro
if (length(found_cvegeo) == 0) found_cvegeo <- names(municipios_sf)[grepl("cve_?geo|cvegeo|cvegeo$", cols_lower)]

# validar y crear CVEGEO
if (length(found_cvegeo) >= 1) {
  # toma el primer campo que parezca CVEGEO
  name_cvegeo <- found_cvegeo[1]
  municipios_sf <- municipios_sf %>% mutate(CVEGEO = as.character(.data[[name_cvegeo]]))
  message("Usando campo '", name_cvegeo, "' como CVEGEO.")
} else if (length(found_cve_ent) >= 1 && length(found_cve_mun) >= 1) {
  name_ent <- found_cve_ent[1]
  name_mun <- found_cve_mun[1]
  municipios_sf <- municipios_sf %>%
    mutate(
      CVE_ENT = str_pad(as.character(.data[[name_ent]]), 2, "left", "0"),
      CVE_MUN = str_pad(as.character(.data[[name_mun]]), 3, "left", "0"),
      CVEGEO = paste0(CVE_ENT, CVE_MUN)
    )
  message("Creada CVEGEO desde '", name_ent, "' y '", name_mun, "'.")
} else {
  # como último recurso, mostrar las columnas para que identifiques en caso extremo
  stop("No pude identificar campos CVE_ENT/CVE_MUN/CVEGEO en el shapefile. Columnas encontradas: ", paste(names(municipios_sf), collapse = ", "),
       "\nRevisa el shapefile y confirma el nombre del campo correspondiente a la clave de entidad/municipio.")
}

# Asegurar CRS 4326 para compatibilidad con puntos
municipios_sf <- st_transform(municipios_sf, 4326)

# opcional: simplificar geometrías para plotting (acelerar si es muy grande)
if (usar_simplify) {
  message("Simplificando geometrías para acelerar el render (tolerance = ", simplify_tol, ") ...")
  # usar st_simplify sólo en geometría
  municipios_sf$geometry <- st_simplify(municipios_sf$geometry, dTolerance = simplify_tol)
}

# -----------------------------
# 4) Unir datos (datos tiene CVEGEO)
# -----------------------------
municipios_datos <- municipios_sf %>%
  left_join(datos %>% select(CVEGEO, regional, colaboradores_activos), by = "CVEGEO")

num_unidos <- sum(!is.na(municipios_datos$regional))
message("Municipios del shapefile que quedaron con 'regional' asignada: ", num_unidos, " (filas de shapefile con presencia)")

# -----------------------------
# 5) Cargar contornos de estados para referencia
# -----------------------------
estados_mexico <- ne_states(country = "Mexico", returnclass = "sf")
estados_mexico <- st_transform(estados_mexico, 4326)

# -----------------------------
# 6) Armar el mapa: municipios fill suave + puntos intensos
# -----------------------------
mapa_con_municipios <- ggplot() +
  geom_sf(data = municipios_datos,
          aes(fill = regional),
          color = "#B0BEC5", size = 0.12, show.legend = FALSE) + # show.legend FALSE para no repetir en fill; leyenda la mostraremos con puntos
  geom_sf(data = estados_mexico, fill = NA, color = "#2E3440", size = 0.35, alpha = 0.6) +
  geom_sf(data = puntos_municipios,
          aes(color = regional, size = colaboradores_activos),
          alpha = 0.95, shape = 19, stroke = 0.3) +
  # destacar León si existe
  { if (nrow(leon_gto) > 0) geom_sf(data = leon_gto, color = "#FFD700", size = 7, shape = 1, stroke = 2.2, inherit.aes = FALSE) } +
  { if (nrow(leon_gto) > 0) geom_sf_text(data = leon_gto, aes(label = "CORPORATIVO\nLEÓN"), size = 5.5, fontface = "bold", color = "#2E3440", nudge_y = 0.4) } +
  scale_fill_manual(values = colores_fill, na.value = "#F8F9FA", name = "Regional (municipio)") +
  scale_color_manual(values = colores_regionales, na.value = "#B0BEC5", name = "Regional (puntos)") +
  scale_size_continuous(range = c(2, 10),
                        name = "Colaboradores",
                        breaks = c(1, 10, 50, 100),
                        labels = c("1-9", "10-49", "50-99", "100+")) +
  theme_mapa_real() +
  labs(
    title = "PRESENCIA DE COLABORADORES EN MÉXICO",
    subtitle = "Municipios pintados por Regional (fill suave) y ubicaciones puntuales"
  ) +
  coord_sf(xlim = c(-118, -86), ylim = c(14, 33), expand = FALSE) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

# -----------------------------
# 7) Panel lateral de estadísticas (igual que tu script original)
# -----------------------------
resumen_regional <- datos %>%
  group_by(regional) %>%
  summarise(
    total_colaboradores = sum(colaboradores_activos),
    total_municipios = n_distinct(CVEGEO),
    .groups = 'drop'
  ) %>%
  mutate(
    porcentaje = round(total_colaboradores / sum(total_colaboradores) * 100, 1),
    etiqueta = sprintf("%s\n%s colaboradores\n%s municipios",
                       regional,
                       format(total_colaboradores, big.mark = ","),
                       total_municipios)
  )

panel_estadisticas <- ggplot(resumen_regional, aes(x = 1, y = reorder(regional, total_colaboradores))) +
  geom_tile(aes(fill = regional), width = 0.25, height = 0.7, alpha = 0.95) +
  geom_text(aes(label = etiqueta), hjust = 0, nudge_x = 0.3, size = 9,
            lineheight = 0.8, color = "#2E3440", fontface = "bold") +
  scale_fill_manual(values = colores_regionales) +
  scale_x_continuous(limits = c(1, 8)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F0F2F6", color = NA),
    plot.title = element_text(face = "bold", size = 34, hjust = 0.5, margin = margin(b = 10)),
    legend.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 16, face = "bold")
  ) +
  labs(title = "RESUMEN POR REGIONAL")

# -----------------------------
# 8) Combinar y guardar (alta resolución para videowall)
# -----------------------------
layout_final <- mapa_con_municipios + panel_estadisticas + plot_layout(widths = c(2, 1))

output_file <- file.path(ruta_salida, "mapa_municipios_por_regional.png")
ggsave(
  filename = output_file,
  plot = layout_final,
  width = 3840/100,   # ancho en pulgadas (ajusta si quieres otra resolución)
  height = 2160/100,  # alto
  dpi = 300,
  bg = "#F0F2F6",
  limitsize = FALSE
)

message("✅ MAPA creado: ", output_file)
message("📍 Municipios con presencia (filas en 'datos'): ", nrow(datos))
message("👥 Total colaboradores (sum): ", sum(resumen_regional$total_colaboradores))
