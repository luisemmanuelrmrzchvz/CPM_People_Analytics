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
# MAPA DE PRESENCIA MUNICIPAL - COOPERATIVA
# Ajustado: pinta municipios por regional + puntos de ubicaciones
# ==============================

library(RSQLite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(patchwork)
library(rnaturalearth)
library(rnaturalearthhires) # remotes::install_github("ropensci/rnaturalearthhires")
library(scales) # para alpha()

# -----------------------------
# CONFIGURACIÓN
# -----------------------------
ruta_db <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
ruta_salida <- "C:/Users/racl26345/Documents/Reportes Automatizados/"
ruta_shp_municipios <- "C:/Users/racl26345/Documents/Shapes/mg_municipios.shp" # ajusta nombre si aplica

# PALETA CORPORATIVA (colores intensos para puntos; usaremos versiones con alpha para fills)
colores_regionales <- c(
  "CENTRO"   = "#1f77b4",
  "NORESTE"  = "#ff7f0e", 
  "NORTE"    = "#2ca02c",
  "OCCIDENTE"= "#d62728",
  "ODG"      = "#9467bd",
  "SUR"      = "#8c564b",
  "SURESTE"  = "#e377c2"
)
# versiones suaves para el fill de municipios (alpha)
colores_fill <- alpha(colores_regionales, 0.45)

# ESTILO MAPA
theme_mapa_real <- function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill = "#F0F2F6", color = NA),
      panel.background = element_rect(fill = "#F0F2F6", color = NA),
      text = element_text(color = "#2E3440", family = "sans"),
      plot.title = element_text(face = "bold", size = 45, hjust = 0.5, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 36, hjust = 0.5, margin = margin(b = 20)),
      legend.text = element_text(size = 18),
      legend.title = element_text(face = "bold", size = 22),
      plot.margin = margin(30, 30, 30, 30)
    )
}

# -----------------------------
# EXTRAER DATOS (tu consulta original)
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

print(paste("Municipios con presencia (filas de 'datos'):", nrow(datos)))

# CREAR PUNTOS (oficinas/ubicaciones)
puntos_municipios <- st_as_sf(datos, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

# identificar León (ejemplo tu CVEGEO original "11020")
leon_gto <- puntos_municipios %>% filter(CVEGEO == "11020")

# -----------------------------
# CARGAR SHAPEFILE MUNICIPIOS (Marco Geoestadístico INEGI 2024)
# -----------------------------
message("Cargando shapefile de municipios (INEGI 2024)...")
municipios_sf <- st_read(ruta_shp_municipios, quiet = TRUE)

# normalizar nombres de columnas (distintos shapefiles traen distintos nombres)
cols_lower <- tolower(names(municipios_sf))

# si ya hay CVEGEO, usarlo; si no, crear a partir de cve_ent/cve_mun (variantes en mayúsculas/minúsculas)
if ("CVEGEO" %in% names(municipios_sf)) {
  municipios_sf <- municipios_sf %>%
    mutate(CVEGEO = as.character(CVEGEO))
} else if (any(grepl("^cve_ent$|^cveent$", cols_lower)) & any(grepl("^cve_mun$|^cvemun$|^cve_mun$|^cvemun$", cols_lower))) {
  # encontrar nombres exactos (case-insensitive)
  name_cve_ent <- names(municipios_sf)[grepl("^cve_ent$|^cveent$", cols_lower)]
  name_cve_mun <- names(municipios_sf)[grepl("^cve_mun$|^cvemun$|^cve_mun$|^cvemun$", cols_lower)]
  municipios_sf <- municipios_sf %>%
    mutate(
      CVE_ENT = str_pad(as.character(.data[[name_cve_ent]]), 2, "left", "0"),
      CVE_MUN = str_pad(as.character(.data[[name_cve_mun]]), 3, "left", "0"),
      CVEGEO = paste0(CVE_ENT, CVE_MUN)
    )
} else if (any(grepl("^cvegeo$|^cvgeo$|^cve_geo$|^cvegeo$", cols_lower))) {
  # fallback por si el campo aparece en minúsculas con otro nombre
  name_cvegeo <- names(municipios_sf)[grepl("^cvegeo$|^cvgeo$|^cve_geo$|^cvegeo$", cols_lower)]
  municipios_sf <- municipios_sf %>%
    mutate(CVEGEO = as.character(.data[[name_cvegeo]]))
} else {
  stop("No pude encontrar campos CVE_ENT/CVE_MUN/CVEGEO en el shapefile. Revisa la estructura del shapefile.")
}

# Asegurar CRS 4326 (lon/lat)
municipios_sf <- st_transform(municipios_sf, 4326)

# -----------------------------
# UNIR DATOS (por CVEGEO)
# -----------------------------
municipios_datos <- municipios_sf %>%
  left_join(datos %>% select(CVEGEO, regional, colaboradores_activos), by = "CVEGEO")

# cuantos municipios con presencia quedaron unidos?
num_mun_presencia <- sum(!is.na(municipios_datos$regional))
message(paste("Municipios con presencia unidos al shapefile:", num_mun_presencia))

# -----------------------------
# CARGAR SHAPEFILE ESTADOS (contornos)
# -----------------------------
estados_mexico <- ne_states(country = "Mexico", returnclass = "sf")
estados_mexico <- st_transform(estados_mexico, 4326)

# -----------------------------
# ARMAR MAPA: municipios (fill suave) + puntos (color intenso y tamaño)
# -----------------------------
mapa_con_municipios <- ggplot() +
  # municipios coloreados (fill suave)
  geom_sf(data = municipios_datos,
          aes(geometry = geometry, fill = regional),
          color = "#B0BEC5", size = 0.15, alpha = 0.7, show.legend = TRUE) +
  # contorno de estados encima
  geom_sf(data = estados_mexico, fill = NA, color = "#2E3440", size = 0.35, alpha = 0.6) +
  # puntos de presencia (más intensos)
  geom_sf(data = puntos_municipios,
          aes(color = regional, size = colaboradores_activos),
          alpha = 0.95, shape = 19, stroke = 0.4, show.legend = TRUE) +
  # destacar León con borde y etiqueta (si existe)
  { if (nrow(leon_gto) > 0) geom_sf(data = leon_gto, color = "#FFD700", size = 8, shape = 1, stroke = 2.5, alpha = 0.9) } +
  { if (nrow(leon_gto) > 0) geom_sf_text(data = leon_gto,
                                          aes(label = "CORPORATIVO\nLEÓN"),
                                          size = 6.5, fontface = "bold", color = "#2E3440",
                                          lineheight = 0.8, nudge_y = 0.5) } +
  # escalas: fill suave y color intenso (compartiendo paleta)
  scale_fill_manual(values = colores_fill, na.value = "#F8F9FA", name = "Regional (municipio)") +
  scale_color_manual(values = colores_regionales, na.value = "#B0BEC5", name = "Regional (puntos)") +
  scale_size_continuous(range = c(2, 10),
                        name = "Colaboradores",
                        breaks = c(1, 10, 50, 100),
                        labels = c("1-9", "10-49", "50-99", "100+")) +
  theme_mapa_real() +
  labs(
    title = "PRESENCIA DE COLABORADORES EN MÉXICO",
    subtitle = "Municipios coloreados por Regional (fill suave) y ubicaciones puntuales"
  ) +
  coord_sf(xlim = c(-118, -86), ylim = c(14, 33), expand = FALSE) +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    legend.background = element_rect(fill = "#F0F2F6", color = NA)
  )

# -----------------------------
# PANEL ESTADÍSTICAS (igual que antes)
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
  geom_text(aes(label = etiqueta), hjust = 0, nudge_x = 0.3, size = 10,
            lineheight = 0.8, color = "#2E3440", fontface = "bold") +
  scale_fill_manual(values = colores_regionales) +
  scale_x_continuous(limits = c(1, 8)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F0F2F6", color = NA),
    plot.title = element_text(face = "bold", size = 36, hjust = 0.5, margin = margin(b = 10)),
    legend.text = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 20, face = "bold")
  ) +
  labs(title = "RESUMEN POR REGIONAL")

# -----------------------------
# COMBINAR Y GUARDAR
# -----------------------------
layout_final <- mapa_con_municipios + panel_estadisticas + plot_layout(widths = c(2, 1))

# guardar en alta resolución para videowall
ggsave(
  filename = file.path(ruta_salida, "mapa_municipios_por_regional.png"),
  plot = layout_final,
  width = 3840/100,  # ajusta si quieres otra resolución
  height = 2160/100,
  dpi = 300,
  bg = "#F0F2F6"
)

message("✅ MAPA CON MUNICIPIOS (fill suave) Y PUNTOS CREADO!")
message(paste("📍 Municipios mapeados (filas en 'datos'):", nrow(datos)))
message(paste("👥 Total colaboradores (sum):", sum(resumen_regional$total_colaboradores)))
