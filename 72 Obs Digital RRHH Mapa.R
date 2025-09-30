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

# --- CONFIGURACIÓN INICIAL ---
ruta_db <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
ruta_salida <- "C:/Users/racl26345/Documents/Reportes Automatizados/"
ruta_shp_municipios <- "C:/Users/racl26345/Documents/Shapes/inegi_municipios.shp" # Ajustar ruta

# --- PALETA DE COLORES CORPORATIVA ---
colores_regionales <- c(
  "CENTRO"   = "#1f77b4",
  "NORESTE"  = "#ff7f0e", 
  "NORTE"    = "#2ca02c",
  "OCCIDENTE"= "#d62728",
  "ODG"      = "#9467bd",
  "SUR"      = "#8c564b",
  "SURESTE"  = "#e377c2"
)

# --- ESTILO PARA MAPA ---
theme_mapa_real <- function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill = "#F0F2F6", color = NA),
      panel.background = element_rect(fill = "#F0F2F6", color = NA),
      text = element_text(color = "#2E3440", family = "sans"),
      plot.title = element_text(face = "bold", size = 45, hjust = 0.5, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 36, hjust = 0.5, margin = margin(b = 20)),
      legend.text = element_text(size = 20),
      legend.title = element_text(face = "bold", size = 24),
      plot.margin = margin(30, 30, 30, 30)
    )
}

# --- EXTRAER DATOS DESDE SQLITE ---
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
    COUNT(DISTINCT ca.id_colaborador) AS colaboradores_activos
FROM colaboradores_activos ca
LEFT JOIN inegi_unicos iu
    ON (ca.estado || '_' || ca.municipio) = (iu.estado_rhenueva || '_' || iu.municipio_rhenueva)
WHERE iu.cve_ent IS NOT NULL AND iu.cve_mun IS NOT NULL
GROUP BY ca.regional, iu.cve_ent, iu.cve_mun
"

datos <- dbGetQuery(con, query)
dbDisconnect(con)

# --- PREPARAR DATOS ---
datos <- datos %>%
  mutate(
    cve_ent = str_pad(as.character(cve_ent), 2, "left", "0"),
    cve_mun = str_pad(as.character(cve_mun), 3, "left", "0"),
    CVEGEO = paste0(cve_ent, cve_mun)
  )

print(paste("Municipios con presencia:", nrow(datos)))

# --- CARGAR SHAPEFILE MUNICIPIOS ---
print("Cargando shapefile de municipios de México...")
municipios_mexico <- st_read(ruta_shp_municipios, quiet = TRUE)

# Algunos shapefiles traen CVEGEO ya listo, otros lo traen como CVE_ENT + CVE_MUN
if(!"CVEGEO" %in% names(municipios_mexico)) {
  municipios_mexico <- municipios_mexico %>%
    mutate(
      CVE_ENT = str_pad(as.character(CVE_ENT), 2, "left", "0"),
      CVE_MUN = str_pad(as.character(CVE_MUN), 3, "left", "0"),
      CVEGEO = paste0(CVE_ENT, CVE_MUN)
    )
}

# --- UNIR DATOS DE COLABORADORES CON MUNICIPIOS ---
municipios_datos <- municipios_mexico %>%
  left_join(datos, by = "CVEGEO")

# --- CARGAR SHAPEFILE DE ESTADOS PARA CONTORNOS ---
estados_mexico <- ne_states(country = "Mexico", returnclass = "sf")

# --- CREAR MAPA MUNICIPAL ---
mapa_con_municipios <- ggplot() +
  geom_sf(data = municipios_datos, 
          aes(fill = regional),
          color = "#B0BEC5", size = 0.1, alpha = 0.9) +
  geom_sf(data = estados_mexico, 
          fill = NA, color = "#2E3440", size = 0.4) +
  scale_fill_manual(values = colores_regionales, na.value = "#F8F9FA", name = "Regional") +
  theme_mapa_real() +
  labs(
    title = "PRESENCIA DE COLABORADORES EN MÉXICO",
    subtitle = "Municipios pintados por Regional"
  ) +
  coord_sf(xlim = c(-118, -86), ylim = c(14, 33), expand = FALSE) +
  theme(legend.position = "right", legend.box = "vertical")

# --- PANEL DE ESTADÍSTICAS ---
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
  geom_text(aes(label = etiqueta), hjust = 0, nudge_x = 0.3, size = 11,
            lineheight = 0.8, color = "#2E3440", fontface = "bold") +
  scale_fill_manual(values = colores_regionales) +
  scale_x_continuous(limits = c(1, 8)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F0F2F6", color = NA),
    plot.title = element_text(face = "bold", size = 40, hjust = 0.5, margin = margin(b = 10)),
    legend.text = element_text(size = 24, face = "bold"),
    legend.title = element_text(size = 26, face = "bold")
  ) +
  labs(title = "RESUMEN POR REGIONAL")

# --- COMBINAR MAPA Y PANEL ---
layout_final <- mapa_con_municipios + panel_estadisticas + plot_layout(widths = c(2, 1))

# --- GUARDAR MAPA FINAL ---
ggsave(
  filename = paste0(ruta_salida, "mapa_municipios_mexico.png"),
  plot = layout_final,
  width = 3840/100,
  height = 2160/100,
  dpi = 300,
  bg = "#F0F2F6"
)

print("✅ MAPA CON MUNICIPIOS CREADO EXITOSAMENTE!")
print(paste("📍 Municipios mapeados:", nrow(datos)))
print(paste("👥 Total colaboradores:", sum(resumen_regional$total_colaboradores)))




#####################################################################################


> # ==============================
> # MAPA DE PRESENCIA MUNICIPAL - COOPERATIVA
  > # ==============================
> 
  > library(RSQLite)
> library(ggplot2)
> library(dplyr)
> library(tidyr)
> library(stringr)
> library(sf)
> library(patchwork)
> library(rnaturalearth)
> library(rnaturalearthhires) # remotes::install_github("ropensci/rnaturalearthhires")
> 
  > # --- CONFIGURACIÓN INICIAL ---
  > ruta_db <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
> ruta_salida <- "C:/Users/racl26345/Documents/Reportes Automatizados/"
> ruta_shp_municipios <- "C:/Users/racl26345/Documents/Shapes/inegi_municipios.shp" # Ajustar ruta
> 
  > # --- PALETA DE COLORES CORPORATIVA ---
  > colores_regionales <- c(
    +   "CENTRO"   = "#1f77b4",
    +   "NORESTE"  = "#ff7f0e", 
    +   "NORTE"    = "#2ca02c",
    +   "OCCIDENTE"= "#d62728",
    +   "ODG"      = "#9467bd",
    +   "SUR"      = "#8c564b",
    +   "SURESTE"  = "#e377c2"
    + )
> 
  > # --- ESTILO PARA MAPA ---
  > theme_mapa_real <- function() {
    +   theme_void() +
      +     theme(
        +       plot.background = element_rect(fill = "#F0F2F6", color = NA),
        +       panel.background = element_rect(fill = "#F0F2F6", color = NA),
        +       text = element_text(color = "#2E3440", family = "sans"),
        +       plot.title = element_text(face = "bold", size = 45, hjust = 0.5, margin = margin(b = 15)),
        +       plot.subtitle = element_text(size = 36, hjust = 0.5, margin = margin(b = 20)),
        +       legend.text = element_text(size = 20),
        +       legend.title = element_text(face = "bold", size = 24),
        +       plot.margin = margin(30, 30, 30, 30)
        +     )
    + }
> 
  > # --- EXTRAER DATOS DESDE SQLITE ---
  > con <- dbConnect(RSQLite::SQLite(), ruta_db)
> 
  > query <- "
+ WITH colaboradores_activos AS (
+     SELECT 
+         id_colaborador,
+         regional,
+         estado,
+         municipio
+     FROM hist_posiciones
+     WHERE fecha_daily = DATE('now', 'start of month', '-1 day')
+         AND status = 'A'
+         AND vacante = 'False'
+ ),
+ inegi_unicos AS (
+     SELECT 
+         estado_rhenueva,
+         municipio_rhenueva,
+         cve_ent,
+         cve_mun,
+         lat_decimal,
+         lon_decimal
+     FROM inegi_stds_mncps
+     GROUP BY estado_rhenueva, municipio_rhenueva
+ )
+ SELECT
+     ca.regional,
+     iu.cve_ent,
+     iu.cve_mun,
+     COUNT(DISTINCT ca.id_colaborador) AS colaboradores_activos
+ FROM colaboradores_activos ca
+ LEFT JOIN inegi_unicos iu
+     ON (ca.estado || '_' || ca.municipio) = (iu.estado_rhenueva || '_' || iu.municipio_rhenueva)
+ WHERE iu.cve_ent IS NOT NULL AND iu.cve_mun IS NOT NULL
+ GROUP BY ca.regional, iu.cve_ent, iu.cve_mun
+ "
> 
  > datos <- dbGetQuery(con, query)
> dbDisconnect(con)
> 
  > # --- PREPARAR DATOS ---
  > datos <- datos %>%
  +   mutate(
    +     cve_ent = str_pad(as.character(cve_ent), 2, "left", "0"),
    +     cve_mun = str_pad(as.character(cve_mun), 3, "left", "0"),
    +     CVEGEO = paste0(cve_ent, cve_mun)
    +   )
> 
  > print(paste("Municipios con presencia:", nrow(datos)))
[1] "Municipios con presencia: 331"
> 
  > # --- CARGAR SHAPEFILE MUNICIPIOS ---
  > print("Cargando shapefile de municipios de México...")
[1] "Cargando shapefile de municipios de México..."
> municipios_mexico <- st_read(ruta_shp_municipios, quiet = TRUE)
Error: Cannot open "C:/Users/racl26345/Documents/Shapes/inegi_municipios.shp"; The file doesn't seem to exist.