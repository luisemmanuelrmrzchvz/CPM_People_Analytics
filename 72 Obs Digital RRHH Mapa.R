# SOLUCIÓN CON MAPA REAL DE ESTADOS DE MÉXICO
library(RSQLite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(patchwork)

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
      plot.title = element_text(face = "bold", size = 36, hjust = 0.5, margin = margin(b = 15)),
      plot.subtitle = element_text(size = 24, hjust = 0.5, margin = margin(b = 20)),
      legend.text = element_text(size = 14),
      legend.title = element_text(face = "bold", size = 16),
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

# DESCARGAR SHAPEFILE REAL DE ESTADOS DE MÉXICO
print("Descargando shapefile de estados de México...")

# Vamos a usar una fuente confiable de Natural Earth
tryCatch({
  # Intentar descargar de Natural Earth
  url_estados <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_1_states_provinces.zip"
  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempdir()
  
  download.file(url_estados, temp_zip, mode = "wb", quiet = TRUE)
  unzip(temp_zip, exdir = temp_dir)
  
  # Buscar el archivo .shp
  shp_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
  estados_mundo <- st_read(shp_files[1], quiet = TRUE)
  
  # Filtrar solo México
  estados_mexico <- estados_mundo %>% filter(admin == "Mexico")
  
  print(paste("Estados de México cargados:", nrow(estados_mexico)))
  
}, error = function(e) {
  print(paste("Error con Natural Earth:", e$message))
  print("Usando fuente alternativa...")
  
  # Fuente alternativa: datos de OpenDataSoft
  tryCatch({
    url_alternativo <- "https://raw.githubusercontent.com/datasets/geo-boundaries-world-110m/master/data/countries-110m.geojson"
    temp_geojson <- tempfile(fileext = ".geojson")
    download.file(url_alternativo, temp_geojson, mode = "wb", quiet = TRUE)
    mundo <- st_read(temp_geojson, quiet = TRUE)
    
    # Filtrar México
    mexico_pais <- mundo %>% filter(ADMIN == "Mexico")
    
    # Si no funciona, crear estados simplificados manualmente
    if (nrow(mexico_pais) == 0) {
      stop("No se pudo obtener México de la fuente alternativa")
    }
    
    estados_mexico <- mexico_pais
    print("Mapa de México (país) cargado")
    
  }, error = function(e2) {
    print(paste("Error con fuente alternativa:", e2$message))
    print("Creando estados simplificados manualmente...")
    
    # Crear estados muy simplificados manualmente
    source <- "manual"
  })
})

# SI NO SE PUDO DESCARGAR, CREAR ESTADOS SIMPLIFICADOS MANUALMENTE
if (!exists("estados_mexico")) {
  print("Creando mapa simplificado de estados de México...")
  
  # Coordenadas aproximadas de los centroides de los estados para crear círculos
  estados_coords <- data.frame(
    estado = c("Baja California", "Baja California Sur", "Sonora", "Chihuahua", 
               "Coahuila", "Nuevo León", "Tamaulipas", "Sinaloa", "Durango",
               "Zacatecas", "San Luis Potosí", "Aguascalientes", "Jalisco",
               "Colima", "Michoacán", "Guanajuato", "Querétaro", "Hidalgo",
               "México", "Ciudad de México", "Morelos", "Puebla", "Tlaxcala",
               "Veracruz", "Guerrero", "Oaxaca", "Chiapas", "Tabasco",
               "Campeche", "Yucatán", "Quintana Roo", "Nayarit"),
    lon = c(-115.0, -111.5, -110.5, -106.5, -102.0, -100.0, -98.5, -107.0, -104.5,
            -102.5, -100.5, -102.0, -103.5, -103.8, -101.5, -101.0, -100.0, -98.5,
            -99.5, -99.1, -99.0, -98.0, -98.0, -96.5, -100.0, -96.5, -92.5, -92.5,
            -90.5, -89.0, -88.0, -105.0),
    lat = c(30.0, 25.0, 29.0, 28.0, 27.0, 25.0, 24.0, 25.0, 24.5,
            23.0, 22.5, 22.0, 20.5, 19.0, 19.0, 21.0, 20.5, 20.5,
            19.5, 19.4, 18.5, 19.0, 19.5, 19.5, 17.5, 17.0, 16.5, 18.0,
            19.0, 20.5, 19.5, 21.5)
  )
  
  # Crear círculos alrededor de los centroides
  crear_circulo <- function(lon_centro, lat_centro, radio = 1.0) {
    angulos <- seq(0, 2 * pi, length.out = 36)
    circle_coords <- cbind(
      lon_centro + radio * cos(angulos),
      lat_centro + radio * sin(angulos)
    )
    st_polygon(list(circle_coords))
  }
  
  estados_sf_list <- list()
  for (i in 1:nrow(estados_coords)) {
    estado_sf <- st_sf(
      estado = estados_coords$estado[i],
      geometry = st_sfc(crear_circulo(estados_coords$lon[i], estados_coords$lat[i], 0.8)),
      crs = 4326
    )
    estados_sf_list[[i]] <- estado_sf
  }
  
  estados_mexico <- do.call(rbind, estados_sf_list)
  print("Estados simplificados creados manualmente")
}

# CREAR EL MAPA CON CONTORNOS DE ESTADOS
mapa_con_estados <- ggplot() +
  # Estados de México - contornos grises
  geom_sf(data = estados_mexico, 
          fill = "#F8F9FA", 
          color = "#B0BEC5", 
          size = 0.6,
          alpha = 0.8) +
  
  # Puntos de municipios
  geom_sf(data = puntos_municipios, 
          aes(color = regional, size = colaboradores_activos),
          alpha = 0.8, 
          shape = 19,
          stroke = 0.5) +
  
  # Resaltar León
  geom_sf(data = leon_gto, 
          color = "#FFD700", 
          size = 8, 
          shape = 1, 
          stroke = 2.5,
          alpha = 0.9) +
  
  # Etiqueta para León
  geom_sf_text(data = leon_gto,
               aes(label = "CORPORATIVO\nLEÓN"),
               size = 4.5, 
               fontface = "bold", 
               color = "#2E3440",
               lineheight = 0.8, 
               nudge_y = 0.5) +
  
  # Etiquetas para algunos estados principales (opcional, para referencia)
  geom_sf_text(data = estados_mexico %>% 
                 filter(estado %in% c("Baja California", "Sonora", "Chihuahua", 
                                      "Nuevo León", "Jalisco", "Veracruz", 
                                      "Yucatán", "Ciudad de México")),
               aes(label = estado),
               size = 3, 
               color = "#666666",
               fontface = "bold") +
  
  # Escalas
  scale_color_manual(values = colores_regionales, name = "Regional") +
  scale_size_continuous(
    range = c(2, 10),
    name = "Colaboradores",
    breaks = c(1, 10, 50, 100),
    labels = c("1-9", "10-49", "50-99", "100+")
  ) +
  
  # Tema y estilo
  theme_mapa_real() +
  labs(
    title = "PRESENCIA DE COLABORADORES EN MÉXICO",
    subtitle = "Distribución Municipal por Regional"
  ) +
  
  # Ajustar los límites del mapa
  coord_sf(
    xlim = c(-118, -86),
    ylim = c(14, 33),
    expand = FALSE
  ) +
  
  # Leyenda
  theme(
    legend.position = "right",
    legend.box = "vertical"
  )

# CREAR PANEL DE ESTADÍSTICAS
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
  geom_text(aes(label = etiqueta), hjust = 0, nudge_x = 0.3, size = 4.5,
            lineheight = 0.8, color = "#2E3440", fontface = "bold") +
  scale_fill_manual(values = colores_regionales) +
  scale_x_continuous(limits = c(1, 8)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F0F2F6", color = NA),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 10))
  ) +
  labs(title = "RESUMEN POR REGIONAL")

# COMBINAR MAPA Y PANEL
layout_final <- mapa_con_estados + panel_estadisticas +
  plot_layout(widths = c(2, 1))

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
print(paste("🗺️ Tipo de mapa:", ifelse(exists("source") && source == "manual", "Estados simplificados", "Estados reales")))

# MOSTRAR RESUMEN
print("RESUMEN POR REGIONAL:")
print(resumen_regional %>% select(regional, total_colaboradores, total_municipios))

# VERSIÓN ALTERNATIVA: SOLO PUNTOS EN EL MAPA DE ESTADOS (más limpia)
mapa_limpio <- ggplot() +
  # Solo contornos de estados, sin relleno
  geom_sf(data = estados_mexico, 
          fill = NA, 
          color = "#90A4AE", 
          size = 0.5,
          alpha = 0.7) +
  
  # Puntos de municipios
  geom_sf(data = puntos_municipios, 
          aes(color = regional, size = colaboradores_activos),
          alpha = 0.8, 
          shape = 19) +
  
  # Resaltar León
  geom_sf(data = leon_gto, 
          color = "#FFD700", 
          size = 8, 
          shape = 1, 
          stroke = 2) +
  
  geom_sf_text(data = leon_gto,
               aes(label = "CORPORATIVO\nLEÓN"),
               size = 4, 
               fontface = "bold", 
               color = "#2E3440",
               lineheight = 0.8, 
               nudge_y = 0.4) +
  
  # Escalas
  scale_color_manual(values = colores_regionales, name = "Regional") +
  scale_size_continuous(range = c(2, 8), name = "Colaboradores") +
  
  # Tema
  theme_mapa_real() +
  labs(
    title = "PRESENCIA DE COLABORADORES EN MÉXICO",
    subtitle = "Distribución Municipal - Vista Geográfica"
  ) +
  coord_sf(xlim = c(-118, -86), ylim = c(14, 33))

# GUARDAR MAPA LIMPIO
ggsave(
  filename = paste0(ruta_salida, "mapa_estados_mexico_limpio.png"),
  plot = mapa_limpio,
  width = 3840/100,
  height = 2160/100,
  dpi = 300,
  bg = "#F0F2F6"
)

print("🎉 PROCESO COMPLETADO!")
print(paste("📁 Archivos guardados en:", ruta_salida))
















###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################
###############################################################################################################







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
