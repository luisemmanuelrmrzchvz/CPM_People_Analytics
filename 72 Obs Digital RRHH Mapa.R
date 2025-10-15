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
ruta_salida <- "C:/Users/racl26345/Documents/Reportes Automatizados/Observatorio Digital/"
ruta_shapes_base <- "C:/Users/racl26345/Documents/Shapes"  # carpeta donde descomprimiste el ZIP del MGI 2024
pattern_shp <- "00mun.*\\.shp$"  # patrón esperado del shapefile municipal en MGI 2024

# control de performance: si el shapefile es muy grande, puedes simplificar las geometrías
usar_simplify <- TRUE
simplify_tol <- 0.001  # aumenta si quieres más simplificación (en grados) -> menor detalle y más velocidad

# Colores (intensos para puntos; fills con alpha suave) - MANTENIDOS IGUAL
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

# Tema del mapa CON FONDO OSCURO - SOLO CAMBIOS EN FONDOS Y TEXTOS
theme_mapa_real <- function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill = "#1E1E1E", color = NA),  # FONDO OSCURO
      panel.background = element_rect(fill = "#1E1E1E", color = NA),  # FONDO OSCURO
      text = element_text(color = "#E8E8E8", family = "sans"),  # TEXTO CLARO
      plot.title = element_text(face = "bold", size = 40, hjust = 0.5, margin = margin(b = 12), color = "#F5F5F5"),  # TEXTO BLANCO
      plot.subtitle = element_text(size = 28, hjust = 0.5, margin = margin(b = 10), color = "#B0B0B0"),  # TEXTO GRIS CLARO
      legend.text = element_text(size = 14, color = "#E8E8E8"),  # TEXTO CLARO
      legend.title = element_text(face = "bold", size = 16, color = "#E8E8E8"),  # TEXTO CLARO
      legend.background = element_rect(fill = "#2D2D2D", color = NA),  # FONDO OSCURO PARA LEYENDA
      legend.key = element_rect(fill = "#2D2D2D", color = NA),  # FONDO OSCURO PARA LEYENDA
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
          color = "#404040", size = 0.12, show.legend = FALSE) + # Borde más oscuro para fondo oscuro
  geom_sf(data = estados_mexico, fill = NA, color = "#E8E8E8", size = 0.35, alpha = 0.6) + # Contornos claros
  geom_sf(data = puntos_municipios,
          aes(color = regional, size = colaboradores_activos),
          alpha = 0.95, shape = 19, stroke = 0.3) +
  # destacar León si existe
  { if (nrow(leon_gto) > 0) geom_sf(data = leon_gto, color = "#FFD700", size = 7, shape = 1, stroke = 2.2, inherit.aes = FALSE) } +
  { if (nrow(leon_gto) > 0) geom_sf_text(data = leon_gto, aes(label = "CORPORATIVO\nLEÓN"), size = 5.5, fontface = "bold", color = "#FFD700", nudge_y = 0.4) } + # Texto dorado
  scale_fill_manual(values = colores_fill, na.value = "#2D2D2D", name = "Regional (municipio)") + # Fondo oscuro para municipios sin datos
  scale_color_manual(values = colores_regionales, na.value = "#606060", name = "Regional (puntos)") +
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
# 7) Panel lateral de estadísticas CON FONDO OSCURO
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
            lineheight = 0.8, color = "#F5F5F5", fontface = "bold") + # Texto blanco
  scale_fill_manual(values = colores_regionales) +
  scale_x_continuous(limits = c(1, 8)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#1E1E1E", color = NA),  # FONDO OSCURO
    plot.title = element_text(face = "bold", size = 34, hjust = 0.5, margin = margin(b = 10), color = "#F5F5F5"),  # TEXTO BLANCO
    legend.position = "none"  # Ocultar leyenda redundante
  ) +
  labs(title = "RESUMEN POR REGIONAL")

# -----------------------------
# 8) Combinar y guardar (alta resolución para videowall)
# -----------------------------
layout_final <- mapa_con_municipios + panel_estadisticas + plot_layout(widths = c(2, 1))

output_file <- file.path(ruta_salida, "mapa_municipios_por_regional_OSCURO.png")

ggsave(
  filename = output_file,
  plot = layout_final,
  width = 5760/100,   # ancho en pulgadas (ajusta si quieres otra resolución) 3840
  height = 2160/100,  # alto 2160
  dpi = 100, # 300
  bg = "#1E1E1E",  # FONDO OSCURO
  limitsize = FALSE
)

library(magick)

img <- image_read(output_file)

nuevo_ancho <- 3840
nuevo_alto <- 2160

img_apretado <-image_resize(img,paste0(nuevo_ancho,"x",nuevo_alto,"!"))

image_write(img_apretado,output_file)

message("✅ MAPA OSCURO creado: ", output_file)
message("📍 Municipios con presencia (filas en 'datos'): ", nrow(datos))
message("👥 Total colaboradores (sum): ", sum(resumen_regional$total_colaboradores))