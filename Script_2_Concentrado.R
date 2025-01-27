# Cargar librerías necesarias 
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)

# Ruta del archivo de entrada
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"

# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

# Fechas de inicio y fin para la selección de registros
fecha_inicio <- as.Date("2021-12-01")
fecha_fin <- as.Date("2025-01-23")

# Leer el archivo Excel, omitiendo la primera fila de títulos
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)

# Asignar nombres únicos a las columnas automáticamente
colnames(datos) <- make.names(paste0("Col", seq_along(datos)), unique = TRUE)

# Renombrar las columnas relevantes (columna 17 y 6) para identificar fechas correctamente
datos <- datos %>%
  rename(fecha_aprobacion = Col17, fecha_solicitud = Col6)

# Convertir las columnas de fecha en formato POSIXct (fecha y hora) y luego a Date (solo fecha)
datos <- datos %>%
  mutate(
    fecha_aprobacion = as.Date(as.POSIXct(fecha_aprobacion, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")),
    fecha_solicitud = as.Date(as.POSIXct(fecha_solicitud, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  )

# Filtrar los registros en el rango de fechas
datos_filtrados <- datos %>%
  filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>%
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin)

# Seleccionar y renombrar columnas para que coincidan con la estructura de la tabla SQLite
datos_filtrados <- datos_filtrados %>%
  mutate(
    id_colaborador = as.integer(Col1), 
    nombre = as.character(Col2),
    antiguedad_meses = as.numeric(Col3),
    antiguedad_years = as.numeric(Col4),
    id_sancion = as.integer(Col5),
    clasificacion_sancion = as.character(Col7),
    motivo_sancion = as.character(Col8),
    detalle_sancion = as.character(Col9),
    descripcion_breve = as.character(Col10),
    acta_hechos = as.logical(Col11),
    causa_sancion = as.character(Col12),
    solicitado_por = as.character(Col13),
    analista_rl = as.character(Col14),
    tipo_sancion = as.character(Col15),
    dias_suspension = as.integer(Col16),
    # Convertir las fechas a formato texto en YYYY-MM-DD
    fecha_solicitud = format(fecha_solicitud, "%Y-%m-%d"),
    fecha_aprobacion = format(fecha_aprobacion, "%Y-%m-%d")
  ) %>%
  select(
    id_colaborador, nombre, antiguedad_meses, antiguedad_years, id_sancion, 
    fecha_solicitud, clasificacion_sancion, motivo_sancion, detalle_sancion, 
    descripcion_breve, acta_hechos, causa_sancion, solicitado_por, analista_rl, 
    tipo_sancion, dias_suspension, fecha_aprobacion
  )

# Establecer conexión con la base de datos SQLite
con <- dbConnect(SQLite(), dbname = db_path)

# Insertar los datos en la tabla existente
tryCatch(
  {
    dbWriteTable(con, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)
    cat("Datos respaldados exitosamente en la tabla 'sanciones'.\n")
  },
  error = function(e) {
    cat("Error al insertar los datos: ", e$message, "\n")
  }
)

# Confirmar los registros insertados
total_registros <- dbGetQuery(con, "SELECT COUNT(*) as total FROM sanciones")
cat("Total de registros en la tabla 'sanciones':", total_registros$total, "\n")

# Cerrar conexión con SQLite
dbDisconnect(con)
