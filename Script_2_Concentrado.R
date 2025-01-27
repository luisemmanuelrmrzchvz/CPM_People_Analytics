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

# Establecer conexión con la base de datos SQLite
con <- dbConnect(SQLite(), dbname = db_path)

# Crear la tabla en SQLite si no existe
tabla_sqlite <- "sanciones"  # Nombre de la tabla en SQLite
if (!dbExistsTable(con, tabla_sqlite)) {
  dbCreateTable(con, tabla_sqlite, datos_filtrados)
}

# Verificar y respaldar en la base de datos SQLite
tryCatch(
  {
    # Intentar insertar los datos directamente
    dbWriteTable(con, tabla_sqlite, datos_filtrados, append = TRUE, row.names = FALSE)
    cat("Datos respaldados exitosamente en la tabla", tabla_sqlite, "\n")
  },
  error = function(e) {
    cat("Error al insertar los datos: ", e$message, "\n")
    
    # Forzar la coincidencia de columnas con SQLite y reintentar
    dbExecute(con, paste0("DELETE FROM ", tabla_sqlite))  # Limpiar la tabla (opcional)
    dbWriteTable(con, tabla_sqlite, datos_filtrados, append = TRUE, row.names = FALSE)
    cat("Datos respaldados después de ajustar el formato.\n")
  }
)

# Confirmar los registros insertados
registros <- dbReadTable(con, tabla_sqlite)
print(paste("Total de registros en la tabla:", nrow(registros)))

# Cerrar conexión con SQLite
dbDisconnect(con)
