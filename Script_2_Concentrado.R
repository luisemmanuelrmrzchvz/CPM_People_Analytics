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

# Convertir la columna 6 (fecha_solicitud) de formato numérico de Excel a "YYYY-MM-DD"
datos[[6]] <- format(as.Date(as.numeric(datos[[6]]), origin = "1899-12-30"), "%Y-%m-%d")

# Convertir la columna 17 (fecha_aprobacion) de timestamp a "YYYY-MM-DD"
if (inherits(datos[[17]], "POSIXt")) {
  datos[[17]] <- format(as.Date(datos[[17]]), "%Y-%m-%d")
} else {
  datos[[17]] <- format(as.Date(as.POSIXct(datos[[17]], format = "%d/%m/%Y %I:%M:%S %p", tz = "UTC")), "%Y-%m-%d")
}

# Filtrar registros que estén dentro del rango de fechas definido
datos_filtrados <- datos %>%
  filter(as.Date(`...6`) >= fecha_inicio & as.Date(`...6`) <= fecha_fin)

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
columnas_db <- dbListFields(conn, "sanciones")
columnas_db <- columnas_db[columnas_db != "id_key"]

# Renombrar las columnas del data frame para coincidir con la base de datos
colnames(datos_filtrados) <- columnas_db

# Insertar los datos filtrados en la tabla sanciones
dbWriteTable(conn, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)

# Cerrar la conexión a la base de datos
dbDisconnect(conn)

print("Datos filtrados e insertados en la base de datos correctamente.")
