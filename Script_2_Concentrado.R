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

# Convertir la columna 6 (fecha_solicitud) a formato de fecha "YYYY-MM-DD"
if (is.numeric(datos[[6]])) {
  datos[[6]] <- as.Date(datos[[6]], origin = "1899-12-30")  # Conversión desde el formato numérico de Excel
} else {
  datos[[6]] <- as.Date(datos[[6]], format = "%d/%m/%Y")  # Conversión desde texto
}

# Convertir la columna 17 (fecha_aprobacion) a formato de fecha "YYYY-MM-DD" extrayendo solo la fecha
if (is.numeric(datos[[17]])) {
  # Si la columna es numérica (timestamp), convertirla a fecha
  datos[[17]] <- as.Date(datos[[17]], origin = "1899-12-30")
} else {
  # Si la columna es texto, extraer solo la fecha (ignorando la hora)
  datos[[17]] <- as.Date(substr(datos[[17]], 1, 10), format = "%d/%m/%Y")
}

# Filtrar registros que estén dentro del rango de fechas definido
datos_filtrados <- datos %>%
  filter(datos[[6]] >= fecha_inicio & datos[[6]] <= fecha_fin)

# Verificar que las columnas 6 y 17 ahora estén en formato correcto antes de guardar
datos_filtrados[[6]] <- format(datos_filtrados[[6]], "%Y-%m-%d")
datos_filtrados[[17]] <- format(datos_filtrados[[17]], "%Y-%m-%d")

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
