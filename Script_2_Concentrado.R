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

# Asegurarse de que la columna 17 esté en formato numérico (si es timestamp)
# Convertir la columna 17 (fecha_aprobacion) desde número (timestamp de Excel) a fecha "YYYY-MM-DD"
if (is.numeric(datos[[17]])) {
  # Convertir el número de timestamp de Excel a fecha
  # Convertir la parte entera a fecha y la parte decimal a hora
  datos[[17]] <- as.POSIXct(datos[[17]], origin = "1899-12-30", tz = "UTC")
} else {
  # Si la columna no es numérica, se extrae la fecha de la cadena de texto
  datos[[17]] <- as.Date(substr(datos[[17]], 1, 10), format = "%d/%m/%Y")
}

# Verificar que la conversión a fecha fue exitosa
cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos[[17]])), "\n")

# Filtrar los registros en el rango de fechas de la columna 17 (fecha_aprobacion)
datos_filtrados <- datos %>%
  mutate(fecha_aprobacion = as.Date(datos[[17]])) %>%  # Asegurarse que la fecha esté en formato Date
  filter(!is.na(fecha_aprobacion)) %>%  # Filtrar solo si la columna fecha_aprobacion no tiene NA
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin)  # Filtrar por rango de fechas

# Verificar las dimensiones después del filtrado
cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")

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
