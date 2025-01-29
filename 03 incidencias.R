# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)

# Ruta del archivo de entrada y base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
archivo_excel <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Incidencias.xlsx"

# Leer el archivo XLSX ignorando la primera fila (títulos de columnas)
datos <- read_excel(archivo_excel, skip = 1, col_names = FALSE)

# Definir las columnas a formatear como fechas (3, 10, 11, 15)
columnas_fecha <- c(3, 10, 11, 15)

# Convertir las columnas seleccionadas a formato de fecha "YYYY-MM-DD"
for (col in columnas_fecha) {
  datos[[col]] <- format(as.Date(datos[[col]], origin = "1899-12-30"), "%Y-%m-%d")
}

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Obtener los nombres de las columnas de la base de datos (sin incluir id_key)
columnas_db <- dbListFields(conn, "incidencias")
columnas_db <- columnas_db[columnas_db != "id_key"]

# Renombrar las columnas para coincidir con la base de datos
colnames(datos) <- columnas_db

# Insertar los datos en la tabla incidencias
dbWriteTable(conn, "incidencias", datos, append = TRUE, row.names = FALSE)

# Cerrar la conexión
dbDisconnect(conn)

print("Datos insertados en la base de datos correctamente.")
