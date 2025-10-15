# Cargar librerías necesarias
library(readxl)
library(DBI)
library(RSQLite)

# Ruta del archivo de entrada y base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
archivo_excel <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Requisiciones Abiertas.xlsx"

# Leer el archivo XLSX ignorando la primera fila (títulos de columnas)
datos <- read_excel(archivo_excel, skip = 1, col_names = FALSE)

# Definir las columnas a formatear como fechas (14, 15, 16, 19)
columnas_fecha <- c(9, 10)

# Convertir las columnas seleccionadas a formato de fecha "YYYY-MM-DD"
for (col in columnas_fecha) {
  datos[[col]] <- format(as.Date(datos[[col]], origin = "1899-12-30"), "%Y-%m-%d")
}

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Verificar si existen registros en la tabla
registros_existentes <- dbGetQuery(conn, "SELECT COUNT(*) FROM rcltmnt_req_abiertas")$`COUNT(*)`

# Si existen registros, eliminarlos
if (registros_existentes > 0) {
  dbExecute(conn, "DELETE FROM rcltmnt_req_abiertas")
}

# Definir las columnas de la tabla de base de datos
columnas_db <- c("id_requisicion", "id_posicion", "puesto_nombre", "departamento", "ubicacion", 
                 "division", "motivo_requisicion", "reemplaza_a", "fecha_aprobacion", "fecha_creacion", "nombre_ejecutivo_rcltmnt", 
                 "apellidos_ejecutivo_rcltmnt", "nombre_coordinador_rcltmnt", "apellidos_coordinador_rcltmnt", "estado_requisicion", 
                 "progreso_candidato", "segmento_puesto")

# Renombrar las columnas de los datos para que coincidan con la tabla de SQLite
colnames(datos) <- columnas_db

# Insertar los datos en la tabla rcltmnt_req_abiertas
dbWriteTable(conn, "rcltmnt_req_abiertas", datos, append = TRUE, row.names = FALSE)

# Cerrar la conexión
dbDisconnect(conn)

print("Datos insertados en la base de datos correctamente.")