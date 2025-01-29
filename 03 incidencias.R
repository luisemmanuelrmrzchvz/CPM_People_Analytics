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

# Obtener IDs existentes en la base de datos
existentes <- dbGetQuery(conn, "SELECT id_incidencia FROM incidencias")[[1]]

# Separar datos nuevos y existentes
nuevos <- datos[!datos$id_incidencia %in% existentes, ]
actualizaciones <- datos[datos$id_incidencia %in% existentes, ]

# Función para imprimir diferencias
imprimir_cambios <- function(viejo, nuevo) {
  cambios <- which(viejo != nuevo)
  if (length(cambios) > 0) {
    cat("Campos modificados:\n")
    for (i in cambios) {
      cat(sprintf("- %s: De '%s' a '%s'\n", 
                  names(viejo)[i], 
                  viejo[[i]], 
                  nuevo[[i]]))
    }
  } else {
    cat("Sin cambios en los datos\n")
  }
}

# Actualizar registros existentes
if (nrow(actualizaciones) > 0) {
  # Preparar consulta de actualización
  columnas_actualizar <- setdiff(columnas_db, "id_incidencia")
  sql_update <- sprintf(
    "UPDATE incidencias SET %s WHERE id_incidencia = ?",
    paste(columnas_actualizar, "= ?", collapse = ", ")
  )
  
  # Procesar cada actualización
  for (i in 1:nrow(actualizaciones)) {
    registro <- actualizaciones[i, ]
    
    # Obtener versión anterior
    viejo <- dbGetQuery(conn, 
                        "SELECT * FROM incidencias WHERE id_incidencia = ?",
                        params = list(registro$id_incidencia))
    
    # Imprimir comparación
    cat(sprintf("\nActualizando registro ID: %s\n", registro$id_incidencia))
    imprimir_cambios(viejo[columnas_db], registro)
    
    # Ejecutar actualización
    dbExecute(conn, sql_update, params = c(unname(registro[columnas_actualizar]), 
                                           registro$id_incidencia))
  }
}

# Insertar nuevos registros
if (nrow(nuevos) > 0) {
  dbWriteTable(conn, "incidencias", nuevos, append = TRUE, row.names = FALSE)
  cat(sprintf("\nSe insertaron %d nuevos registros\n", nrow(nuevos)))
}

# Cerrar la conexión
dbDisconnect(conn)

print("Proceso completado exitosamente.")