library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)

# Ruta de la carpeta de entrada
ruta_carpeta <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/"

# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

# Fechas de inicio y fin para la búsqueda de registros
fecha_inicio <- as.Date("2025-01-01")
fecha_fin <- as.Date("2025-01-23")

# Leer archivo de sanciones
nombre_archivo <- "Sanciones.xlsx"
ruta_archivo <- file.path(ruta_carpeta, nombre_archivo)

if (file.exists(ruta_archivo)) {
  print(paste("Cargando archivo:", nombre_archivo))
  
  # Leer archivo ignorando la primera fila (títulos de columnas)
  datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
  
  # Convertir la columna 17 a fecha (formato "YYYY-MM-DD")
  datos[[17]] <- as.Date(as.numeric(datos[[17]]), origin = "1899-12-30")
  
  # Convertir la columna 6 a fecha (formato "YYYY-MM-DD")
  datos[[6]] <- as.Date(as.numeric(datos[[6]]), origin = "1899-12-30")
  
  # Filtrar registros dentro del rango de fechas especificado
  datos_filtrados <- datos %>%
    filter(between(datos[[17]], fecha_inicio, fecha_fin))
  
  if (nrow(datos_filtrados) > 0) {
    # Conectar a la base de datos SQLite
    conn <- dbConnect(SQLite(), db_path)
    
    # Obtener nombres de columnas de la base de datos excluyendo id_key
    columnas_db <- dbListFields(conn, "sanciones")
    columnas_db <- columnas_db[columnas_db != "id_key"]
    
    # Renombrar las columnas del dataframe
    colnames(datos_filtrados) <- columnas_db
    
    # Insertar datos en la tabla sanciones
    dbWriteTable(conn, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)
    
    # Cerrar conexión
    dbDisconnect(conn)
    print("Datos respaldados en la base de datos correctamente.")
  } else {
    print("No se encontraron registros en el rango de fechas especificado.")
  }
} else {
  print("Archivo no encontrado:", nombre_archivo)
}
