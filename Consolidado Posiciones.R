# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)

# Ruta de la carpeta de entrada
ruta_carpeta <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Posiciones_Hist"

# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

# Fechas de inicio y fin para la búsqueda de archivos
fecha_inicio <- as.Date("2025-01-01")
fecha_fin <- as.Date("2025-01-07")

# Generar secuencia de fechas entre fecha_inicio y fecha_fin en formato YYYYMMDD
formato_fechas <- format(seq(fecha_inicio, fecha_fin, by = "day"), "%Y%m%d")

# Lista para almacenar los data frames
lista_datos <- list()

# Leer archivos dentro del rango de fechas
for (fecha in formato_fechas) {
  nombre_archivo <- paste0("Posiciones_", fecha, ".xlsx")
  ruta_archivo <- file.path(ruta_carpeta, nombre_archivo)

  if (file.exists(ruta_archivo)) {
    mensaje <- paste("Cargando archivo:", nombre_archivo)
    print(mensaje)
    
    # Leer el archivo XLSX ignorando la primera fila (títulos de columnas)
    datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
    
    # Añadir la fecha del archivo como última columna
    datos$Fecha_Archivo <- as.Date(fecha, format = "%Y%m%d")

    # Agregar a la lista
    lista_datos[[length(lista_datos) + 1]] <- datos
  } else {
    mensaje <- paste("Archivo no encontrado:", nombre_archivo)
    print(mensaje)
  }
}

# Concatenar todos los data frames en uno solo
if (length(lista_datos) > 0) {
  datos_consolidados <- bind_rows(lista_datos)
  
  # Definir las columnas a formatear como fechas (21, 22, 23, 25, 26, 32, 33, 46)
  columnas_fecha <- c(21, 22, 23, 25, 26, 32, 33, 46)
  
  # Convertir las columnas seleccionadas a formato de fecha "YYYY-MM-DD"
  for (col in columnas_fecha) {
    datos_consolidados[[col]] <- as.Date(datos_consolidados[[col]], origin = "1899-12-30")
  }
  
  # Conectar a la base de datos SQLite
  conn <- dbConnect(SQLite(), db_path)
  
  # Renombrar las columnas para coincidir con la base de datos (ajustar si es necesario)
  colnames(datos_consolidados) <- dbListFields(conn, "hist_posiciones")
  
  # Excluir la columna autoincremental id_key si existe
  datos_consolidados <- datos_consolidados %>% select(-id_key)
  
  # Insertar los datos en la tabla hist_posiciones
  dbWriteTable(conn, "hist_posiciones", datos_consolidados, append = TRUE, row.names = FALSE)
  
  # Cerrar la conexión
  dbDisconnect(conn)
  
  print("Datos consolidados insertados en la base de datos correctamente.")
} else {
  print("No se encontraron archivos en el rango de fechas especificado.")
}
