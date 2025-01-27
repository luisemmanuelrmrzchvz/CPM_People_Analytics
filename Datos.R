library(readxl)
library(DBI)
library(RSQLite)
library(dplyr)

# Definir la ruta de los archivos y la conexión a la base de datos
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Posiciones_Hist"
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(SQLite(), db_path)

# Definir rango de fechas
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2024-12-02")

# Inicializar un dataframe vacío para almacenar los datos consolidados
consolidated_data <- data.frame()

# Recorrer fechas y procesar archivos
for (date in seq(start_date, end_date, by = "day")) {
  file_name <- paste0("Posiciones_", format(date, "%Y%m%d"), ".xls")
  file_full_path <- file.path(file_path, file_name)
  
  if (file.exists(file_full_path)) {
    # Leer el archivo de Excel omitiendo la fila de títulos
    data <- read_excel(file_full_path, skip = 1, col_names = TRUE)
    
    # Añadir la columna de fecha del archivo
    data$fecha_info <- format(date, "%Y-%m-%d")
    
    # Consolidar los datos
    consolidated_data <- bind_rows(consolidated_data, data)
  }
}

# Insertar los datos en la tabla SQLite
if (nrow(consolidated_data) > 0) {
  dbWriteTable(conn, "hist_posiciones", consolidated_data, append = TRUE, row.names = FALSE)
}

# Cerrar la conexión a la base de datos
dbDisconnect(conn)

print("Proceso completado exitosamente.")



########################################
> cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos[[17]])), "\n")
Número de valores NA en columna 17 (fecha_aprobacion): 6412 
> cat("Primeros valores de la columna 17 después de la conversión:\n")
Primeros valores de la columna 17 después de la conversión:
  > head(datos[[17]])
[1] NA NA NA NA NA NA
> 
  > # Filtrar los registros en el rango de fechas de la columna 17 (fecha_aprobacion)
  > datos_filtrados <- datos %>%
  +   mutate(fecha_aprobacion = as.Date(datos[[17]])) %>%  # Asegurarse que la fecha esté en formato Date
  +   filter(!is.na(fecha_aprobacion)) %>%  # Filtrar solo si la columna fecha_aprobacion no tiene NA
  +   filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin)  # Filtrar por rango de fechas
> 
  > # Verificar las dimensiones después del filtrado
  > cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
Dimensión después del filtrado: 0 
> 
  > # Conectar a la base de datos SQLite
  > conn <- dbConnect(SQLite(), db_path)
> 
  > # Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
  > columnas_db <- dbListFields(conn, "sanciones")
> columnas_db <- columnas_db[columnas_db != "id_key"]
> 
  > # Renombrar las columnas del data frame para coincidir con la base de datos
  > colnames(datos_filtrados) <- columnas_db
> 
  > # Insertar los datos filtrados en la tabla sanciones
  > dbWriteTable(conn, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)
Error: Columns `NA` not found

########################


10/01/2025  01:27:25 p. m.
45667.5607060185
