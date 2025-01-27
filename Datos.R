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
> # Cargar librerías necesarias
  > library(readxl)
> library(dplyr)
> library(DBI)
> library(RSQLite)
> 
  > # Ruta del archivo de entrada
  > ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
> 
  > # Ruta de la base de datos SQLite
  > db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
> 
  > # Fechas de inicio y fin para la selección de registros
  > fecha_inicio <- as.Date("2021-12-01")
> fecha_fin <- as.Date("2025-01-23")
> 
  > # Leer el archivo Excel, omitiendo la primera fila de títulos
  > datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
New names:
  • `` -> `...1`
• `` -> `...2`
• `` -> `...3`
• `` -> `...4`
• `` -> `...5`
• `` -> `...6`
• `` -> `...7`
• `` -> `...8`
• `` -> `...9`
• `` -> `...10`
• `` -> `...11`
• `` -> `...12`
• `` -> `...13`
• `` -> `...14`
• `` -> `...15`
• `` -> `...16`
• `` -> `...17`
> 
  > # Asegurarse de que la columna 17 esté en formato numérico (si es timestamp)
  > # Convertir la columna 17 (fecha_aprobacion) desde número (timestamp de Excel) a fecha "YYYY-MM-DD"
  > if (is.numeric(datos[[17]])) {
    +   # Convertir el número de timestamp de Excel a fecha
      +   datos[[17]] <- as.Date(datos[[17]], origin = "1899-12-30")  # Conversión de timestamp a fecha
      + } else {
        +   # Si la columna no es numérica, se extrae la fecha de la cadena de texto
          +   datos[[17]] <- as.Date(substr(datos[[17]], 1, 10), format = "%d/%m/%Y")
          + }
> 
  > # Verificar que la conversión a fecha fue exitosa
  > cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos[[17]])), "\n")
Número de valores NA en columna 17 (fecha_aprobacion): 6412 
> 
  > # Filtrar los registros en el rango de fechas de la columna 17 (fecha_aprobacion)
  > datos_filtrados <- datos %>%
  +   mutate(fecha_aprobacion = as.Date(datos[[17]], origin = "1899-12-30")) %>%  # Asegurarse que la fecha esté en formato Date
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
