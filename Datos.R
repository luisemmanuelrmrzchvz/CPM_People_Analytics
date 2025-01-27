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
  > # Convertir la columna 6 (fecha_solicitud) a formato de fecha "YYYY-MM-DD"
  > if (is.numeric(datos[[6]])) {
    +   datos[[6]] <- as.Date(datos[[6]], origin = "1899-12-30")  # Conversión desde el formato numérico de Excel
    + } else {
      +   datos[[6]] <- as.Date(datos[[6]], format = "%d/%m/%Y")  # Conversión desde texto
      + }
> 
  > # Convertir la columna 17 (fecha_aprobacion) de timestamp a fecha "YYYY-MM-DD"
  > # Si el valor de la columna es numérico (timestamp de Excel), convertirlo a fecha.
  > if (is.numeric(datos[[17]])) {
    +   datos[[17]] <- as.Date(datos[[17]], origin = "1899-12-30")  # Conversión desde el formato numérico de Excel
    + } else {
      +   # Si es texto, extraemos solo la parte de la fecha
        +   datos[[17]] <- as.Date(substr(datos[[17]], 1, 10), format = "%d/%m/%Y")
        + }
> 
  > # Verificar dimensiones de las columnas antes de continuar
  > cat("Dimensión de la columna 6:", length(datos[[6]]), "\n")
Dimensión de la columna 6: 6412 
> cat("Dimensión de la columna 17:", length(datos[[17]]), "\n")
Dimensión de la columna 17: 6412 
> 
  > # Asegurarse de que la columna 17 esté en formato "YYYY-MM-DD"
  > datos[[17]] <- format(datos[[17]], "%Y-%m-%d")
> 
  > # Comprobar si hay valores NA en las columnas de fecha
  > cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos[[6]])), "\n")
Número de valores NA en columna 6 (fecha_solicitud): 0 
> cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos[[17]])), "\n")
Número de valores NA en columna 17 (fecha_aprobacion): 6412 
> 
  > # Filtrar solo los registros de la columna 17 (fecha_aprobacion) en el rango de fechas
  > datos_filtrados <- datos %>%
  +   filter(!is.na(datos[[17]])) %>%  # Filtrar solo si la columna 17 no tiene NA
  +   filter(datos[[17]] >= fecha_inicio & datos[[17]] <= fecha_fin)
Error in `filter()`:
  ℹ In argument: `datos[[17]] >= fecha_inicio & datos[[17]] <= fecha_fin`.
Caused by error:
  ! `..1` must be of size 0 or 1, not size 6412.
Run `rlang::last_trace()` to see where the error occurred.
