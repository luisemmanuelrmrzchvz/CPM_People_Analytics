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
Aviso:
  package ‘readxl’ was built under R version 4.4.2 
> library(dplyr)

Adjuntando el paquete: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union

> library(DBI)
> library(RSQLite)
Aviso:
  package ‘RSQLite’ was built under R version 4.4.2 
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
  > # Asignar nombres únicos a las columnas automáticamente
  > colnames(datos) <- make.names(paste0("Col", seq_along(datos)), unique = TRUE)
> 
  > # Renombrar las columnas relevantes (columna 17 y 6) para identificar fechas correctamente
  > datos <- datos %>%
  +   rename(fecha_aprobacion = Col17, fecha_solicitud = Col6)
> 
  > # Convertir las columnas de fecha en formato POSIXct (fecha y hora) y luego a Date (solo fecha)
  > datos <- datos %>%
  +   mutate(
    +     fecha_aprobacion = as.Date(as.POSIXct(fecha_aprobacion, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")),
    +     fecha_solicitud = as.Date(as.POSIXct(fecha_solicitud, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    +   )
> 
  > # Filtrar los registros en el rango de fechas
  > datos_filtrados <- datos %>%
  +   filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>%
  +   filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin)
> 
  > # Establecer conexión con la base de datos SQLite
  > con <- dbConnect(SQLite(), dbname = db_path)
> 
  > # Crear la tabla en SQLite si no existe
  > tabla_sqlite <- "sanciones"  # Nombre de la tabla en SQLite
> if (!dbExistsTable(con, tabla_sqlite)) {
  +   dbCreateTable(con, tabla_sqlite, datos_filtrados)
  + }
> 
  > # Insertar los datos filtrados en la tabla de SQLite
  > dbWriteTable(con, tabla_sqlite, datos_filtrados, append = TRUE, row.names = FALSE)
Error: Columns `Col1`, `Col2`, `Col3`, `Col4`, `Col5`, `Col7`, `Col8`, `Col9`, `Col10`, `Col11`, `Col12`, `Col13`, `Col14`, `Col15`, `Col16` not found
> View(datos_filtrados)


########################


10/01/2025  01:27:25 p. m.
45667.5607060185
