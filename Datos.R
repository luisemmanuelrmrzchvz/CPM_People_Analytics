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
  > # Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
  > datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
> 
  > # Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
  > datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
> 
  > # Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
  > datos$fecha_aprobacion <- as.Date(datos[[17]])  # Convertir la columna 17 a solo fecha
> datos$fecha_solicitud <- as.Date(datos[[6]])    # Convertir la columna 6 a solo fecha
> 
  > # Verificar los resultados después de la conversión
  > cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos$fecha_aprobacion)), "\n")
Número de valores NA en columna 17 (fecha_aprobacion): 0 
> cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos$fecha_solicitud)), "\n")
Número de valores NA en columna 6 (fecha_solicitud): 0 
> cat("Primeros valores de la columna 17 después de la conversión:\n")
Primeros valores de la columna 17 después de la conversión:
  > head(datos$fecha_aprobacion)
[1] "2025-01-10" "2025-01-21" "2024-10-23" "2024-09-10" "2024-10-24" "2024-10-10"
> cat("Primeros valores de la columna 6 después de la conversión:\n")
Primeros valores de la columna 6 después de la conversión:
  > head(datos$fecha_solicitud)
[1] "2025-01-04" "2025-01-07" "2024-10-09" "2024-09-07" "2024-10-23" "2024-10-02"
> 
  > # Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
  > datos <- datos %>%
  +   select(-c(17, 6))  # Eliminar las columnas originales que no son necesarias
> 
  > # Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
  > datos_filtrados <- datos %>%
  +   filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>%  # Filtrar registros válidos
  +   filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin)  # Filtrar por rango de fechas
> 
  > # Verificar las dimensiones después del filtrado
  > cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
Dimensión después del filtrado: 6403 
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
  > # Convertir las fechas a texto en formato "YYYY-MM-DD"
  > datos_filtrados$fecha_solicitud <- format(datos_filtrados$fecha_solicitud, "%Y-%m-%d")
Error en format.default(datos_filtrados$fecha_solicitud, "%Y-%m-%d"): 
  argumento 'trim' inválido

########################


10/01/2025  01:27:25 p. m.
45667.5607060185
