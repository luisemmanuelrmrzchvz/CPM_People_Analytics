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
  > # Verificar los nombres de las columnas y corregir si hay duplicados
  > colnames(datos)
[1] "...1"  "...2"  "...3"  "...4"  "...5"  "...6"  "...7"  "...8"  "...9"  "...10" "...11" "...12" "...13" "...14" "...15" "...16" "...17"
> colnames(datos) <- make.names(colnames(datos), unique = TRUE)
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
  > # Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
  > datos <- datos %>%
  +   select(-c(17, 6))  # Eliminar las columnas originales que no son necesarias
> 
  > # Reordenar las columnas para restaurar el orden original
  > # Vamos a reinsertar las columnas de fecha en las posiciones correctas
  > colnames(datos)[colnames(datos) == "fecha_solicitud"] <- colnames(datos)[6]  # Restablecer fecha_solicitud en su lugar
> colnames(datos)[colnames(datos) == "fecha_aprobacion"] <- colnames(datos)[17]  # Restablecer fecha_aprobacion en su lugar
> 
  > # Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
  > datos_filtrados <- datos %>%
  +   filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>%  # Filtrar registros válidos
  +   filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin)  # Filtrar por rango de fechas
Error in `filter()`:
  ! Can't transform a data frame with duplicate names.
Run `rlang::last_trace()` to see where the error occurred.
> rlang::last_trace()
<error/rlang_error>
Error in `filter()`:
! Can't transform a data frame with duplicate names.
---
  Backtrace:
  ▆
1. ├─... %>% ...
2. ├─dplyr::filter(...)
3. ├─dplyr::filter(., !is.na(fecha_aprobacion) & !is.na(fecha_solicitud))
4. └─dplyr:::filter.data.frame(., !is.na(fecha_aprobacion) & !is.na(fecha_solicitud))
Run rlang::last_trace(drop = FALSE) to see 4 hidden frames.
Aviso: unable to access index for repository https://cran.rstudio.com/src/contrib:
  no fue posible abrir la URL 'https://cran.rstudio.com/src/contrib/PACKAGES'
> rlang::last_trace(drop = FALSE)
<error/rlang_error>
  Error in `filter()`:
  ! Can't transform a data frame with duplicate names.
---
Backtrace:
    ▆
 1. ├─... %>% ...
 2. ├─dplyr::filter(...)
 3. ├─dplyr::filter(., !is.na(fecha_aprobacion) & !is.na(fecha_solicitud))
 4. └─dplyr:::filter.data.frame(., !is.na(fecha_aprobacion) & !is.na(fecha_solicitud))
 5.   └─dplyr:::filter_rows(.data, dots, by)
 6.     └─DataMask$new(data, by, "filter", error_call = error_call)
 7.       └─dplyr (local) initialize(...)
 8.         └─rlang::abort(...)


########################


10/01/2025  01:27:25 p. m.
45667.5607060185
