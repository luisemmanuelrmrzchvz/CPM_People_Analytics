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



# Fechas de inicio y fin para la selección de registros
fecha_inicio <- as.Date("2021-12-01")
fecha_fin <- as.Date("2025-01-23")
# Leer el archivo Excel, omitiendo la primera fila de títulos
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
# Verificar los resultados después de la conversión
cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos$fecha_aprobacion)), "\n")
cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos$fecha_solicitud)), "\n")
cat("Primeros valores de la columna 17 después de la conversión:\n")
head(datos$fecha_aprobacion)
cat("Primeros valores de la columna 6 después de la conversión:\n")
head(datos$fecha_solicitud)
# Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
datos <- datos %>%
  select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
# Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
datos_filtrados <- datos %>%
  filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
# Verificar las dimensiones después del filtrado
cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)
# Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
columnas_db <- dbListFields(conn, "sanciones")
columnas_db <- columnas_db[columnas_db != "id_key"]
# Renombrar las columnas del data frame para coincidir con la base de datos
colnames(datos_filtrados) <- columnas_db
# Verificar que las columnas coincidan
cat("Nombres de columnas en los datos filtrados:\n")
print(colnames(datos_filtrados))
# Insertar los datos filtrados en la tabla sanciones
dbWriteTable(conn, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)
# Cerrar la conexión a la base de datos
dbDisconnect(conn)
print("Datos filtrados e insertados en la base de datos correctamente.")
# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
# Ruta del archivo de entrada
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Fechas de inicio y fin para la selección de registros
fecha_inicio <- as.Date("2021-12-01")
fecha_fin <- as.Date("2025-01-23")
# Leer el archivo Excel, omitiendo la primera fila de títulos
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Verificar las primeras filas para asegurarse de que los datos se han cargado correctamente
head(datos)
# Renombrar las columnas del archivo para evitar el uso de nombres automáticos como "...1", "...2", etc.
colnames(datos) <- c("id_colaborador", "nombre", "antiguedad_meses", "antiguedad_years", "id_sancion", "fecha_solicitud",
                     "clasificacion_sancion", "motivo_sancion", "detalle_sancion", "descripcion_breve", "acta_hechos",
                     "causa_sancion", "solicitado_por", "analista_rl", "tipo_sancion", "dias_suspension", "fecha_aprobacion")
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos$fecha_aprobacion <- as.Date(datos$fecha_aprobacion, format = "%Y-%m-%d")
datos$fecha_solicitud <- as.Date(datos$fecha_solicitud, format = "%Y-%m-%d")
# Verificar los resultados después de la conversión
cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos$fecha_aprobacion)), "\n")
cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos$fecha_solicitud)), "\n")
# Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
datos_filtrados <- datos %>%
  filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
# Verificar las dimensiones después del filtrado
cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)
# Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
columnas_db <- dbListFields(conn, "sanciones")
columnas_db <- columnas_db[columnas_db != "id_key"]
# Verificar que las columnas en los datos filtrados coincidan con las de la base de datos
cat("Columnas en la base de datos:\n")
print(columnas_db)
cat("Columnas en los datos filtrados:\n")
print(colnames(datos_filtrados))
# Renombrar las columnas del data frame para coincidir con la base de datos
colnames(datos_filtrados) <- columnas_db
# Insertar los datos filtrados en la tabla sanciones
dbWriteTable(conn, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)
# Cerrar la conexión a la base de datos
dbDisconnect(conn)
# Mensaje final
print("Datos filtrados e insertados en la base de datos correctamente.")
# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
# Ruta del archivo de entrada
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Fechas de inicio y fin para la selección de registros
fecha_inicio <- as.Date("2021-12-01")
fecha_fin <- as.Date("2025-01-23")
# Leer el archivo Excel, omitiendo la primera fila de títulos
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
# Verificar los resultados después de la conversión
cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos$fecha_aprobacion)), "\n")
cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos$fecha_solicitud)), "\n")
cat("Primeros valores de la columna 17 después de la conversión:\n")
head(datos$fecha_aprobacion)
cat("Primeros valores de la columna 6 después de la conversión:\n")
head(datos$fecha_solicitud)
# Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
datos <- datos %>%
  select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
# Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
datos_filtrados <- datos %>%
  filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
# Verificar las dimensiones después del filtrado
cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)
# Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
columnas_db <- dbListFields(conn, "sanciones")
columnas_db <- columnas_db[columnas_db != "id_key"]
# Renombrar las columnas del data frame para coincidir con la base de datos
colnames(datos_filtrados) <- columnas_db
# Convertir las fechas a texto en formato "YYYY-MM-DD"
datos_filtrados$fecha_solicitud <- format(datos_filtrados$fecha_solicitud, "%Y-%m-%d")
# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
# Ruta del archivo de entrada
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Fechas de inicio y fin para la selección de registros
fecha_inicio <- as.Date("2021-12-01")
fecha_fin <- as.Date("2025-01-23")
# Leer el archivo Excel, omitiendo la primera fila de títulos
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
# Verificar los resultados después de la conversión
cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos$fecha_aprobacion)), "\n")
cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos$fecha_solicitud)), "\n")
cat("Primeros valores de la columna 17 después de la conversión:\n")
head(datos$fecha_aprobacion)
cat("Primeros valores de la columna 6 después de la conversión:\n")
head(datos$fecha_solicitud)
# Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
datos <- datos %>%
  select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
# Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
datos_filtrados <- datos %>%
  filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
# Verificar las dimensiones después del filtrado
cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
# Asegurarse de que las columnas de fecha sean de tipo Date antes de aplicar format
datos_filtrados$fecha_solicitud <- as.Date(datos_filtrados$fecha_solicitud)
datos_filtrados$fecha_aprobacion <- as.Date(datos_filtrados$fecha_aprobacion)
# Convertir las fechas a texto en formato "YYYY-MM-DD"
datos_filtrados$fecha_solicitud <- format(datos_filtrados$fecha_solicitud, "%Y-%m-%d")
datos_filtrados$fecha_aprobacion <- format(datos_filtrados$fecha_aprobacion, "%Y-%m-%d")
# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)
# Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
columnas_db <- dbListFields(conn, "sanciones")
columnas_db <- columnas_db[columnas_db != "id_key"]
# Renombrar las columnas del data frame para coincidir con la base de datos
colnames(datos_filtrados) <- columnas_db
# Insertar los datos filtrados en la tabla sanciones
dbWriteTable(conn, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)
# Cerrar la conexión a la base de datos
dbDisconnect(conn)
# Mensaje final
print("Datos filtrados e insertados en la base de datos correctamente.")
# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
# Ruta del archivo de entrada
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Fechas de inicio y fin para la selección de registros
fecha_inicio <- as.Date("2021-12-01")
fecha_fin <- as.Date("2025-01-23")
# Leer el archivo Excel, omitiendo la primera fila de títulos
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
# Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
datos <- datos %>%
  select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
# Reordenar las columnas para restaurar el orden original
# Vamos a reinsertar las columnas de fecha en las posiciones correctas
colnames(datos)[colnames(datos) == "fecha_solicitud"] <- colnames(datos)[6] # Restablecer fecha_solicitud en su lugar
colnames(datos)[colnames(datos) == "fecha_aprobacion"] <- colnames(datos)[17] # Restablecer fecha_aprobacion en su lugar
# Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
datos_filtrados <- datos %>%
  filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
rlang::last_trace()
# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
# Ruta del archivo de entrada
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Fechas de inicio y fin para la selección de registros
fecha_inicio <- as.Date("2021-12-01")
fecha_fin <- as.Date("2025-01-23")
# Leer el archivo Excel, omitiendo la primera fila de títulos
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Asignar nombres únicos a las columnas para evitar duplicados
colnames(datos) <- make.names(colnames(datos), unique = TRUE)
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
# Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
datos <- datos %>%
  select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
# Reordenar las columnas para restaurar el orden original
# Vamos a reinsertar las columnas de fecha en las posiciones correctas
colnames(datos)[colnames(datos) == "fecha_solicitud"] <- colnames(datos)[6] # Restablecer fecha_solicitud en su lugar
colnames(datos)[colnames(datos) == "fecha_aprobacion"] <- colnames(datos)[17] # Restablecer fecha_aprobacion en su lugar
# Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
datos_filtrados <- datos %>%
  filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
rlang::last_trace()
View(datos)
# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
# Ruta del archivo de entrada
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Fechas de inicio y fin para la selección de registros
fecha_inicio <- as.Date("2021-12-01")
fecha_fin <- as.Date("2025-01-23")
# Leer el archivo Excel, omitiendo la primera fila de títulos
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Verificar los nombres de las columnas y corregir si hay duplicados
colnames(datos)
colnames(datos) <- make.names(colnames(datos), unique = TRUE)
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
# Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
datos <- datos %>%
  select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
# Reordenar las columnas para restaurar el orden original
# Vamos a reinsertar las columnas de fecha en las posiciones correctas
colnames(datos)[colnames(datos) == "fecha_solicitud"] <- colnames(datos)[6] # Restablecer fecha_solicitud en su lugar
colnames(datos)[colnames(datos) == "fecha_aprobacion"] <- colnames(datos)[17] # Restablecer fecha_aprobacion en su lugar
# Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
datos_filtrados <- datos %>%
  filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
rlang::last_trace()
rlang::last_trace()
filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  datos_filtrados <- datos %>%
  # Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
  colnames(datos)[colnames(datos) == "fecha_aprobacion"] <- colnames(datos)[17] # Restablecer fecha_aprobacion en su lugar
colnames(datos)[colnames(datos) == "fecha_solicitud"] <- colnames(datos)[6] # Restablecer fecha_solicitud en su lugar
# Vamos a reinsertar las columnas de fecha en las posiciones correctas
# Reordenar las columnas para restaurar el orden original
select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
datos <- datos %>%
  # Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
  datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
colnames(datos) <- make.names(colnames(datos), unique = TRUE)
colnames(datos)
# Verificar los nombres de las columnas y corregir si hay duplicados
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Leer el archivo Excel, omitiendo la primera fila de títulos
fecha_fin <- as.Date("2025-01-23")
fecha_inicio <- as.Date("2021-12-01")
# Fechas de inicio y fin para la selección de registros
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Ruta de la base de datos SQLite
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta del archivo de entrada
library(RSQLite)
library(DBI)
library(dplyr)
library(readxl)
# Cargar librerías necesarias
View(datos)
rlang::last_trace()
filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  datos_filtrados <- datos %>%
  # Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
  colnames(datos)[colnames(datos) == "fecha_aprobacion"] <- colnames(datos)[17] # Restablecer fecha_aprobacion en su lugar
colnames(datos)[colnames(datos) == "fecha_solicitud"] <- colnames(datos)[6] # Restablecer fecha_solicitud en su lugar
# Vamos a reinsertar las columnas de fecha en las posiciones correctas
# Reordenar las columnas para restaurar el orden original
select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
datos <- datos %>%
  # Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
  datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
colnames(datos) <- make.names(colnames(datos), unique = TRUE)
# Asignar nombres únicos a las columnas para evitar duplicados
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Leer el archivo Excel, omitiendo la primera fila de títulos
fecha_fin <- as.Date("2025-01-23")
fecha_inicio <- as.Date("2021-12-01")
# Fechas de inicio y fin para la selección de registros
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Ruta de la base de datos SQLite
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta del archivo de entrada
library(RSQLite)
library(DBI)
library(dplyr)
library(readxl)
# Cargar librerías necesarias
rlang::last_trace()
filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  datos_filtrados <- datos %>%
  # Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
  colnames(datos)[colnames(datos) == "fecha_aprobacion"] <- colnames(datos)[17] # Restablecer fecha_aprobacion en su lugar
colnames(datos)[colnames(datos) == "fecha_solicitud"] <- colnames(datos)[6] # Restablecer fecha_solicitud en su lugar
# Vamos a reinsertar las columnas de fecha en las posiciones correctas
# Reordenar las columnas para restaurar el orden original
select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
datos <- datos %>%
  # Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
  datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Leer el archivo Excel, omitiendo la primera fila de títulos
fecha_fin <- as.Date("2025-01-23")
fecha_inicio <- as.Date("2021-12-01")
# Fechas de inicio y fin para la selección de registros
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Ruta de la base de datos SQLite
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta del archivo de entrada
library(RSQLite)
library(DBI)
library(dplyr)
library(readxl)
# Cargar librerías necesarias
print("Datos filtrados e insertados en la base de datos correctamente.")
# Mensaje final
dbDisconnect(conn)
# Cerrar la conexión a la base de datos
dbWriteTable(conn, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)
# Insertar los datos filtrados en la tabla sanciones
colnames(datos_filtrados) <- columnas_db
# Renombrar las columnas del data frame para coincidir con la base de datos
columnas_db <- columnas_db[columnas_db != "id_key"]
columnas_db <- dbListFields(conn, "sanciones")
# Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
conn <- dbConnect(SQLite(), db_path)
# Conectar a la base de datos SQLite
datos_filtrados$fecha_aprobacion <- format(datos_filtrados$fecha_aprobacion, "%Y-%m-%d")
datos_filtrados$fecha_solicitud <- format(datos_filtrados$fecha_solicitud, "%Y-%m-%d")
# Convertir las fechas a texto en formato "YYYY-MM-DD"
datos_filtrados$fecha_aprobacion <- as.Date(datos_filtrados$fecha_aprobacion)
datos_filtrados$fecha_solicitud <- as.Date(datos_filtrados$fecha_solicitud)
# Asegurarse de que las columnas de fecha sean de tipo Date antes de aplicar format
cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
# Verificar las dimensiones después del filtrado
filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  datos_filtrados <- datos %>%
  # Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
  select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
datos <- datos %>%
  # Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
  head(datos$fecha_solicitud)
cat("Primeros valores de la columna 6 después de la conversión:\n")
head(datos$fecha_aprobacion)
cat("Primeros valores de la columna 17 después de la conversión:\n")
cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos$fecha_solicitud)), "\n")
cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos$fecha_aprobacion)), "\n")
# Verificar los resultados después de la conversión
datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Leer el archivo Excel, omitiendo la primera fila de títulos
fecha_fin <- as.Date("2025-01-23")
fecha_inicio <- as.Date("2021-12-01")
# Fechas de inicio y fin para la selección de registros
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Ruta de la base de datos SQLite
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta del archivo de entrada
library(RSQLite)
library(DBI)
library(dplyr)
library(readxl)
# Cargar librerías necesarias
datos_filtrados$fecha_solicitud <- format(datos_filtrados$fecha_solicitud, "%Y-%m-%d")
# Convertir las fechas a texto en formato "YYYY-MM-DD"
colnames(datos_filtrados) <- columnas_db
# Renombrar las columnas del data frame para coincidir con la base de datos
columnas_db <- columnas_db[columnas_db != "id_key"]
columnas_db <- dbListFields(conn, "sanciones")
# Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
conn <- dbConnect(SQLite(), db_path)
# Conectar a la base de datos SQLite
cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
# Verificar las dimensiones después del filtrado
filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  datos_filtrados <- datos %>%
  # Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
  select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
datos <- datos %>%
  # Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
  head(datos$fecha_solicitud)
cat("Primeros valores de la columna 6 después de la conversión:\n")
head(datos$fecha_aprobacion)
cat("Primeros valores de la columna 17 después de la conversión:\n")
cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos$fecha_solicitud)), "\n")
cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos$fecha_aprobacion)), "\n")
# Verificar los resultados después de la conversión
datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
# Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
# Leer el archivo Excel, omitiendo la primera fila de títulos
fecha_fin <- as.Date("2025-01-23")
fecha_inicio <- as.Date("2021-12-01")
# Fechas de inicio y fin para la selección de registros
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
# Ruta de la base de datos SQLite
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
# Ruta del archivo de entrada
library(RSQLite)
library(DBI)
library(dplyr)
library(readxl)
# Cargar librerías necesarias
print("Datos filtrados e insertados en la base de datos correctamente.")
# Mensaje final
dbDisconnect(conn)
# Cerrar la conexión a la base de datos
dbWriteTable(conn, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)
# Insertar los datos filtrados en la tabla sanciones
colnames(datos_filtrados) <- columnas_db
# Renombrar las columnas del data frame para coincidir con la base de datos
print(colnames(datos_filtrados))
cat("Columnas en los datos filtrados:\n")
print(columnas_db)
cat("Columnas en la base de datos:\n")
# Verificar que las columnas en los datos filtrados coincidan con las de la base de datos
columnas_db <- columnas_db[columnas_db != "id_key"]
columnas_db <- dbListFields(conn, "sanciones")
# Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
conn <- dbConnect(SQLite(), db_path)
# Conectar a la base de datos SQLite
cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
# Verificar las dimensiones después del filtrado
filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
  datos_filtrados <- datos %>%
  # Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
  cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos$fecha_solicitud)), "\n")
cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos$fecha_aprobacion)), "\n")
# Verificar los resultados después de la conversión
datos$fecha_solicitud <- as.Date(datos$fecha_solicitud, format = "%Y-%m-%d")
datos$fecha_aprobacion <- as.Date(datos$fecha_aprobacion, format = "%Y-%m-%d")
# Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
"causa_sancion", "solicitado_por", "analista_rl", "tipo_sancion", "dias_suspension", "fecha_aprobacion")
"clasificacion_sancion", "motivo_sancion", "detalle_sancion", "descripcion_breve", "acta_hechos",
colnames(datos) <- c("id_colaborador", "nombre", "antiguedad_meses", "antiguedad_years", "id_sancion", "fecha_solicitud",
                     # Renombrar las columnas del archivo para evitar el uso de nombres automáticos como "...1", "...2", etc.
                     head(datos)
                     # Verificar las primeras filas para asegurarse de que los datos se han cargado correctamente
                     datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
                     # Leer el archivo Excel, omitiendo la primera fila de títulos
                     fecha_fin <- as.Date("2025-01-23")
                     fecha_inicio <- as.Date("2021-12-01")
                     # Fechas de inicio y fin para la selección de registros
                     db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
                     # Ruta de la base de datos SQLite
                     ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Sanciones.xlsx"
                     # Ruta del archivo de entrada
                     library(RSQLite)
                     library(DBI)
                     library(dplyr)
                     library(readxl)
                     # Cargar librerías necesarias
                     print("Datos filtrados e insertados en la base de datos correctamente.")
                     dbDisconnect(conn)
                     # Cerrar la conexión a la base de datos
                     dbWriteTable(conn, "sanciones", datos_filtrados, append = TRUE, row.names = FALSE)
                     # Insertar los datos filtrados en la tabla sanciones
                     print(colnames(datos_filtrados))
                     cat("Nombres de columnas en los datos filtrados:\n")
                     # Verificar que las columnas coincidan
                     colnames(datos_filtrados) <- columnas_db
                     # Renombrar las columnas del data frame para coincidir con la base de datos
                     columnas_db <- columnas_db[columnas_db != "id_key"]
                     columnas_db <- dbListFields(conn, "sanciones")
                     # Obtener los nombres de las columnas de la tabla SQLite (excluyendo id_key)
                     conn <- dbConnect(SQLite(), db_path)
                     # Conectar a la base de datos SQLite
                     cat("Dimensión después del filtrado:", nrow(datos_filtrados), "\n")
                     # Verificar las dimensiones después del filtrado
                     filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin) # Filtrar por rango de fechas
                     filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>% # Filtrar registros válidos
                       datos_filtrados <- datos %>%
                       # Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
                       select(-c(17, 6)) # Eliminar las columnas originales que no son necesarias
                     datos <- datos %>%
                       # Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
                       head(datos$fecha_solicitud)
                     cat("Primeros valores de la columna 6 después de la conversión:\n")
                     head(datos$fecha_aprobacion)
                     cat("Primeros valores de la columna 17 después de la conversión:\n")
                     cat("Número de valores NA en columna 6 (fecha_solicitud):", sum(is.na(datos$fecha_solicitud)), "\n")
                     cat("Número de valores NA en columna 17 (fecha_aprobacion):", sum(is.na(datos$fecha_aprobacion)), "\n")
                     # Verificar los resultados después de la conversión
                     datos$fecha_solicitud <- as.Date(datos[[6]]) # Convertir la columna 6 a solo fecha
                     datos$fecha_aprobacion <- as.Date(datos[[17]]) # Convertir la columna 17 a solo fecha
                     # Convertir las columnas a solo fecha (sin hora) y asignarlas a las columnas correctas
                     datos[[6]] <- as.POSIXct(datos[[6]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
                     # Convertir la columna 6 (fecha_solicitud) a formato POSIXct (fecha y hora)
                     datos[[17]] <- as.POSIXct(datos[[17]], format = "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")
                     # Asegurarse de que la columna 17 (fecha_aprobacion) esté en formato POSIXct (fecha y hora)
                     datos <- read_excel(ruta_archivo, skip = 1, col_names = FALSE)
                     # Leer el archivo Excel, omitiendo la primera fila de títulos
                     fecha_fin <- as.Date("2025-01-23")
                     fecha_inicio <- as.Date("2021-12-01")
                     # Fechas de inicio y fin para la selección de registros
                     # Cargar librerías necesarias
                     library(readxl)
                     
                     
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

########################


10/01/2025  01:27:25 p. m.
45667.5607060185
