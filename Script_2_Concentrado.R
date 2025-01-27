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
datos$fecha_aprobacion <- as.Date(datos[[17]])  # Convertir la columna 17 a solo fecha
datos$fecha_solicitud <- as.Date(datos[[6]])    # Convertir la columna 6 a solo fecha

# Eliminar las columnas adicionales que fueron generadas en el proceso de conversión
datos <- datos %>%
  select(-c(17, 6))  # Eliminar las columnas originales que no son necesarias

# Reordenar las columnas para restaurar el orden original
# Vamos a reinsertar las columnas de fecha en las posiciones correctas
colnames(datos)[colnames(datos) == "fecha_solicitud"] <- colnames(datos)[6]  # Restablecer fecha_solicitud en su lugar
colnames(datos)[colnames(datos) == "fecha_aprobacion"] <- colnames(datos)[17]  # Restablecer fecha_aprobacion en su lugar

# Filtrar los registros en el rango de fechas de las columnas fecha_aprobacion y fecha_solicitud
datos_filtrados <- datos %>%
  filter(!is.na(fecha_aprobacion) & !is.na(fecha_solicitud)) %>%  # Filtrar registros válidos
  filter(fecha_aprobacion >= fecha_inicio & fecha_aprobacion <= fecha_fin)  # Filtrar por rango de fechas

# Mostrar el resultado
head(datos_filtrados)
