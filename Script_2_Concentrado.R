# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(readr)

# Ruta de la carpeta de entrada
ruta_carpeta <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Posiciones_Hist"

# Fechas de inicio y fin para la búsqueda de archivos
fecha_inicio <- as.Date("2024-12-01")
fecha_fin <- as.Date("2024-12-02")

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
  
  # Guardar el archivo consolidado en formato CSV en la misma carpeta
  archivo_salida <- file.path(ruta_carpeta, "Posiciones_Consolidado.csv")
  write_csv(datos_consolidados, archivo_salida, col_names = FALSE)
  
  print(paste("Archivo consolidado guardado en:", archivo_salida))
} else {
  print("No se encontraron archivos en el rango de fechas especificado.")
}