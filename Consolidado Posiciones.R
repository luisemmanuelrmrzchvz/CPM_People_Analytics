library(dplyr)

# Definir la ruta de los archivos
file_path <- "C:/Users/racl26345/Documents/Proyecto Capacity/Posiciones_Hist"

# Definir rango de fechas
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2024-12-10")

# Inicializar un dataframe vacío para almacenar los datos consolidados
consolidated_data <- data.frame()

# Recorrer fechas y procesar archivos
for (date in seq(start_date, end_date, by = "day")) {
  file_name <- paste0("Posiciones_", format(date, "%Y%m%d"), ".csv")
  file_full_path <- file.path(file_path, file_name)

  if (file.exists(file_full_path)) {
    tryCatch({
      # Leer el archivo CSV con codificación UTF-8, omitir la primera fila (títulos) y manejar caracteres especiales
      data <- read.csv(file_full_path, fileEncoding = "UTF-8", skip = 1, stringsAsFactors = FALSE)
      
      # Limpiar nombres de columnas eliminando acentos y caracteres especiales
      names(data) <- iconv(names(data), from = "UTF-8", to = "ASCII//TRANSLIT")

      # Convertir los registros de texto para eliminar acentos
      data[] <- lapply(data, function(x) if (is.character(x)) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT") else x)

      # Añadir columna de fecha del archivo
      data$fecha_info <- as.character(date)
      
      # Consolidar los datos
      consolidated_data <- bind_rows(consolidated_data, data)

      print(paste("Archivo procesado:", file_name, "- Filas:", nrow(data)))

    }, error = function(e) {
      print(paste("Error al leer:", file_name, "-", e$message))
    })
  } else {
    print(paste("Archivo no encontrado:", file_name))
  }
}

# Verificar el número total de registros consolidados
print(paste("Total de registros consolidados:", nrow(consolidated_data)))

# Guardar el consolidado en un nuevo archivo CSV
write.csv(consolidated_data, file.path(file_path, "Consolidado_Posiciones.csv"), row.names = FALSE, fileEncoding = "UTF-8")

print("Proceso completado exitosamente.")
