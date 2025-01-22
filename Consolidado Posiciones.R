library(dplyr)

# Definir la ruta de los archivos
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Posiciones_Hist/"

# Definir rango de fechas
start_date <- as.Date("2024-12-01")
end_date <- as.Date("2024-12-02")

# Inicializar un dataframe vacío para almacenar los datos consolidados
consolidated_data <- data.frame()

# Recorrer fechas y procesar archivos
for (date in seq(start_date, end_date, by = "day")) {
  file_name <- paste0("Posiciones_", format(date, "%Y%m%d"), ".csv")
  file_full_path <- file.path(file_path, file_name)

  if (file.exists(file_full_path)) {
    print(paste("Procesando archivo:", file_name))
    tryCatch({
      # Leer archivo CSV con UTF-8, detectando separador y revisando nombres de columnas
      data <- read.csv(file_full_path, fileEncoding = "UTF-8", skip = 1, stringsAsFactors = FALSE, na.strings = c("", "NA"))

      # Verificar contenido del archivo
      if (nrow(data) == 0) {
        stop(paste("El archivo", file_name, "está vacío o mal estructurado."))
      }

      # Mostrar columnas y filas cargadas para depuración
      print(paste("Columnas detectadas en", file_name, ":", paste(colnames(data), collapse = ", ")))
      print(paste("Número de filas:", nrow(data)))

      # Normalizar nombres de columnas para evitar errores por acentos o espacios
      colnames(data) <- iconv(colnames(data), from = "UTF-8", to = "ASCII//TRANSLIT")

      # Convertir todas las columnas de texto para evitar problemas de codificación
      data[] <- lapply(data, function(x) {
        if (is.character(x)) {
          iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
        } else {
          x
        }
      })

      # Agregar la columna de fecha con formato seguro
      data$fecha_info <- as.character(format(date, "%Y-%m-%d"))

      # Consolidar datos usando bind_rows
      consolidated_data <- bind_rows(consolidated_data, data)

      print(paste("Archivo procesado correctamente:", file_name, "- Filas agregadas:", nrow(data)))

    }, error = function(e) {
      print(paste("Error al procesar archivo:", file_name, "-", conditionMessage(e)))
    })
  } else {
    print(paste("Archivo no encontrado:", file_name))
  }
}

# Verificar si se consolidaron registros
if (nrow(consolidated_data) == 0) {
  print("No se consolidaron registros. Verificar los archivos de origen.")
} else {
  # Guardar el consolidado en un archivo CSV con UTF-8
  write.csv(consolidated_data, file.path(file_path, "Consolidado_Posiciones.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  print("Consolidado guardado exitosamente.")
}

print(paste("Total de registros consolidados:", nrow(consolidated_data)))
print("Proceso completado exitosamente.")
