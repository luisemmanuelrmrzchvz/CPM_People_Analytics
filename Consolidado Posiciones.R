library(readxl) 
library(dplyr)
library(janitor)

file_path <- "C:/Users/racl26345/Documents/Proyecto Capacity/Posiciones_Hist"

start_date <- as.Date("2024-12-01")
end_date <- as.Date("2024-12-10")

consolidated_data <- data.frame()

for (date in seq(start_date, end_date, by = "day")) {
  file_name <- paste0("Posiciones_", format(date, "%Y%m%d"), ".xls")
  file_full_path <- file.path(file_path, file_name)

  if (file.exists(file_full_path)) {
    tryCatch({
      data <- read_excel(file_full_path, skip = 1, col_names = TRUE, .name_repair = "minimal")
      
      # Limpiar nombres de columnas
      names(data) <- iconv(names(data), from = "UTF-8", to = "ASCII//TRANSLIT")
      
      # Convertir los registros de texto para eliminar acentos
      data[] <- lapply(data, function(x) if (is.character(x)) iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT") else x)
      
      # Añadir columna de fecha
      data$fecha_info <- as.character(date)
      
      consolidated_data <- bind_rows(consolidated_data, data)
      
      print(paste("Archivo procesado:", file_name, "- Filas:", nrow(data)))

    }, error = function(e) {
      print(paste("Error al leer:", file_name, "-", e$message))
    })
  } else {
    print(paste("Archivo no encontrado:", file_name))
  }
}

print(paste("Total de registros consolidados:", nrow(consolidated_data)))
print("Proceso completado exitosamente.")
