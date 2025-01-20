library(readxl) 
library(dplyr)

file_path <- "C:/Users/racl26345/Documents/Proyecto Capacity/Posiciones_Hist"

start_date <- as.Date("2024-12-01")
end_date <- as.Date("2024-12-10")

consolidated_data <- data.frame()

for (date in seq(start_date, end_date, by = "day")) {
  file_name <- paste0("Posiciones_", format(date, "%Y%m%d"), ".xls")
  file_full_path <- file.path(file_path, file_name)

  if (file.exists(file_full_path)) {
    tryCatch({
      data <- read_excel(file_full_path, skip = 1, col_names = TRUE)
      data$fecha_info <- format(date, "%Y-%m-%d")
      consolidated_data <- bind_rows(consolidated_data, data)
    }, error = function(e) {
      print(paste("Error al leer:", file_full_path, "-", e$message))
    })
  } else {
    print(paste("Archivo no encontrado:", file_full_path))
  }
}

print(paste("Total de registros consolidados:", nrow(consolidated_data)))
print("Proceso completado exitosamente.")


############################################################################

> library(readxl) 
Aviso:
  package ‘readxl’ was built under R version 4.4.2 
> library(dplyr)

Adjuntando el paquete: ‘dplyr’

The following objects are masked from ‘package:stats’:
  
  filter, lag

The following objects are masked from ‘package:base’:
  
  intersect, setdiff, setequal, union

> 
  > file_path <- "C:/Users/racl26345/Documents/Proyecto Capacity/Posiciones_Hist"
> 
  > start_date <- as.Date("2024-12-01")
> end_date <- as.Date("2024-12-10")
> 
  > consolidated_data <- data.frame()
> 
  > for (date in seq(start_date, end_date, by = "day")) {
    +   file_name <- paste0("Posiciones_", format(date, "%Y%m%d"), ".xls")
    +   file_full_path <- file.path(file_path, file_name)
    + 
      +   if (file.exists(file_full_path)) {
        +     tryCatch({
          +       data <- read_excel(file_full_path, skip = 1, col_names = TRUE)
          +       data$fecha_info <- format(date, "%Y-%m-%d")
          +       consolidated_data <- bind_rows(consolidated_data, data)
          +     }, error = function(e) {
            +       print(paste("Error al leer:", file_full_path, "-", e$message))
            +     })
        +   } else {
          +     print(paste("Archivo no encontrado:", file_full_path))
          +   }
    + }
Error en prettyNum(.Internal(format(x, trim, digits, nsmall, width, 3L, : 
                                      argumento 'trim' inválido