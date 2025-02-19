# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
library(lubridate)

# Definir la ruta del archivo de Excel
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/CPM_10_Historico_Estados.xlsx"

# Leer el archivo de Excel, omitiendo las primeras 10 filas
datos <- read_excel(ruta_archivo, range = cell_rows(11:100000), col_names = FALSE)

# Asignar nombres a las columnas seleccionadas (columnas 1, 3, 7, 9-15)
nombres_columnas <- c("id_ticket", "fecha_creado", "agente_servicio", "prioridad", 
                      "time_start_status", "time_end_status", "code_estado_ticket", 
                      "estado_ticket", "siguiente_accion_para", "seg_duracion")

# Seleccionar y renombrar columnas
datos <- datos %>%
  select(c(1, 3, 7, 9, 10, 11, 12, 13, 14, 15)) %>%
  setNames(nombres_columnas)

# Eliminar saltos de línea en todas las columnas
datos <- datos %>%
  mutate(across(everything(), ~ gsub("[\r\n]", "", .)))

# Convertir formatos de fecha y hora
datos <- datos %>%
  mutate(
    fecha_creado = format(as.Date(fecha_creado, format = "%m/%d/%Y"), "%Y-%m-%d"),
    time_start_status = format(as.POSIXct(time_start_status, 
                                          format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
                               "%Y-%m-%d %H:%M:%S"),
    time_end_status = format(as.POSIXct(time_end_status,
                                        format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
                             "%Y-%m-%d %H:%M:%S"),
    id_ticket = as.character(id_ticket)  # Convertir id_ticket a character
  )

# Conectar a la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(SQLite(), db_path)

# Obtener tickets válidos y fecha máxima de referencia
id_ticket_validos <- dbGetQuery(conn, "SELECT id_ticket FROM codigo_tickets")$id_ticket
max_fecha_db <- dbGetQuery(conn, "SELECT MAX(fecha_creado) FROM hist_status_tickets")[[1]]

# Manejar caso de primera ejecución
if (is.na(max_fecha_db)) max_fecha_db <- "1900-01-01"

# Filtrar datos por tickets válidos
datos <- datos %>% filter(id_ticket %in% id_ticket_validos)

# 1. ACTUALIZAR REGISTROS ABIERTOS EXISTENTES
# ----------------------------------------------------------
# Obtener registros abiertos de la base de datos
open_records <- dbGetQuery(conn, 
                           "SELECT id_ticket, time_start_status 
   FROM hist_status_tickets 
   WHERE time_end_status = '9999-12-30 00:00:00' 
     AND estado_ticket != 'Cerrado'")

# Convertir id_ticket a character en open_records
open_records <- open_records %>%
  mutate(id_ticket = as.character(id_ticket))

if (nrow(open_records) > 0) {
  # Encontrar coincidencias en los datos nuevos
  actualizaciones <- datos %>%
    inner_join(open_records, by = c("id_ticket", "time_start_status"))
  
  if (nrow(actualizaciones) > 0) {
    # Actualizar registros en la base de datos
    for (i in 1:nrow(actualizaciones)) {
      registro <- actualizaciones[i, ]
      
      dbExecute(conn, 
                "UPDATE hist_status_tickets SET 
          fecha_creado = ?,
          agente_servicio = ?,
          prioridad = ?,
          time_end_status = ?,
          code_estado_ticket = ?,
          estado_ticket = ?,
          siguiente_accion_para = ?,
          seg_duracion = ?
        WHERE id_ticket = ? AND time_start_status = ?",
                params = list(
                  registro$fecha_creado,
                  registro$agente_servicio,
                  registro$prioridad,
                  registro$time_end_status,
                  registro$code_estado_ticket,
                  registro$estado_ticket,
                  registro$siguiente_accion_para,
                  registro$seg_duracion,
                  registro$id_ticket,
                  registro$time_start_status
                )
      )
    }
    print(paste("Actualizados", nrow(actualizaciones), "registros existentes"))
  }
}

# 2. INSERTAR NUEVOS REGISTROS
# ----------------------------------------------------------
# Filtrar datos nuevos posteriores a la última fecha registrada
nuevos_registros <- datos %>%
  filter(as.Date(fecha_creado) > as.Date(max_fecha_db))

if (nrow(nuevos_registros) > 0) {
  # Insertar nuevos registros
  dbWriteTable(conn, "hist_status_tickets", nuevos_registros, 
               append = TRUE, row.names = FALSE)
  print(paste("Insertados", nrow(nuevos_registros), "nuevos registros"))
} else {
  print("No hay nuevos registros para insertar")
}

# Cerrar conexión
dbDisconnect(conn)
print("Proceso completado exitosamente")