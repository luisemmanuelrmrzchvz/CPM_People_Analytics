# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
library(lubridate)

# Definir la ruta del archivo de Excel
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/CPM_10_Historico_Estados.xlsx"

# Leer y preparar datos
datos <- read_excel(ruta_archivo, range = cell_rows(11:100000), col_names = FALSE) %>%
  select(c(1, 3, 7, 9, 10, 11, 12, 13, 14, 15)) %>%
  setNames(c("id_ticket", "fecha_creado", "agente_servicio", "prioridad", 
             "time_start_status", "time_end_status", "code_estado_ticket", 
             "estado_ticket", "siguiente_accion_para", "seg_duracion")) %>%
  mutate(
    across(everything(), ~ gsub("[\r\n]", "", .)),
    fecha_creado = format(as.Date(fecha_creado, format = "%m/%d/%Y"), "%Y-%m-%d"),
    time_start_status = format(as.POSIXct(time_start_status, 
                                          format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
                               "%Y-%m-%d %H:%M:%S"),
    time_end_status = format(as.POSIXct(time_end_status,
                                        format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
                             "%Y-%m-%d %H:%M:%S"),
    id_ticket = as.character(id_ticket),
    code_estado_ticket = as.character(code_estado_ticket)  # Asegurar que sea character
  )

# Conectar a la base de datos
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(SQLite(), db_path)

# Obtener tickets válidos y registros existentes
id_ticket_validos <- dbGetQuery(conn, "SELECT id_ticket FROM codigo_tickets")$id_ticket
existentes <- dbGetQuery(conn, 
                         "SELECT id_ticket, time_start_status, code_estado_ticket 
   FROM hist_status_tickets") %>%
  mutate(
    id_ticket = as.character(id_ticket),
    code_estado_ticket = as.character(code_estado_ticket)  # Convertir a character
  )

# Filtrar y preparar datos
datos <- datos %>%
  filter(id_ticket %in% id_ticket_validos) %>%
  mutate(id_ticket = as.character(id_ticket)) %>%
  anti_join(existentes, by = c("id_ticket", "time_start_status", "code_estado_ticket"))

# 1. ACTUALIZAR REGISTROS ABIERTOS
# ----------------------------------------------------------
open_records <- dbGetQuery(conn, 
                           "SELECT id_ticket, time_start_status, code_estado_ticket 
   FROM hist_status_tickets 
   WHERE time_end_status = '9999-12-30 00:00:00' 
     AND estado_ticket != 'Cerrado'") %>%
  mutate(
    id_ticket = as.character(id_ticket),
    code_estado_ticket = as.character(code_estado_ticket)  # Convertir a character
  )

if(nrow(open_records) > 0 && nrow(datos) > 0){
  actualizaciones <- datos %>%
    inner_join(open_records, by = c("id_ticket", "time_start_status", "code_estado_ticket"))
  
  if(nrow(actualizaciones) > 0){
    for(i in 1:nrow(actualizaciones)){
      registro <- actualizaciones[i, ]
      
      dbExecute(conn, 
                "UPDATE hist_status_tickets SET 
          time_end_status = ?,
          seg_duracion = ?,
          agente_servicio = ?
         WHERE id_ticket = ? 
           AND time_start_status = ? 
           AND code_estado_ticket = ? 
           AND estado_ticket != 'Cerrado' 
           AND time_end_status = '9999-12-30 00:00:00'",
                params = list(
                  registro$time_end_status,
                  registro$seg_duracion,
                  registro$agente_servicio,
                  registro$id_ticket,
                  registro$time_start_status,
                  registro$code_estado_ticket
                )
      )
    }
    print(paste("Actualizados", nrow(actualizaciones), "registros abiertos"))
  }
}

# 2. INSERTAR NUEVOS REGISTROS
# ----------------------------------------------------------
nuevos_registros <- datos %>%
  anti_join(existentes, by = c("id_ticket", "time_start_status", "code_estado_ticket"))

if(nrow(nuevos_registros) > 0){
  # Insertar solo campos relevantes para histórico
  dbWriteTable(conn, "hist_status_tickets", 
               nuevos_registros %>%
                 select(id_ticket, fecha_creado, agente_servicio, prioridad,
                        time_start_status, time_end_status, code_estado_ticket,
                        estado_ticket, siguiente_accion_para, seg_duracion),
               append = TRUE, row.names = FALSE)
  print(paste("Insertados", nrow(nuevos_registros), "nuevos registros"))
} else {
  print("No hay nuevos registros para insertar")
}

# Cerrar conexión
dbDisconnect(conn)
print("Proceso completado exitosamente")