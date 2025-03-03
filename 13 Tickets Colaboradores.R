# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
library(lubridate)

# Definir la ruta del archivo de Excel
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/CPM_04_Creador.xlsx"

# Leer y preparar datos
datos <- read_excel(ruta_archivo, range = cell_rows(11:100000), col_names = FALSE) %>%
  select(c(2, 3, 4, 9, 10, 12, 13, 14)) %>%
  setNames(c("id_ticket", "id_colaborador", "nombre_cliente", "fecha_creado", "estado_ticket", 
             "agente_servicio", "nombre_procesador", "id_procesador")) %>%
  mutate(
    across(everything(), ~ gsub("[\r\n]", "", .)),
    fecha_creado = format(as.Date(fecha_creado, format = "%m/%d/%Y"), "%Y-%m-%d"),
    id_ticket = as.character(id_ticket),
    id_colaborador = as.character(id_colaborador),
    id_procesador = as.character(id_procesador)
  )

# Conectar a la base de datos
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(SQLite(), db_path)

# Obtener tickets válidos y registros existentes
id_ticket_validos <- dbGetQuery(conn, "SELECT id_ticket FROM codigo_tickets")$id_ticket
existentes <- dbGetQuery(conn, 
                         "SELECT id_ticket, id_colaborador
   FROM colaboradores_tickets") %>%
  mutate(
    id_ticket = as.character(id_ticket),
    id_colaborador = as.character(id_colaborador)
  )

# Filtrar y preparar datos
datos <- datos %>%
  filter(id_ticket %in% id_ticket_validos) %>%
  mutate(id_ticket = as.character(id_ticket)) %>%
  anti_join(existentes, by = c("id_ticket", "id_colaborador"))

# 1. ACTUALIZAR REGISTROS ABIERTOS
# ----------------------------------------------------------
open_records <- dbGetQuery(conn, 
                           "SELECT id_ticket, id_colaborador
   FROM colaboradores_tickets 
   WHERE estado_ticket != 'Cerrado'") %>%
  mutate(
    id_ticket = as.character(id_ticket),
    id_colaborador = as.character(id_colaborador)  # Convertir a character
  )

if(nrow(open_records) > 0 && nrow(datos) > 0){
  actualizaciones <- datos %>%
    inner_join(open_records, by = c("id_ticket", "id_colaborador"))
  
  if(nrow(actualizaciones) > 0){
    for(i in 1:nrow(actualizaciones)){
      registro <- actualizaciones[i, ]
      
      dbExecute(conn, 
                "UPDATE colaboradores_tickets SET 
          estado_ticket = ?,
          agente_servicio = ?,
          nombre_procesador = ?,
          id_procesador = ?
         WHERE id_ticket = ? 
           AND id_colaborador = ?
           AND estado_ticket != 'Cerrado'",
                params = list(
                  registro$estado_ticket,
                  registro$agente_servicio,
                  registro$nombre_procesador,
                  registro$id_procesador,
                  registro$id_ticket,
                  registro$id_colaborador
                )
      )
    }
    print(paste("Actualizados", nrow(actualizaciones), "registros abiertos"))
  }
}

# 2. INSERTAR NUEVOS REGISTROS
# ----------------------------------------------------------
nuevos_registros <- datos %>%
  anti_join(existentes, by = c("id_ticket", "id_colaborador"))

if(nrow(nuevos_registros) > 0){
  # Insertar solo campos relevantes para histórico
  dbWriteTable(conn, "colaboradores_tickets", 
               nuevos_registros %>%
                 select(id_ticket, id_colaborador, nombre_cliente, fecha_creado,
                        estado_ticket, agente_servicio, nombre_procesador,
                        id_procesador),
               append = TRUE, row.names = FALSE)
  print(paste("Insertados", nrow(nuevos_registros), "nuevos registros"))
} else {
  print("No hay nuevos registros para insertar")
}

# Cerrar conexión
dbDisconnect(conn)
print("Proceso completado exitosamente")