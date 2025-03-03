########################################################################
########################## QUERY SQLite ################################
########################################################################
########################################################################

# Cargar las librerías necesarias
library(DBI)        # Para conectarse a SQLite
library(openxlsx)   # Para crear archivos de Excel

# 1. Conectar a la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(RSQLite::SQLite(), db_path)

# 2. Definir la consulta SQL que deseas ejecutar
query <- "
WITH RECURSIVE dates AS (
    SELECT DATE('2025-01-01') AS fecha
    UNION ALL
    SELECT DATE(fecha, '+1 day')
    FROM dates
    WHERE fecha < DATE('2025-01-31')
),

down_positions AS (
    SELECT hist_posiciones.id_posicion
    FROM hist_posiciones
    WHERE (hist_posiciones.status = 'I' AND DATE(hist_posiciones.fecha_inicio) <= '2025-01-31')
),

daily_comparison AS (
    SELECT 
        d.fecha,
        yesterday.id_posicion AS yesterday_id_posicion,
        yesterday.id_colaborador AS yesterday_id_colaborador,
        yesterday.status AS yesterday_status,
        today.id_posicion AS today_id_posicion,
        today.id_colaborador AS today_id_colaborador,
        today.status AS today_status,
        today.area_de_cobranza,
        today.nivel_gestion,
        today.vacante
    FROM dates d
    LEFT JOIN hist_posiciones yesterday 
        ON DATE(yesterday.fecha_daily) = DATE(d.fecha, '-1 day')
        AND yesterday.id_posicion NOT IN (SELECT id_posicion FROM down_positions)
    LEFT JOIN hist_posiciones today 
        ON DATE(today.fecha_daily) = d.fecha
        AND today.id_posicion NOT IN (SELECT id_posicion FROM down_positions)
),

inactivations AS (
    SELECT 
        fecha,
        today_id_posicion AS id_posicion
    FROM daily_comparison
    WHERE today_status = 'I'
        AND yesterday_status = 'A'
),

news AS (
    SELECT 
        fecha,
        today_id_posicion AS id_posicion
    FROM daily_comparison
    WHERE today_id_posicion NOT IN (SELECT yesterday_id_posicion FROM daily_comparison WHERE fecha = daily_comparison.fecha)
        AND today_status = 'A'
),

transfers AS (
    SELECT 
        fecha,
        today_id_posicion AS id_posicion,
        today_id_colaborador,
        yesterday_id_colaborador
    FROM daily_comparison
    WHERE today_status = 'A'
        AND (today_id_colaborador IS NOT NULL AND yesterday_id_colaborador IS NOT NULL)
        AND today_id_colaborador <> yesterday_id_colaborador
),

termination AS (
    SELECT 
        fecha,
        today_id_posicion AS id_posicion,
        today_id_colaborador,
        yesterday_id_colaborador
    FROM daily_comparison
    WHERE today_status = 'A'
        AND (today_id_colaborador IS NULL AND yesterday_id_colaborador IS NOT NULL)
),

hires AS (
    SELECT 
        fecha,
        today_id_posicion AS id_posicion,
        today_id_colaborador,
        yesterday_id_colaborador
    FROM daily_comparison
    WHERE today_status = 'A'
        AND (today_id_colaborador IS NOT NULL AND yesterday_id_colaborador IS NULL)
),

status AS (
    SELECT
        dc.fecha,
        dc.today_id_posicion AS id_posicion,
        dc.today_id_colaborador,
        CASE WHEN dc.area_de_cobranza = 'Cobranza administrativa' THEN 'COBRANZA'
            WHEN dc.area_de_cobranza = 'Cobranza en campo' THEN 'COBRANZA'
            ELSE dc.nivel_gestion END AS nivel_gestion,
        CASE WHEN dc.today_id_posicion IN (SELECT id_posicion FROM inactivations WHERE fecha = dc.fecha) THEN 'Posicion Inactivada'
            WHEN dc.today_id_posicion IN (SELECT id_posicion FROM news WHERE fecha = dc.fecha) THEN 'Posicion Creada'
            WHEN dc.today_id_posicion IN (SELECT id_posicion FROM termination WHERE fecha = dc.fecha) THEN 'Posicion Vacante'
            WHEN dc.today_id_posicion IN (SELECT id_posicion FROM hires WHERE fecha = dc.fecha) THEN 'Posicion Cubierta'
            WHEN dc.today_status = 'I' THEN 'Sin Cambios - Posiciones Inactivas'
            WHEN dc.vacante = 'True' THEN 'Sin Cambios - Posicion Activa Vacante'   
            ELSE 'Sin Cambios - Posicion Activa Ocupada' END AS Cambios
    FROM daily_comparison dc
)

SELECT
    s.fecha,
    s.nivel_gestion,
    s.Cambios,
    COUNT(s.id_posicion) AS Total_Posiciones
FROM status s
GROUP BY s.fecha, s.nivel_gestion, s.Cambios
ORDER BY s.fecha, s.nivel_gestion, s.Cambios;
"

# 3. Ejecutar la consulta y obtener los resultados
resultados <- dbGetQuery(conn, query)

# 4. Cerrar la conexión a la base de datos
dbDisconnect(conn)

# 5. Guardar los resultados en un archivo de Excel
output_path <- "C:/Users/racl26345/Downloads/Resultado Query SQLite.xlsx"
write.xlsx(resultados, file = output_path, rowNames = FALSE)

# 6. Mensaje de confirmación
cat("Los resultados se han guardado en:", output_path, "\n")


########################################################################
########################################################################
########################################################################
########################################################################
############################# PRUEBAS ##################################
########################################################################
########################################################################


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
# Insertar todos los registros sin filtrar por fecha
dbWriteTable(conn, "hist_status_tickets", datos, 
             append = TRUE, row.names = FALSE)
print(paste("Insertados", nrow(datos), "nuevos registros"))

# Cerrar conexión
dbDisconnect(conn)
print("Proceso completado exitosamente")