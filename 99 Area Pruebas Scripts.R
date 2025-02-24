# Cargar librerías necesarias
library(DBI)
library(RSQLite)
library(dplyr)
library(openxlsx)

# Paso 1: Conectar a la base de datos SQLite y extraer datos de los días relevantes
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

# Conectar a la base de datos
conn <- dbConnect(SQLite(), db_path)

# Definir las fechas de inicio y fin para la comparación
start_date <- '2024-12-31'
end_date <- '2025-01-31'

# Crear un vector de fechas
dates <- seq(as.Date(start_date), as.Date(end_date), by = "days")

# Crear una lista vacía para almacenar los resultados
result_list <- list()

# Ciclo para comparar los datos de cada día con el día anterior
for (i in 2:length(dates)) {  # Comenzamos desde el segundo día
  # Fecha de hoy (día actual)
  today_date <- dates[i]
  # Fecha de ayer (día anterior)
  previous_date <- dates[i - 1]
  
  # Consultas para obtener los datos de hoy y ayer
  query_today <- sprintf("
    SELECT 
      id_posicion, 
      id_colaborador, 
      status, 
      vacante, 
      fecha_daily
    FROM hist_posiciones
    WHERE fecha_daily = '%s';
  ", today_date)
  
  query_previous <- sprintf("
    SELECT 
      id_posicion, 
      id_colaborador, 
      status, 
      vacante, 
      fecha_daily
    FROM hist_posiciones
    WHERE fecha_daily = '%s';
  ", previous_date)
  
  # Obtener los datos de hoy y ayer
  data_today <- dbGetQuery(conn, query_today)
  data_previous <- dbGetQuery(conn, query_previous)
  
  # Comparar los datos entre hoy y ayer
  comparison <- data_today %>%
    left_join(data_previous, by = "id_posicion", suffix = c("_today", "_previous")) %>%
    mutate(
      Cambios = case_when(
        is.na(id_colaborador_previous) & !is.na(id_colaborador_today) ~ 'Nueva Posicion Ocupada',
        is.na(id_colaborador_previous) & is.na(id_colaborador_today) ~ 'Nueva Posicion Vacante',
        status_today == 'I' & is.na(id_colaborador_previous) ~ 'Posicion Inactivada Vacante',
        status_today == 'I' & !is.na(id_colaborador_previous) ~ 'Posicion Inactivada Ocupada',
        !is.na(id_colaborador_previous) & is.na(id_colaborador_today) ~ 'Posicion Vacante',
        status_today == "I" ~ 'Sin Cambios - Posiciones Inactivas',
        vacante_today == "True" ~ 'Sin Cambios - Posicion Activa Vacante',
        TRUE ~ 'Sin Cambios - Posicion Activa Ocupada'
      )
    ) %>%
    select(fecha_daily_today = fecha_daily_today, Cambios) %>%
    group_by(fecha_daily_today, Cambios) %>%
    summarise(Total_Posiciones = n(), .groups = "drop")
  
  # Añadir el resultado a la lista
  result_list[[length(result_list) + 1]] <- comparison
}

# Cerrar conexión a la base de datos
dbDisconnect(conn)

# Unir todos los resultados
final_result <- bind_rows(result_list)

# Paso 2: Guardar el resultado en un archivo de Excel
output_path <- "C:/Users/racl26345/Downloads/reporte_comparativo_posiciones.xlsx"

# Guardar el resultado en un archivo de Excel
write.xlsx(final_result, output_path)

# Mensaje de confirmación
cat("✅ El archivo ha sido guardado en:", output_path, "\n")







########################################################################
########################################################################
########################################################################
########################################################################
############################# PRUEBAS ##################################
########################################################################
########################################################################

# Cargar las librerías necesarias
library(DBI)
library(RSQLite)
library(lubridate)

# Conectar a la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(SQLite(), db_path)

# ------------------------------------------------------------------------------
# Función para calcular segundos dentro de la ventana de servicio
# ------------------------------------------------------------------------------
calculate_effective_seconds <- function(start_str, end_str) {
  if (is.null(start_str) || is.null(end_str) || is.na(start_str) || is.na(end_str)) {
    return(0)
  }
  
  # Convertir TEXT (UTC) a POSIXct con zona horaria UTC
  start_utc <- as.POSIXct(start_str, tz = "UTC")
  end_utc <- as.POSIXct(end_str, tz = "UTC")
  
  # Convertir UTC a zona horaria de Ciudad de México (maneja automáticamente CST/CDT)
  start <- with_tz(start_utc, "America/Mexico_City")
  end <- with_tz(end_utc, "America/Mexico_City")
  
  if (is.na(start) || is.na(end) || start >= end) {
    return(0)
  }
  
  total_seconds <- 0
  current_date <- as.Date(start, tz = "America/Mexico_City")
  end_date <- as.Date(end, tz = "America/Mexico_City")
  
  while (current_date <= end_date) {
    day_of_week <- as.POSIXlt(current_date)$wday  # 0 = domingo, 1 = lunes, ..., 6 = sábado
    
    # Definir ventana de servicio según día
    if (day_of_week %in% 1:5) {  # Lunes a Viernes
      day_start <- as.POSIXct(paste(current_date, "09:00:00"), tz = "America/Mexico_City")
      day_end <- as.POSIXct(paste(current_date, "18:00:00"), tz = "America/Mexico_City")
    } else if (day_of_week == 6) {  # Sábado
      day_start <- as.POSIXct(paste(current_date, "09:00:00"), tz = "America/Mexico_City")
      day_end <- as.POSIXct(paste(current_date, "14:00:00"), tz = "America/Mexico_City")
    } else {  # Domingo
      current_date <- current_date + 1
      next
    }
    
    # Calcular intersección entre el intervalo del ticket y la ventana del día
    interval_start <- max(start, day_start)
    interval_end <- min(end, day_end)
    
    if (interval_start < interval_end) {
      total_seconds <- total_seconds + as.numeric(difftime(interval_end, interval_start, units = "secs"))
    }
    
    current_date <- current_date + 1  # Siguiente día
  }
  
  return(total_seconds)
}

# ------------------------------------------------------------------------------
# Paso 1: Obtener registros nuevos de hist_status_tickets
# ------------------------------------------------------------------------------
query_nuevos_registros <- "
SELECT *
FROM hist_status_tickets
WHERE id_key NOT IN (SELECT id_key FROM hist_status_tickets_sw);
"

nuevos_registros <- dbGetQuery(conn, query_nuevos_registros)

# Calcular segundos efectivos para los registros nuevos
nuevos_registros <- nuevos_registros %>%
  rowwise() %>%
  mutate(
    seg_duracion = calculate_effective_seconds(time_start_status, time_end_status)
  ) %>%
  ungroup()

# Insertar registros nuevos en hist_status_tickets_sw
dbWriteTable(conn, "hist_status_tickets_sw", nuevos_registros, append = TRUE, row.names = FALSE)

# ------------------------------------------------------------------------------
# Paso 2: Actualizar registros con time_end_status = '9999-12-30 00:00:00'
# ------------------------------------------------------------------------------
query_registros_abiertos <- "
SELECT *
FROM hist_status_tickets
WHERE time_end_status = '9999-12-30 00:00:00'
  AND hist_status_tickets.code_estado_ticket <> 6;
"

registros_abiertos <- dbGetQuery(conn, query_registros_abiertos)

# Calcular segundos efectivos hasta las 6 PM CST del día anterior
fecha_corte <- Sys.Date() - 1  # Día anterior
hora_corte <- "18:00:00"
corte <- as.POSIXct(paste(fecha_corte, hora_corte), tz = "America/Mexico_City")

registros_abiertos <- registros_abiertos %>%
  rowwise() %>%
  mutate(
    seg_duracion = calculate_effective_seconds(time_start_status, as.character(corte))
  ) %>%
  ungroup()

# Actualizar registros en hist_status_tickets_sw
for (i in 1:nrow(registros_abiertos)) {
  query_update <- sprintf("
    UPDATE hist_status_tickets_sw
    SET time_start_status = '%s',
        time_end_status = '%s',
        seg_duracion = %d
    WHERE id_key = %d;
  ", 
                          registros_abiertos$time_start_status[i],
                          as.character(corte),
                          registros_abiertos$seg_duracion[i],
                          registros_abiertos$id_key[i])
  
  dbExecute(conn, query_update)
}

# Desconectar de la base de datos
dbDisconnect(conn)

# Mensaje de confirmación
cat("Proceso completado: Tabla hist_status_tickets_sw actualizada.\n")