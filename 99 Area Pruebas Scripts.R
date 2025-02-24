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

# Cargar librerías
library(DBI)
library(RSQLite)
library(lubridate)
library(data.table)  # Para procesamiento rápido

# Conectar a la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(SQLite(), db_path)

# ------------------------------------------------------------------------------ 
# Función optimizada para calcular segundos en horario laboral sin bucles lentos
# ------------------------------------------------------------------------------

calculate_effective_seconds <- function(start, end) {
  if (is.na(start) || is.na(end) || start >= end) {
    return(0)
  }

  # Definir horarios de trabajo
  weekdays_hours <- c("09:00:00", "18:00:00")  
  saturday_hours <- c("09:00:00", "14:00:00")  

  # Crear secuencia de días laborables
  days_seq <- seq(as.Date(start), as.Date(end), by = "day")
  working_seconds <- 0

  for (day in days_seq) {
    weekday <- wday(day)  # Ajuste aquí, eliminando label = FALSE

    if (weekday %in% 2:6) {  # Lunes a viernes (lubridate usa 2 = lunes, 6 = viernes)
      work_start <- as.POSIXct(paste(day, weekdays_hours[1]), tz = "America/Mexico_City")
      work_end <- as.POSIXct(paste(day, weekdays_hours[2]), tz = "America/Mexico_City")
    } else if (weekday == 7) {  # Sábado
      work_start <- as.POSIXct(paste(day, saturday_hours[1]), tz = "America/Mexico_City")
      work_end <- as.POSIXct(paste(day, saturday_hours[2]), tz = "America/Mexico_City")
    } else {
      next  # Saltar domingos (weekday == 1)
    }

    # Calcular intersección del tiempo dentro del horario laboral
    interval_start <- max(start, work_start)
    interval_end <- min(end, work_end)

    if (interval_start < interval_end) {
      working_seconds <- working_seconds + as.numeric(difftime(interval_end, interval_start, units = "secs"))
    }
  }

  return(working_seconds)
}

# ------------------------------------------------------------------------------ 
# Poblar la tabla espejo por primera vez con optimización
# ------------------------------------------------------------------------------

query <- "SELECT * FROM hist_status_tickets;"
data <- dbGetQuery(conn, query)

# Convertir a data.table para procesamiento más rápido
data <- as.data.table(data)

# Asegurar que `code_estado_ticket` sea numérico para evitar errores de comparación
data[, code_estado_ticket := as.numeric(code_estado_ticket)]

# Aplicar condición especial
data[, time_end_status := fifelse(code_estado_ticket == 6, time_start_status, time_end_status)]

# Calcular duración en segundos sin usar `mapply()`
data[, seg_duracion := fifelse(code_estado_ticket == 6, 0, calculate_effective_seconds(time_start_status, time_end_status)), by = id_key]

# Cerrar la conexión a la base de datos
dbDisconnect(conn)













#########################################################################





> # Cargar librerías
  > library(DBI)
> library(RSQLite)
> library(lubridate)
> library(data.table)  # Para procesamiento rápido
> 
  > # Conectar a la base de datos SQLite
  > db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
> conn <- dbConnect(SQLite(), db_path)
> 
  > # ------------------------------------------------------------------------------ 
> # Función optimizada para calcular segundos en horario laboral sin bucles lentos
  > # ------------------------------------------------------------------------------
> 
  > calculate_effective_seconds <- function(start, end) {
    +   if (is.na(start) || is.na(end) || start >= end) {
      +     return(0)
      +   }
    + 
      +   # Definir horarios de trabajo
      +   weekdays_hours <- c("09:00:00", "18:00:00")  
      +   saturday_hours <- c("09:00:00", "14:00:00")  
      + 
        +   # Crear secuencia de días laborables
        +   days_seq <- seq(as.Date(start), as.Date(end), by = "day")
        +   working_seconds <- 0
        + 
          +   for (day in days_seq) {
            +     weekday <- wday(day)  # Ajuste aquí, eliminando label = FALSE
            + 
              +     if (weekday %in% 2:6) {  # Lunes a viernes (lubridate usa 2 = lunes, 6 = viernes)
                +       work_start <- as.POSIXct(paste(day, weekdays_hours[1]), tz = "America/Mexico_City")
                +       work_end <- as.POSIXct(paste(day, weekdays_hours[2]), tz = "America/Mexico_City")
                +     } else if (weekday == 7) {  # Sábado
                  +       work_start <- as.POSIXct(paste(day, saturday_hours[1]), tz = "America/Mexico_City")
                  +       work_end <- as.POSIXct(paste(day, saturday_hours[2]), tz = "America/Mexico_City")
                  +     } else {
                    +       next  # Saltar domingos (weekday == 1)
                    +     }
            + 
              +     # Calcular intersección del tiempo dentro del horario laboral
              +     interval_start <- max(start, work_start)
              +     interval_end <- min(end, work_end)
              + 
                +     if (interval_start < interval_end) {
                  +       working_seconds <- working_seconds + as.numeric(difftime(interval_end, interval_start, units = "secs"))
                  +     }
              +   }
        + 
          +   return(working_seconds)
        + }
> 
  > # ------------------------------------------------------------------------------ 
> # Poblar la tabla espejo por primera vez con optimización
  > # ------------------------------------------------------------------------------
> 
  > query <- "SELECT * FROM hist_status_tickets;"
> data <- dbGetQuery(conn, query)
Aviso:
  Column `code_estado_ticket`: mixed type, first seen values of type integer, coercing other values of type string 
> 
  > # Convertir a data.table para procesamiento más rápido
  > data <- as.data.table(data)
> 
  > # Asegurar que `code_estado_ticket` sea numérico para evitar errores de comparación
  > data[, code_estado_ticket := as.numeric(code_estado_ticket)]
> 
  > # Aplicar condición especial
  > data[, time_end_status := fifelse(code_estado_ticket == 6, time_start_status, time_end_status)]
> 
  > # Calcular duración en segundos sin usar `mapply()`
  > data[, seg_duracion := fifelse(code_estado_ticket == 6, 0, calculate_effective_seconds(time_start_status, time_end_status)), by = id_key]
Error en as.POSIXlt.character(x, tz, ...): 
  la cadena de caracteres no está en un formato estándar inequívoco
10.
stop("character string is not in a standard unambiguous format")
9.
as.POSIXlt.character(x, tz, ...)
8.
as.POSIXlt(x, tz, ...)
7.
as.POSIXct(as.POSIXlt(x, tz, ...), tz, ...)
6.
as.POSIXct.default(paste(day, weekdays_hours[1]), tz = "America/Mexico_City")
5.
as.POSIXct(paste(day, weekdays_hours[1]), tz = "America/Mexico_City")
4.
calculate_effective_seconds(time_start_status, time_end_status)
3.
fifelse(code_estado_ticket == 6, 0, calculate_effective_seconds(time_start_status,
                                                                time_end_status))
2.
`[.data.table`(data, , `:=`(seg_duracion, fifelse(code_estado_ticket ==
                                                    6, 0, calculate_effective_seconds(time_start_status, time_end_status))),
               by = id_key)
1.
data[, `:=`(seg_duracion, fifelse(code_estado_ticket == 6, 0,
                                  calculate_effective_seconds(time_start_status, time_end_status))),
     by = id_key]

> # Cargar librerías
  > library(DBI)
> library(RSQLite)
> library(lubridate)
> library(data.table)  # Para procesamiento rápido
> 
  > # Conectar a la base de datos SQLite
  > db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
> conn <- dbConnect(SQLite(), db_path)
> 
  > # ------------------------------------------------------------------------------ 
> # Función optimizada para calcular segundos en horario laboral sin bucles lentos
  > # ------------------------------------------------------------------------------
> 
  > calculate_effective_seconds <- function(start, end) {
    +   if (is.na(start) || is.na(end) || start >= end) {
      +     return(0)
      +   }
    + 
      +   # Definir horarios de trabajo
      +   weekdays_hours <- c("09:00:00", "18:00:00")  
      +   saturday_hours <- c("09:00:00", "14:00:00")  
      + 
        +   # Crear secuencia de días laborables
        +   days_seq <- seq(as.Date(start), as.Date(end), by = "day")
        +   working_seconds <- 0
        + 
          +   for (day in days_seq) {
            +     weekday <- wday(day)  # Ajuste aquí, eliminando label = FALSE
            + 
              +     if (weekday %in% 2:6) {  # Lunes a viernes (lubridate usa 2 = lunes, 6 = viernes)
                +       work_start <- as.POSIXct(paste(day, weekdays_hours[1]), tz = "America/Mexico_City")
                +       work_end <- as.POSIXct(paste(day, weekdays_hours[2]), tz = "America/Mexico_City")
                +     } else if (weekday == 7) {  # Sábado
                  +       work_start <- as.POSIXct(paste(day, saturday_hours[1]), tz = "America/Mexico_City")
                  +       work_end <- as.POSIXct(paste(day, saturday_hours[2]), tz = "America/Mexico_City")
                  +     } else {
                    +       next  # Saltar domingos (weekday == 1)
                    +     }
            + 
              +     # Calcular intersección del tiempo dentro del horario laboral
              +     interval_start <- max(start, work_start)
              +     interval_end <- min(end, work_end)
              + 
                +     if (interval_start < interval_end) {
                  +       working_seconds <- working_seconds + as.numeric(difftime(interval_end, interval_start, units = "secs"))
                  +     }
              +   }
        + 
          +   return(working_seconds)
        + }
> 
  > # ------------------------------------------------------------------------------ 
> # Poblar la tabla espejo por primera vez con optimización
  > # ------------------------------------------------------------------------------
> 
  > query <- "SELECT * FROM hist_status_tickets;"
> data <- dbGetQuery(conn, query)
Aviso:
  Column `code_estado_ticket`: mixed type, first seen values of type integer, coercing other values of type string 
> 
  > # Convertir a data.table para procesamiento más rápido
  > data <- as.data.table(data)
> 
  > # Asegurar que `code_estado_ticket` sea numérico para evitar errores de comparación
  > data[, code_estado_ticket := as.numeric(code_estado_ticket)]
> 
  > # Aplicar condición especial
  > data[, time_end_status := fifelse(code_estado_ticket == 6, time_start_status, time_end_status)]
> 
  > # Calcular duración en segundos sin usar `mapply()`
  > data[, seg_duracion := fifelse(code_estado_ticket == 6, 0, calculate_effective_seconds(time_start_status, time_end_status)), by = id_key]
Error en as.POSIXlt.character(x, tz, ...): 
  la cadena de caracteres no está en un formato estándar inequívoco
Called from: as.POSIXlt.character(x, tz, ...)
Browse[1]> 
