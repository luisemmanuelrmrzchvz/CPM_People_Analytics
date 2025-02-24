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



library(reticulate)

# Importar las bibliotecas de Python
pd <- import("pandas")
sqlite3 <- import("sqlite3")
pytz <- import("pytz")

# ------------------------------------------------------------------------------
# Funciones en Python para el cálculo (CON ZONAS HORARIAS)
# ------------------------------------------------------------------------------
py_run_string("
import pytz
from datetime import datetime, timedelta
import pandas as pd

def calculate_effective_seconds(start_utc, end_utc):
    # Convertir UTC a CST (Ciudad de México)
    utc_zone = pytz.utc
    cst_zone = pytz.timezone('America/Mexico_City')
    
    start = start_utc.astimezone(cst_zone) if start_utc.tzinfo else utc_zone.localize(start_utc).astimezone(cst_zone)
    end = end_utc.astimezone(cst_zone) if end_utc.tzinfo else utc_zone.localize(end_utc).astimezone(cst_zone)

    if end == start:
        return 0
    if pd.isna(start) or pd.isna(end) or start >= end:
        return 0

    total_seconds = 0
    current_date = start.date()
    end_date = end.date()

    while current_date <= end_date:
        day_of_week = current_date.weekday()

        # Definir ventanas de servicio en CST
        if day_of_week in range(5):  # Lunes a viernes (0-4)
            servicio_start = cst_zone.localize(datetime.combine(current_date, datetime.strptime('09:00:00', '%H:%M:%S').time()))
            servicio_end = cst_zone.localize(datetime.combine(current_date, datetime.strptime('18:00:00', '%H:%M:%S').time()))
        elif day_of_week == 5:  # Sábado (5)
            servicio_start = cst_zone.localize(datetime.combine(current_date, datetime.strptime('09:00:00', '%H:%M:%S').time()))
            servicio_end = cst_zone.localize(datetime.combine(current_date, datetime.strptime('14:00:00', '%H:%M:%S').time()))
        else:  # Domingo (6)
            current_date += timedelta(days=1)
            continue

        # Asegurar que los intervalos están en CST
        interval_start = max(start, servicio_start)
        interval_end = min(end, servicio_end)

        if interval_start < interval_end:
            total_seconds += (interval_end - interval_start).total_seconds()

        current_date += timedelta(days=1)

    return total_seconds

def calculate_row_seconds(row):
    try:
        if row['code_estado_ticket'] != '6':
            return calculate_effective_seconds(row['time_start_status'], row['time_end_status_parsed'])
        else:
            return 0
    except Exception as e:
        print(f'Error en fila {row.name}: {str(e)}')
        return 0
")

# ------------------------------------------------------------------------------
# Conectar a la base de datos y procesar (CONVERSIÓN UTC -> CST)
# ------------------------------------------------------------------------------
py_run_string("
import sqlite3
import pandas as pd
import pytz
from datetime import datetime, timedelta

# Conectar a la base de datos
conn = sqlite3.connect('C:/Users/racl26345/Documents/DataBases/people_analytics.db')

# Consulta SQL
query = '''
    SELECT 
        id_key, id_ticket, fecha_creado, agente_servicio, prioridad,
        time_start_status, 
        CASE 
            WHEN code_estado_ticket = 6 THEN time_start_status 
            ELSE time_end_status 
        END AS time_end_status,
        CAST(code_estado_ticket AS TEXT) AS code_estado_ticket,
        estado_ticket, siguiente_accion_para,
        seg_duracion
    FROM hist_status_tickets
    WHERE id_key NOT IN (SELECT id_key FROM hist_status_tickets_sw);
'''

# Leer datos en DataFrame
nuevos_registros = pd.read_sql_query(query, conn)

# Guardar el time_end_status original como string
nuevos_registros['original_time_end_status'] = nuevos_registros['time_end_status']

# Convertir a datetime con zona horaria UTC
nuevos_registros['time_start_status'] = pd.to_datetime(
    nuevos_registros['time_start_status'],
    format='%Y-%m-%d %H:%M:%S',
    errors='coerce'
).dt.tz_localize('UTC')

# Parsear time_end_status, manteniendo los originales
nuevos_registros['time_end_status_parsed'] = pd.to_datetime(
    nuevos_registros['time_end_status'],
    format='%Y-%m-%d %H:%M:%S',
    errors='coerce'
).dt.tz_localize('UTC')

# Obtener la fecha actual en CST
cst = pytz.timezone('America/Mexico_City')
now_cst = datetime.now(cst)
current_date_cst = now_cst.date()

# Función para obtener dynamic_end_utc
def get_dynamic_end_utc(current_date):
    day_of_week = current_date.weekday()
    if day_of_week in range(5):  # Lunes a viernes
        end_time = datetime.strptime('18:00:00', '%H:%M:%S').time()
        effective_date = current_date
    elif day_of_week == 5:  # Sábado
        end_time = datetime.strptime('14:00:00', '%H:%M:%S').time()
        effective_date = current_date
    else:  # Domingo
        effective_date = current_date - timedelta(days=1)
        end_time = datetime.strptime('14:00:00', '%H:%M:%S').time()

    naive = datetime.combine(effective_date, end_time)
    localized = cst.localize(naive)
    return localized.astimezone(pytz.UTC)

dynamic_end_utc = get_dynamic_end_utc(current_date_cst)

# Reemplazar NaT en time_end_status_parsed para tickets abiertos
mask = (nuevos_registros['code_estado_ticket'] != '6') & nuevos_registros['time_end_status_parsed'].isna()
nuevos_registros.loc[mask, 'time_end_status_parsed'] = dynamic_end_utc

# Calcular seg_duracion usando time_end_status_parsed
nuevos_registros['seg_duracion'] = nuevos_registros.apply(calculate_row_seconds, axis=1)

# Convertir a CST y eliminar zona horaria
nuevos_registros['time_start_status'] = nuevos_registros['time_start_status'].dt.tz_convert('America/Mexico_City').dt.tz_localize(None)
nuevos_registros['time_end_status_parsed'] = nuevos_registros['time_end_status_parsed'].dt.tz_convert('America/Mexico_City').dt.tz_localize(None)

# Restaurar original_time_end_status para tickets abiertos
nuevos_registros['time_end_status'] = nuevos_registros.apply(
    lambda row: row['original_time_end_status'] if (row['code_estado_ticket'] != '6' and pd.isna(pd.to_datetime(row['original_time_end_status'], errors='coerce'))) else row['time_end_status_parsed'].strftime('%Y-%m-%d %H:%M:%S'),
    axis=1
)

# Eliminar columnas temporales
nuevos_registros = nuevos_registros.drop(columns=['time_end_status_parsed', 'original_time_end_status'])

# Guardar resultados en la tabla espejo
nuevos_registros.to_sql('hist_status_tickets_sw', conn, if_exists='append', index=False)

# Cerrar conexión
conn.close()
")

# Mensaje de finalización
cat("¡Proceso completado con conversión UTC -> CST a las", format(Sys.time(), "%H:%M:%S"), "\n")