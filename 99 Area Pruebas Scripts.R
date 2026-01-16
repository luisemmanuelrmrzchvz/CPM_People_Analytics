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



SELECT 
actual.id_colaborador,
actual.nombre as nombre_colaborador,
actual.id_posicion as id_posicion_nuevo,
actual.nombre_puesto as nombre_puesto_nuevo,
actual.nivel_gestion as nivel_gestion_nuevo,
actual.fecha_efectiva_movimiento as fecha_efectiva_nueva,
actual.evento_asociado as razon_evento_nuevo,
actual.razon_evento as evento_asociado_nueva,
actual.posicion_regional as regional_nueva,
actual.posicion_plaza as plaza_nueva,
actual.posicion_estado as  estado_nuevo,
actual.posicion_municipio as municipio_nuevo,
actual.posicion_localidad as localidad_nueva,
actual.posicion_ubicacion as posicion_ubicacion_nueva,
actual.id_centro_costos as id_centro_costos_nuevo,
actual.posicion_centro_costos as posicion_centro_costos_nuevo,
actual.puesto_generico as puesto_generico_nuevo,
actual.tabulador_salarial as tabulador_salarial_nuevo,
actual.familia_puestos as familia_puestos_nuevo,
actual.modalidad_puesto as modalidad_puesto_nuevo,
actual.etiqueta_plan_horario as etiqueta_plan_horario_nuevo,
anterior.id_posicion as id_posicion_anterior,
anterior.nombre_puesto as nombre_puesto_anterior,
anterior.nivel_gestion as nivel_gestion_anterior,
anterior.fecha_efectiva_movimiento as fecha_efectiva_anterior,
anterior.evento_asociado as evento_asociado_anterior,
anterior.razon_evento as razon_evento_anterior,
anterior.posicion_regional as regional_anterior,
anterior.posicion_plaza as plaza_anterior,
anterior.posicion_estado as estado_anterior,
anterior.posicion_municipio as municipio_anterior,
anterior.posicion_localidad as localidad_anterior,
anterior.posicion_ubicacion as posicion_ubicacion_anterior,
anterior.id_centro_costos as id_centro_costos_anterior,
anterior.posicion_centro_costos as posicion_centro_costos_anterior,
anterior.puesto_generico as puesto_generico_anterior,
anterior.tabulador_salarial as tabulador_salarial_anterior,
anterior.familia_puestos as familia_puestos_anterior,
anterior.modalidad_puesto as modalidad_puesto_anterior,
anterior.etiqueta_plan_horario as etiqueta_plan_horario_anterior,
CASE WHEN actual.nombre_puesto = anterior.nombre_puesto THEN 'false' ELSE 'true' END AS cambio_nombre_puesto,
CASE WHEN actual.nivel_gestion = anterior.nivel_gestion THEN 'false' ELSE 'true' END AS cambio_nivel_gestion,
CASE WHEN actual.posicion_regional = anterior.posicion_regional THEN 'false' ELSE 'true' END AS cambio_regional,
CASE WHEN actual.posicion_plaza = anterior.posicion_plaza THEN 'false' ELSE 'true' END AS cambio_plaza,
CASE WHEN actual.posicion_estado = anterior.posicion_estado THEN 'false' ELSE 'true' END AS cambio_estado,
CASE WHEN actual.posicion_municipio = anterior.posicion_municipio THEN 'false' ELSE 'true' END AS cambio_municipio,
CASE WHEN actual.posicion_localidad = anterior.posicion_localidad THEN 'false' ELSE 'true' END AS cambio_localidad,
CASE WHEN actual.posicion_ubicacion = anterior.posicion_ubicacion THEN 'false' ELSE 'true' END AS cambio_posicion_ubicacion,
CASE WHEN actual.id_centro_costos = anterior.id_centro_costos THEN 'false' ELSE 'true' END AS cambio_id_centro_costos,
CASE WHEN actual.puesto_generico = anterior.puesto_generico THEN 'false' ELSE 'true' END AS cambio_puesto_generico,
CASE WHEN actual.tabulador_salarial = anterior.tabulador_salarial THEN 'false' ELSE 'true' END AS cambio_tabulador_salarial,
CASE WHEN actual.familia_puestos = anterior.familia_puestos THEN 'false' ELSE 'true' END AS cambio_familia_puestos,
CASE WHEN actual.modalidad_puesto = anterior.modalidad_puesto THEN 'false' ELSE 'true' END AS cambio_modalidad_puesto,
CASE WHEN actual.etiqueta_plan_horario = anterior.etiqueta_plan_horario THEN 'false' ELSE 'true' END AS cambio_etiqueta_plan_horario,
actual.secuencia_movimiento
FROM 
(SELECT 
  *,
  ROW_NUMBER() OVER (PARTITION BY id_colaborador ORDER BY fecha_efectiva_movimiento) as secuencia_movimiento
  FROM hist_movimientos) actual
LEFT JOIN 
(SELECT 
  *,
  ROW_NUMBER() OVER (PARTITION BY id_colaborador ORDER BY fecha_efectiva_movimiento) as secuencia_movimiento
  FROM hist_movimientos) anterior 
ON actual.id_colaborador = anterior.id_colaborador 
AND actual.secuencia_movimiento = anterior.secuencia_movimiento + 1
WHERE 
actual.fecha_efectiva_movimiento >= '2022-08-22'
AND actual.secuencia_movimiento > 1
ORDER BY actual.id_colaborador, actual.secuencia_movimiento
;