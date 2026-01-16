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

WITH base AS (
  SELECT
    *,
    ROW_NUMBER() OVER (
      PARTITION BY id_colaborador
      ORDER BY fecha_efectiva_movimiento, id_key
    ) AS rn_colab
  FROM hist_movimientos
  WHERE fecha_efectiva_movimiento >= '2022-08-22'
),

/* ===============================
   Movimiento anterior del colaborador
   =============================== */
mov_colab AS (
  SELECT
    cur.*,
    prev.id_posicion               AS id_posicion_anterior_colaborador,
    prev.nombre_puesto             AS nombre_puesto_anterior_colaborador,
    prev.fecha_efectiva_movimiento AS fecha_mov_anterior_colaborador,
    prev.evento_asociado           AS evento_anterior_colaborador,
    prev.razon_evento              AS razon_anterior_colaborador
  FROM base cur
  LEFT JOIN base prev
    ON cur.id_colaborador = prev.id_colaborador
   AND cur.rn_colab = prev.rn_colab + 1
),

/* ===============================
   Orden histórico por posición
   =============================== */
pos_base AS (
  SELECT
    *,
    ROW_NUMBER() OVER (
      PARTITION BY id_posicion
      ORDER BY fecha_efectiva_movimiento, id_key
    ) AS rn_pos
  FROM hist_movimientos
  WHERE fecha_efectiva_movimiento >= '2022-08-22'
),

/* ===============================
   Ocupante anterior de la posición
   =============================== */
ocupante_anterior AS (
  SELECT
    cur.id_key,
    cur.id_colaborador            AS id_colaborador_actual,
    cur.id_posicion               AS id_posicion_actual,
    cur.fecha_efectiva_movimiento AS fecha_mov_actual,
    prev.id_colaborador           AS id_colaborador_anterior_posicion,
    prev.fecha_efectiva_movimiento AS fecha_mov_anterior_posicion
  FROM pos_base cur
  LEFT JOIN pos_base prev
    ON cur.id_posicion = prev.id_posicion
   AND cur.rn_pos = prev.rn_pos + 1
   AND cur.id_colaborador <> prev.id_colaborador
),

/* ===============================
   Posibles salidas del ocupante anterior
   =============================== */
salidas_posibles AS (
  SELECT
    oap.id_key,
    oap.id_colaborador_actual,
    oap.id_posicion_actual,
    oap.fecha_mov_actual,
    oap.id_colaborador_anterior_posicion,
    s.id_posicion               AS id_posicion_destino_ocupante,
    s.nombre_puesto             AS nombre_puesto_destino_ocupante,
    s.evento_asociado           AS evento_salida_ocupante,
    s.razon_evento              AS razon_salida_ocupante,
    s.fecha_efectiva_movimiento AS fecha_salida_ocupante,
    ROW_NUMBER() OVER (
      PARTITION BY oap.id_key
      ORDER BY s.fecha_efectiva_movimiento, s.id_key
    ) AS rn_salida
  FROM ocupante_anterior oap
  LEFT JOIN base s
    ON s.id_colaborador = oap.id_colaborador_anterior_posicion
   AND s.fecha_efectiva_movimiento > oap.fecha_mov_anterior_posicion
),

/* ===============================
   Salida real (primer movimiento)
   =============================== */
salida_ocupante AS (
  SELECT *
  FROM salidas_posibles
  WHERE rn_salida = 1
)

/* ===============================
   RESULTADO FINAL
   =============================== */
SELECT
  mc.id_colaborador,
  mc.nombre                                AS nombre_colaborador,
  mc.id_posicion                           AS id_posicion_actual,
  mc.nombre_puesto                         AS nombre_puesto_actual,
  mc.fecha_efectiva_movimiento             AS fecha_movimiento_actual,
  mc.evento_asociado                       AS evento_actual,
  mc.razon_evento                          AS razon_actual,

  mc.id_posicion_anterior_colaborador,
  mc.nombre_puesto_anterior_colaborador,
  mc.fecha_mov_anterior_colaborador,
  mc.evento_anterior_colaborador,
  mc.razon_anterior_colaborador,

  so.id_colaborador_anterior_posicion,
  so.id_posicion_destino_ocupante,
  so.nombre_puesto_destino_ocupante,
  so.evento_salida_ocupante,
  so.razon_salida_ocupante,
  so.fecha_salida_ocupante,

  CASE
    WHEN so.id_colaborador_anterior_posicion IS NULL
      THEN 'POSICION_NUEVA'
    WHEN UPPER(COALESCE(so.evento_salida_ocupante, '')) LIKE '%PROMO%'
      THEN 'SUSTITUCION_POR_PROMOCION'
    WHEN UPPER(COALESCE(so.evento_salida_ocupante, '')) LIKE '%BAJA%'
      OR UPPER(COALESCE(so.evento_salida_ocupante, '')) LIKE '%TERM%'
      OR UPPER(COALESCE(so.evento_salida_ocupante, '')) LIKE '%RENUNCIA%'
      THEN 'SUSTITUCION_POR_ROTACION'
    ELSE 'SUSTITUCION_NO_CLASIFICADA'
  END AS tipo_sustitucion
FROM mov_colab mc
LEFT JOIN salida_ocupante so
  ON mc.id_key = so.id_key
ORDER BY
  mc.id_colaborador,
  mc.fecha_efectiva_movimiento,
  mc.id_key;


  
  
  C:\Users\racl26345\Documents\Gestión de Indicadores\Indicadores de RH (2015-2024)\Indicadores 2025\12. Diciembre\05 Otros
