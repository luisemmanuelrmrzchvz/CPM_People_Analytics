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

WITH movimientos_base AS (
  SELECT
    *
  FROM hist_movimientos
  WHERE fecha_efectiva_movimiento >= '2022-08-22'
),

/* ===============================
   MOVIMIENTO DEL COLABORADOR
   (y su movimiento anterior real)
   =============================== */
movimiento_colaborador AS (
  SELECT
    m_actual.id_key,
    m_actual.id_colaborador,
    m_actual.nombre,
    m_actual.id_posicion,
    m_actual.nombre_puesto,
    m_actual.fecha_efectiva_movimiento,
    m_actual.evento_asociado,
    m_actual.razon_evento,

    /* Movimiento anterior del colaborador */
    (
      SELECT m_prev.id_posicion
      FROM movimientos_base m_prev
      WHERE m_prev.id_colaborador = m_actual.id_colaborador
        AND m_prev.fecha_efectiva_movimiento < m_actual.fecha_efectiva_movimiento
      ORDER BY m_prev.fecha_efectiva_movimiento DESC, m_prev.id_key DESC
      LIMIT 1
    ) AS id_posicion_anterior_colaborador,

    (
      SELECT m_prev.nombre_puesto
      FROM movimientos_base m_prev
      WHERE m_prev.id_colaborador = m_actual.id_colaborador
        AND m_prev.fecha_efectiva_movimiento < m_actual.fecha_efectiva_movimiento
      ORDER BY m_prev.fecha_efectiva_movimiento DESC, m_prev.id_key DESC
      LIMIT 1
    ) AS nombre_puesto_anterior_colaborador,

    (
      SELECT m_prev.fecha_efectiva_movimiento
      FROM movimientos_base m_prev
      WHERE m_prev.id_colaborador = m_actual.id_colaborador
        AND m_prev.fecha_efectiva_movimiento < m_actual.fecha_efectiva_movimiento
      ORDER BY m_prev.fecha_efectiva_movimiento DESC, m_prev.id_key DESC
      LIMIT 1
    ) AS fecha_mov_anterior_colaborador,

    (
      SELECT m_prev.evento_asociado
      FROM movimientos_base m_prev
      WHERE m_prev.id_colaborador = m_actual.id_colaborador
        AND m_prev.fecha_efectiva_movimiento < m_actual.fecha_efectiva_movimiento
      ORDER BY m_prev.fecha_efectiva_movimiento DESC, m_prev.id_key DESC
      LIMIT 1
    ) AS evento_anterior_colaborador,

    (
      SELECT m_prev.razon_evento
      FROM movimientos_base m_prev
      WHERE m_prev.id_colaborador = m_actual.id_colaborador
        AND m_prev.fecha_efectiva_movimiento < m_actual.fecha_efectiva_movimiento
      ORDER BY m_prev.fecha_efectiva_movimiento DESC, m_prev.id_key DESC
      LIMIT 1
    ) AS razon_anterior_colaborador

  FROM movimientos_base m_actual
),

/* =====================================
   OCUPANTE ANTERIOR DE LA POSICIÓN
   ===================================== */
ocupante_anterior_posicion AS (
  SELECT
    m.id_key,
    m.id_colaborador            AS id_colaborador_actual,
    m.id_posicion               AS id_posicion_actual,
    m.fecha_efectiva_movimiento AS fecha_mov_actual,

    (
      SELECT p.id_colaborador
      FROM movimientos_base p
      WHERE p.id_posicion = m.id_posicion
        AND p.id_colaborador <> m.id_colaborador
        AND p.fecha_efectiva_movimiento < m.fecha_efectiva_movimiento
      ORDER BY p.fecha_efectiva_movimiento DESC, p.id_key DESC
      LIMIT 1
    ) AS id_colaborador_anterior_posicion,

    (
      SELECT p.fecha_efectiva_movimiento
      FROM movimientos_base p
      WHERE p.id_posicion = m.id_posicion
        AND p.id_colaborador <> m.id_colaborador
        AND p.fecha_efectiva_movimiento < m.fecha_efectiva_movimiento
      ORDER BY p.fecha_efectiva_movimiento DESC, p.id_key DESC
      LIMIT 1
    ) AS fecha_mov_anterior_posicion

  FROM movimientos_base m
),

/* =====================================
   SALIDA DEL OCUPANTE ANTERIOR
   ===================================== */
movimiento_salida_ocupante AS (
  SELECT
    oap.id_key,
    oap.id_colaborador_actual,
    oap.id_posicion_actual,
    oap.fecha_mov_actual,
    oap.id_colaborador_anterior_posicion,

    (
      SELECT s.id_posicion
      FROM movimientos_base s
      WHERE s.id_colaborador = oap.id_colaborador_anterior_posicion
        AND s.fecha_efectiva_movimiento > oap.fecha_mov_anterior_posicion
      ORDER BY s.fecha_efectiva_movimiento ASC, s.id_key ASC
      LIMIT 1
    ) AS id_posicion_destino_ocupante,

    (
      SELECT s.nombre_puesto
      FROM movimientos_base s
      WHERE s.id_colaborador = oap.id_colaborador_anterior_posicion
        AND s.fecha_efectiva_movimiento > oap.fecha_mov_anterior_posicion
      ORDER BY s.fecha_efectiva_movimiento ASC, s.id_key ASC
      LIMIT 1
    ) AS nombre_puesto_destino_ocupante,

    (
      SELECT s.evento_asociado
      FROM movimientos_base s
      WHERE s.id_colaborador = oap.id_colaborador_anterior_posicion
        AND s.fecha_efectiva_movimiento > oap.fecha_mov_anterior_posicion
      ORDER BY s.fecha_efectiva_movimiento ASC, s.id_key ASC
      LIMIT 1
    ) AS evento_salida_ocupante,

    (
      SELECT s.razon_evento
      FROM movimientos_base s
      WHERE s.id_colaborador = oap.id_colaborador_anterior_posicion
        AND s.fecha_efectiva_movimiento > oap.fecha_mov_anterior_posicion
      ORDER BY s.fecha_efectiva_movimiento ASC, s.id_key ASC
      LIMIT 1
    ) AS razon_salida_ocupante,

    (
      SELECT s.fecha_efectiva_movimiento
      FROM movimientos_base s
      WHERE s.id_colaborador = oap.id_colaborador_anterior_posicion
        AND s.fecha_efectiva_movimiento > oap.fecha_mov_anterior_posicion
      ORDER BY s.fecha_efectiva_movimiento ASC, s.id_key ASC
      LIMIT 1
    ) AS fecha_salida_ocupante

  FROM ocupante_anterior_posicion oap
)

/* =====================================
   RESULTADO FINAL
   ===================================== */
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

  o.id_colaborador_anterior_posicion,
  o.id_posicion_destino_ocupante,
  o.nombre_puesto_destino_ocupante,
  o.evento_salida_ocupante,
  o.razon_salida_ocupante,
  o.fecha_salida_ocupante,

  CASE
    WHEN o.id_colaborador_anterior_posicion IS NULL
      THEN 'POSICION_NUEVA'
    WHEN UPPER(COALESCE(o.evento_salida_ocupante, '')) LIKE '%PROMO%'
      THEN 'SUSTITUCION_POR_PROMOCION'
    WHEN UPPER(COALESCE(o.evento_salida_ocupante, '')) LIKE '%BAJA%'
      OR UPPER(COALESCE(o.evento_salida_ocupante, '')) LIKE '%TERM%'
      OR UPPER(COALESCE(o.evento_salida_ocupante, '')) LIKE '%RENUNCIA%'
      THEN 'SUSTITUCION_POR_ROTACION'
    ELSE 'SUSTITUCION_NO_CLASIFICADA'
  END AS tipo_sustitucion

FROM movimiento_colaborador mc
LEFT JOIN movimiento_salida_ocupante o
  ON mc.id_key = o.id_key
ORDER BY
  mc.id_colaborador,
  mc.fecha_efectiva_movimiento,
  mc.id_key;

  
  
  C:\Users\racl26345\Documents\Gestión de Indicadores\Indicadores de RH (2015-2024)\Indicadores 2025\12. Diciembre\05 Otros