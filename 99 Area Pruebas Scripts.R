# Cargar librerías
library(DBI)
library(RSQLite)
library(writexl)

# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Query adaptado a R
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

# Ejecutar el query y guardar resultados en un dataframe
resultados <- dbGetQuery(conn, query)

# Cerrar conexión a la base de datos
dbDisconnect(conn)

# Ruta para guardar el archivo Excel
output_path <- "C:/Users/racl26345/Downloads/resultado_query.xlsx"

# Guardar resultados en Excel
write_xlsx(resultados, output_path)

# Mensaje de confirmación
cat("El archivo se ha guardado en:", output_path, "\n")