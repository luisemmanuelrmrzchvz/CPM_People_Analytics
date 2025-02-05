# Cargar librerías necesarias
library(DBI)
library(RSQLite)
library(dplyr)
library(openxlsx)

# 📌 Paso 1: Conectar a la base de datos SQLite y extraer datos optimizados
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

# Conectar a la base de datos
conn <- dbConnect(SQLite(), db_path)

# Query optimizada para extraer datos de ayer y hoy y comparar directamente en SQL
query <- "
WITH data_yesterday AS (
  SELECT 
    id_posicion, 
    id_colaborador, 
    status, 
    vacante, 
    fecha_daily
  FROM hist_posiciones
  WHERE fecha_daily = '2024-12-31'
),
data_today AS (
  SELECT 
    id_posicion, 
    id_colaborador, 
    status, 
    vacante, 
    fecha_daily
  FROM hist_posiciones
  WHERE fecha_daily = '2025-01-01'
)
SELECT 
  t.id_posicion,
  t.id_colaborador AS id_colaborador_today,
  y.id_colaborador AS id_colaborador_yesterday,
  t.status AS status_today,
  y.status AS status_yesterday,
  t.vacante AS vacante_today,
  y.vacante AS vacante_yesterday,
  CASE
    WHEN y.id_colaborador IS NULL AND t.id_colaborador IS NULL THEN 'Nueva Posicion Vacante'
    WHEN y.id_colaborador IS NULL AND t.id_colaborador IS NOT NULL THEN 'Nueva Posicion Ocupada'
    WHEN y.status = 'I' AND t.id_colaborador IS NULL THEN 'Posicion Inactivada Vacante'
    WHEN y.status = 'I' AND t.id_colaborador IS NOT NULL THEN 'Posicion Inactivada Ocupada'
    WHEN y.id_colaborador IS NULL AND t.id_colaborador IS NOT NULL THEN 'Posicion Cubierta'
    WHEN y.id_colaborador IS NOT NULL AND t.id_colaborador IS NULL THEN 'Posicion Vacante'
    WHEN t.status = 'I' THEN 'Sin Cambios - Posiciones Inactivas'
    WHEN t.vacante = 'True' THEN 'Sin Cambios - Posicion Activa Vacante'
    ELSE 'Sin Cambios - Posicion Activa Ocupada'
  END AS Cambios
FROM data_today t
LEFT JOIN data_yesterday y ON t.id_posicion = y.id_posicion;
"

# Obtener los datos optimizados
data <- dbGetQuery(conn, query)

# Cerrar conexión a la base de datos
dbDisconnect(conn)

# 📌 Paso 2: Procesar los datos en R (si es necesario)
# Convertir la columna fecha a Date
data <- data %>%
  mutate(fecha = as.Date(ifelse(is.na(id_colaborador_today), "2024-12-31", "2025-01-01")))

# Agrupar y contar los cambios por fecha y tipo de cambio
status_summary <- data %>%
  group_by(fecha, Cambios) %>%
  summarise(Total_Posiciones = n(), .groups = "drop")

# 📌 Paso 3: Guardar el resultado en un archivo de Excel
output_path <- "C:/Users/racl26345/Downloads/reporte_status.xlsx"

# Guardar el resultado en un archivo de Excel
write.xlsx(status_summary, output_path)

# Mensaje de confirmación
cat("✅ El archivo ha sido guardado en:", output_path, "\n")