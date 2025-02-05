# Cargar librerías necesarias
library(DBI)
library(RSQLite)
library(dplyr)
library(openxlsx)

# Paso 1: Conectar a la base de datos SQLite y extraer datos de los días relevantes
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

# Conectar a la base de datos
conn <- dbConnect(SQLite(), db_path)

# Query ajustado para obtener datos entre el 31 de diciembre de 2024 y el 31 de enero de 2025
query <- "
WITH data_previous AS (
  SELECT 
    id_posicion, 
    id_colaborador, 
    status, 
    vacante, 
    fecha_daily
  FROM hist_posiciones
  WHERE fecha_daily BETWEEN '2024-12-31' AND '2025-01-31'
),
data_current AS (
  SELECT 
    id_posicion, 
    id_colaborador, 
    status, 
    vacante, 
    fecha_daily
  FROM hist_posiciones
  WHERE fecha_daily BETWEEN '2024-12-31' AND '2025-01-31'
)
SELECT 
  t.id_posicion,
  t.id_colaborador AS id_colaborador_current,
  p.id_colaborador AS id_colaborador_previous,
  t.status AS status_current,
  p.status AS status_previous,
  t.vacante AS vacante_current,
  p.vacante AS vacante_previous,
  CASE
    WHEN p.id_colaborador IS NULL AND t.id_colaborador IS NULL THEN 'Nueva Posicion Vacante'
    WHEN p.id_colaborador IS NULL AND t.id_colaborador IS NOT NULL THEN 'Nueva Posicion Ocupada'
    WHEN p.status = 'I' AND t.id_colaborador IS NULL THEN 'Posicion Inactivada Vacante'
    WHEN p.status = 'I' AND t.id_colaborador IS NOT NULL THEN 'Posicion Inactivada Ocupada'
    WHEN p.id_colaborador IS NULL AND t.id_colaborador IS NOT NULL THEN 'Posicion Cubierta'
    WHEN p.id_colaborador IS NOT NULL AND t.id_colaborador IS NULL THEN 'Posicion Vacante'
    WHEN t.status = 'I' THEN 'Sin Cambios - Posiciones Inactivas'
    WHEN t.vacante = 'True' THEN 'Sin Cambios - Posicion Activa Vacante'
    ELSE 'Sin Cambios - Posicion Activa Ocupada'
  END AS Cambios
FROM data_current t
LEFT JOIN data_previous p ON t.id_posicion = p.id_posicion;
"

# Obtener los datos optimizados
data <- dbGetQuery(conn, query)

# Cerrar conexión a la base de datos
dbDisconnect(conn)

# Paso 2: Procesar los datos en R (si es necesario)
# Convertir la columna fecha a Date
data <- data %>%
  mutate(fecha = as.Date(ifelse(is.na(id_colaborador_current), "2024-12-31", "2025-01-31")))

# Agrupar y contar los cambios por fecha y tipo de cambio
status_summary <- data %>%
  group_by(fecha, Cambios) %>%
  summarise(Total_Posiciones = n(), .groups = "drop")

# Paso 3: Guardar el resultado en un archivo de Excel
output_path <- "C:/Users/racl26345/Downloads/reporte_status.xlsx"

# Guardar el resultado en un archivo de Excel
write.xlsx(status_summary, output_path)

# Mensaje de confirmación
cat("✅ El archivo ha sido guardado en:", output_path, "\n")