# Cargar librerías necesarias
library(DBI)
library(RSQLite)
library(dplyr)
library(openxlsx)

#  Paso 1: Conectar a la base de datos SQLite y extraer datos en bruto
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

# Conectar a la base de datos
conn <- dbConnect(SQLite(), db_path)

# Query para extraer los datos de enero 2025
query <- "
SELECT 
    id_posicion, 
    id_colaborador, 
    status, 
    area_de_cobranza, 
    nivel_gestion, 
    vacante, 
    fecha_daily
FROM hist_posiciones
WHERE fecha_daily BETWEEN '2024-12-31' AND '2025-01-31';
"

# Obtener los datos
data <- dbGetQuery(conn, query)

# Cerrar conexión a la base de datos
dbDisconnect(conn)

#  Paso 2: Procesar los datos en R
# Convertir la columna fecha_daily a formato Date
data <- data %>%
  mutate(fecha_daily = as.Date(fecha_daily))

# Crear un vector con todas las fechas del mes
fechas <- seq(as.Date("2025-01-01"), as.Date("2025-01-31"), by="day")

# Unir los datos consigo mismos para comparar ayer y hoy
daily_comparison <- data %>%
  full_join(data, by = "id_posicion", suffix = c("_yesterday", "_today")) %>%
  filter(fecha_daily_today == fecha_daily_yesterday + 1 | is.na(fecha_daily_yesterday))

# Definir cambios de estado, incluyendo nuevos escenarios
status <- daily_comparison %>%
  mutate(
    Cambios = case_when(
      status_today == 'I' & status_yesterday == 'A' & is.na(id_colaborador_yesterday) ~ 'Posicion Inactivada Vacante',
      status_today == 'I' & status_yesterday == 'A' & !is.na(id_colaborador_yesterday) ~ 'Posicion Inactivada Ocupada',
      is.na(id_colaborador_yesterday) & !is.na(id_colaborador_today) ~ 'Posicion Cubierta',
      !is.na(id_colaborador_yesterday) & is.na(id_colaborador_today) ~ 'Posicion Vacante',
      is.na(id_posicion_yesterday) & !is.na(id_posicion_today) & is.na(id_colaborador_today) ~ 'Nueva Posicion Vacante',
      is.na(id_posicion_yesterday) & !is.na(id_posicion_today) & !is.na(id_colaborador_today) ~ 'Nueva Posicion Ocupada',
      status_today == "I" ~ 'Sin Cambios - Posiciones Inactivas',
      vacante_today == "True" ~ 'Sin Cambios - Posicion Activa Vacante',
      TRUE ~ 'Sin Cambios - Posicion Activa Ocupada'
    )
  ) %>%
  group_by(fecha_daily_today, nivel_gestion_today, Cambios) %>%
  summarise(Total_Posiciones = n(), .groups = "drop") %>%
  rename(fecha = fecha_daily_today, nivel_gestion = nivel_gestion_today)

# Paso 3: Guardar el resultado en un archivo de Excel
output_path <- "C:/Users/racl26345/Downloads/reporte_status.xlsx"

# Guardar el resultado en un archivo de Excel
write.xlsx(status, output_path)

# Mensaje de confirmación
cat("El archivo ha sido guardado en:", output_path, "\n")