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

# ===============================
# 1. Librerías
# ===============================
library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(openxlsx)

# ===============================
# 2. Conexión a la base SQLite
# ===============================
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

con <- dbConnect(SQLite(), db_path)

# ===============================
# 3. Cargar tabla de movimientos
# ===============================
hist_movimientos <- dbReadTable(con, "hist_movimientos")

# Cerrar conexión (ya no se necesita)
dbDisconnect(con)

# ===============================
# 4. Procesar traza de movimientos
# ===============================
traza_movimientos <- hist_movimientos %>%
  arrange(id_colaborador, id_key) %>%   # Respeta secuencia real
  group_by(id_colaborador) %>%
  mutate(
    id_posicion_anterior = lag(id_posicion),
    posicion_anterior    = lag(posicion),
    fecha_mov_anterior   = lag(fecha_movimiento)
  ) %>%
  ungroup() %>%
  
  # Renombrar columnas actuales para claridad
  rename(
    id_posicion_nueva = id_posicion,
    posicion_nueva    = posicion
  )

# ===============================
# 5. Seleccionar SOLO columnas necesarias
# ===============================
traza_final <- traza_movimientos %>%
  select(
    id_key,
    id_colaborador,
    nombre_colaborador,
    fecha_movimiento,
    tipo_movimiento,
    id_posicion_anterior,
    posicion_anterior,
    id_posicion_nueva,
    posicion_nueva
  )

# ===============================
# 6. Exportar a Excel
# ===============================
output_path <- "C:/Users/racl26345/Documents/Gestión de Indicadores/Indicadores de RH (2015-2024)/Indicadores 2025/12. Diciembre/05 Otros/traza_movimientos_validacion.xlsx"

write.xlsx(
  traza_final,
  file = output_path,
  overwrite = TRUE
)

# ===============================
# 7. Mensaje final
# ===============================
cat("Archivo generado correctamente en:\n", output_path)
