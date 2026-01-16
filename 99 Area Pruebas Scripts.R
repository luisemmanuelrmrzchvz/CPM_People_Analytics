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

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(openxlsx)

# =========================
# 1. Conexión a SQLite
# =========================
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

con <- dbConnect(SQLite(), db_path)

# =========================
# 2. Cargar movimientos
# =========================
hist_movimientos <- dbGetQuery(con, "
  SELECT *
  FROM hist_movimientos
  WHERE fecha_efectiva_movimiento >= '2022-08-22'
")

dbDisconnect(con)

# Asegurar tipo fecha
hist_movimientos <- hist_movimientos %>%
  mutate(fecha_efectiva_movimiento = as.Date(fecha_efectiva_movimiento))

# =========================
# 3. Movimiento anterior del colaborador
# =========================
mov_colab <- hist_movimientos %>%
  arrange(id_colaborador, fecha_efectiva_movimiento, id_key) %>%
  group_by(id_colaborador) %>%
  mutate(
    id_posicion_anterior_colaborador      = lag(id_posicion),
    nombre_puesto_anterior_colaborador    = lag(nombre_puesto),
    fecha_mov_anterior_colaborador        = lag(fecha_efectiva_movimiento),
    evento_anterior_colaborador           = lag(evento_asociado),
    razon_anterior_colaborador            = lag(razon_evento)
  ) %>%
  ungroup()

# =========================
# 4. Ocupante anterior de la posición
# =========================
ocupante_anterior <- hist_movimientos %>%
  arrange(id_posicion, fecha_efectiva_movimiento, id_key) %>%
  group_by(id_posicion) %>%
  mutate(
    id_colaborador_anterior_posicion  = lag(id_colaborador),
    fecha_mov_anterior_posicion       = lag(fecha_efectiva_movimiento),
    id_key_anterior_posicion           = lag(id_key)
  ) %>%
  ungroup() %>%
  select(
    id_key,
    id_colaborador_anterior_posicion,
    fecha_mov_anterior_posicion
  )

# =========================
# 5. Unir ocupante anterior al movimiento
# =========================
mov_base <- mov_colab %>%
  left_join(ocupante_anterior, by = "id_key")

# =========================
# 6. Salida del ocupante anterior
# =========================
salida_ocupante <- hist_movimientos %>%
  rename(
    id_colaborador_anterior_posicion = id_colaborador,
    fecha_salida_ocupante            = fecha_efectiva_movimiento,
    id_posicion_destino_ocupante     = id_posicion,
    nombre_puesto_destino_ocupante   = nombre_puesto,
    evento_salida_ocupante           = evento_asociado,
    razon_salida_ocupante            = razon_evento
  ) %>%
  select(
    id_colaborador_anterior_posicion,
    fecha_salida_ocupante,
    id_posicion_destino_ocupante,
    nombre_puesto_destino_ocupante,
    evento_salida_ocupante,
    razon_salida_ocupante
  )

mov_final <- mov_base %>%
  left_join(
    salida_ocupante,
    by = "id_colaborador_anterior_posicion"
  ) %>%
  filter(
    is.na(fecha_mov_anterior_posicion) |
    fecha_salida_ocupante > fecha_mov_anterior_posicion
  ) %>%
  arrange(id_key, fecha_salida_ocupante) %>%
  group_by(id_key) %>%
  slice(1) %>%        # <<< CLAVE: una sola fila por movimiento
  ungroup()

# =========================
# 7. Clasificación de sustitución
# =========================
mov_final <- mov_final %>%
  mutate(
    tipo_sustitucion = case_when(
      is.na(id_colaborador_anterior_posicion) ~ "POSICION_NUEVA",
      grepl("PROMO", toupper(evento_salida_ocupante)) ~ "SUSTITUCION_POR_PROMOCION",
      grepl("BAJA|TERM|RENUNCIA", toupper(evento_salida_ocupante)) ~ "SUSTITUCION_POR_ROTACION",
      TRUE ~ "SUSTITUCION_NO_CLASIFICADA"
    )
  )

# =========================
# 8. Exportar a Excel
# =========================
output_path <- "C:/Users/racl26345/Documents/Gestión de Indicadores/Indicadores de RH (2015-2024)/Indicadores 2025/12. Diciembre/05 Otros/movilidad_colaboradores.xlsx"

write.xlsx(mov_final, output_path)

message("Archivo generado correctamente en:\n", output_path)



  
  
  C:\Users\racl26345\Documents\Gestión de Indicadores\Indicadores de RH (2015-2024)\Indicadores 2025\12. Diciembre\05 Otros
