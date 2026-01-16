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

# Paquetes (instalar si hace falta)
# install.packages(c("DBI","RSQLite","dplyr","lubridate","openxlsx"))

library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(openxlsx)

# Rutas
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
output_path <- "C:/Users/racl26345/Documents/Gestión de Indicadores/Indicadores de RH (2015-2024)/Indicadores 2025/12. Diciembre/05 Otros/traza_movimientos_validacion.xlsx"

# ======================
# 1) Conectar y leer solo columnas necesarias
# ======================
con <- dbConnect(SQLite(), db_path)

sql <- "
SELECT
  id_key,
  id_colaborador,
  nombre,                       -- nombre del colaborador en tu tabla (lo vimos antes)
  id_posicion,
  nombre_puesto,
  fecha_efectiva_movimiento,
  evento_asociado,
  razon_evento
FROM hist_movimientos
WHERE fecha_efectiva_movimiento >= '2022-08-22'
"

hist_movimientos <- dbGetQuery(con, sql)

dbDisconnect(con)

# Asegurar tipos
hist_movimientos <- hist_movimientos %>%
  mutate(
    fecha_efectiva_movimiento = as.Date(fecha_efectiva_movimiento)
  )

# ======================
# 2) Normalizar nombres para trabajar (nombres consistentes en el pipeline)
# ======================
hist_movimientos <- hist_movimientos %>%
  rename(
    nombre_colaborador = nombre,
    fecha_movimiento   = fecha_efectiva_movimiento,
    posicion_nueva     = nombre_puesto,    # texto del puesto en la fila actual
    id_posicion_nueva  = id_posicion
  )

# Si tu tabla ya tuviera una columna 'tipo_movimiento' y quisieras conservarla,
# podrías seleccionarla arriba y renombrarla aquí. Como no la tenemos, la derivamos luego.

# ======================
# 3) Movimiento anterior del colaborador (lag por colaborador)
# ======================
mov_colab <- hist_movimientos %>%
  arrange(id_colaborador, fecha_movimiento, id_key) %>%
  group_by(id_colaborador) %>%
  mutate(
    id_posicion_anterior = lag(id_posicion_nueva),
    posicion_anterior    = lag(posicion_nueva),
    fecha_mov_anterior   = lag(fecha_movimiento),
    evento_anterior_colaborador = lag(evento_asociado),
    razon_anterior_colaborador  = lag(razon_evento)
  ) %>%
  ungroup()

# ======================
# 4) Ocupante anterior de la posición (lag por id_posicion)
# ======================
ocupante_anterior <- hist_movimientos %>%
  arrange(id_posicion_nueva, fecha_movimiento, id_key) %>%
  group_by(id_posicion_nueva) %>%
  mutate(
    id_colaborador_anterior_posicion = lag(id_colaborador),
    fecha_mov_anterior_posicion      = lag(fecha_movimiento),
    id_key_anterior_posicion         = lag(id_key)
  ) %>%
  ungroup() %>%
  select(id_key, id_colaborador_anterior_posicion, fecha_mov_anterior_posicion)

# ======================
# 5) Unir ocupante anterior al movimiento del colaborador
# ======================
mov_base <- mov_colab %>%
  left_join(ocupante_anterior, by = "id_key")

# ======================
# 6) Determinar la primera salida (primer movimiento posterior) del ocupante anterior
#    — Para cada movimiento, buscamos el primer registro posterior del ocupante anterior
# ======================
# Preparamos un dataframe con los movimientos para hacer el join
mov_for_join <- hist_movimientos %>%
  select(
    id_key_destino = id_key,
    id_colaborador_destino = id_colaborador,
    fecha_movimiento_destino = fecha_movimiento,
    id_posicion_destino = id_posicion_nueva,
    posicion_destino = posicion_nueva,
    evento_salida = evento_asociado,
    razon_salida = razon_evento
  )

# Hacemos LEFT JOIN y quedamos con la primera salida posterior (por id_key original)
salidas_joined <- mov_base %>%
  left_join(
    mov_for_join,
    by = c("id_colaborador_anterior_posicion" = "id_colaborador_destino")
  ) %>%
  # Solo las salidas posteriores a la fecha en que dejó la posición
  filter(
    is.na(fecha_mov_anterior_posicion) | fecha_movimiento_destino > fecha_mov_anterior_posicion
  ) %>%
  arrange(id_key, fecha_movimiento_destino, id_key_destino) %>%
  group_by(id_key) %>%
  slice(1) %>%    # <<< importante: si hay varias filas, nos quedamos con la primera salida posterior (o NA si no hay)
  ungroup()

# ======================
# 7) Clasificación tipo_sustitucion (usando evento_salida)
# ======================
salidas_joined <- salidas_joined %>%
  mutate(
    tipo_sustitucion = case_when(
      is.na(id_colaborador_anterior_posicion) ~ "POSICION_NUEVA",
      grepl("PROMO", toupper(evento_salida), fixed = TRUE) ~ "SUSTITUCION_POR_PROMOCION",
      grepl("BAJA", toupper(evento_salida), fixed = TRUE) |
      grepl("TERM", toupper(evento_salida), fixed = TRUE) |
      grepl("RENUNCIA", toupper(evento_salida), fixed = TRUE) ~ "SUSTITUCION_POR_ROTACION",
      TRUE ~ "SUSTITUCION_NO_CLASIFICADA"
    )
  )

# ======================
# 8) Seleccionar SOLO las columnas que quieres validar
# ======================
traza_final <- salidas_joined %>%
  transmute(
    id_key,
    id_colaborador,
    nombre_colaborador,
    id_posicion_nueva,
    posicion_nueva,
    fecha_movimiento,
    # anterior del colaborador
    id_posicion_anterior,
    posicion_anterior,
    fecha_mov_anterior,
    # ocupante anterior de la posición y su salida
    id_colaborador_anterior_posicion,
    fecha_mov_anterior_posicion,
    id_posicion_destino_ocupante = id_posicion_destino,
    nombre_puesto_destino_ocupante = posicion_destino,
    evento_salida_ocupante = evento_salida,
    razon_salida_ocupante = razon_salida,
    fecha_salida_ocupante = fecha_movimiento_destino,
    tipo_sustitucion
  )

# ======================
# 9) Comprobaciones rápidas
# ======================
# 9.a) ¿Algún id_key duplicado?
dups <- traza_final %>%
  count(id_key) %>%
  filter(n > 1)

if(nrow(dups) == 0) {
  message("OK: No hay id_key duplicados en traza_final.")
} else {
  warning("ATENCION: Hay id_key duplicados. Ejemplos:")
  print(head(dups))
}

# 9.b) Mostrar primeras filas para revisión rápida
print(head(traza_final, 20))

# ======================
# 10) Exportar a Excel
# ======================
write.xlsx(traza_final, output_path, overwrite = TRUE)
message("Archivo exportado en:\n", output_path)
