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



# INSTALACIÓN (solo primera vez)
# install.packages(c("reticulate", "readxl", "openxlsx", "dplyr", "tidyr"))
library(reticulate)
library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)

# Configurar entorno Anaconda
use_condaenv("base", required = TRUE)

# ---- PARTE EN PYTHON ----
py_run_string("
import pandas as pd
import numpy as np
from geopy.geocoders import Nominatim
from geopy.extra.rate_limiter import RateLimiter
from geopy.distance import geodesic
import time
import os

# Configuración mejorada de geocodificación
def configurar_geocodificador():
    try:
        geolocator = Nominatim(
            user_agent='codigos_postales_mexico',
            domain='nominatim.openstreetmap.org',
            timeout=10
        )
        return RateLimiter(geolocator.geocode, min_delay_seconds=1)
    except Exception as e:
        print(f'Error configurando geocodificador: {str(e)}')
        return None

geocode = configurar_geocodificador()

# Función mejorada para obtener coordenadas
def obtener_coordenadas(cp, intentos=3):
    if geocode is None:
        return (np.nan, np.nan)
        
    for i in range(intentos):
        try:
            # Búsqueda optimizada para códigos postales mexicanos
            location = geocode(query=f'{cp}, Mexico', addressdetails=True, country_codes='mx')
            if location:
                return (location.latitude, location.longitude)
        except Exception as e:
            print(f'Intento {i+1} fallido para CP {cp}: {str(e)}')
            time.sleep(2)
    return (np.nan, np.nan)

# Función para calcular distancia con cache en disco
def calcular_distancia(cp1, cp2, cache_file='cp_cache.csv'):
    try:
        # Cargar cache existente o crear nuevo
        if os.path.exists(cache_file):
            datos_cp = pd.read_csv(cache_file)
        else:
            datos_cp = pd.DataFrame(columns=['CP', 'Latitud', 'Longitud'])
        
        # Buscar o obtener coordenadas para cp1
        if cp1 in datos_cp['CP'].values:
            lat1, lon1 = datos_cp.loc[datos_cp['CP'] == cp1, ['Latitud', 'Longitud']].values[0]
        else:
            print(f'Buscando coordenadas para CP: {cp1}')
            lat1, lon1 = obtener_coordenadas(cp1)
            nuevos_datos = pd.DataFrame({'CP': [cp1], 'Latitud': [lat1], 'Longitud': [lon1]})
            datos_cp = pd.concat([datos_cp, nuevos_datos], ignore_index=True)
            datos_cp.to_csv(cache_file, index=False)
        
        # Buscar o obtener coordenadas para cp2
        if cp2 in datos_cp['CP'].values:
            lat2, lon2 = datos_cp.loc[datos_cp['CP'] == cp2, ['Latitud', 'Longitud']].values[0]
        else:
            print(f'Buscando coordenadas para CP: {cp2}')
            lat2, lon2 = obtener_coordenadas(cp2)
            nuevos_datos = pd.DataFrame({'CP': [cp2], 'Latitud': [lat2], 'Longitud': [lon2]})
            datos_cp = pd.concat([datos_cp, nuevos_datos], ignore_index=True)
            datos_cp.to_csv(cache_file, index=False)
        
        # Calcular distancia
        if np.isnan(lat1) or np.isnan(lat2):
            return {
                'cp1': cp1,
                'cp2': cp2,
                'lat1': lat1,
                'lon1': lon1,
                'lat2': lat2,
                'lon2': lon2,
                'distancia_km': np.nan,
                'error': 'Coordenadas no encontradas'
            }
            
        distancia = geodesic((lat1, lon1), (lat2, lon2)).kilometers
        return {
            'cp1': cp1,
            'cp2': cp2,
            'lat1': lat1,
            'lon1': lon1,
            'lat2': lat2,
            'lon2': lon2,
            'distancia_km': round(distancia, 2),
            'error': None
        }
        
    except Exception as e:
        print(f'Error procesando {cp1}-{cp2}: {str(e)}')
        return {
            'cp1': cp1,
            'cp2': cp2,
            'lat1': np.nan,
            'lon1': np.nan,
            'lat2': np.nan,
            'lon2': np.nan,
            'distancia_km': np.nan,
            'error': str(e)
        }
")

# ---- PARTE EN R ----
# Función para seleccionar archivo interactivamente
seleccionar_archivo <- function() {
  ruta <- file.choose()
  if (length(ruta) == 0) {
    stop("No se seleccionó ningún archivo")
  }
  return(ruta)
}

# Función mejorada para procesar el archivo
procesar_distancias <- function(ruta_entrada = NULL) {
  # Seleccionar archivo si no se proporciona ruta
  if (is.null(ruta_entrada)) {
    cat("Por favor selecciona tu archivo Excel con los códigos postales\n")
    ruta_entrada <- seleccionar_archivo()
  }
  
  # Leer archivo Excel
  datos <- tryCatch({
    read_excel(ruta_entrada) %>%
      mutate(across(c(cp_1, cp_2), as.character))
  }, error = function(e) {
    stop(paste("Error leyendo el archivo:", e$message))
  })
  
  # Verificar columnas
  if (!all(c("cp_1", "cp_2") %in% colnames(datos))) {
    stop("El archivo debe contener columnas 'cp_1' y 'cp_2'")
  }
  
  cat("\nCalculando distancias...\n")
  
  # Calcular distancias para cada par
  resultados <- list()
  for (i in 1:nrow(datos)) {
    cp1 <- datos$cp_1[i]
    cp2 <- datos$cp_2[i]
    
    cat(sprintf("Procesando par %d de %d: %s - %s\n", i, nrow(datos), cp1, cp2))
    
    # Llamar a la función Python
    resultado <- py$calcular_distancia(cp1, cp2)
    
    # Convertir a dataframe
    resultados[[i]] <- as.data.frame(resultado)
    
    # Pequeña pausa para evitar saturación
    if (i %% 5 == 0) Sys.sleep(1)
  }
  
  # Combinar todos los resultados
  resultados_final <- bind_rows(resultados)
  
  return(resultados_final)
}

# Función para guardar resultados
guardar_resultados <- function(resultados, ruta_salida = NULL) {
  if (is.null(ruta_salida)) {
    ruta_salida <- file.path(dirname(seleccionar_archivo()), "resultados_distancias.xlsx")
  }
  
  # Crear lista de hojas
  hojas <- list(
    "Distancias" = resultados,
    "Coordenadas" = bind_rows(
      resultados %>%
        select(CP = cp1, Latitud = lat1, Longitud = lon1) %>%
        distinct(),
      resultados %>%
        select(CP = cp2, Latitud = lat2, Longitud = lon2) %>%
        distinct()
    ) %>%
      distinct(CP, .keep_all = TRUE)
  )
  
  # Guardar archivo Excel
  write.xlsx(hojas, file = ruta_salida)
  
  return(ruta_salida)
}

# Ejecución principal
main <- function() {
  tryCatch({
    # Procesar archivo
    resultados <- procesar_distancias()
    
    # Mostrar resumen
    cat("\nResultados obtenidos:\n")
    print(resultados %>% select(cp1, cp2, distancia_km))
    
    # Guardar resultados
    ruta_guardado <- guardar_resultados(resultados)
    cat(paste("\nResultados guardados en:", ruta_guardado, "\n"))
    
  }, error = function(e) {
    cat(paste("\nError en el proceso:", e$message, "\n"))
  })
}

# Ejecutar
main()










































###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################


WITH movimientos_base AS (
    SELECT *,
           ROW_NUMBER() OVER (
               PARTITION BY id_colaborador
               ORDER BY fecha_efectiva_movimiento
           ) AS secuencia_movimiento
    FROM hist_movimientos
    WHERE fecha_efectiva_movimiento >= '2022-08-22'
),

movimiento_colaborador AS (
    SELECT
        actual.*,

        anterior.id_posicion                 AS id_posicion_anterior_colaborador,
        anterior.nombre_puesto               AS nombre_puesto_anterior_colaborador,
        anterior.fecha_efectiva_movimiento   AS fecha_mov_anterior_colaborador,
        anterior.evento_asociado             AS evento_anterior_colaborador

    FROM movimientos_base actual
    LEFT JOIN movimientos_base anterior
        ON actual.id_colaborador = anterior.id_colaborador
       AND actual.secuencia_movimiento = anterior.secuencia_movimiento + 1
),

ocupante_anterior_posicion AS (
    SELECT
        m_actual.id_colaborador              AS id_colaborador_actual,
        m_actual.id_posicion                 AS id_posicion_actual,
        m_actual.fecha_efectiva_movimiento   AS fecha_mov_actual,

        m_prev.id_colaborador                AS id_colaborador_anterior_posicion,
        m_prev.fecha_efectiva_movimiento     AS fecha_mov_anterior_posicion,

        ROW_NUMBER() OVER (
            PARTITION BY m_actual.id_colaborador, m_actual.id_posicion
            ORDER BY m_prev.fecha_efectiva_movimiento DESC
        ) AS rn

    FROM movimientos_base m_actual
    LEFT JOIN movimientos_base m_prev
        ON m_actual.id_posicion = m_prev.id_posicion
       AND m_prev.fecha_efectiva_movimiento < m_actual.fecha_efectiva_movimiento
       AND m_prev.id_colaborador <> m_actual.id_colaborador
),

movimiento_salida_ocupante AS (
    SELECT
        oap.id_colaborador_actual,
        oap.id_posicion_actual,
        oap.id_colaborador_anterior_posicion,

        m_sig.id_posicion                    AS id_posicion_destino_ocupante,
        m_sig.nombre_puesto                  AS nombre_puesto_destino_ocupante,
        m_sig.evento_asociado                AS evento_salida_ocupante,
        m_sig.fecha_efectiva_movimiento      AS fecha_salida_ocupante

    FROM ocupante_anterior_posicion oap
    LEFT JOIN movimientos_base m_sig
        ON oap.id_colaborador_anterior_posicion = m_sig.id_colaborador
       AND m_sig.fecha_efectiva_movimiento > oap.fecha_mov_anterior_posicion
    WHERE oap.rn = 1
)

SELECT
    mc.id_colaborador,
    mc.nombre                                AS nombre_colaborador,
    mc.id_posicion                           AS id_posicion_actual,
    mc.nombre_puesto                         AS nombre_puesto_actual,
    mc.fecha_efectiva_movimiento             AS fecha_movimiento_actual,
    mc.evento_asociado                       AS evento_actual,

    mc.id_posicion_anterior_colaborador,
    mc.nombre_puesto_anterior_colaborador,
    mc.fecha_mov_anterior_colaborador,
    mc.evento_anterior_colaborador,

    o.id_colaborador_anterior_posicion,
    o.id_posicion_destino_ocupante,
    o.nombre_puesto_destino_ocupante,
    o.evento_salida_ocupante,
    o.fecha_salida_ocupante,

    CASE
        WHEN o.id_colaborador_anterior_posicion IS NULL THEN 'POSICION_NUEVA'
        WHEN o.evento_salida_ocupante LIKE '%PROMO%' THEN 'SUSTITUCION_POR_PROMOCION'
        WHEN o.evento_salida_ocupante LIKE '%BAJA%' 
          OR o.evento_salida_ocupante LIKE '%TERM%' 
          OR o.evento_salida_ocupante LIKE '%RENUNCIA%' THEN 'SUSTITUCION_POR_ROTACION'
        ELSE 'SUSTITUCION_NO_CLASIFICADA'
    END AS tipo_sustitucion

FROM movimiento_colaborador mc
LEFT JOIN movimiento_salida_ocupante o
    ON mc.id_colaborador = o.id_colaborador_actual
ORDER BY
    mc.id_colaborador,
    mc.fecha_efectiva_movimiento;
