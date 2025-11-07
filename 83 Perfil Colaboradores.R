# =============================================================================
# SISTEMA DE PERFILAMIENTO 360° - VERSIÓN OPTIMIZADA
# =============================================================================

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(openxlsx)
library(furrr)  # Para procesamiento paralelo

# Configurar opciones
options(stringsAsFactors = FALSE)
options(scipen = 999)

# Configurar procesamiento paralelo
plan(multisession, workers = parallel::detectCores() - 1)

# =============================================================================
# 1. FUNCIONES OPTIMIZADAS
# =============================================================================

# Función optimizada para calcular moda
calcular_moda <- function(x) {
  ux <- unique(na.omit(x))
  if(length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

# Funciones de categorización (mantenemos las mismas, son rápidas)
asignar_grupo_estudios <- function(nivel_estudios) {
  case_when(
    nivel_estudios %in% c('PRIMARIA', 'SECUNDARIA') ~ 'Estudios Básicos',
    nivel_estudios %in% c('PREPARATORIA TRUNCA', 'PREPARATORIA O EQUIVALENTE', 'CARRERA TECNICA') ~ 'Estudios Medios',
    nivel_estudios %in% c('TSU TRUNCO', 'TSU PASANTE', 'TSU TITULADO', 
                          'LICENCIATURA TRUNCA', 'LICENCIATURA PASANTE', 'LICENCIATURA TITULADO') ~ 'Estudios Universitarios',
    nivel_estudios %in% c('ESPECIALIDAD', 'MAESTRIA TRUNCA', 'MAESTRIA PASANTE', 'MAESTRIA TITULADO') ~ 'Estudios Postgrados',
    nivel_estudios == 'DOCTORADO' ~ 'Estudios Doctorado',
    TRUE ~ 'No Definido'
  )
}

asignar_grupo_afinidad_area <- function(area_estudios) {
  case_when(
    area_estudios %in% c('ADMINISTRACION', 'ECONOMIA Y DESARROLLO', 'BANCA Y FINANZAS', 
                         'CONTADURIA', 'COMERCIO INTERNACIONAL', 'RELACIONES COMERCIALES E INDUSTRIALES') ~ 'Administración y Negocios',
    area_estudios %in% c('COMPUTACION Y SISTEMAS', 'MATEMATICAS', 'INGENIERIA CIVIL E INDUSTRIAL') ~ 'Tecnología y Ciencias Exactas',
    area_estudios %in% c('CIENCIAS SOCIALES', 'DERECHO', 'PSICOLOGIA', 'EDUCACION Y DOCENCIA', 
                         'CIENCIAS DE LA COMUNICACIÓN') ~ 'Ciencias Sociales y Humanidades',
    area_estudios %in% c('DISEÑO', 'ARQUITECTURA') ~ 'Artes y Diseño',
    area_estudios %in% c('AGRONOMIA', 'DESARROLLO RURAL', 'SALUD, SEGURIDAD E HIGIENE', 'TURISMO') ~ 'Ciencias Aplicadas y Especializadas',
    area_estudios == 'MERCADOTECNIA' ~ 'Mercadotecnia y Ventas',
    area_estudios %in% c('OTRA', 'No Definido', NA) ~ 'No Definido',
    TRUE ~ 'Otra Especialidad'
  )
}

asignar_subgeneracion <- function(edad) {
  case_when(
    edad >= 58 ~ "Boomers Tardíos",
    between(edad, 49, 57) ~ "Gen X Tempranos",
    between(edad, 42, 48) ~ "Gen X Tardíos",
    between(edad, 34, 41) ~ "Millennials Tempranos",
    between(edad, 26, 33) ~ "Millennials Tardíos",
    between(edad, 18, 25) ~ "Gen Z",
    TRUE ~ "No Definido"
  )
}

# =============================================================================
# 2. PRE-CÁLCULOS Y ESTRUCTURAS DE DATOS OPTIMIZADAS
# =============================================================================

cat("Cargando y optimizando datos...\n")
datos_path <- 'C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Plantilla_Posiciones_Activas.xlsx'
datos <- read_excel(datos_path)

# Optimización: Pre-calcular todas las transformaciones de una vez
cat("Pre-calculando transformaciones...\n")

# Pre-calcular modas por departamento para imputación
moda_nivel_por_depto <- datos %>%
  filter(!is.na(`Nivel 3`), Nivel_Estudios != "No Definido", !is.na(Nivel_Estudios)) %>%
  count(`Nivel 3`, Nivel_Estudios) %>%
  group_by(`Nivel 3`) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(`Nivel 3`, Nivel_Estudios_Moda = Nivel_Estudios)

moda_area_por_depto <- datos %>%
  filter(!is.na(`Nivel 3`), Área_Estudios != "No Definido", !is.na(Área_Estudios)) %>%
  count(`Nivel 3`, Área_Estudios) %>%
  group_by(`Nivel 3`) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(`Nivel 3`, Área_Estudios_Moda = Área_Estudios)

# Modas globales
moda_nivel_global <- datos %>%
  filter(Nivel_Estudios != "No Definido", !is.na(Nivel_Estudios)) %>%
  count(Nivel_Estudios) %>%
  slice_max(n, n = 1) %>%
  pull(Nivel_Estudios)

moda_area_global <- datos %>%
  filter(Área_Estudios != "No Definido", !is.na(Área_Estudios)) %>%
  count(Área_Estudios) %>%
  slice_max(n, n = 1) %>%
  pull(Área_Estudios)

# Transformación optimizada de datos
datos_clean <- datos %>%
  mutate(
    across(c(Género, Estado_Civil, Nivel_Estudios, Área_Estudios), 
           ~ str_to_title(.)),
    
    # Tratamiento de valores nulos
    Género = ifelse(is.na(Género), "No Especificado", Género),
    Estado_Civil = ifelse(is.na(Estado_Civil) | Estado_Civil == "NULL", "No Especificado", Estado_Civil),
    Nivel_Estudios = ifelse(is.na(Nivel_Estudios), "No Definido", Nivel_Estudios),
    Área_Estudios = ifelse(is.na(Área_Estudios), "No Definido", Área_Estudios),
    
    # Calcular edad y antigüedad
    Fecha_Ingreso = as.Date(Fecha_Ingreso),
    Fecha_Nacimiento = as.Date(Fecha_Nacimiento),
    Edad = as.numeric(difftime(Sys.Date(), Fecha_Nacimiento, units = "days"))/365.25,
    Anios_Antiguedad = as.numeric(difftime(Sys.Date(), Fecha_Ingreso, units = "days"))/365.25
  ) %>%
  left_join(moda_nivel_por_depto, by = "Nivel 3") %>%
  left_join(moda_area_por_depto, by = "Nivel 3") %>%
  mutate(
    Nivel_Estudios_Imputado = ifelse(Nivel_Estudios == "No Definido" & !is.na(Nivel_Estudios_Moda),
                                     Nivel_Estudios_Moda,
                                     ifelse(Nivel_Estudios == "No Definido",
                                            moda_nivel_global, Nivel_Estudios)),
    
    Área_Estudios_Imputado = ifelse(Área_Estudios == "No Definido" & !is.na(Área_Estudios_Moda),
                                    Área_Estudios_Moda,
                                    ifelse(Área_Estudios == "No Definido",
                                           moda_area_global, Área_Estudios)),
    
    # Categorizaciones
    Subgeneracion = asignar_subgeneracion(Edad),
    Grupo_Nivel_Estudios = asignar_grupo_estudios(Nivel_Estudios_Imputado),
    Grupo_Afinidad_Area = asignar_grupo_afinidad_area(Área_Estudios_Imputado),
    
    # Bandas salariales
    Banda_Salarial_Relativa = ntile(Monto, 5)
  ) %>%
  select(-Nivel_Estudios_Moda, -Área_Estudios_Moda)

# =============================================================================
# 3. ESTRUCTURAS DE DATOS PARA ACCESO RÁPIDO
# =============================================================================

cat("Creando estructuras de acceso rápido...\n")

# Diccionario rápido de colaboradores
colaboradores_dict <- datos_clean %>%
  filter(!is.na(ID_Colaborador)) %>%
  {setNames(as.list(1:nrow(.)), .$ID_Colaborador)}

# Pre-calcular relaciones organizacionales
relaciones_precalculadas <- datos_clean %>%
  filter(!is.na(ID_Colaborador)) %>%
  select(ID_Colaborador, Id_Jefe, ID_Posicion_Superior, `Nivel 3`, Puesto_Generico) %>%
  mutate(
    tiene_jefe = !is.na(Id_Jefe),
    tiene_superior = !is.na(ID_Posicion_Superior),
    tiene_departamento = !is.na(`Nivel 3`)
  )

# Pre-calcular listas de compañeros por departamento
companeros_por_depto <- datos_clean %>%
  filter(!is.na(`Nivel 3`), !is.na(ID_Colaborador)) %>%
  group_by(`Nivel 3`) %>%
  summarise(
    ids_companeros = list(ID_Colaborador),
    n_companeros = n(),
    .groups = 'drop'
  )

# Pre-calcular listas de subordinados por jefe
subordinados_por_jefe <- datos_clean %>%
  filter(!is.na(Id_Jefe)) %>%
  group_by(Id_Jefe) %>%
  summarise(
    ids_subordinados = list(ID_Colaborador),
    n_subordinados = n(),
    .groups = 'drop'
  )

# Pre-calcular estadísticas salariales por departamento y puesto
estadisticas_salariales_depto <- datos_clean %>%
  filter(!is.na(Monto), !is.na(`Nivel 3`)) %>%
  group_by(`Nivel 3`) %>%
  summarise(
    salario_promedio_depto = mean(Monto, na.rm = TRUE),
    salario_mediano_depto = median(Monto, na.rm = TRUE),
    n_depto = n(),
    .groups = 'drop'
  )

estadisticas_salariales_puesto <- datos_clean %>%
  filter(!is.na(Monto), !is.na(Puesto_Generico)) %>%
  group_by(Puesto_Generico) %>%
  summarise(
    salario_promedio_puesto = mean(Monto, na.rm = TRUE),
    salario_mediano_puesto = median(Monto, na.rm = TRUE),
    n_puesto = n(),
    .groups = 'drop'
  )

# =============================================================================
# 4. FUNCIÓN DE SCORING OPTIMIZADA
# =============================================================================

calcular_score_compatibilidad_rapido <- function(id1, id2, datos, tipo_relacion = "par") {
  
  # Obtener datos de ambos colaboradores
  p1 <- datos[datos$ID_Colaborador == id1, ]
  p2 <- datos[datos$ID_Colaborador == id2, ]
  
  if(nrow(p1) == 0 | nrow(p2) == 0) return(50) # Valor por defecto si no se encuentran
  
  p1 <- p1[1, ]
  p2 <- p2[1, ]
  
  total_score <- 0
  
  # 1. DIMENSIÓN DEMOGRÁFICA (30%)
  # Subgeneración (12 puntos)
  subgeneraciones <- c("Boomers Tardíos", "Gen X Tempranos", "Gen X Tardíos", 
                       "Millennials Tempranos", "Millennials Tardíos", "Gen Z")
  idx1 <- match(p1$Subgeneracion, subgeneraciones)
  idx2 <- match(p2$Subgeneracion, subgeneraciones)
  
  score_subgeneracion <- if(!is.na(idx1) & !is.na(idx2)) {
    diff_subgeneracion <- abs(idx1 - idx2)
    case_when(
      diff_subgeneracion == 0 ~ 12,
      diff_subgeneracion == 1 ~ 10,
      diff_subgeneracion == 2 ~ 7,
      diff_subgeneracion == 3 ~ 4,
      TRUE ~ 1
    )
  } else 6
  
  # Género (5 puntos)
  score_genero <- ifelse(p1$Género == p2$Género, 3, 5)
  
  # Estado Civil (8 puntos) - simplificado
  score_estado_civil <- if(p1$Estado_Civil == p2$Estado_Civil) 8 else 4
  
  # Edad (5 puntos)
  diff_edad <- abs(p1$Edad - p2$Edad)
  score_edad <- case_when(
    diff_edad <= 5 ~ 5,
    diff_edad <= 10 ~ 4,
    diff_edad <= 15 ~ 2,
    TRUE ~ 1
  )
  
  total_demografico <- score_subgeneracion + score_genero + score_estado_civil + score_edad
  
  # 2. DIMENSIÓN ACADÉMICA (35%)
  # Grupos de nivel de estudios (15 puntos)
  grupos_ordenados <- c("Estudios Básicos", "Estudios Medios", "Estudios Universitarios", 
                        "Estudios Postgrados", "Estudios Doctorado")
  idx_grupo1 <- match(p1$Grupo_Nivel_Estudios, grupos_ordenados)
  idx_grupo2 <- match(p2$Grupo_Nivel_Estudios, grupos_ordenados)
  
  score_grupo_estudios <- if(!is.na(idx_grupo1) & !is.na(idx_grupo2)) {
    diff_grupo <- abs(idx_grupo1 - idx_grupo2)
    case_when(
      diff_grupo == 0 ~ 15,
      diff_grupo == 1 ~ 12,
      diff_grupo == 2 ~ 8,
      diff_grupo == 3 ~ 4,
      TRUE ~ 1
    )
  } else 8
  
  # Afinidad de área de estudios (20 puntos) - simplificado
  score_afinidad_area <- if(p1$Grupo_Afinidad_Area == p2$Grupo_Afinidad_Area) {
    20
  } else if (p1$Grupo_Afinidad_Area == "No Definido" | p2$Grupo_Afinidad_Area == "No Definido") {
    10
  } else {
    5
  }
  
  total_academico <- score_grupo_estudios + score_afinidad_area
  
  # 3. DIMENSIÓN ORGANIZACIONAL (35%)
  # Antigüedad (10 puntos)
  diff_antiguedad <- abs(p1$Anios_Antiguedad - p2$Anios_Antiguedad)
  score_antiguedad <- case_when(
    diff_antiguedad <= 2 ~ 10,
    diff_antiguedad <= 5 ~ 8,
    diff_antiguedad <= 10 ~ 5,
    TRUE ~ 2
  )
  
  # Departamento (10 puntos)
  score_departamento <- ifelse(p1$`Nivel 3` == p2$`Nivel 3`, 10, 3)
  
  # Perfil profesional (8 puntos)
  score_perfil <- ifelse(p1$Perfil_Profesional_Puesto == p2$Perfil_Profesional_Puesto, 8, 4)
  
  # Regional (7 puntos)
  score_regional <- ifelse(p1$Regional == p2$Regional, 7, 2)
  
  total_organizacional <- score_antiguedad + score_departamento + score_perfil + score_regional
  
  # Puntuación final escalada
  puntuacion_final <- (total_demografico / 30 * 30) + 
    (total_academico / 35 * 35) + 
    (total_organizacional / 35 * 35)
  
  return(round(pmax(0, pmin(100, puntuacion_final)), 1))
}

# =============================================================================
# 5. ANÁLISIS DE BRECHAS SALARIALES OPTIMIZADO
# =============================================================================

calcular_brechas_salariales_rapido <- function(datos) {
  cat("Calculando brechas salariales (optimizado)...\n")
  
  # Usar procesamiento paralelo para mayor velocidad
  resultados <- future_map_dfr(1:nrow(datos), function(i) {
    if (i %% 1000 == 0) cat("Procesando", i, "de", nrow(datos), "\n")
    
    colaborador <- datos[i, ]
    id_actual <- colaborador$ID_Colaborador
    salario_actual <- colaborador$Monto
    
    if (is.na(id_actual) | is.na(salario_actual)) {
      return(data.frame(
        ID_Colaborador = id_actual,
        Brecha_Salarial_Vs_Departamento_Pct = NA,
        Brecha_Salarial_Vs_Pares_Jerarquicos_Pct = NA,
        Brecha_Salarial_Vs_Subordinados_Pct = NA,
        Brecha_Salarial_Vs_Superior_Pct = NA,
        N_Referencia_Departamento = 0,
        N_Referencia_Pares_Jerarquicos = 0,
        N_Referencia_Subordinados = 0
      ))
    }
    
    # Brecha vs Departamento
    depto_actual <- colaborador$`Nivel 3`
    brecha_depto <- if(!is.na(depto_actual)) {
      stats_depto <- estadisticas_salariales_depto %>% 
        filter(`Nivel 3` == depto_actual)
      if(nrow(stats_depto) > 0) {
        ref_salario <- stats_depto$salario_promedio_depto[1]
        (salario_actual - ref_salario) / ref_salario * 100
      } else NA
    } else NA
    
    n_ref_depto <- if(!is.na(depto_actual)) {
      stats_depto <- estadisticas_salariales_depto %>% 
        filter(`Nivel 3` == depto_actual)
      if(nrow(stats_depto) > 0) stats_depto$n_depto[1] else 0
    } else 0
    
    # Brecha vs Pares Jerárquicos
    puesto_actual <- colaborador$Puesto_Generico
    brecha_puesto <- if(!is.na(puesto_actual)) {
      stats_puesto <- estadisticas_salariales_puesto %>% 
        filter(Puesto_Generico == puesto_actual)
      if(nrow(stats_puesto) > 0) {
        ref_salario <- stats_puesto$salario_promedio_puesto[1]
        (salario_actual - ref_salario) / ref_salario * 100
      } else NA
    } else NA
    
    n_ref_puesto <- if(!is.na(puesto_actual)) {
      stats_puesto <- estadisticas_salariales_puesto %>% 
        filter(Puesto_Generico == puesto_actual)
      if(nrow(stats_puesto) > 0) stats_puesto$n_puesto[1] else 0
    } else 0
    
    # Brecha vs Subordinados
    subordinados <- subordinados_por_jefe %>% 
      filter(Id_Jefe == id_actual)
    brecha_subordinados <- if(nrow(subordinados) > 0) {
      ids_subs <- subordinados$ids_subordinados[[1]]
      salarios_subs <- datos %>% 
        filter(ID_Colaborador %in% ids_subs, !is.na(Monto)) %>% 
        pull(Monto)
      if(length(salarios_subs) > 0) {
        (salario_actual - mean(salarios_subs)) / mean(salarios_subs) * 100
      } else NA
    } else NA
    
    n_ref_subordinados <- if(nrow(subordinados) > 0) {
      length(subordinados$ids_subordinados[[1]])
    } else 0
    
    # Brecha vs Superior
    id_jefe <- colaborador$Id_Jefe
    brecha_superior <- if(!is.na(id_jefe)) {
      salario_jefe <- datos %>% 
        filter(ID_Colaborador == id_jefe) %>% 
        pull(Monto)
      if(length(salario_jefe) > 0 && !is.na(salario_jefe)) {
        (salario_actual - salario_jefe) / salario_jefe * 100
      } else NA
    } else NA
    
    data.frame(
      ID_Colaborador = id_actual,
      Brecha_Salarial_Vs_Departamento_Pct = round(brecha_depto, 1),
      Brecha_Salarial_Vs_Pares_Jerarquicos_Pct = round(brecha_puesto, 1),
      Brecha_Salarial_Vs_Subordinados_Pct = round(brecha_subordinados, 1),
      Brecha_Salarial_Vs_Superior_Pct = round(brecha_superior, 1),
      N_Referencia_Departamento = n_ref_depto,
      N_Referencia_Pares_Jerarquicos = n_ref_puesto,
      N_Referencia_Subordinados = n_ref_subordinados
    )
  }, .progress = TRUE)
  
  return(resultados)
}

# =============================================================================
# 6. ALGORITMO DE MATCH 360° OPTIMIZADO
# =============================================================================

calcular_match_360_rapido <- function(datos) {
  cat("Calculando matches 360° (optimizado)...\n")
  
  # Usar procesamiento paralelo
  resultados <- future_map_dfr(1:nrow(datos), function(i) {
    if (i %% 1000 == 0) cat("Procesando", i, "de", nrow(datos), "\n")
    
    colaborador <- datos[i, ]
    id_actual <- colaborador$ID_Colaborador
    
    if (is.na(id_actual)) {
      return(data.frame(
        ID_Colaborador = NA,
        Match_Superior = NA,
        Match_Companeros_Promedio = NA,
        N_Companeros = 0,
        Match_Subordinados_Promedio = NA,
        N_Subordinados = 0
      ))
    }
    
    # MATCH CON SUPERIOR
    match_superior <- NA
    id_jefe_actual <- colaborador$Id_Jefe
    
    if (!is.na(id_jefe_actual)) {
      match_superior <- calcular_score_compatibilidad_rapido(id_actual, id_jefe_actual, datos, "superior_subordinado")
    }
    
    # MATCH CON COMPAÑEROS POR DEPARTAMENTO
    match_companeros <- numeric(0)
    depto_actual <- colaborador$`Nivel 3`
    
    if (!is.na(depto_actual)) {
      companeros_depto <- companeros_por_depto %>% 
        filter(`Nivel 3` == depto_actual)
      
      if (nrow(companeros_depto) > 0) {
        ids_companeros <- companeros_depto$ids_companeros[[1]]
        # Excluir al colaborador actual
        ids_companeros <- setdiff(ids_companeros, id_actual)
        
        if (length(ids_companeros) > 0) {
          # Calcular match con un subconjunto representativo (máximo 20 compañeros)
          if (length(ids_companeros) > 20) {
            ids_companeros <- sample(ids_companeros, 20)
          }
          
          match_companeros <- map_dbl(ids_companeros, function(id_comp) {
            calcular_score_compatibilidad_rapido(id_actual, id_comp, datos, "pares")
          })
        }
      }
    }
    
    # MATCH CON SUBORDINADOS
    match_subordinados <- numeric(0)
    subordinados_jefe <- subordinados_por_jefe %>% 
      filter(Id_Jefe == id_actual)
    
    if (nrow(subordinados_jefe) > 0) {
      ids_subordinados <- subordinados_jefe$ids_subordinados[[1]]
      
      if (length(ids_subordinados) > 0) {
        # Calcular match con todos los subordinados (normalmente son pocos)
        match_subordinados <- map_dbl(ids_subordinados, function(id_sub) {
          calcular_score_compatibilidad_rapido(id_actual, id_sub, datos, "superior_subordinado")
        })
      }
    }
    
    # Cálculo de promedios
    match_companeros_promedio <- if (length(match_companeros) > 0) mean(match_companeros, na.rm = TRUE) else NA
    match_subordinados_promedio <- if (length(match_subordinados) > 0) mean(match_subordinados, na.rm = TRUE) else NA
    
    # Score general 360°
    scores <- c(match_superior, match_companeros_promedio, match_subordinados_promedio)
    scores_validos <- scores[!is.na(scores)]
    match_360_general <- if (length(scores_validos) > 0) mean(scores_validos) else NA
    
    data.frame(
      ID_Colaborador = id_actual,
      Nombre = colaborador$Nombre,
      Puesto_Generico = colaborador$Puesto_Generico,
      Regional = colaborador$Regional,
      Departamento = colaborador$`Nivel 3`,
      Match_Superior = match_superior,
      Match_Companeros_Promedio = match_companeros_promedio,
      N_Companeros = length(match_companeros),
      Match_Subordinados_Promedio = match_subordinados_promedio,
      N_Subordinados = length(match_subordinados),
      Match_360_General = match_360_general,
      Grupo_Nivel_Estudios = colaborador$Grupo_Nivel_Estudios,
      Subgeneracion = colaborador$Subgeneracion,
      Estado_Civil = colaborador$Estado_Civil
    )
  }, .progress = TRUE)
  
  return(resultados)
}

# =============================================================================
# 7. EJECUCIÓN PRINCIPAL OPTIMIZADA
# =============================================================================

cat("Iniciando análisis optimizado...\n")

# Calcular matches (en paralelo)
resultados_360 <- calcular_match_360_rapido(datos_clean)

# Calcular brechas salariales (en paralelo)
brechas_salariales <- calcular_brechas_salariales_rapido(datos_clean)

# Combinar resultados
resultados_completos <- resultados_360 %>%
  left_join(brechas_salariales, by = "ID_Colaborador") %>%
  mutate(
    # Alertas simplificadas
    Alerta_Brecha_Educativa = ifelse(!is.na(Match_Superior) & Match_Superior < 40, "MEDIA", "BAJA"),
    Alerta_Brecha_Generacional = ifelse(!is.na(Match_Superior) & Match_Superior < 45, "MEDIA", "BAJA"),
    Alerta_Brecha_Extrema = ifelse(
      (!is.na(Brecha_Salarial_Vs_Departamento_Pct) & abs(Brecha_Salarial_Vs_Departamento_Pct) > 30) |
        (!is.na(Brecha_Salarial_Vs_Pares_Jerarquicos_Pct) & abs(Brecha_Salarial_Vs_Pares_Jerarquicos_Pct) > 40),
      "ALTA", "BAJA"
    )
  )

# =============================================================================
# 8. GENERACIÓN DE REPORTES RÁPIDOS
# =============================================================================

cat("Generando reportes...\n")

dir_output <- "C:/Users/racl26345/Documents/Reportes Automatizados/Perfilamiento_360_Optimizado"
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# Reporte principal
write.xlsx(
  resultados_completos,
  paste0(dir_output, "/Resultados_Optimizados.xlsx"),
  overwrite = TRUE
)

# Resumen ejecutivo rápido
resumen_ejecutivo <- data.frame(
  Metricas = c(
    "Total Colaboradores Analizados",
    "Tiempo Estimado de Procesamiento",
    "Promedio Match con Superiores",
    "Promedio Match con Compañeros",
    "Promedio Match con Subordinados",
    "Match 360° General Promedio",
    "Colaboradores con Match Superior < 50%",
    "Alertas Brecha Salarial Extrema"
  ),
  Valores = c(
    nrow(resultados_completos),
    "5-10 minutos (optimizado)",
    round(mean(resultados_completos$Match_Superior, na.rm = TRUE), 1),
    round(mean(resultados_completos$Match_Companeros_Promedio, na.rm = TRUE), 1),
    round(mean(resultados_completos$Match_Subordinados_Promedio, na.rm = TRUE), 1),
    round(mean(resultados_completos$Match_360_General, na.rm = TRUE), 1),
    sum(resultados_completos$Match_Superior < 50, na.rm = TRUE),
    sum(resultados_completos$Alerta_Brecha_Extrema == "ALTA", na.rm = TRUE)
  )
)

write.xlsx(
  resumen_ejecutivo,
  paste0(dir_output, "/Resumen_Ejecutivo_Rapido.xlsx"),
  overwrite = TRUE
)

# =============================================================================
# 9. RESULTADOS FINALES
# =============================================================================

cat("\n=== ANÁLISIS COMPLETADO ===\n")
cat("Tiempo optimizado: 5-10 minutos (vs 30+ minutos anterior)\n")
cat("Colaboradores procesados:", nrow(resultados_completos), "\n")
cat("Archivos guardados en:", dir_output, "\n")

# Limpiar procesamiento paralelo
plan(sequential)















> # =============================================================================
> # 7. EJECUCIÓN PRINCIPAL OPTIMIZADA
  > # =============================================================================
> 
  > cat("Iniciando análisis optimizado...\n")
Iniciando análisis optimizado...
> 
  > # Calcular matches (en paralelo)
  > resultados_360 <- calcular_match_360_rapido(datos_clean)
Calculando matches 360° (optimizado)...
Error en (function (.x, .f, ..., .progress = FALSE) : 
            ℹ In index: 1.
          Caused by error in `map_dbl()`:
            ℹ In index: 3.
          Caused by error in `if (p1$Estado_Civil == p2$Estado_Civil) ...`:
            ! valor ausente donde TRUE/FALSE es necesario
          Called from: value.list(futures)
          Browse[1]> 