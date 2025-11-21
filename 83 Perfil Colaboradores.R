# Perfilamiento 360° Optimizado - VERSIÓN MEJORADA CON PONDERACIONES Y UMBRALES ADAPTATIVOS
# Genera vectores por colaborador, calcula brechas por relaciones (jefe, pares, subordinados)
# incluyendo análisis salarial, y produce perfiles de Disonancia 360. Guarda resultados en carpeta de salida.

# Librerías
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(openxlsx)
library(furrr)
library(scales)

options(stringsAsFactors = FALSE)
options(scipen = 999)

# Configurar procesamiento paralelo
plan(multisession, workers = parallel::detectCores() - 1)

# Rutas (ajusta si fuera necesario)
datos_path <- 'C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Plantilla_Posiciones_Activas.xlsx'
dir_output <- 'C:/Users/racl26345/Documents/Reportes Automatizados/Perfilamiento_360_Optimizado'
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# Cargar datos
cat('Cargando datos desde:', datos_path, '\n')
datos <- read_excel(datos_path)

# Verificar que existe el campo Monto
if (!'Monto' %in% names(datos)) {
  stop("El campo 'Monto' no se encuentra en los datos. Por favor, verifica el archivo de entrada.")
}

# -----------------------------
# Funciones auxiliares MEJORADAS
# -----------------------------

# 1) Moda segura
calcular_moda <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 2) Mapas de grupos de estudios y afinidad
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

# 3) Nueva función de subgeneración por año de nacimiento
asignar_subgeneracion_anio <- function(anio_nac) {
  case_when(
    anio_nac <= 1945 ~ "Tradicionalistas",
    between(anio_nac, 1946, 1955) ~ "Boomers - Tempranos",
    between(anio_nac, 1956, 1964) ~ "Boomers - Tardíos",
    between(anio_nac, 1965, 1973) ~ "Gen X - Tempranos",
    between(anio_nac, 1974, 1980) ~ "Gen X - Tardíos",
    between(anio_nac, 1981, 1988) ~ "Millennials - Tempranos",
    between(anio_nac, 1989, 1996) ~ "Millennials - Tardíos",
    between(anio_nac, 1997, 2004) ~ "Gen Z - Tempranos",
    between(anio_nac, 2005, 2012) ~ "Gen Z - Tardíos",
    TRUE ~ "No Definido"
  )
}

# Comparación segura
eq_safe <- function(a, b) {
  if (is.na(a) | is.na(b)) return(NA)
  return(a == b)
}

# 4) FUNCIONES DE DISTANCIA MEJORADAS CON PONDERACIONES

# Pesos específicos para dimensiones clave (valores más altos = más importancia)
pesos_dimensiones <- c(
  Genero_num = 1.0,
  EstadoCivil_num = 0.8,
  Subgen_num = 1.5,        # Generación más importante
  NivelEst_num = 1.8,      # Estudios mucho más importante
  AfinidadArea_num = 1.6,  # Área más importante
  Edad_norm = 1.0,
  Antiguedad_norm = 1.3,   # Antigüedad importante
  Salario_norm = 2.5       # Salario muy importante
)

# Distancia Euclidiana Ponderada (entre vectores numéricos)
distancia_vector_ponderada <- function(vec1, vec2, pesos = pesos_dimensiones) {
  if (any(is.na(vec1)) | any(is.na(vec2))) {
    # ignorar dimensiones NA: calcular solo sobre dimensiones disponibles en ambos
    good <- !is.na(vec1) & !is.na(vec2)
    if (!any(good)) return(NA_real_)
    vec1 <- vec1[good]; vec2 <- vec2[good]
    pesos <- pesos[good]
  }
  sqrt(sum(pesos * (vec1 - vec2)^2))
}

# Normaliza una distancia a 0-1 (dividiendo por máximo posible con ponderaciones)
normalizar_distancia_ponderada <- function(d, pesos = pesos_dimensiones) {
  if (is.na(d)) return(NA_real_)
  # Máximo posible con pesos: sqrt(sum(pesos * (1-0)^2)) = sqrt(sum(pesos))
  maxd <- sqrt(sum(pesos))
  pmin(1, d / maxd)
}

# 5) FUNCIONES DE CLASIFICACIÓN MEJORADAS CON UMBRALES ADAPTATIVOS

# Calcular umbrales adaptativos basados en distribución real
calcular_umbrales_adaptativos <- function(distancias_norm) {
  distancias_clean <- distancias_norm[!is.na(distancias_norm)]
  if (length(distancias_clean) == 0) return(c(0.2, 0.4, 0.6, 0.8))
  
  # Usar quintiles para los 5 niveles
  quantile(distancias_clean, probs = c(0.2, 0.4, 0.6, 0.8), na.rm = TRUE)
}

# Función para clasificar disonancia en 5 niveles con umbrales adaptativos
clasificar_disonancia_adaptativa <- function(distancia_norm, umbrales) {
  case_when(
    is.na(distancia_norm) ~ NA_character_,
    distancia_norm < umbrales[1] ~ "Muy Baja",
    distancia_norm < umbrales[2] ~ "Baja",
    distancia_norm < umbrales[3] ~ "Neutra",
    distancia_norm < umbrales[4] ~ "Alta",
    TRUE ~ "Muy Alta"
  )
}

# 6) Función para determinar perfil 360 basado en combinaciones - MEJORADA
determinar_perfil_360 <- function(disonancia_superior, disonancia_pares, disonancia_subordinados, 
                                  tiene_superior, tiene_subordinados) {
  
  # Manejar valores NA
  if (is.na(tiene_superior)) tiene_superior <- FALSE
  if (is.na(tiene_subordinados)) tiene_subordinados <- FALSE
  if (is.na(disonancia_superior)) disonancia_superior <- "No Definido"
  if (is.na(disonancia_pares)) disonancia_pares <- "No Definido"
  if (is.na(disonancia_subordinados)) disonancia_subordinados <- "No Definido"
  
  # Caso 1: Colaborador individual (con superior y pares, sin subordinados)
  if (tiene_superior & !tiene_subordinados) {
    perfil <- case_when(
      disonancia_superior %in% c("Muy Baja", "Baja") & disonancia_pares %in% c("Muy Baja", "Baja") ~ "Armonía Organizacional",
      disonancia_superior %in% c("Muy Baja", "Baja") & disonancia_pares == "Neutra" ~ "Alineación con Liderazgo - Integración Neutral",
      disonancia_superior %in% c("Muy Baja", "Baja") & disonancia_pares %in% c("Alta", "Muy Alta") ~ "Alineación con Liderazgo - Desafío con Pares",
      disonancia_superior == "Neutra" & disonancia_pares %in% c("Muy Baja", "Baja") ~ "Relación Neutral con Liderazgo - Buena Integración",
      disonancia_superior == "Neutra" & disonancia_pares == "Neutra" ~ "Equilibrio Organizacional",
      disonancia_superior == "Neutra" & disonancia_pares %in% c("Alta", "Muy Alta") ~ "Relación Neutral con Liderazgo - Desafío con Pares",
      disonancia_superior %in% c("Alta", "Muy Alta") & disonancia_pares %in% c("Muy Baja", "Baja") ~ "Desafío con Liderazgo - Buena Integración",
      disonancia_superior %in% c("Alta", "Muy Alta") & disonancia_pares == "Neutra" ~ "Desafío con Liderazgo - Integración Neutral",
      disonancia_superior %in% c("Alta", "Muy Alta") & disonancia_pares %in% c("Alta", "Muy Alta") ~ "Entorno Relacional Adverso",
      TRUE ~ "Perfil Mixto"
    )
    factor_estres <- case_when(
      disonancia_superior %in% c("Alta", "Muy Alta") & disonancia_pares %in% c("Alta", "Muy Alta") ~ "Estrés alto",
      disonancia_superior %in% c("Alta", "Muy Alta") | disonancia_pares %in% c("Alta", "Muy Alta") ~ "Estrés moderado",
      TRUE ~ "Sin estrés aparente"
    )
  }
  
  # Caso 2: Líder con equipo (con superior, pares y subordinados)
  else if (tiene_superior & tiene_subordinados) {
    perfil <- case_when(
      disonancia_superior %in% c("Muy Baja", "Baja") & 
        disonancia_pares %in% c("Muy Baja", "Baja") & 
        disonancia_subordinados %in% c("Muy Baja", "Baja") ~ "Liderazgo Efectivo Integral",
      
      disonancia_superior %in% c("Muy Baja", "Baja") & 
        disonancia_subordinados %in% c("Muy Baja", "Baja") & 
        disonancia_pares %in% c("Alta", "Muy Alta") ~ "Liderazgo Vertical Efectivo",
      
      disonancia_pares %in% c("Muy Baja", "Baja") & 
        disonancia_subordinados %in% c("Muy Baja", "Baja") & 
        disonancia_superior %in% c("Alta", "Muy Alta") ~ "Liderazgo Horizontal Efectivo",
      
      disonancia_superior %in% c("Muy Baja", "Baja") & 
        disonancia_pares %in% c("Muy Baja", "Baja") & 
        disonancia_subordinados %in% c("Alta", "Muy Alta") ~ "Liderazgo Estratégico con Desafío de Equipo",
      
      disonancia_superior == "Neutra" & 
        disonancia_pares == "Neutra" & 
        disonancia_subordinados == "Neutra" ~ "Liderazgo en Transición",
      
      disonancia_superior %in% c("Alta", "Muy Alta") | 
        disonancia_pares %in% c("Alta", "Muy Alta") | 
        disonancia_subordinados %in% c("Alta", "Muy Alta") ~ "Liderazgo Desafiante",
      
      TRUE ~ "Liderazgo con Dinámica Compleja"
    )
    factor_estres <- case_when(
      sum(c(disonancia_superior, disonancia_pares, disonancia_subordinados) %in% c("Alta", "Muy Alta")) >= 2 ~ "Estrés alto",
      sum(c(disonancia_superior, disonancia_pares, disonancia_subordinados) %in% c("Alta", "Muy Alta")) == 1 ~ "Estrés moderado",
      TRUE ~ "Sin estrés aparente"
    )
  }
  
  # Caso 3: Alta Dirección (sin superior, con pares y subordinados)
  else if (!tiene_superior & tiene_subordinados) {
    perfil <- case_when(
      disonancia_pares %in% c("Muy Baja", "Baja") & disonancia_subordinados %in% c("Muy Baja", "Baja") ~ "Dirección Consolidada",
      disonancia_subordinados %in% c("Muy Baja", "Baja") & disonancia_pares %in% c("Neutra", "Alta", "Muy Alta") ~ "Dirección con Soporte de Equipo",
      disonancia_pares %in% c("Muy Baja", "Baja") & disonancia_subordinados %in% c("Neutra", "Alta", "Muy Alta") ~ "Dirección con Red de Pares",
      disonancia_pares %in% c("Alta", "Muy Alta") & disonancia_subordinados %in% c("Alta", "Muy Alta") ~ "Dirección en Aislamiento",
      TRUE ~ "Dirección con Dinámica Mixta"
    )
    factor_estres <- case_when(
      disonancia_pares %in% c("Alta", "Muy Alta") & disonancia_subordinados %in% c("Alta", "Muy Alta") ~ "Estrés alto por aislamiento",
      disonancia_pares %in% c("Alta", "Muy Alta") | disonancia_subordinados %in% c("Alta", "Muy Alta") ~ "Estrés moderado",
      TRUE ~ "Sin estrés aparente"
    )
  }
  
  # Caso 4: Colaborador sin superior (solo pares)
  else if (!tiene_superior & !tiene_subordinados) {
    perfil <- case_when(
      disonancia_pares %in% c("Muy Baja", "Baja") ~ "Autonomía Positiva",
      disonancia_pares == "Neutra" ~ "Autonomía Neutral",
      disonancia_pares %in% c("Alta", "Muy Alta") ~ "Autonomía Estresante"
    )
    factor_estres <- case_when(
      disonancia_pares %in% c("Alta", "Muy Alta") ~ "Estrés alto por falta de soporte",
      disonancia_pares == "Neutra" ~ "Estrés moderado",
      TRUE ~ "Sin estrés aparente"
    )
  }
  
  # Caso 5: Perfil no cubierto
  else {
    perfil <- "Perfil No Definido"
    factor_estres <- "Por evaluar"
  }
  
  return(list(perfil_360 = perfil, factor_estres = factor_estres))
}

# -----------------------------
# 1) PREPARACIÓN Y LIMPIEZA
# -----------------------------
cat('Preparando y limpiando datos...\n')

datos <- datos %>%
  mutate(
    # normalizar nombres a Title Case para consistencia
    across(c(Género, Estado_Civil, Nivel_Estudios, Área_Estudios, Puesto_Generico, Perfil_Profesional_Puesto, Regional), ~ ifelse(is.na(.), NA, str_to_title(.))),
    Fecha_Ingreso = as.Date(Fecha_Ingreso),
    Fecha_Nacimiento = as.Date(Fecha_Nacimiento),
    # Calcular año de nacimiento para subgeneración
    Anio_Nacimiento = year(Fecha_Nacimiento),
    Edad = as.numeric(difftime(Sys.Date(), Fecha_Nacimiento, units = 'days')) / 365.25,
    Anios_Antiguedad = as.numeric(difftime(Sys.Date(), Fecha_Ingreso, units = 'days')) / 365.25,
    # Asegurar que Monto sea numérico
    Monto = as.numeric(Monto)
  )

# Agrupar modas por departamento para variables clave
vars_para_imputar <- c('Género', 'Estado_Civil', 'Nivel_Estudios', 'Área_Estudios', 'Perfil_Profesional_Puesto')

modas_por_depto <- datos %>%
  filter(!is.na(`Nivel 3`)) %>%
  group_by(`Nivel 3`) %>%
  summarise(across(all_of(vars_para_imputar), ~ calcular_moda(.x), .names = 'moda_{col}'), .groups = 'drop')

# modas globales (fallback)
modas_globales <- datos %>% summarise(across(all_of(vars_para_imputar), ~ calcular_moda(.x)))

# Imputar variables usando la moda del departamento y fallback global
datos_imput <- datos %>%
  left_join(modas_por_depto, by = c("Nivel 3" = "Nivel 3"))

for (v in vars_para_imputar) {
  moda_col <- paste0('moda_', v)
  datos_imput[[v]] <- ifelse(is.na(datos_imput[[v]]), datos_imput[[moda_col]], datos_imput[[v]])
  # si aún NA -> usar global
  datos_imput[[v]] <- ifelse(is.na(datos_imput[[v]]), modas_globales[[v]], datos_imput[[v]])
}

# aplicar agrupaciones educativas, afinidad y nueva subgeneración por año
datos_imput <- datos_imput %>%
  mutate(
    Nivel_Estudios = ifelse(is.na(Nivel_Estudios), 'No Definido', Nivel_Estudios),
    Área_Estudios = ifelse(is.na(Área_Estudios), 'No Definido', Área_Estudios),
    Grupo_Nivel_Estudios = asignar_grupo_estudios(Nivel_Estudios),
    Grupo_Afinidad_Area = asignar_grupo_afinidad_area(Área_Estudios),
    # Usar año de nacimiento para subgeneración
    Subgeneracion = asignar_subgeneracion_anio(Anio_Nacimiento)
  )

# Asegurar columnas clave
datos_imput <- datos_imput %>%
  mutate(
    ID_Colaborador = as.character(ID_Colaborador),
    Id_Jefe = as.character(Id_Jefe),
    Puesto_Generico = ifelse(is.na(Puesto_Generico), 'No Definido', Puesto_Generico),
    `Nivel 3` = ifelse(is.na(`Nivel 3`), 'No Asignado', `Nivel 3`),
    Monto = ifelse(is.na(Monto), median(Monto, na.rm = TRUE), Monto)  # Imputar salarios faltantes con mediana
  )

# -----------------------------
# 2) GENERAR VECTORES NUMÉRICOS POR COLABORADOR
# -----------------------------
cat('Generando vectores normalizados por colaborador...\n')

# Elegir variables y codificar a [0,1] (incluyendo salario)
vectores <- datos_imput %>%
  transmute(
    ID_Colaborador = ID_Colaborador,
    Nivel3 = `Nivel 3`,
    Puesto_Generico = Puesto_Generico,
    Id_Jefe = Id_Jefe,
    Monto = Monto,
    Genero_num = case_when(
      str_to_lower(Género) %in% c('hombre','m') ~ 0,
      str_to_lower(Género) %in% c('mujer','f') ~ 1,
      TRUE ~ 0.5
    ),
    EstadoCivil_num = case_when(
      str_to_lower(Estado_Civil) %in% c('casado','unión libre','union libre') ~ 1,
      str_to_lower(Estado_Civil) %in% c('soltero','divorciado') ~ 0,
      str_to_lower(Estado_Civil) %in% c('viudo') ~ 0.5,
      TRUE ~ 0.5
    ),
    Subgen_num = case_when(
      Subgeneracion == 'Tradicionalistas' ~ 0.05,
      Subgeneracion == 'Boomers - Tempranos' ~ 0.15,
      Subgeneracion == 'Boomers - Tardíos' ~ 0.25,
      Subgeneracion == 'Gen X - Tempranos' ~ 0.35,
      Subgeneracion == 'Gen X - Tardíos' ~ 0.45,
      Subgeneracion == 'Millennials - Tempranos' ~ 0.55,
      Subgeneracion == 'Millennials - Tardíos' ~ 0.65,
      Subgeneracion == 'Gen Z - Tempranos' ~ 0.75,
      Subgeneracion == 'Gen Z - Tardíos' ~ 0.85,
      TRUE ~ 0.5
    ),
    NivelEst_num = case_when(
      Grupo_Nivel_Estudios == 'Estudios Básicos' ~ 0.1,
      Grupo_Nivel_Estudios == 'Estudios Medios' ~ 0.3,
      Grupo_Nivel_Estudios == 'Estudios Universitarios' ~ 0.5,
      Grupo_Nivel_Estudios == 'Estudios Postgrados' ~ 0.75,
      Grupo_Nivel_Estudios == 'Estudios Doctorado' ~ 0.95,
      TRUE ~ 0.5
    ),
    AfinidadArea_num = as.numeric(as.factor(Grupo_Afinidad_Area)) / max(1, length(unique(datos_imput$Grupo_Afinidad_Area))),
    Edad_norm = rescale(ifelse(is.na(Edad), 0, Edad), to = c(0, 1)),
    Antiguedad_norm = rescale(ifelse(is.na(Anios_Antiguedad), 0, Anios_Antiguedad), to = c(0, 1)),
    # Normalizar salario (usando transformación logarítmica para manejar asimetría)
    Salario_norm = rescale(log(ifelse(is.na(Monto) | Monto <= 0, 1, Monto)), to = c(0, 1))
  )

# Número de características usadas para distancia (ahora 8 con salario)
caracteristicas <- c('Genero_num','EstadoCivil_num','Subgen_num','NivelEst_num','AfinidadArea_num','Edad_norm','Antiguedad_norm','Salario_norm')
num_features <- length(caracteristicas)

# Mostrar información sobre los pesos
cat('Pesos utilizados para las dimensiones:\n')
for (i in 1:length(pesos_dimensiones)) {
  cat('  ', names(pesos_dimensiones)[i], ':', pesos_dimensiones[i], '\n')
}
cat('Suma total de pesos:', sum(pesos_dimensiones), '\n')
cat('Máxima distancia teórica:', sqrt(sum(pesos_dimensiones)), '\n')

# -----------------------------
# 3) CALCULAR RELACIONES Y BRECHAS CON PONDERACIONES
# -----------------------------
cat('Calculando relaciones: jefe-subordinado, pares (departamento y puesto), subordinados...\n')

# 3a) Subordinados por jefe (lista)
subordinados_por_jefe <- vectores %>%
  filter(!is.na(Id_Jefe) & Id_Jefe != '') %>%
  group_by(Id_Jefe) %>%
  summarise(ids_subordinados = list(ID_Colaborador), .groups = 'drop')

# 3b) Relaciones Jefe -> Sub CON PONDERACIONES
rel_jefe_sub <- vectores %>%
  filter(!is.na(Id_Jefe) & Id_Jefe != '') %>%
  left_join(vectores, by = c('Id_Jefe' = 'ID_Colaborador'), suffix = c('_sub','_jefe')) %>%
  rowwise() %>%
  mutate(
    distancia = distancia_vector_ponderada(
      c_across(all_of(paste0(caracteristicas,'_sub'))),
      c_across(all_of(paste0(caracteristicas,'_jefe')))
    ),
    distancia_norm = normalizar_distancia_ponderada(distancia),
    tipo_relacion = 'Jefe-Subordinado'
  ) %>%
  ungroup() %>%
  rename(ID_Colaborador_sub = ID_Colaborador) %>%
  select(ID_Colaborador_sub, Id_Jefe, tipo_relacion, distancia, distancia_norm)

# 3c) Brechas entre pares por departamento CON PONDERACIONES
cat('Calculando brechas promedio por departamento y puesto...\n')

vectores_promedio <- vectores %>%
  group_by(Nivel3, Puesto_Generico) %>%
  summarise(across(all_of(caracteristicas), mean, na.rm = TRUE), .groups = 'drop')

rel_pares <- vectores %>%
  left_join(vectores_promedio, by = c('Nivel3','Puesto_Generico'), suffix = c('', '_prom')) %>%
  rowwise() %>%
  mutate(
    distancia = distancia_vector_ponderada(
      c_across(all_of(caracteristicas)),
      c_across(all_of(paste0(caracteristicas,'_prom')))
    ),
    distancia_norm = normalizar_distancia_ponderada(distancia),
    tipo_relacion = 'Pares-Depto/Puesto'
  ) %>%
  ungroup() %>%
  select(ID_Colaborador, Nivel3, Puesto_Generico, tipo_relacion, distancia, distancia_norm)

# 3d) Pares por Puesto - VERSIÓN OPTIMIZADA CON PONDERACIONES
cat('Calculando brechas por puesto...\n')

vectores_promedio_puesto <- vectores %>%
  group_by(Puesto_Generico) %>%
  summarise(across(all_of(caracteristicas), mean, na.rm = TRUE), .groups = 'drop')

rel_pares_puesto_opt <- vectores %>%
  left_join(vectores_promedio_puesto, by = 'Puesto_Generico', suffix = c('', '_prom_puesto')) %>%
  rowwise() %>%
  mutate(
    distancia = distancia_vector_ponderada(
      c_across(all_of(caracteristicas)),
      c_across(all_of(paste0(caracteristicas,'_prom_puesto')))
    ),
    distancia_norm = normalizar_distancia_ponderada(distancia),
    tipo_relacion = 'Pares_Puesto'
  ) %>%
  ungroup() %>%
  select(ID_Colaborador, Puesto_Generico, tipo_relacion, distancia, distancia_norm)

# 3e) Pares por Departamento - VERSIÓN OPTIMIZADA CON PONDERACIONES
cat('Calculando brechas por departamento...\n')

vectores_promedio_depto <- vectores %>%
  group_by(Nivel3) %>%
  summarise(across(all_of(caracteristicas), mean, na.rm = TRUE), .groups = 'drop')

rel_pares_depto_opt <- vectores %>%
  left_join(vectores_promedio_depto, by = 'Nivel3', suffix = c('', '_prom_depto')) %>%
  rowwise() %>%
  mutate(
    distancia = distancia_vector_ponderada(
      c_across(all_of(caracteristicas)),
      c_across(all_of(paste0(caracteristicas,'_prom_depto')))
    ),
    distancia_norm = normalizar_distancia_ponderada(distancia),
    tipo_relacion = 'Pares_Depto'
  ) %>%
  ungroup() %>%
  select(ID_Colaborador, Nivel3, tipo_relacion, distancia, distancia_norm)

# 3f) Distancia al promedio del departamento - VERSIÓN CORREGIDA CON PONDERACIONES
cat('Calculando distancia al promedio del departamento...\n')

vectores_con_promedio <- vectores %>%
  left_join(vectores_promedio_depto, by = c('Nivel3'), suffix = c('', '_prom'))

rel_con_promedio_depto <- vectores_con_promedio %>%
  rowwise() %>%
  mutate(
    distancia_promedio_depto = distancia_vector_ponderada(
      c(Genero_num, EstadoCivil_num, Subgen_num, NivelEst_num, AfinidadArea_num, Edad_norm, Antiguedad_norm, Salario_norm),
      c(Genero_num_prom, EstadoCivil_num_prom, Subgen_num_prom, NivelEst_num_prom, AfinidadArea_num_prom, Edad_norm_prom, Antiguedad_norm_prom, Salario_norm_prom)
    ),
    distancia_promedio_depto_norm = normalizar_distancia_ponderada(distancia_promedio_depto)
  ) %>%
  ungroup() %>%
  select(ID_Colaborador, Nivel3, distancia_promedio_depto, distancia_promedio_depto_norm)

# -----------------------------
# 4) AGREGAR BRECHAS POR COLABORADOR (resumen) - CORREGIDO PARA EVITAR DUPLICADOS
# -----------------------------
cat('Agregando métricas por colaborador...\n')

# Helper: promedio de distancias a jefe
promedio_a_jefe <- rel_jefe_sub %>%
  group_by(ID_Colaborador_sub) %>%
  summarise(mean_dist_jefe = mean(distancia_norm, na.rm = TRUE), n_jefe = n(), .groups = 'drop') %>%
  rename(ID_Colaborador = ID_Colaborador_sub)

# Usar las versiones optimizadas para pares
promedio_pares_depto_sum <- rel_pares_depto_opt %>%
  group_by(ID_Colaborador) %>%
  summarise(mean_dist_pares_depto = mean(distancia_norm, na.rm = TRUE), n_pares_depto = n(), .groups = 'drop')

promedio_pares_puesto_sum <- rel_pares_puesto_opt %>%
  group_by(ID_Colaborador) %>%
  summarise(mean_dist_pares_puesto = mean(distancia_norm, na.rm = TRUE), n_pares_puesto = n(), .groups = 'drop')

# Subordinados: distancia promedio a cada subordinado (para jefe)
sub_exp <- rel_jefe_sub %>% transmute(Jefe = Id_Jefe, distancia_norm)
promedio_a_subordinados <- sub_exp %>% 
  group_by(Jefe) %>% 
  summarise(mean_dist_subordinados = mean(distancia_norm, na.rm = TRUE), n_subordinados = n(), .groups = 'drop') %>% 
  rename(ID_Colaborador = Jefe)

# Calcular métricas salariales
salario_promedio_depto <- datos_imput %>%
  group_by(`Nivel 3`) %>%
  summarise(Salario_Promedio_Depto = mean(Monto, na.rm = TRUE), .groups = 'drop')

salario_promedio_puesto <- datos_imput %>%
  group_by(Puesto_Generico) %>%
  summarise(Salario_Promedio_Puesto = mean(Monto, na.rm = TRUE), .groups = 'drop')

analisis_salarial <- datos_imput %>%
  select(ID_Colaborador, Monto, `Nivel 3`, Puesto_Generico) %>%
  left_join(salario_promedio_depto, by = c("Nivel 3" = "Nivel 3")) %>%
  left_join(salario_promedio_puesto, by = "Puesto_Generico") %>%
  mutate(
    Brecha_Salarial_Depto = Monto - Salario_Promedio_Depto,
    Brecha_Salarial_Puesto = Monto - Salario_Promedio_Puesto,
    Percentil_Salarial_Depto = percent_rank(Monto),
    Percentil_Salarial_Global = percent_rank(Monto),
    Indicador_Equidad = case_when(
      Brecha_Salarial_Puesto < -0.1 * Salario_Promedio_Puesto ~ "Por debajo del promedio del puesto",
      Brecha_Salarial_Puesto > 0.1 * Salario_Promedio_Puesto ~ "Por arriba del promedio del puesto",
      TRUE ~ "En el promedio del puesto"
    )
  ) %>%
  select(ID_Colaborador, 
         Departamento = `Nivel 3`, 
         Puesto_Generico, 
         Monto, 
         Salario_Promedio_Depto, 
         Salario_Promedio_Puesto, 
         Brecha_Salarial_Depto, 
         Brecha_Salarial_Puesto,
         Percentil_Salarial_Depto,
         Percentil_Salarial_Global,
         Indicador_Equidad)

# Asegurar que no hay duplicados en todas las tablas antes de los joins
promedio_a_jefe <- promedio_a_jefe %>% distinct(ID_Colaborador, .keep_all = TRUE)
promedio_pares_depto_sum <- promedio_pares_depto_sum %>% distinct(ID_Colaborador, .keep_all = TRUE)
promedio_pares_puesto_sum <- promedio_pares_puesto_sum %>% distinct(ID_Colaborador, .keep_all = TRUE)
promedio_a_subordinados <- promedio_a_subordinados %>% distinct(ID_Colaborador, .keep_all = TRUE)
rel_con_promedio_depto <- rel_con_promedio_depto %>% distinct(ID_Colaborador, .keep_all = TRUE)
analisis_salarial <- analisis_salarial %>% distinct(ID_Colaborador, .keep_all = TRUE)

# Combinar todo en resumen por colaborador - CORREGIDO
resumen_colaborador <- vectores %>% 
  select(ID_Colaborador, Monto, Salario_norm) %>%
  distinct(ID_Colaborador, .keep_all = TRUE) %>%
  left_join(promedio_a_jefe, by = 'ID_Colaborador', relationship = "one-to-one") %>%
  left_join(promedio_pares_depto_sum, by = 'ID_Colaborador', relationship = "one-to-one") %>%
  left_join(promedio_pares_puesto_sum, by = 'ID_Colaborador', relationship = "one-to-one") %>%
  left_join(promedio_a_subordinados, by = 'ID_Colaborador', relationship = "one-to-one") %>%
  left_join(rel_con_promedio_depto, by = 'ID_Colaborador', relationship = "one-to-one") %>%
  left_join(analisis_salarial, by = 'ID_Colaborador', relationship = "one-to-one") %>%
  mutate(
    across(c(mean_dist_jefe, mean_dist_pares_depto, mean_dist_pares_puesto, mean_dist_subordinados, distancia_promedio_depto_norm),
           ~ ifelse(is.nan(.) | is.infinite(.), NA, .))
  )

# Verificar que no hay duplicados
if (any(duplicated(resumen_colaborador$ID_Colaborador))) {
  cat('Advertencia: Se detectaron duplicados en resumen_colaborador. Eliminando...\n')
  resumen_colaborador <- resumen_colaborador %>%
    distinct(ID_Colaborador, .keep_all = TRUE)
}

# -----------------------------
# 5) CALCULAR NIVELES DE DISONANCIA Y PERFILES 360 CON UMBRALES ADAPTATIVOS
# -----------------------------
cat('Calculando niveles de disonancia y perfiles 360...\n')

# Determinar si tienen superior y subordinados - CORREGIDO
tiene_superior <- vectores %>%
  select(ID_Colaborador, Id_Jefe) %>%
  distinct(ID_Colaborador, .keep_all = TRUE) %>%
  mutate(tiene_superior = !is.na(Id_Jefe) & Id_Jefe != '')

tiene_subordinados <- subordinados_por_jefe %>%
  select(ID_Colaborador = Id_Jefe) %>%
  distinct(ID_Colaborador) %>%
  mutate(tiene_subordinados = TRUE) %>%
  right_join(vectores %>% select(ID_Colaborador) %>% distinct(), by = 'ID_Colaborador') %>%
  mutate(tiene_subordinados = ifelse(is.na(tiene_subordinados), FALSE, TRUE)) %>%
  distinct(ID_Colaborador, .keep_all = TRUE)

# CALCULAR UMBRALES ADAPTATIVOS basados en distribución real
cat('Calculando umbrales adaptativos...\n')

# Recolectar todas las distancias para calcular distribución
todas_distancias <- c(
  rel_jefe_sub$distancia_norm,
  rel_pares_depto_opt$distancia_norm,
  rel_pares_puesto_opt$distancia_norm,
  rel_con_promedio_depto$distancia_promedio_depto_norm
)

umbrales_adaptativos <- calcular_umbrales_adaptativos(todas_distancias)

cat('Umbrales adaptativos calculados:\n')
cat('  Muy Baja: <', round(umbrales_adaptativos[1], 3), '\n')
cat('  Baja: <', round(umbrales_adaptativos[2], 3), '\n')
cat('  Neutra: <', round(umbrales_adaptativos[3], 3), '\n')
cat('  Alta: <', round(umbrales_adaptativos[4], 3), '\n')
cat('  Muy Alta: >=', round(umbrales_adaptativos[4], 3), '\n')

# Calcular niveles de disonancia para cada tipo de relación CON UMBRALES ADAPTATIVOS
resumen_colaborador <- resumen_colaborador %>%
  mutate(
    Disonancia_Superior = clasificar_disonancia_adaptativa(mean_dist_jefe, umbrales_adaptativos),
    Disonancia_Pares = clasificar_disonancia_adaptativa(
      coalesce(mean_dist_pares_depto, mean_dist_pares_puesto, distancia_promedio_depto_norm), 
      umbrales_adaptativos
    ),
    Disonancia_Subordinados = clasificar_disonancia_adaptativa(mean_dist_subordinados, umbrales_adaptativos)
  )

# Asegurar que no hay duplicados antes de los joins
resumen_colaborador <- resumen_colaborador %>%
  distinct(ID_Colaborador, .keep_all = TRUE)

# Realizar los joins con verificación de duplicados - CORREGIDO
resumen_colaborador <- resumen_colaborador %>%
  left_join(tiene_superior %>% distinct(ID_Colaborador, .keep_all = TRUE), 
            by = 'ID_Colaborador', 
            relationship = "one-to-one") %>%
  left_join(tiene_subordinados %>% distinct(ID_Colaborador, .keep_all = TRUE), 
            by = 'ID_Colaborador', 
            relationship = "one-to-one")

# Verificar que no hay duplicados después de los joins
if (any(duplicated(resumen_colaborador$ID_Colaborador))) {
  cat('Advertencia: Se detectaron duplicados después de los joins. Eliminando duplicados...\n')
  resumen_colaborador <- resumen_colaborador %>%
    distinct(ID_Colaborador, .keep_all = TRUE)
}

# Aplicar función para determinar perfil 360 - CORREGIDO
cat('Determinando perfiles 360...\n')

perfiles_360 <- resumen_colaborador %>%
  rowwise() %>%
  mutate(
    perfil_resultado = list(determinar_perfil_360(
      Disonancia_Superior, 
      Disonancia_Pares, 
      Disonancia_Subordinados, 
      coalesce(tiene_superior, FALSE), 
      coalesce(tiene_subordinados, FALSE)
    ))
  ) %>%
  ungroup()

# Extraer perfil y factor de estrés de forma segura
perfiles_360 <- perfiles_360 %>%
  mutate(
    Tipo_Perfil_360 = map_chr(perfil_resultado, ~ if(!is.null(.x$perfil_360)) .x$perfil_360 else "Perfil No Definido"),
    Factor_Estrés = map_chr(perfil_resultado, ~ if(!is.null(.x$factor_estres)) .x$factor_estres else "Por evaluar")
  ) %>%
  select(-perfil_resultado)

resumen_colaborador <- perfiles_360

# Verificar resultados
cat('Perfiles 360 calculados:', length(unique(resumen_colaborador$Tipo_Perfil_360)), 'tipos diferentes\n')
cat('Colaboradores con perfil asignado:', sum(!is.na(resumen_colaborador$Tipo_Perfil_360)), '\n')

# Mostrar distribución de disonancias
cat('\nDistribución de Disonancias:\n')
cat('Disonancia_Superior:\n')
print(table(resumen_colaborador$Disonancia_Superior))
cat('Disonancia_Pares:\n')
print(table(resumen_colaborador$Disonancia_Pares))
cat('Disonancia_Subordinados:\n')
print(table(resumen_colaborador$Disonancia_Subordinados))

# -----------------------------
# 6) DETALLE DE PARES (brechas_relacionales) - ACTUALIZADO
# -----------------------------
cat('Generando tabla de brechas relacionales detalladas...\n')

# Solo mantener relaciones jefe-sub para eficiencia
brechas_jefes <- rel_jefe_sub %>% 
  transmute(ID_A = ID_Colaborador_sub, ID_B = Id_Jefe, tipo_relacion, distancia, distancia_norm) %>%
  mutate(nivel_disonancia = clasificar_disonancia_adaptativa(distancia_norm, umbrales_adaptativos))

# Para mostrar brechas salariales en relaciones
brechas_salariales <- rel_jefe_sub %>%
  left_join(vectores %>% select(ID_Colaborador, Monto_A = Monto), by = c('ID_Colaborador_sub' = 'ID_Colaborador')) %>%
  left_join(vectores %>% select(ID_Colaborador, Monto_B = Monto), by = c('Id_Jefe' = 'ID_Colaborador')) %>%
  mutate(Brecha_Salarial = Monto_A - Monto_B) %>%
  select(ID_Colaborador_sub, Id_Jefe, Brecha_Salarial)

brechas_relacionales <- brechas_jefes %>%
  left_join(brechas_salariales, by = c('ID_A' = 'ID_Colaborador_sub', 'ID_B' = 'Id_Jefe'))

# Añadir info de ambos colaboradores
datos_imput_unique <- datos_imput %>%
  distinct(ID_Colaborador, .keep_all = TRUE)

brechas_relacionales <- brechas_relacionales %>%
  left_join(datos_imput_unique %>% 
              select(ID_Colaborador, Nombre = Nombre, Puesto_Generico = Puesto_Generico, Nivel3 = `Nivel 3`), 
            by = c('ID_A' = 'ID_Colaborador')) %>%
  left_join(datos_imput_unique %>% 
              select(ID_Colaborador, Nombre_B = Nombre, Puesto_Generico_B = Puesto_Generico, Nivel3_B = `Nivel 3`), 
            by = c('ID_B' = 'ID_Colaborador'))

# -----------------------------
# 7) Guardar resultados en Excel (varias hojas) - ACTUALIZADO
# -----------------------------
cat('Guardando resultados en carpeta de salida:\n', dir_output, '\n')

archivo_salida <- file.path(dir_output, paste0('Perfilamiento_360_Consolidado_', format(Sys.Date(), '%Y%m%d'), '.xlsx'))

wb <- createWorkbook()

# Función segura para agregar hojas y datos
agregar_hoja_segura <- function(wb, nombre_hoja, datos) {
  addWorksheet(wb, nombre_hoja)
  
  # Limpiar datos antes de escribir
  datos_limpios <- datos
  
  # Convertir listas a caracteres si existen
  if (any(sapply(datos_limpios, is.list))) {
    datos_limpios <- datos_limpios %>%
      mutate(across(where(is.list), ~ map_chr(., function(x) {
        if (is.null(x) || length(x) == 0) {
          return("")
        } else {
          return(paste(x, collapse = ", "))
        }
      })))
  }
  
  # Manejar valores NA en caracteres
  datos_limpios <- datos_limpios %>%
    mutate(across(where(is.character), ~ ifelse(is.na(.), "", .)))
  
  # Manejar valores NaN en numéricos
  datos_limpios <- datos_limpios %>%
    mutate(across(where(is.numeric), ~ ifelse(is.nan(.) | is.infinite(.), NA_real_, .)))
  
  writeData(wb, nombre_hoja, datos_limpios)
  return(TRUE)
}

# CORRECCIÓN: Convertir columnas de lista en subordinados_por_jefe
subordinados_por_jefe_texto <- subordinados_por_jefe %>%
  mutate(ids_subordinados = map_chr(ids_subordinados, ~ paste(., collapse = ", ")))

# Agregar hojas de manera segura
cat('Escribiendo hojas...\n')
agregar_hoja_segura(wb, 'Resumen_Colaborador', resumen_colaborador)
agregar_hoja_segura(wb, 'Brechas_Relacionales', brechas_relacionales)
agregar_hoja_segura(wb, 'Vectores', vectores)
agregar_hoja_segura(wb, 'Subordinados_Por_Jefe', subordinados_por_jefe_texto)
agregar_hoja_segura(wb, 'Analisis_Salarial', analisis_salarial)

# Crear hoja de catálogo de perfiles
perfiles_catalogo <- data.frame(
  Tipo_Perfil = c(
    "Armonía Organizacional",
    "Alineación con Liderazgo - Integración Neutral", 
    "Alineación con Liderazgo - Desafío con Pares",
    "Relación Neutral con Liderazgo - Buena Integración",
    "Equilibrio Organizacional",
    "Relación Neutral con Liderazgo - Desafío con Pares",
    "Desafío con Liderazgo - Buena Integración", 
    "Desafío con Liderazgo - Integración Neutral",
    "Entorno Relacional Adverso",
    "Liderazgo Efectivo Integral",
    "Liderazgo Vertical Efectivo",
    "Liderazgo Horizontal Efectivo",
    "Liderazgo Estratégico con Desafío de Equipo",
    "Liderazgo en Transición",
    "Liderazgo Desafiante",
    "Dirección Consolidada",
    "Dirección con Soporte de Equipo", 
    "Dirección con Red de Pares",
    "Dirección en Aislamiento",
    "Autonomía Positiva",
    "Autonomía Neutral",
    "Autonomía Estresante"
  ),
  Descripcion = c(
    "Excelente integración tanto con superiores como con pares",
    "Buena relación con el jefe pero integración neutral con colegas",
    "Buena relación con el jefe pero desafíos significativos con pares",
    "Relación neutral con el jefe pero buena integración con colegas", 
    "Relaciones equilibradas sin puntos destacados positivos o negativos",
    "Relación neutral con el jefe y desafíos con pares",
    "Desafíos con el jefe pero buena integración con colegas",
    "Desafíos con el jefe e integración neutral con pares",
    "Desafíos significativos tanto con superiores como con pares",
    "Excelentes relaciones en todas las direcciones (superiores, pares y subordinados)",
    "Buena relación con superiores y subordinados, pero desafíos con pares",
    "Buena relación con pares y subordinados, pero desafíos con superiores", 
    "Buena relación con superiores y pares, pero desafíos con el equipo",
    "Relaciones en transición sin patrones definidos",
    "Múltiples desafíos relacionales en diferentes direcciones",
    "Excelente integración como líder sin superiores directos",
    "Buena relación con el equipo pero desafíos con pares directivos",
    "Buena relación con pares directivos pero desafíos con el equipo",
    "Desafíos significativos tanto con pares como con el equipo",
    "Buena integración autónoma sin superiores directos",
    "Integración neutral en autonomía sin superiores directos", 
    "Desafíos significativos en autonomía sin superiores directos"
  )
)

agregar_hoja_segura(wb, 'Perfiles_360', perfiles_catalogo)

# Crear hoja con información de configuración
configuracion <- data.frame(
  Parametro = c(
    "Peso - Género",
    "Peso - Estado Civil", 
    "Peso - Subgeneración",
    "Peso - Nivel Estudios",
    "Peso - Área Estudios",
    "Peso - Edad",
    "Peso - Antigüedad",
    "Peso - Salario",
    "Suma total pesos",
    "Máxima distancia teórica",
    "Umbral Muy Baja",
    "Umbral Baja",
    "Umbral Neutra",
    "Umbral Alta"
  ),
  Valor = c(
    pesos_dimensiones["Genero_num"],
    pesos_dimensiones["EstadoCivil_num"],
    pesos_dimensiones["Subgen_num"],
    pesos_dimensiones["NivelEst_num"],
    pesos_dimensiones["AfinidadArea_num"],
    pesos_dimensiones["Edad_norm"],
    pesos_dimensiones["Antiguedad_norm"],
    pesos_dimensiones["Salario_norm"],
    sum(pesos_dimensiones),
    sqrt(sum(pesos_dimensiones)),
    umbrales_adaptativos[1],
    umbrales_adaptativos[2],
    umbrales_adaptativos[3],
    umbrales_adaptativos[4]
  )
)

agregar_hoja_segura(wb, 'Configuracion_Modelo', configuracion)

# Guardar el workbook
cat('Guardando archivo Excel...\n')
saveWorkbook(wb, archivo_salida, overwrite = TRUE)
cat('Archivo guardado exitosamente en:', archivo_salida, '\n')

# Limpiar plan paralelo
plan(sequential)

cat('Proceso completado exitosamente!\n')

# Mostrar resumen de resultados
cat('\n--- RESUMEN EJECUCIÓN ---\n')
cat('Colaboradores procesados:', nrow(resumen_colaborador), '\n')
cat('Relaciones jefe-subordinado:', nrow(brechas_relacionales), '\n')
cat('Jefes con subordinados:', nrow(subordinados_por_jefe), '\n')
cat('Perfiles 360 calculados:', length(unique(resumen_colaborador$Tipo_Perfil_360)), 'tipos diferentes\n')

if ('Monto' %in% names(resumen_colaborador)) {
  cat('Análisis salarial completado para', sum(!is.na(resumen_colaborador$Monto)), 'colaboradores\n')
  cat('Rango salarial:', 
      round(min(resumen_colaborador$Monto, na.rm = TRUE), 0), '-',
      round(max(resumen_colaborador$Monto, na.rm = TRUE), 0), '\n')
}

# Mostrar distribución final
cat('\n--- DISTRIBUCIÓN FINAL DE DISONANCIAS ---\n')
dist_superior <- table(resumen_colaborador$Disonancia_Superior)
dist_pares <- table(resumen_colaborador$Disonancia_Pares)
dist_subordinados <- table(resumen_colaborador$Disonancia_Subordinados)

cat('Disonancia con Superior:\n')
for (nivel in names(dist_superior)) {
  cat('  ', nivel, ':', dist_superior[nivel], '(', round(100 * dist_superior[nivel]/sum(dist_superior), 1), '%)\n')
}

cat('Disonancia con Pares:\n')
for (nivel in names(dist_pares)) {
  cat('  ', nivel, ':', dist_pares[nivel], '(', round(100 * dist_pares[nivel]/sum(dist_pares), 1), '%)\n')
}

cat('Disonancia con Subordinados:\n')
for (nivel in names(dist_subordinados)) {
  cat('  ', nivel, ':', dist_subordinados[nivel], '(', round(100 * dist_subordinados[nivel]/sum(dist_subordinados), 1), '%)\n')
}