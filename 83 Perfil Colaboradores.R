# Perfilamiento 360° Optimizado - VERSIÓN CON VECTORES Y BRECHAS
# Genera vectores por colaborador, calcula brechas por relaciones (jefe, pares, subordinados)
# y produce un score de Disonancia 360. Guarda resultados en carpeta de salida.

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

# -----------------------------
# Funciones auxiliares
# -----------------------------

# 1) Moda segura
calcular_moda <- function(x) {
  x <- na.omit(x)
  if (length(x) == 0) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 2) Mapas de grupos de estudios y afinidad (como en tu versión previa)
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
    is.na(edad) ~ 'No Definido',
    edad >= 58 ~ 'Boomers Tardíos',
    between(edad, 49, 57) ~ 'Gen X Tempranos',
    between(edad, 42, 48) ~ 'Gen X Tardíos',
    between(edad, 34, 41) ~ 'Millennials Tempranos',
    between(edad, 26, 33) ~ 'Millennials Tardíos',
    between(edad, 18, 25) ~ 'Gen Z',
    TRUE ~ 'No Definido'
  )
}

# Comparación segura
eq_safe <- function(a, b) {
  if (is.na(a) | is.na(b)) return(NA)
  return(a == b)
}

# Distancia Euclidiana (entre vectores numéricos)
distancia_vector <- function(vec1, vec2) {
  if (any(is.na(vec1)) | any(is.na(vec2))) {
    # ignorar dimensiones NA: calcular solo sobre dimensiones disponibles en ambos
    good <- !is.na(vec1) & !is.na(vec2)
    if (!any(good)) return(NA_real_)
    vec1 <- vec1[good]; vec2 <- vec2[good]
  }
  sqrt(sum((vec1 - vec2)^2))
}

# Normaliza una distancia a 0-1 (dividiendo por máximo posible)
# asumimos que cada característica en vector está en [0,1], así máximo euclidiano es sqrt(n)
normalizar_distancia <- function(d, n_features) {
  if (is.na(d)) return(NA_real_)
  maxd <- sqrt(n_features)
  pmin(1, d / maxd)
}

# -----------------------------
# 1) PREPARACIÓN Y LIMPIEZA
# -----------------------------
cat('Preparando y limpiando datos...\n')

datos <- datos %>%
  mutate(
    # normalizar nombres a Title Case para consistencia (opcional)
    across(c(Género, Estado_Civil, Nivel_Estudios, Área_Estudios, Puesto_Generico, Perfil_Profesional_Puesto, Regional), ~ ifelse(is.na(.), NA, str_to_title(.))),
    Fecha_Ingreso = as.Date(Fecha_Ingreso),
    Fecha_Nacimiento = as.Date(Fecha_Nacimiento),
    Edad = as.numeric(difftime(Sys.Date(), Fecha_Nacimiento, units = 'days')) / 365.25,
    Anios_Antiguedad = as.numeric(difftime(Sys.Date(), Fecha_Ingreso, units = 'days')) / 365.25
  )

# Agrupar modas por departamento para variables clave (para imputación por compañeros)
vars_para_imputar <- c('Género', 'Estado_Civil', 'Nivel_Estudios', 'Área_Estudios', 'Perfil_Profesional_Puesto')

modas_por_depto <- datos %>%
  filter(!is.na(`Nivel 3`)) %>%
  group_by(`Nivel 3`) %>%
  summarise(across(all_of(vars_para_imputar), ~ calcular_moda(.x), .names = 'moda_{col}'), .groups = 'drop')

# modas globales (fallback)
modas_globales <- datos %>% summarise(across(all_of(vars_para_imputar), ~ calcular_moda(.x)))

# Imputar variables usando la moda del departamento (Nivel 3) y fallback global
datos_imput <- datos %>%
  left_join(modas_por_depto, by = 'Nivel 3')

for (v in vars_para_imputar) {
  moda_col <- paste0('moda_', v)
  datos_imput[[v]] <- ifelse(is.na(datos_imput[[v]]), datos_imput[[moda_col]], datos_imput[[v]])
  # si aún NA -> usar global
  datos_imput[[v]] <- ifelse(is.na(datos_imput[[v]]), modas_globales[[v]], datos_imput[[v]])
}

# aplicar agrupaciones educativas y afinidad
datos_imput <- datos_imput %>%
  mutate(
    Nivel_Estudios = ifelse(is.na(Nivel_Estudios), 'No Definido', Nivel_Estudios),
    Área_Estudios = ifelse(is.na(Área_Estudios), 'No Definido', Área_Estudios),
    Grupo_Nivel_Estudios = asignar_grupo_estudios(Nivel_Estudios),
    Grupo_Afinidad_Area = asignar_grupo_afinidad_area(Área_Estudios),
    Subgeneracion = asignar_subgeneracion(Edad)
  )

# Asegurar columnas clave
datos_imput <- datos_imput %>%
  mutate(
    ID_Colaborador = as.character(ID_Colaborador),
    Id_Jefe = as.character(Id_Jefe),
    Puesto_Generico = ifelse(is.na(Puesto_Generico), 'No Definido', Puesto_Generico),
    `Nivel 3` = ifelse(is.na(`Nivel 3`), 'No Asignado', `Nivel 3`)
  )

# -----------------------------
# 2) GENERAR VECTORES NUMÉRICOS POR COLABORADOR
# -----------------------------
cat('Generando vectores normalizados por colaborador...\n')

# Elegir variables y codificar a [0,1] (o 0,0.5,1 para categorías)
vectores <- datos_imput %>%
  transmute(
    ID_Colaborador = ID_Colaborador,
    Nivel3 = `Nivel 3`,
    Puesto_Generico = Puesto_Generico,
    Id_Jefe = Id_Jefe,
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
      Subgeneracion == 'Boomers Tardíos' ~ 0.1,
      Subgeneracion == 'Gen X Tempranos' ~ 0.25,
      Subgeneracion == 'Gen X Tardíos' ~ 0.35,
      Subgeneracion == 'Millennials Tempranos' ~ 0.5,
      Subgeneracion == 'Millennials Tardíos' ~ 0.65,
      Subgeneracion == 'Gen Z' ~ 0.85,
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
    Antiguedad_norm = rescale(ifelse(is.na(Anios_Antiguedad), 0, Anios_Antiguedad), to = c(0, 1))
  )

# Número de características usadas para distancia
caracteristicas <- c('Genero_num','EstadoCivil_num','Subgen_num','NivelEst_num','AfinidadArea_num','Edad_norm','Antiguedad_norm')
num_features <- length(caracteristicas)

# -----------------------------
# 3) CALCULAR RELACIONES Y BRECHAS
# -----------------------------
cat('Calculando relaciones: jefe-subordinado, pares (departamento y puesto), subordinados...\n')

# 3a) Subordinados por jefe (lista)
subordinados_por_jefe <- vectores %>%
  filter(!is.na(Id_Jefe) & Id_Jefe != '') %>%
  group_by(Id_Jefe) %>%
  summarise(ids_subordinados = list(ID_Colaborador), .groups = 'drop')

# 3b) Relaciones Jefe -> Sub
rel_jefe_sub <- vectores %>%
  filter(!is.na(Id_Jefe) & Id_Jefe != '') %>%
  left_join(vectores, by = c('Id_Jefe' = 'ID_Colaborador'), suffix = c('_sub','_jefe')) %>%
  rowwise() %>%
  mutate(
    distancia = distancia_vector(c_across(all_of(paste0(caracteristicas,'_sub'))),
                                 c_across(all_of(paste0(caracteristicas,'_jefe')))),
    distancia_norm = normalizar_distancia(distancia, num_features),
    tipo_relacion = 'Jefe-Subordinado'
  ) %>%
  ungroup() %>%
  rename(ID_Colaborador_sub = ID_Colaborador) %>%  # 🔧 aquí está el cambio clave
  select(ID_Colaborador_sub, Id_Jefe, tipo_relacion, distancia, distancia_norm)

# 3c) Relación Subordinado -> Jefe (inversa) (por si quieres asimetría)
rel_sub_jefe <- rel_jefe_sub %>%
  rename(ID_Colaborador = ID_Colaborador_sub, Jefe = Id_Jefe) %>%
  mutate(ID_Contra = Jefe) %>%
  select(ID_Colaborador, ID_Contra, tipo_relacion = tipo_relacion, distancia, distancia_norm) %>%
  mutate(tipo_relacion = 'Subordinado-Jefe')

# 3d) Pares por Departamento (mismo Nivel 3)
rel_pares_depto <- vectores %>%
  group_by(Nivel3) %>%
  filter(n() > 1) %>%
  group_modify(~{
    ids <- .x$ID_Colaborador
    combos <- t(combn(ids, 2))
    map_dfr(1:nrow(combos), function(i) {
      id1 <- combos[i,1]; id2 <- combos[i,2]
      v1 <- .x[.x$ID_Colaborador == id1, caracteristicas] %>% unlist()
      v2 <- .x[.x$ID_Colaborador == id2, caracteristicas] %>% unlist()
      d <- distancia_vector(v1, v2)
      dn <- normalizar_distancia(d, num_features)
      tibble(ID_1 = id1, ID_2 = id2, tipo_relacion = 'Pares_Departamento', distancia = d, distancia_norm = dn)
    })
  }) %>% ungroup()

# 3e) Pares por Puesto (mismo Puesto_Generico)
rel_pares_puesto <- vectores %>%
  group_by(Puesto_Generico) %>%
  filter(n() > 1) %>%
  group_modify(~{
    ids <- .x$ID_Colaborador
    combos <- t(combn(ids, 2))
    map_dfr(1:nrow(combos), function(i) {
      id1 <- combos[i,1]; id2 <- combos[i,2]
      v1 <- .x[.x$ID_Colaborador == id1, caracteristicas] %>% unlist()
      v2 <- .x[.x$ID_Colaborador == id2, caracteristicas] %>% unlist()
      d <- distancia_vector(v1, v2)
      dn <- normalizar_distancia(d, num_features)
      tibble(ID_1 = id1, ID_2 = id2, tipo_relacion = 'Pares_Puesto', distancia = d, distancia_norm = dn)
    })
  }) %>% ungroup()

# 3f) Subordinados list -> calcular distancias al jefe promedio (si quieres)
# Calculamos la distancia del colaborador al promedio de vectores de sus compañeros (misma Nivel3)
promedio_vectores_depto <- vectores %>%
  group_by(Nivel3) %>%
  summarise(across(all_of(caracteristicas), mean, na.rm = TRUE), .groups = 'drop')

rel_con_promedio_depto <- vectores %>%
  left_join(promedio_vectores_depto, by = c('Nivel3')) %>%
  rowwise() %>%
  mutate(
    distancia_promedio_depto = distancia_vector(c_across(all_of(caracteristicas)), c_across(all_of(paste0(caracteristicas,'_1')))),
    distancia_promedio_depto_norm = normalizar_distancia(distancia_promedio_depto, num_features)
  ) %>%
  ungroup() %>%
  select(ID_Colaborador, Nivel3, distancia_promedio_depto, distancia_promedio_depto_norm)

# -----------------------------
# 4) AGREGAR BRECHAS POR COLABORADOR (resumen)
# -----------------------------
cat('Agregando métricas por colaborador...\n')

# Helper: promedio de distancias a jefe
promedio_a_jefe <- rel_jefe_sub %>%
  group_by(ID_Colaborador_sub) %>%
  summarise(mean_dist_jefe = mean(distancia_norm, na.rm = TRUE), n_jefe = n(), .groups = 'drop') %>%
  rename(ID_Colaborador = ID_Colaborador_sub)

# Promedio distancia con compañeros (por depto y por puesto)
promedio_pares_depto <- rel_pares_depto %>%
  gather(key = 'tmp', value = 'val', distancia_norm) # trick to keep structure
# Instead compute per id: for each pair, add distance to both IDs
pares_depto_exp <- rel_pares_depto %>%
  transmute(ID = ID_1, distancia_norm = distancia_norm) %>% bind_rows(rel_pares_depto %>% transmute(ID = ID_2, distancia_norm = distancia_norm))
promedio_pares_depto_sum <- pares_depto_exp %>% group_by(ID) %>% summarise(mean_dist_pares_depto = mean(distancia_norm, na.rm = TRUE), n_pares_depto = n(), .groups = 'drop')

pares_puesto_exp <- rel_pares_puesto %>%
  transmute(ID = ID_1, distancia_norm = distancia_norm) %>% bind_rows(rel_pares_puesto %>% transmute(ID = ID_2, distancia_norm = distancia_norm))
promedio_pares_puesto_sum <- pares_puesto_exp %>% group_by(ID) %>% summarise(mean_dist_pares_puesto = mean(distancia_norm, na.rm = TRUE), n_pares_puesto = n(), .groups = 'drop')

# Subordinados: distancia promedio a cada subordinado (para jefe)
sub_exp <- rel_jefe_sub %>% transmute(Jefe = Id_Jefe, distancia_norm)
promedio_a_subordinados <- sub_exp %>% group_by(Jefe) %>% summarise(mean_dist_subordinados = mean(distancia_norm, na.rm = TRUE), n_subordinados = n(), .groups = 'drop') %>% rename(ID_Colaborador = Jefe)

# Combinar todo en resumen por colaborador
resumen_colaborador <- vectores %>% select(ID_Colaborador) %>%
  left_join(promedio_a_jefe, by = 'ID_Colaborador') %>%
  left_join(promedio_pares_depto_sum %>% rename(ID_Colaborador = ID), by = 'ID_Colaborador') %>%
  left_join(promedio_pares_puesto_sum %>% rename(ID_Colaborador = ID), by = 'ID_Colaborador') %>%
  left_join(promedio_a_subordinados, by = 'ID_Colaborador') %>%
  left_join(rel_con_promedio_depto, by = 'ID_Colaborador') %>%
  mutate(
    mean_dist_jefe = ifelse(is.nan(mean_dist_jefe), NA, mean_dist_jefe),
    mean_dist_pares_depto = ifelse(is.nan(mean_dist_pares_depto), NA, mean_dist_pares_depto),
    mean_dist_pares_puesto = ifelse(is.nan(mean_dist_pares_puesto), NA, mean_dist_pares_puesto),
    mean_dist_subordinados = ifelse(is.nan(mean_dist_subordinados), NA, mean_dist_subordinados)
  )

# -----------------------------
# 5) SCORE DE DISONANCIA 360
# -----------------------------
cat('Calculando score de Disonancia 360 por colaborador...\n')

# Pesos (ajustables): por defecto Jefe 0.4, Pares (depto) 0.3, Subordinados 0.3
calcula_disonancia <- function(row, w_jefe = 0.4, w_pares = 0.3, w_sub = 0.3) {
  # row is a tibble row
  comps <- c(
    jefe = row$mean_dist_jefe,
    pares = coalesce(row$mean_dist_pares_depto, row$mean_dist_pares_puesto, row$distancia_promedio_depto_norm),
    sub = row$mean_dist_subordinados
  )
  # Si faltan algunas componentes, renormalizar pesos
  available <- !is.na(comps)
  if (!any(available)) return(NA_real_)
  pesos <- c(w_jefe, w_pares, w_sub)
  pesos[!available] <- 0
  if (sum(pesos) == 0) return(NA_real_)
  pesos <- pesos / sum(pesos)
  score_norm <- sum(comps[available] * pesos[available], na.rm = TRUE) # ya en 0-1
  # Convertir a escala 0-100: distancia alta = mayor disonancia
  return(round(score_norm * 100, 1))
}

resumen_colaborador <- resumen_colaborador %>%
  rowwise() %>%
  mutate(
    Disonancia_360 = calcula_disonancia(cur_data_all()),
    Alerta_Disonancia = case_when(
      Disonancia_360 >= 75 ~ 'ALTA',
      Disonancia_360 >= 50 ~ 'MEDIA',
      !is.na(Disonancia_360) ~ 'BAJA',
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup()

# -----------------------------
# 6) DETALLE DE PARES (brechas_relacionales)
# -----------------------------
cat('Generando tabla de brechas relacionales detalladas...\n')

# Unificar pares depto y pares puesto y jefes
brechas_pares <- bind_rows(
  rel_pares_depto %>% transmute(ID_A = ID_1, ID_B = ID_2, tipo_relacion, distancia, distancia_norm),
  rel_pares_puesto %>% transmute(ID_A = ID_1, ID_B = ID_2, tipo_relacion, distancia, distancia_norm)
)

brechas_jefes <- rel_jefe_sub %>% transmute(ID_A = ID_Colaborador_sub, ID_B = Id_Jefe, tipo_relacion, distancia, distancia_norm)

brechas_relacionales <- bind_rows(brechas_jefes, brechas_pares)

# Añadir info de ambos colaboradores (opcionales)
brechas_relacionales <- brechas_relacionales %>%
  left_join(datos_imput %>% select(ID_Colaborador, Nombre = Nombre, Puesto_Generico = Puesto_Generico, Nivel3 = `Nivel 3`), by = c('ID_A' = 'ID_Colaborador')) %>%
  left_join(datos_imput %>% select(ID_Colaborador, Nombre_B = Nombre, Puesto_Generico_B = Puesto_Generico, Nivel3_B = `Nivel 3`), by = c('ID_B' = 'ID_Colaborador'))

# -----------------------------
# 7) Guardar resultados en Excel (varias hojas)
# -----------------------------
cat('Guardando resultados en carpeta de salida:\n', dir_output, '\n')

archivo_salida <- file.path(dir_output, paste0('Perfilamiento_360_Consolidado_', format(Sys.Date(), '%Y%m%d'), '.xlsx'))

wb <- createWorkbook()
addWorksheet(wb, 'Resumen_Colaborador')
addWorksheet(wb, 'Brechas_Relacionales')
addWorksheet(wb, 'Vectores')
addWorksheet(wb, 'Subordinados_Por_Jefe')

writeData(wb, 'Resumen_Colaborador', resumen_colaborador)
writeData(wb, 'Brechas_Relacionales', brechas_relacionales)
writeData(wb, 'Vectores', vectores)
writeData(wb, 'Subordinados_Por_Jefe', subordinados_por_jefe)

saveWorkbook(wb, archivo_salida, overwrite = TRUE)

cat('Archivo guardado en:', archivo_salida, '\n')

# Limpiar plan paralelo
plan(sequential)

cat('\n=== PROCESO COMPLETADO ===\n')
cat('Resumen guardado como:', archivo_salida, '\n')

