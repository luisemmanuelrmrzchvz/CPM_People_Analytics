# =========================================================
# LIBRERÍAS
# =========================================================
library(readxl)
library(dplyr)
library(purrr)
library(ggplot2)
library(writexl)
library(tidyr)
library(cluster)
library(factoextra)
library(stats)

# =========================================================
# CONFIGURACIÓN GENERAL
# =========================================================
output_dir <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
datos <- read_excel(file_path)

# =========================================================
# VALIDACIONES BÁSICAS
# =========================================================
if (!"Grupo" %in% colnames(datos)) {
  stop("La columna 'Grupo' no existe en los datos.")
}

incluir_reclutador <- "Nombre Reclutador" %in% colnames(datos)

# =========================================================
# SELECCIÓN Y LIMPIEZA DE DATOS
# =========================================================
columnas_base <- c(
  "Días cobertura con capacitación",
  "Grupo",
  "DescripcionCC",
  "Perfil Profesional",
  "Segmento de puesto",
  "Tabulador Salarial",
  "Area de Personal",
  "Puesto Generico",
  "Familia de Puesto",
  "Regional",
  "Plaza",
  "Estado"
)

if (incluir_reclutador) {
  columnas_interes <- c(columnas_base, "Nombre Reclutador")
} else {
  columnas_interes <- columnas_base
}

datos_limpieza <- datos %>%
  select(all_of(columnas_interes)) %>%
  mutate(across(-`Días cobertura con capacitación`, as.factor)) %>%
  filter(!is.na(`Días cobertura con capacitación`), !is.na(Grupo))

# =========================================================
# FUNCIÓN: GOAL ADAPTATIVO SEGÚN VARIABILIDAD
# =========================================================
calcular_goal_mejorado <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 5) return(NA)
  
  cv <- sd(x) / mean(x)
  if (is.nan(cv) || is.infinite(cv)) cv <- 1
  
  if (cv < 0.35) {
    goal <- quantile(x, 0.40)
  } else if (cv < 0.60) {
    goal <- quantile(x, 0.50)
  } else {
    goal <- quantile(x, 0.60)
  }
  round(goal)
}

# =========================================================
# FUNCIÓN: ETA-SQUARED ROBUSTO
# =========================================================
calcular_eta_safe <- function(data, var) {
  tryCatch({
    if (length(unique(data[[var]])) < 2) return(0)
    f <- as.formula(paste0("`Días cobertura con capacitación` ~ `", var, "`"))
    a <- aov(f, data = data)
    an <- anova(a)
    ss_between <- an[1, "Sum Sq"]
    ss_total <- sum(an$`Sum Sq`)
    ifelse(ss_total > 0, ss_between / ss_total, 0)
  }, error = function(e) 0)
}

# =========================================================
# FUNCIÓN: BOOTSTRAP DE ESTABILIDAD DEL GOAL
# =========================================================
bootstrap_stability <- function(data, agrupador, R = 100) {
  goals <- replicate(R, {
    samp <- data[sample(nrow(data), replace = TRUE), ]
    tryCatch({
      g <- samp %>%
        group_by_at(agrupador) %>%
        summarise(goal = calcular_goal_mejorado(`Días cobertura con capacitación`),
                  .groups = "drop")
      paste(sort(g$goal), collapse = "|")
    }, error = function(e) NA)
  })
  
  tab <- table(goals)
  if (length(tab) == 0) return(0)
  max(tab) / R
}

# =========================================================
# FUNCIÓN: EVALUAR AGRUPADOR
# =========================================================
evaluar_agrupador <- function(col, data) {
  
  resumen <- data %>%
    group_by_at(col) %>%
    summarise(
      n = n(),
      media = mean(`Días cobertura con capacitación`),
      sd = sd(`Días cobertura con capacitación`),
      cv = sd / media,
      .groups = "drop"
    ) %>%
    filter(n >= 5)
  
  if (nrow(resumen) < 2) return(NULL)
  
  eta <- calcular_eta_safe(data, col)
  cv_prom <- mean(resumen$cv, na.rm = TRUE)
  estabilidad <- bootstrap_stability(data, col, R = 100)
  
  score_final <- 0.5 * eta +
                 0.3 * (1 - cv_prom) +
                 0.2 * estabilidad
  
  data.frame(
    agrupador = col,
    grupos_n = nrow(resumen),
    eta_squared = round(eta, 4),
    cv_promedio = round(cv_prom, 4),
    estabilidad = round(estabilidad, 4),
    score_final = round(score_final, 4)
  )
}

# =========================================================
# FUNCIÓN PRINCIPAL POR GRUPO
# =========================================================
analizar_grupo <- function(grupo, data) {
  
  datos_g <- data %>% filter(Grupo == grupo)
  if (nrow(datos_g) < 20) return(NULL)
  
  grupo_dir <- file.path(output_dir, grupo)
  dir.create(grupo_dir, showWarnings = FALSE)
  
  agrupadores <- setdiff(colnames(datos_g),
                         c("Días cobertura con capacitación", "Grupo"))
  
  evaluaciones <- map_dfr(agrupadores, evaluar_agrupador, data = datos_g)
  evaluaciones <- evaluaciones %>% arrange(desc(score_final))
  
  write_xlsx(evaluaciones,
             file.path(grupo_dir, "evaluacion_agrupadores.xlsx"))
  
  mejor_agrupador <- evaluaciones$agrupador[1]
  
  goals <- datos_g %>%
    group_by_at(mejor_agrupador) %>%
    summarise(
      n = n(),
      goal = calcular_goal_mejorado(`Días cobertura con capacitación`),
      mediana = median(`Días cobertura con capacitación`),
      .groups = "drop"
    ) %>%
    filter(n >= 5)
  
  write_xlsx(goals,
             file.path(grupo_dir, "goals_recomendados.xlsx"))
  
  resumen <- data.frame(
    Grupo = grupo,
    Mejor_Agrupador = mejor_agrupador,
    Score_Final = evaluaciones$score_final[1],
    Eta_Squared = evaluaciones$eta_squared[1],
    CV_Promedio = evaluaciones$cv_promedio[1],
    Estabilidad = evaluaciones$estabilidad[1],
    Registros = nrow(datos_g),
    Fecha_Analisis = as.character(Sys.Date())
  )
  
  write_xlsx(resumen,
             file.path(grupo_dir, "resumen_ejecutivo.xlsx"))
  
  return(resumen)
}

# =========================================================
# EJECUCIÓN GENERAL
# =========================================================
grupos <- c("SUCURSAL", "COBRANZA", "PLAZA", "ODG")
grupos <- intersect(grupos, unique(datos_limpieza$Grupo))

resultados <- map_dfr(grupos, analizar_grupo, data = datos_limpieza)

write_xlsx(resultados,
           file.path(output_dir, "resumen_consolidado.xlsx"))

print(resultados)
