# =========================================================
# LIBRERÍAS
# =========================================================
library(readxl)
library(dplyr)
library(purrr)
library(writexl)
library(stats)

# =========================================================
# CONFIGURACIÓN
# =========================================================
output_dir <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
datos <- read_excel(file_path)

cat("\n==============================\n")
cat("INICIO DEL ANÁLISIS DE GOALS\n")
cat("==============================\n")

# =========================================================
# LIMPIEZA
# =========================================================
datos_limpieza <- datos %>%
  filter(!is.na(`Días cobertura con capacitación`),
         !is.na(Grupo))

# =========================================================
# FUNCIÓN: GOAL ADAPTATIVO
# =========================================================
calcular_goal_mejorado <- function(x) {
  x <- x[!is.na(x)]
  cv <- sd(x) / mean(x)
  if (cv < 0.35) p <- 0.40
  else if (cv < 0.60) p <- 0.50
  else p <- 0.60
  list(goal = round(quantile(x, p)), percentil = p, cv = round(cv, 2))
}

# =========================================================
# ETA-SQUARED
# =========================================================
calcular_eta_safe <- function(data, var) {
  tryCatch({
    if (length(unique(data[[var]])) < 2) return(0)
    a <- aov(`Días cobertura con capacitación` ~ data[[var]])
    an <- anova(a)
    an[1, "Sum Sq"] / sum(an$`Sum Sq`)
  }, error = function(e) 0)
}

# =========================================================
# BOOTSTRAP ESTABILIDAD
# =========================================================
bootstrap_stability <- function(data, agrupador, R = 80) {
  goals <- replicate(R, {
    samp <- data[sample(nrow(data), replace = TRUE), ]
    g <- samp %>%
      group_by_at(agrupador) %>%
      summarise(goal = median(`Días cobertura con capacitación`),
                .groups = "drop")
    paste(sort(g$goal), collapse = "|")
  })
  max(table(goals)) / R
}

# =========================================================
# EVALUAR AGRUPADOR (CON LOG EN CONSOLA)
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
    ) %>% filter(n >= 5)

  if (nrow(resumen) < 2) {
    cat(" -", col, ": descartado (pocos subgrupos)\n")
    return(NULL)
  }

  eta <- calcular_eta_safe(data, col)
  cv_prom <- mean(resumen$cv, na.rm = TRUE)
  estabilidad <- bootstrap_stability(data, col)

  score <- 0.5 * eta + 0.3 * (1 - cv_prom) + 0.2 * estabilidad

  cat("\nAGRUPADOR:", col, "\n")
  cat(" Subgrupos válidos:", nrow(resumen), "\n")
  cat(" Eta²:", round(eta, 3), "\n")
  cat(" CV promedio:", round(cv_prom, 3), "\n")
  cat(" Estabilidad:", round(estabilidad, 3), "\n")
  cat(" SCORE FINAL:", round(score, 3), "\n")

  if (score < 0.3) cat("  ⚠️ Agrupador débil\n")
  else if (score < 0.5) cat("  🟡 Agrupador usable\n")
  else cat("  🟢 Agrupador fuerte\n")

  data.frame(
    agrupador = col,
    score = score
  )
}

# =========================================================
# ANÁLISIS POR GRUPO
# =========================================================
analizar_grupo <- function(grupo, data) {

  cat("\n==============================\n")
  cat("GRUPO:", grupo, "\n")
  cat("==============================\n")

  datos_g <- data %>% filter(Grupo == grupo)

  cat("Registros totales:", nrow(datos_g), "\n")
  cat("Mediana global:", median(datos_g$`Días cobertura con capacitación`), "\n")
  cat("CV global:",
      round(sd(datos_g$`Días cobertura con capacitación`) /
            mean(datos_g$`Días cobertura con capacitación`), 2), "\n")

  agrupadores <- setdiff(colnames(datos_g),
                         c("Días cobertura con capacitación", "Grupo"))

  evaluaciones <- map_dfr(agrupadores, evaluar_agrupador, data = datos_g)

  mejor <- evaluaciones %>% arrange(desc(score)) %>% slice(1) %>% pull(agrupador)

  cat("\n>>> MEJOR AGRUPADOR SELECCIONADO:", mejor, "\n")

  cat("\n--- GOALS POR SUBGRUPO ---\n")

  datos_g %>%
    group_by_at(mejor) %>%
    summarise(
      n = n(),
      mediana = median(`Días cobertura con capacitación`),
      info = list(calcular_goal_mejorado(`Días cobertura con capacitación`)),
      .groups = "drop"
    ) %>%
    filter(n >= 5) %>%
    rowwise() %>%
    mutate(
      goal = info$goal,
      percentil = info$percentil,
      cv = info$cv,
      tipo = case_when(
        cv < 0.4 ~ "🟢 Proceso maduro",
        cv < 0.7 ~ "🟡 Proceso exigente",
        TRUE ~ "🔴 Proceso complejo / especializado"
      )
    ) %>%
    select(-info) %>%
    { print(.) }

}

# =========================================================
# EJECUCIÓN
# =========================================================
grupos <- unique(datos_limpieza$Grupo)
walk(grupos, analizar_grupo, data = datos_limpieza)

cat("\nANÁLISIS FINALIZADO\n")
















> # =========================================================
> # LIBRERÍAS
  > # =========================================================
> library(readxl)
> library(dplyr)
> library(purrr)
> 
  > # =========================================================
> # CARGA DE DATOS
  > # =========================================================
> file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
> datos <- read_excel(file_path)
> 
  > cat("\n==============================\n")

==============================
  > cat("DIAGNÓSTICO INICIAL DEL DATASET\n")
DIAGNÓSTICO INICIAL DEL DATASET
> cat("==============================\n")
==============================
  > 
  > # =========================================================
> # LIMPIEZA BÁSICA
  > # =========================================================
> datos_limpieza <- datos %>%
  +   filter(!is.na(`Días cobertura con capacitación`),
             +          !is.na(Grupo))
> 
  > cat("\nRegistros totales válidos:", nrow(datos_limpieza), "\n")

Registros totales válidos: 2403 
> cat("Grupos disponibles:", paste(unique(datos_limpieza$Grupo), collapse = ", "), "\n")
Grupos disponibles: COBRANZA, ODG, PLAZA, SUCURSAL 
> 
  > # =========================================================
> # FUNCIÓN: RESUMEN POR GRUPO
  > # =========================================================
> resumen_grupo <- function(grupo, data) {
  + 
    +   cat("\n====================================\n")
  +   cat("GRUPO:", grupo, "\n")
  +   cat("====================================\n")
  + 
    +   datos_g <- data %>% filter(Grupo == grupo)
    + 
      +   cat("Registros:", nrow(datos_g), "\n")
    +   cat("Mediana días:", median(datos_g$`Días cobertura con capacitación`), "\n")
    +   cat("Media días:", round(mean(datos_g$`Días cobertura con capacitación`), 1), "\n")
    +   cat("CV global:",
            +       round(sd(datos_g$`Días cobertura con capacitación`) /
                            +             mean(datos_g$`Días cobertura con capacitación`), 2), "\n")
    + 
      +   cat("\n--- COMPLEJIDAD DE AGRUPADORES ---\n")
    + 
      +   agrupadores <- setdiff(colnames(datos_g),
                                 +                          c("Días cobertura con capacitación", "Grupo"))
      + 
        +   for (col in agrupadores) {
          + 
            +     niveles <- n_distinct(datos_g[[col]])
            +     registros_por_nivel <- datos_g %>%
              +       group_by_at(col) %>%
              +       summarise(n = n(), .groups = "drop")
            + 
              +     min_n <- min(registros_por_nivel$n)
              +     med_n <- median(registros_por_nivel$n)
              + 
                +     cat("\n", col, "\n")
              +     cat("  Niveles:", niveles, "\n")
              +     cat("  Registros mín. por nivel:", min_n, "\n")
              +     cat("  Mediana registros por nivel:", med_n, "\n")
              + 
                +     if (niveles > 30) {
                  +       cat("  ⚠️ Alta granularidad\n")
                  +     }
              +     if (med_n < 10) {
                +       cat("  ❌ Poco soporte estadístico\n")
                +     }
              +   }
      + }
> 
  > # =========================================================
> # EJECUCIÓN
  > # =========================================================
> grupos <- unique(datos_limpieza$Grupo)
> walk(grupos, resumen_grupo, data = datos_limpieza)

====================================
  GRUPO: COBRANZA 
====================================
  Registros: 670 
Mediana días: 41 
Media días: 53 
CV global: 0.82 

--- COMPLEJIDAD DE AGRUPADORES ---
  
  Año 
Niveles: 3 
Registros mín. por nivel: 196 
Mediana registros por nivel: 220 

Mes 
Niveles: 12 
Registros mín. por nivel: 31 
Mediana registros por nivel: 60.5 

IDColaborador 
Niveles: 668 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Nombre 
Niveles: 668 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Evento 
Niveles: 2 
Registros mín. por nivel: 22 
Mediana registros por nivel: 335 

MotivoEvento 
Niveles: 5 
Registros mín. por nivel: 1 
Mediana registros por nivel: 16 

FechaEfectiva 
Niveles: 210 
Registros mín. por nivel: 1 
Mediana registros por nivel: 2 
⚠️ Alta granularidad
❌ Poco soporte estadístico

IDPosicion 
Niveles: 448 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

CentroCostos 
Niveles: 31 
Registros mín. por nivel: 1 
Mediana registros por nivel: 20 
⚠️ Alta granularidad

DescripcionCC 
Niveles: 31 
Registros mín. por nivel: 1 
Mediana registros por nivel: 20 
⚠️ Alta granularidad

Puesto 
Niveles: 13 
Registros mín. por nivel: 1 
Mediana registros por nivel: 7 
❌ Poco soporte estadístico

Regional 
Niveles: 7 
Registros mín. por nivel: 4 
Mediana registros por nivel: 96 

Plaza 
Niveles: 28 
Registros mín. por nivel: 4 
Mediana registros por nivel: 20.5 

Estado 
Niveles: 28 
Registros mín. por nivel: 1 
Mediana registros por nivel: 12.5 

Nombre Reclutador 
Niveles: 57 
Registros mín. por nivel: 1 
Mediana registros por nivel: 12 
⚠️ Alta granularidad

FechaVacante 
Niveles: 379 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Fecha término de capacitación 
Niveles: 235 
Registros mín. por nivel: 1 
Mediana registros por nivel: 2 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Perfil Profesional 
Niveles: 2 
Registros mín. por nivel: 66 
Mediana registros por nivel: 335 

Segmento de puesto 
Niveles: 3 
Registros mín. por nivel: 1 
Mediana registros por nivel: 75 

Tabulador Salarial 
Niveles: 7 
Registros mín. por nivel: 1 
Mediana registros por nivel: 2 
❌ Poco soporte estadístico

Area de Personal 
Niveles: 2 
Registros mín. por nivel: 119 
Mediana registros por nivel: 335 

Puesto Generico 
Niveles: 8 
Registros mín. por nivel: 1 
Mediana registros por nivel: 26.5 

Familia de Puesto 
Niveles: 2 
Registros mín. por nivel: 4 
Mediana registros por nivel: 335 

====================================
  GRUPO: ODG 
====================================
  Registros: 339 
Mediana días: 39 
Media días: 51.3 
CV global: 0.96 

--- COMPLEJIDAD DE AGRUPADORES ---
  
  Año 
Niveles: 3 
Registros mín. por nivel: 93 
Mediana registros por nivel: 113 

Mes 
Niveles: 12 
Registros mín. por nivel: 22 
Mediana registros por nivel: 27.5 

IDColaborador 
Niveles: 339 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Nombre 
Niveles: 339 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Evento 
Niveles: 2 
Registros mín. por nivel: 18 
Mediana registros por nivel: 169.5 

MotivoEvento 
Niveles: 6 
Registros mín. por nivel: 2 
Mediana registros por nivel: 28 

FechaEfectiva 
Niveles: 171 
Registros mín. por nivel: 1 
Mediana registros por nivel: 2 
⚠️ Alta granularidad
❌ Poco soporte estadístico

IDPosicion 
Niveles: 295 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

CentroCostos 
Niveles: 45 
Registros mín. por nivel: 1 
Mediana registros por nivel: 3 
⚠️ Alta granularidad
❌ Poco soporte estadístico

DescripcionCC 
Niveles: 53 
Registros mín. por nivel: 1 
Mediana registros por nivel: 2 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Puesto 
Niveles: 133 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Regional 
Niveles: 1 
Registros mín. por nivel: 339 
Mediana registros por nivel: 339 

Plaza 
Niveles: 1 
Registros mín. por nivel: 339 
Mediana registros por nivel: 339 

Estado 
Niveles: 17 
Registros mín. por nivel: 1 
Mediana registros por nivel: 2 
❌ Poco soporte estadístico

Nombre Reclutador 
Niveles: 38 
Registros mín. por nivel: 1 
Mediana registros por nivel: 3.5 
⚠️ Alta granularidad
❌ Poco soporte estadístico

FechaVacante 
Niveles: 216 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Fecha término de capacitación 
Niveles: 21 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
❌ Poco soporte estadístico

Perfil Profesional 
Niveles: 7 
Registros mín. por nivel: 1 
Mediana registros por nivel: 16 

Segmento de puesto 
Niveles: 4 
Registros mín. por nivel: 1 
Mediana registros por nivel: 53.5 

Tabulador Salarial 
Niveles: 15 
Registros mín. por nivel: 1 
Mediana registros por nivel: 17 

Area de Personal 
Niveles: 2 
Registros mín. por nivel: 4 
Mediana registros por nivel: 169.5 

Puesto Generico 
Niveles: 9 
Registros mín. por nivel: 1 
Mediana registros por nivel: 22 

Familia de Puesto 
Niveles: 25 
Registros mín. por nivel: 1 
Mediana registros por nivel: 3 
❌ Poco soporte estadístico

====================================
  GRUPO: PLAZA 
====================================
  Registros: 446 
Mediana días: 25 
Media días: 34.2 
CV global: 0.88 

--- COMPLEJIDAD DE AGRUPADORES ---
  
  Año 
Niveles: 3 
Registros mín. por nivel: 125 
Mediana registros por nivel: 141 

Mes 
Niveles: 12 
Registros mín. por nivel: 15 
Mediana registros por nivel: 37.5 

IDColaborador 
Niveles: 446 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Nombre 
Niveles: 446 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Evento 
Niveles: 2 
Registros mín. por nivel: 10 
Mediana registros por nivel: 223 

MotivoEvento 
Niveles: 5 
Registros mín. por nivel: 1 
Mediana registros por nivel: 43 

FechaEfectiva 
Niveles: 173 
Registros mín. por nivel: 1 
Mediana registros por nivel: 2 
⚠️ Alta granularidad
❌ Poco soporte estadístico

IDPosicion 
Niveles: 331 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

CentroCostos 
Niveles: 76 
Registros mín. por nivel: 1 
Mediana registros por nivel: 4 
⚠️ Alta granularidad
❌ Poco soporte estadístico

DescripcionCC 
Niveles: 50 
Registros mín. por nivel: 1 
Mediana registros por nivel: 3 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Puesto 
Niveles: 20 
Registros mín. por nivel: 1 
Mediana registros por nivel: 3.5 
❌ Poco soporte estadístico

Regional 
Niveles: 6 
Registros mín. por nivel: 42 
Mediana registros por nivel: 73.5 

Plaza 
Niveles: 27 
Registros mín. por nivel: 4 
Mediana registros por nivel: 13 

Estado 
Niveles: 27 
Registros mín. por nivel: 1 
Mediana registros por nivel: 10 

Nombre Reclutador 
Niveles: 52 
Registros mín. por nivel: 1 
Mediana registros por nivel: 9 
⚠️ Alta granularidad
❌ Poco soporte estadístico

FechaVacante 
Niveles: 219 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Fecha término de capacitación 
Niveles: 13 
Registros mín. por nivel: 1 
Mediana registros por nivel: 2 
❌ Poco soporte estadístico

Perfil Profesional 
Niveles: 4 
Registros mín. por nivel: 33 
Mediana registros por nivel: 107.5 

Segmento de puesto 
Niveles: 2 
Registros mín. por nivel: 10 
Mediana registros por nivel: 223 

Tabulador Salarial 
Niveles: 9 
Registros mín. por nivel: 1 
Mediana registros por nivel: 15 

Area de Personal 
Niveles: 3 
Registros mín. por nivel: 1 
Mediana registros por nivel: 71 

Puesto Generico 
Niveles: 5 
Registros mín. por nivel: 1 
Mediana registros por nivel: 87 

Familia de Puesto 
Niveles: 1 
Registros mín. por nivel: 446 
Mediana registros por nivel: 446 

====================================
  GRUPO: SUCURSAL 
====================================
  Registros: 948 
Mediana días: 25 
Media días: 29.3 
CV global: 0.6 

--- COMPLEJIDAD DE AGRUPADORES ---
  
  Año 
Niveles: 3 
Registros mín. por nivel: 285 
Mediana registros por nivel: 286 

Mes 
Niveles: 12 
Registros mín. por nivel: 56 
Mediana registros por nivel: 80 

IDColaborador 
Niveles: 948 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Nombre 
Niveles: 948 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Evento 
Niveles: 2 
Registros mín. por nivel: 24 
Mediana registros por nivel: 474 

MotivoEvento 
Niveles: 4 
Registros mín. por nivel: 4 
Mediana registros por nivel: 29.5 

FechaEfectiva 
Niveles: 193 
Registros mín. por nivel: 1 
Mediana registros por nivel: 4 
⚠️ Alta granularidad
❌ Poco soporte estadístico

IDPosicion 
Niveles: 426 
Registros mín. por nivel: 1 
Mediana registros por nivel: 2 
⚠️ Alta granularidad
❌ Poco soporte estadístico

CentroCostos 
Niveles: 199 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

DescripcionCC 
Niveles: 199 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Puesto 
Niveles: 9 
Registros mín. por nivel: 1 
Mediana registros por nivel: 5 
❌ Poco soporte estadístico

Regional 
Niveles: 6 
Registros mín. por nivel: 95 
Mediana registros por nivel: 156 

Plaza 
Niveles: 27 
Registros mín. por nivel: 8 
Mediana registros por nivel: 35 

Estado 
Niveles: 28 
Registros mín. por nivel: 3 
Mediana registros por nivel: 18 

Nombre Reclutador 
Niveles: 53 
Registros mín. por nivel: 1 
Mediana registros por nivel: 17 
⚠️ Alta granularidad

FechaVacante 
Niveles: 459 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Fecha término de capacitación 
Niveles: 170 
Registros mín. por nivel: 1 
Mediana registros por nivel: 5 
⚠️ Alta granularidad
❌ Poco soporte estadístico

Perfil Profesional 
Niveles: 2 
Registros mín. por nivel: 2 
Mediana registros por nivel: 474 

Segmento de puesto 
Niveles: 2 
Registros mín. por nivel: 2 
Mediana registros por nivel: 474 

Tabulador Salarial 
Niveles: 5 
Registros mín. por nivel: 1 
Mediana registros por nivel: 1 
❌ Poco soporte estadístico

Area de Personal 
Niveles: 3 
Registros mín. por nivel: 2 
Mediana registros por nivel: 3 
❌ Poco soporte estadístico

Puesto Generico 
Niveles: 5 
Registros mín. por nivel: 1 
Mediana registros por nivel: 14 

Familia de Puesto 
Niveles: 1 
Registros mín. por nivel: 948 
Mediana registros por nivel: 948 
> 
  > cat("\nFIN DEL DIAGNÓSTICO\n")

FIN DEL DIAGNÓSTICO