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
> library(writexl)
> library(stats)
> 
  > # =========================================================
> # CONFIGURACIÓN
  > # =========================================================
> output_dir <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura"
> dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
> 
  > file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
> datos <- read_excel(file_path)
> 
  > cat("\n==============================\n")

==============================
  > cat("INICIO DEL ANÁLISIS DE GOALS\n")
INICIO DEL ANÁLISIS DE GOALS
> cat("==============================\n")
==============================
  > 
  > # =========================================================
> # LIMPIEZA
  > # =========================================================
> datos_limpieza <- datos %>%
  +   filter(!is.na(`Días cobertura con capacitación`),
             +          !is.na(Grupo))
> 
  > # =========================================================
> # FUNCIÓN: GOAL ADAPTATIVO
  > # =========================================================
> calcular_goal_mejorado <- function(x) {
  +   x <- x[!is.na(x)]
  +   cv <- sd(x) / mean(x)
  +   if (cv < 0.35) p <- 0.40
  +   else if (cv < 0.60) p <- 0.50
  +   else p <- 0.60
  +   list(goal = round(quantile(x, p)), percentil = p, cv = round(cv, 2))
  + }
> 
  > # =========================================================
> # ETA-SQUARED
  > # =========================================================
> calcular_eta_safe <- function(data, var) {
  +   tryCatch({
    +     if (length(unique(data[[var]])) < 2) return(0)
    +     a <- aov(`Días cobertura con capacitación` ~ data[[var]])
    +     an <- anova(a)
    +     an[1, "Sum Sq"] / sum(an$`Sum Sq`)
    +   }, error = function(e) 0)
  + }
> 
  > # =========================================================
> # BOOTSTRAP ESTABILIDAD
  > # =========================================================
> bootstrap_stability <- function(data, agrupador, R = 80) {
  +   goals <- replicate(R, {
    +     samp <- data[sample(nrow(data), replace = TRUE), ]
    +     g <- samp %>%
      +       group_by_at(agrupador) %>%
      +       summarise(goal = median(`Días cobertura con capacitación`),
                        +                 .groups = "drop")
    +     paste(sort(g$goal), collapse = "|")
    +   })
  +   max(table(goals)) / R
  + }
> 
  > # =========================================================
> # EVALUAR AGRUPADOR (CON LOG EN CONSOLA)
  > # =========================================================
> evaluar_agrupador <- function(col, data) {
  + 
    +   resumen <- data %>%
      +     group_by_at(col) %>%
      +     summarise(
        +       n = n(),
        +       media = mean(`Días cobertura con capacitación`),
        +       sd = sd(`Días cobertura con capacitación`),
        +       cv = sd / media,
        +       .groups = "drop"
        +     ) %>% filter(n >= 5)
    + 
      +   if (nrow(resumen) < 2) {
        +     cat(" -", col, ": descartado (pocos subgrupos)\n")
        +     return(NULL)
        +   }
    + 
      +   eta <- calcular_eta_safe(data, col)
      +   cv_prom <- mean(resumen$cv, na.rm = TRUE)
      +   estabilidad <- bootstrap_stability(data, col)
      + 
        +   score <- 0.5 * eta + 0.3 * (1 - cv_prom) + 0.2 * estabilidad
        + 
          +   cat("\nAGRUPADOR:", col, "\n")
        +   cat(" Subgrupos válidos:", nrow(resumen), "\n")
        +   cat(" Eta²:", round(eta, 3), "\n")
        +   cat(" CV promedio:", round(cv_prom, 3), "\n")
        +   cat(" Estabilidad:", round(estabilidad, 3), "\n")
        +   cat(" SCORE FINAL:", round(score, 3), "\n")
        + 
          +   if (score < 0.3) cat("  ⚠️ Agrupador débil\n")
        +   else if (score < 0.5) cat("  🟡 Agrupador usable\n")
        +   else cat("  🟢 Agrupador fuerte\n")
        + 
          +   data.frame(
            +     agrupador = col,
            +     score = score
            +   )
        + }
> 
  > # =========================================================
> # ANÁLISIS POR GRUPO
  > # =========================================================
> analizar_grupo <- function(grupo, data) {
  + 
    +   cat("\n==============================\n")
  +   cat("GRUPO:", grupo, "\n")
  +   cat("==============================\n")
  + 
    +   datos_g <- data %>% filter(Grupo == grupo)
    + 
      +   cat("Registros totales:", nrow(datos_g), "\n")
    +   cat("Mediana global:", median(datos_g$`Días cobertura con capacitación`), "\n")
    +   cat("CV global:",
            +       round(sd(datos_g$`Días cobertura con capacitación`) /
                            +             mean(datos_g$`Días cobertura con capacitación`), 2), "\n")
    + 
      +   agrupadores <- setdiff(colnames(datos_g),
                                 +                          c("Días cobertura con capacitación", "Grupo"))
      + 
        +   evaluaciones <- map_dfr(agrupadores, evaluar_agrupador, data = datos_g)
        + 
          +   mejor <- evaluaciones %>% arrange(desc(score)) %>% slice(1) %>% pull(agrupador)
          + 
            +   cat("\n>>> MEJOR AGRUPADOR SELECCIONADO:", mejor, "\n")
          + 
            +   cat("\n--- GOALS POR SUBGRUPO ---\n")
          + 
            +   datos_g %>%
            +     group_by_at(mejor) %>%
            +     summarise(
              +       n = n(),
              +       mediana = median(`Días cobertura con capacitación`),
              +       info = list(calcular_goal_mejorado(`Días cobertura con capacitación`)),
              +       .groups = "drop"
              +     ) %>%
            +     filter(n >= 5) %>%
            +     rowwise() %>%
            +     mutate(
              +       goal = info$goal,
              +       percentil = info$percentil,
              +       cv = info$cv,
              +       tipo = case_when(
                +         cv < 0.4 ~ "🟢 Proceso maduro",
                +         cv < 0.7 ~ "🟡 Proceso exigente",
                +         TRUE ~ "🔴 Proceso complejo / especializado"
                +       )
              +     ) %>%
            +     select(-info) %>%
            +     { print(.) }
          + 
            + }
> 
  > # =========================================================
> # EJECUCIÓN
  > # =========================================================
> grupos <- unique(datos_limpieza$Grupo)
> walk(grupos, analizar_grupo, data = datos_limpieza)

==============================
  GRUPO: COBRANZA 
==============================
  Registros totales: 670 
Mediana global: 41 
CV global: 0.82 

AGRUPADOR: Año 
Subgrupos válidos: 3 
Eta²: 0 
CV promedio: 0.813 
Estabilidad: 0.025 
SCORE FINAL: 0.061 
⚠️ Agrupador débil

AGRUPADOR: Mes 
Subgrupos válidos: 12 
Eta²: 0 
CV promedio: 0.762 
Estabilidad: 0.013 
SCORE FINAL: 0.074 
⚠️ Agrupador débil
- IDColaborador : descartado (pocos subgrupos)
- Nombre : descartado (pocos subgrupos)

AGRUPADOR: Evento 
Subgrupos válidos: 2 
Eta²: 0 
CV promedio: 0.879 
Estabilidad: 0.138 
SCORE FINAL: 0.064 
⚠️ Agrupador débil

AGRUPADOR: MotivoEvento 
Subgrupos válidos: 4 
Eta²: 0 
CV promedio: 0.805 
Estabilidad: 0.025 
SCORE FINAL: 0.063 
⚠️ Agrupador débil

AGRUPADOR: FechaEfectiva 
Subgrupos válidos: 57 
Eta²: 0 
CV promedio: 0.605 
Estabilidad: 0.013 
SCORE FINAL: 0.121 
⚠️ Agrupador débil

AGRUPADOR: IDPosicion 
Subgrupos válidos: 3 
Eta²: 0 
CV promedio: 0.634 
Estabilidad: 0.013 
SCORE FINAL: 0.112 
⚠️ Agrupador débil

AGRUPADOR: CentroCostos 
Subgrupos válidos: 26 
Eta²: 0 
CV promedio: 0.632 
Estabilidad: 0.013 
SCORE FINAL: 0.113 
⚠️ Agrupador débil

AGRUPADOR: DescripcionCC 
Subgrupos válidos: 26 
Eta²: 0 
CV promedio: 0.632 
Estabilidad: 0.013 
SCORE FINAL: 0.113 
⚠️ Agrupador débil

AGRUPADOR: Puesto 
Subgrupos válidos: 8 
Eta²: 0 
CV promedio: 0.691 
Estabilidad: 0.013 
SCORE FINAL: 0.095 
⚠️ Agrupador débil

AGRUPADOR: Regional 
Subgrupos válidos: 6 
Eta²: 0 
CV promedio: 0.735 
Estabilidad: 0.013 
SCORE FINAL: 0.082 
⚠️ Agrupador débil

AGRUPADOR: Plaza 
Subgrupos válidos: 26 
Eta²: 0 
CV promedio: 0.632 
Estabilidad: 0.013 
SCORE FINAL: 0.113 
⚠️ Agrupador débil

AGRUPADOR: Estado 
Subgrupos válidos: 21 
Eta²: 0 
CV promedio: 0.616 
Estabilidad: 0.013 
SCORE FINAL: 0.118 
⚠️ Agrupador débil

AGRUPADOR: Nombre Reclutador 
Subgrupos válidos: 42 
Eta²: 0 
CV promedio: 0.549 
Estabilidad: 0.013 
SCORE FINAL: 0.138 
⚠️ Agrupador débil

AGRUPADOR: FechaVacante 
Subgrupos válidos: 17 
Eta²: 0 
CV promedio: 0.735 
Estabilidad: 0.013 
SCORE FINAL: 0.082 
⚠️ Agrupador débil

AGRUPADOR: Fecha término de capacitación 
Subgrupos válidos: 23 
Eta²: 0 
CV promedio: 0.475 
Estabilidad: 0.013 
SCORE FINAL: 0.16 
⚠️ Agrupador débil

AGRUPADOR: Perfil Profesional 
Subgrupos válidos: 2 
Eta²: 0 
CV promedio: 0.838 
Estabilidad: 0.088 
SCORE FINAL: 0.066 
⚠️ Agrupador débil

AGRUPADOR: Segmento de puesto 
Subgrupos válidos: 2 
Eta²: 0 
CV promedio: 0.824 
Estabilidad: 0.062 
SCORE FINAL: 0.065 
⚠️ Agrupador débil

AGRUPADOR: Tabulador Salarial 
Subgrupos válidos: 3 
Eta²: 0 
CV promedio: 0.624 
Estabilidad: 0.025 
SCORE FINAL: 0.118 
⚠️ Agrupador débil

AGRUPADOR: Area de Personal 
Subgrupos válidos: 2 
Eta²: 0 
CV promedio: 0.844 
Estabilidad: 0.1 
SCORE FINAL: 0.067 
⚠️ Agrupador débil

AGRUPADOR: Puesto Generico 
Subgrupos válidos: 5 
Eta²: 0 
CV promedio: 0.755 
Estabilidad: 0.013 
SCORE FINAL: 0.076 
⚠️ Agrupador débil
- Familia de Puesto : descartado (pocos subgrupos)

>>> MEJOR AGRUPADOR SELECCIONADO: Fecha término de capacitación 

--- GOALS POR SUBGRUPO ---
  Error in `map()`:
  ℹ In index: 1.
Caused by error in `summarise()`:
  ℹ In argument: `info = list(calcular_goal_mejorado(`Días cobertura con capacitación`))`.
ℹ In group 2: `Fecha término de capacitación = 2023-01-13`.
Caused by error in `if (cv < 0.35) ...`:
  ! valor ausente donde TRUE/FALSE es necesario
Run `rlang::last_trace()` to see where the error occurred.
Called from: signal_abort(cnd, .file)
Browse[1]> 
  > rlang::last_trace()
<error/purrr_error_indexed>
  Error in `map()`:
  ℹ In index: 1.
Caused by error in `summarise()`:
  ℹ In argument: `info = list(calcular_goal_mejorado(`Días cobertura con capacitación`))`.
ℹ In group 2: `Fecha término de capacitación = 2023-01-13`.
Caused by error in `if (cv < 0.35) ...`:
  ! valor ausente donde TRUE/FALSE es necesario
---
  Backtrace:
  ▆
1. ├─purrr::walk(grupos, analizar_grupo, data = datos_limpieza)
2. │ └─purrr::map(.x, .f, ..., .progress = .progress)
3. │   └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
4. │     ├─purrr:::with_indexed_errors(...)
5. │     │ └─base::withCallingHandlers(...)
6. │     ├─purrr:::call_with_cleanup(...)
7. │     └─global .f(.x[[i]], ...)
8. │       └─... %>% ...
9. ├─dplyr::select(., -info)
10. ├─dplyr::mutate(...)
11. ├─dplyr::rowwise(.)
12. ├─dplyr::filter(., n >= 5)
13. ├─dplyr::summarise(...)
14. ├─dplyr:::summarise.grouped_df(...)
15. │ └─dplyr:::summarise_cols(.data, dplyr_quosures(...), by, "summarise")
16. │   ├─base::withCallingHandlers(...)
17. │   └─dplyr:::map(quosures, summarise_eval_one, mask = mask)
18. │     └─base::lapply(.x, .f, ...)
19. │       └─dplyr (local) FUN(X[[i]], ...)
20. │         └─mask$eval_all_summarise(quo)
21. │           └─dplyr (local) eval()
22. └─global calcular_goal_mejorado(`Días cobertura con capacitación`)
