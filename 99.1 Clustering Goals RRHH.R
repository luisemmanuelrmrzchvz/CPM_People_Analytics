# =========================================================
# EXPLORACIÓN DE VIABILIDAD DE COMBINACIONES
# =========================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(cluster)
library(rpart)

set.seed(123)

# =========================
# CONFIGURACIÓN
# =========================
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"

min_n <- 15
max_levels <- 30

# =========================
# CARGA DE DATOS
# =========================
datos <- read_excel(file_path) %>%
  filter(!is.na(`Días cobertura con capacitación`),
         !is.na(Grupo))

target <- "Días cobertura con capacitación"

# =========================
# VARIABLES DE INTERÉS
# =========================
vars_estructura <- c("Regional", "Plaza", "DescripcionCC", "Estado")
vars_gestion    <- c("Nombre Reclutador", "Area de Personal")
vars_puesto     <- c("Perfil Profesional", "Segmento de puesto",
                     "Puesto Generico", "Familia de Puesto")
vars_formacion  <- c("Escolaridad", "Especialización")
vars_tecnicas   <- c("Software-Avanzado", "Software-Intermedio", "Software-Básico")

vars_all <- c(vars_estructura, vars_gestion, vars_puesto,
              vars_formacion, vars_tecnicas)

vars_all <- intersect(vars_all, colnames(datos))

# =========================
# FUNCIONES DE MÉTRICAS
# =========================

cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

eta_squared <- function(y, g) {
  grand_mean <- mean(y)
  ss_total <- sum((y - grand_mean)^2)
  ss_between <- sum(tapply(y, g, function(x)
    length(x) * (mean(x) - grand_mean)^2))
  ss_between / ss_total
}

evaluate_grouping <- function(df, vars) {

  df <- df %>%
    mutate(grp = interaction(across(all_of(vars)), drop = TRUE))

  sizes <- table(df$grp)
  valid <- names(sizes[sizes >= min_n])

  if (length(valid) < 2) return(NULL)

  df <- df %>% filter(grp %in% valid)

  y <- df[[target]]
  g <- df$grp

  data.frame(
    Variables = paste(vars, collapse = " + "),
    Grupos = length(unique(g)),
    Min_n = min(table(g)),
    Eta2 = eta_squared(y, g),
    CV_prom = mean(tapply(y, g, cv)),
    Rango_medias = diff(range(tapply(y, g, median))),
    stringsAsFactors = FALSE
  )
}

# =========================
# GENERACIÓN DE COMBINACIONES
# =========================

combos <- list()

# 1 variable
combos <- c(combos, lapply(vars_all, function(v) c(v)))

# 2 variables (misma capa o adyacentes)
capas <- list(
  estructura = vars_estructura,
  gestion    = vars_gestion,
  puesto     = vars_puesto,
  formacion  = vars_formacion,
  tecnica    = vars_tecnicas
)

for (i in seq_along(capas)) {
  for (j in i:length(capas)) {
    v1 <- capas[[i]]
    v2 <- capas[[j]]
    cmb <- expand.grid(v1, v2, stringsAsFactors = FALSE)
    cmb <- cmb[cmb[,1] != cmb[,2], ]
    combos <- c(combos, split(cmb, seq(nrow(cmb))))
  }
}

combos <- unique(lapply(combos, sort))

# =========================
# EJECUCIÓN POR GRUPO
# =========================
resultados <- list()

for (grp in sort(unique(datos$Grupo))) {

  cat("\n=========================\n")
  cat("GRUPO:", grp, "\n")
  cat("=========================\n")

  df_g <- datos %>% filter(Grupo == grp)

  res_grp <- map_dfr(combos, ~ evaluate_grouping(df_g, .x))

  if (!is.null(res_grp)) {
    res_grp <- res_grp %>%
      mutate(
        Score = 0.4 * Eta2 +
                0.3 * (1 / CV_prom) +
                0.3 * (1 / Grupos)
      ) %>%
      arrange(desc(Score))

    resultados[[grp]] <- res_grp

    print(head(res_grp, 10))
  }
}

cat("\n✔ Exploración de combinaciones finalizada\n")







> # =========================
> # CARGA DE DATOS
  > # =========================
> datos <- read_excel(file_path) %>%
  +   filter(!is.na(`Días cobertura con capacitación`),
             +          !is.na(Grupo))
Error in `filter()`:
  ℹ In argument: `!is.na(`Días cobertura con capacitación`)`.
Caused by error:
  ! objeto 'Días cobertura con capacitación' no encontrado
Run `rlang::last_trace()` to see where the error occurred.

> rlang::last_trace()
<error/rlang_error>
  Error in `filter()`:
  ℹ In argument: `!is.na(`Días cobertura con capacitación`)`.
Caused by error:
  ! objeto 'Días cobertura con capacitación' no encontrado
---
  Backtrace:
  ▆
1. ├─read_excel(file_path) %>% ...
2. ├─dplyr::filter(...)
3. └─dplyr:::filter.data.frame(., !is.na(`Días cobertura con capacitación`), !is.na(Grupo))
4.   └─dplyr:::filter_rows(.data, dots, by)
5.     └─dplyr:::filter_eval(...)
6.       ├─base::withCallingHandlers(...)
7.       └─mask$eval_all_filter(dots, env_filter)
8.         └─dplyr (local) eval()
Run rlang::last_trace(drop = FALSE) to see 3 hidden frames.
> rlang::last_trace(drop = FALSE)
<error/rlang_error>
  Error in `filter()`:
  ℹ In argument: `!is.na(`Días cobertura con capacitación`)`.
Caused by error:
  ! objeto 'Días cobertura con capacitación' no encontrado
---
  Backtrace:
  ▆
1. ├─read_excel(file_path) %>% ...
2. ├─dplyr::filter(...)
3. ├─dplyr:::filter.data.frame(., !is.na(`Días cobertura con capacitación`), !is.na(Grupo))
4. │ └─dplyr:::filter_rows(.data, dots, by)
5. │   └─dplyr:::filter_eval(...)
6. │     ├─base::withCallingHandlers(...)
7. │     └─mask$eval_all_filter(dots, env_filter)
8. │       └─dplyr (local) eval()
9. └─base::.handleSimpleError(...)
10.   └─dplyr (local) h(simpleError(msg, call))
11.     └─rlang::abort(message, class = error_class, parent = parent, call = error_call)