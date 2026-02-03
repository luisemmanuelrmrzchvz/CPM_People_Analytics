# =========================================================
# EXPLORACIÓN DE COMBINACIONES Y VIABILIDAD DE MÉTODOS
# =========================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(cluster)
library(stringi)

set.seed(123)

# =========================
# CONFIGURACIÓN
# =========================
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
min_n <- 15

# =========================
# CARGA DE DATOS
# =========================
datos_raw <- read_excel(file_path)

# =========================
# NORMALIZAR NOMBRES (interno)
# =========================
clean_names <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")
}

orig_names <- colnames(datos_raw)
colnames(datos_raw) <- clean_names(orig_names)

# =========================
# MAPEO FIJO DE COLUMNAS
# =========================
target_col <- "dias_cobertura_con_capacitacion"

datos <- datos_raw %>%
  filter(
    !is.na(.data[[target_col]]),
    !is.na(grupo)
  )

# =========================
# VARIABLES DE INTERÉS
# =========================
vars_estructura <- c(
  "regional", "plaza", "descripcioncc", "estado"
)

vars_gestion <- c(
  "nombre_reclutador", "area_de_personal"
)

vars_puesto <- c(
  "perfil_profesional", "segmento_de_puesto",
  "puesto_generico", "familia_de_puesto"
)

vars_formacion <- c(
  "escolaridad", "especializacion"
)

vars_tecnicas <- c(
  "software_avanzado", "software_intermedio", "software_basico"
)

vars_all <- c(
  vars_estructura,
  vars_gestion,
  vars_puesto,
  vars_formacion,
  vars_tecnicas
)

vars_all <- intersect(vars_all, colnames(datos))

# =========================
# MÉTRICAS
# =========================
cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

eta_squared <- function(y, g) {
  grand_mean <- mean(y, na.rm = TRUE)
  ss_total <- sum((y - grand_mean)^2, na.rm = TRUE)
  ss_between <- sum(tapply(y, g, function(x)
    length(x) * (mean(x, na.rm = TRUE) - grand_mean)^2), na.rm = TRUE)
  if (ss_total == 0) return(0)
  ss_between / ss_total
}

evaluate_grouping <- function(df, vars) {

  df <- df %>%
    mutate(grp = interaction(across(all_of(vars)), drop = TRUE))

  sizes <- table(df$grp)
  valid <- names(sizes[sizes >= min_n])

  if (length(valid) < 2) return(NULL)

  df <- df %>% filter(grp %in% valid)

  y <- df[[target_col]]
  g <- df$grp

  data.frame(
    Variables = paste(vars, collapse = " + "),
    Grupos = length(unique(g)),
    Min_n = min(table(g)),
    Eta2 = eta_squared(y, g),
    CV_prom = mean(tapply(y, g, cv), na.rm = TRUE),
    Rango_medias = diff(range(tapply(y, g, median), na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
}

# =========================
# COMBINACIONES (1 a 3 variables)
# =========================
combos <- list()

# 1 variable
combos <- c(combos, lapply(vars_all, c))

# 2 variables
combos <- c(combos, combn(vars_all, 2, simplify = FALSE))

# 3 variables (limitado para no explotar)
set.seed(123)
combos_3 <- combn(vars_all, 3, simplify = FALSE)
if (length(combos_3) > 150) {
  combos_3 <- sample(combos_3, 150)
}
combos <- c(combos, combos_3)

# =========================
# EJECUCIÓN POR GRUPO
# =========================
resultados <- list()

for (g in sort(unique(datos$grupo))) {

  cat("\n=================================================\n")
  cat("ANALIZANDO GRUPO:", g, "\n")
  cat("=================================================\n")

  df_g <- datos %>% filter(grupo == g)

  res <- map_dfr(
    combos,
    ~ evaluate_grouping(df_g, .x)
  )

  if (!is.null(res) && nrow(res) > 0) {

    res <- res %>%
      mutate(
        Score = 0.45 * Eta2 +
                0.35 * (1 / (CV_prom + 1e-6)) +
                0.20 * (1 / (Grupos + 1e-6))
      ) %>%
      arrange(desc(Score))

    resultados[[g]] <- res

    print(head(res, 12))
  } else {
    cat("Sin combinaciones viables para este grupo\n")
  }
}

cat("\n✔ Exploración de combinaciones completada\n")
