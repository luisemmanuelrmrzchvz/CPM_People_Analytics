# =========================================================
# EXPLORACIÓN DE VIABILIDAD DE COMBINACIONES (ROBUSTO)
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

# =========================
# CARGA DE DATOS
# =========================
datos_raw <- read_excel(file_path)

# =========================
# NORMALIZACIÓN DE NOMBRES
# =========================
clean_names <- function(x) {
  x %>%
    str_to_lower() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("_+", "_") %>%
    str_replace_all("^_|_$", "")
}

colnames(datos_raw) <- clean_names(colnames(datos_raw))

# =========================
# DETECCIÓN AUTOMÁTICA DE TARGET
# =========================
target_col <- colnames(datos_raw)[
  str_detect(colnames(datos_raw), "dias") &
  str_detect(colnames(datos_raw), "cobertura")
]

if (length(target_col) != 1) {
  stop("❌ No se pudo identificar de forma única la columna de días de cobertura")
}

cat("✔ Columna target detectada:", target_col, "\n")

# =========================
# LIMPIEZA BÁSICA
# =========================
datos <- datos_raw %>%
  filter(
    !is.na(.data[[target_col]]),
    !is.na(grupo)
  )

# =========================
# VARIABLES DE INTERÉS
# =========================
vars_estructura <- c("regional", "plaza", "descripcioncc", "estado")
vars_gestion    <- c("nombre_reclutador", "area_de_personal")
vars_puesto     <- c("perfil_profesional", "segmento_de_puesto",
                     "puesto_generico", "familia_de_puesto")
vars_formacion  <- c("escolaridad", "especializacion")
vars_tecnicas   <- c("software_avanzado", "software_intermedio", "software_basico")

vars_all <- intersect(
  c(vars_estructura, vars_gestion, vars_puesto,
    vars_formacion, vars_tecnicas),
  colnames(datos)
)

# =========================
# MÉTRICAS
# =========================
cv <- function(x) sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)

eta_squared <- function(y, g) {
  grand_mean <- mean(y)
  ss_total <- sum((y - grand_mean)^2)
  ss_between <- sum(tapply(y, g, function(x)
    length(x) * (mean(x) - grand_mean)^2))
  ss_between / ss_total
}

evaluate_grouping <- function(df, vars, target_col) {

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
    CV_prom = mean(tapply(y, g, cv)),
    Rango_medias = diff(range(tapply(y, g, median))),
    stringsAsFactors = FALSE
  )
}

# =========================
# COMBINACIONES CONTROLADAS
# =========================
capas <- list(
  estructura = vars_estructura,
  gestion    = vars_gestion,
  puesto     = vars_puesto,
  formacion  = vars_formacion,
  tecnica    = vars_tecnicas
)

combos <- list()

# 1 variable
combos <- c(combos, lapply(vars_all, function(v) c(v)))

# 2 variables lógicas
for (i in seq_along(capas)) {
  for (j in i:length(capas)) {
    v1 <- intersect(capas[[i]], vars_all)
    v2 <- intersect(capas[[j]], vars_all)

    if (length(v1) > 0 && length(v2) > 0) {
      cmb <- expand.grid(v1, v2, stringsAsFactors = FALSE)
      cmb <- cmb[cmb[,1] != cmb[,2], ]
      combos <- c(combos, split(cmb, seq(nrow(cmb))))
    }
  }
}

combos <- unique(lapply(combos, sort))

# =========================
# EJECUCIÓN POR GRUPO
# =========================
resultados <- list()

for (grp in sort(unique(datos$grupo))) {

  cat("\n=========================\n")
  cat("GRUPO:", grp, "\n")
  cat("=========================\n")

  df_g <- datos %>% filter(grupo == grp)

  res_grp <- map_dfr(
    combos,
    ~ evaluate_grouping(df_g, .x, target_col)
  )

  if (nrow(res_grp) > 0) {
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












Año
Mes
IDColaborador
Nombre
Evento
MotivoEvento
FechaEfectiva
IDPosicion
CentroCostos
DescripcionCC
Puesto
Grupo
Regional
Plaza
Estado
Nombre Reclutador
FechaVacante
Fecha término de capacitación
Días cobertura con capacitación
Perfil Profesional
Segmento de puesto
Tabulador Salarial
Area de Personal
Puesto Generico
Familia de Puesto
Escolaridad
Especialización
Software-Avanzado
Software-Básico
Software-Intermedio

