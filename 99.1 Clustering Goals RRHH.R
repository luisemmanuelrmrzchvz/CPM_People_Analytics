############################################################
#  EXPLORACIÓN AVANZADA – DÍAS DE COBERTURA CON CAPACITACIÓN
#  Análisis diagnóstico por Grupo
############################################################

# =========================
# 1. LIBRERÍAS
# =========================
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(broom)
library(forcats)

# =========================
# 2. RUTAS FIJAS
# =========================
input_file <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
output_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura"

# =========================
# 3. CARGA DE DATOS
# =========================
df <- read_excel(input_file)

target_var <- "Días cobertura con capacitación"

# =========================
# 4. VARIABLES A EVALUAR
# =========================
vars_exploratorias <- c(
  "Regional", "Plaza", "DescripcionCC", "Estado",
  "Nombre Reclutador", "Perfil Profesional",
  "Segmento de puesto", "Area de Personal",
  "Puesto Generico", "Familia de Puesto",
  "Escolaridad", "Especialización",
  "Software-Avanzado", "Software-Intermedio", "Software-Básico"
)

# =========================
# 5. FUNCIONES AUXILIARES
# =========================

# ---- Métricas descriptivas ----
calc_dispersion <- function(x) {
  tibble(
    n = length(x),
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    cv = sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE),
    iqr = IQR(x, na.rm = TRUE)
  )
}

# ---- Eta Squared ----
calc_eta_sq <- function(df, formula) {
  aov_model <- aov(formula, data = df)
  aov_tab <- summary(aov_model)[[1]]
  eta <- aov_tab["Sum Sq"][1] / sum(aov_tab["Sum Sq"])
  return(as.numeric(eta))
}

# =========================
# 6. ANÁLISIS POR GRUPO
# =========================
resultados <- list()

for (g in unique(df$Grupo)) {

  df_g <- df %>% filter(Grupo == g)

  # -------------------------
  # 6.1 VARIABLES INDIVIDUALES
  # -------------------------
  indiv <- map_dfr(vars_exploratorias, function(v) {

    tmp <- df_g %>%
      filter(!is.na(.data[[v]])) %>%
      group_by(.data[[v]]) %>%
      summarise(calc_dispersion(.data[[target_var]]), .groups = "drop") %>%
      filter(n >= 15)

    if (nrow(tmp) < 2) return(NULL)

    eta <- calc_eta_sq(
      df_g %>% filter(!is.na(.data[[v]])),
      as.formula(paste0("`", target_var, "` ~ `", v, "`"))
    )

    tmp %>%
      mutate(
        Grupo = g,
        Variable = v,
        Tipo = "Individual",
        Eta2 = eta
      )
  })

  # -------------------------
  # 6.2 COMBINACIONES DE 2
  # -------------------------
  comb2 <- combn(vars_exploratorias, 2, simplify = FALSE)

  pairwise <- map_dfr(comb2, function(vs) {

    tmp <- df_g %>%
      filter(!is.na(.data[[vs[1]]]), !is.na(.data[[vs[2]]])) %>%
      group_by(.data[[vs[1]]], .data[[vs[2]]]) %>%
      summarise(calc_dispersion(.data[[target_var]]), .groups = "drop") %>%
      filter(n >= 20)

    if (nrow(tmp) < 3) return(NULL)

    eta <- calc_eta_sq(
      df_g %>% filter(!is.na(.data[[vs[1]]]), !is.na(.data[[vs[2]]])),
      as.formula(paste0("`", target_var, "` ~ `", vs[1], "` + `", vs[2], "`"))
    )

    tmp %>%
      mutate(
        Grupo = g,
        Variable = paste(vs, collapse = " + "),
        Tipo = "Combinación 2",
        Eta2 = eta
      )
  })

  resultados[[g]] <- bind_rows(indiv, pairwise)
}

# =========================
# 7. OUTPUT
# =========================
final_results <- bind_rows(resultados)

write.csv(
  final_results,
  file = file.path(output_path, "Exploracion_Dias_Cobertura_Por_Grupo.csv"),
  row.names = FALSE
)
