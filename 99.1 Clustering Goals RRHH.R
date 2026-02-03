############################################################
#  EXPLORACIÓN AVANZADA – DÍAS DE COBERTURA CON CAPACITACIÓN
#  Versión: imprime resumen en consola + guarda CSV
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
# 2. RUTAS FIJAS (NO CAMBIAR)
# =========================
input_file <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
output_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura"

# =========================
# 3. PARÁMETROS (ajustables)
# =========================
target_var <- "Días cobertura con capacitación"  # EXACTO
vars_exploratorias <- c(
  "Regional", "Plaza", "DescripcionCC", "Estado",
  "Nombre Reclutador", "Perfil Profesional",
  "Segmento de puesto", "Area de Personal",
  "Puesto Generico", "Familia de Puesto",
  "Escolaridad", "Especialización",
  "Software-Avanzado", "Software-Intermedio", "Software-Básico"
)
min_n_indiv <- 15   # mínimo por nivel para considerar variable individual
min_n_pair  <- 20   # mínimo por celda para combinaciones de 2
top_print   <- 10   # cuántas filas mostrar por tabla en consola

# =========================
# 4. FUNCIONES AUXILIARES
# =========================
calc_dispersion <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n == 0) return(tibble(n = 0, mean = NA_real_, sd = NA_real_, cv = NA_real_, iqr = NA_real_))
  m <- mean(x)
  sdv <- sd(x)
  tibble(
    n = n,
    mean = m,
    sd = sdv,
    cv = ifelse(is.na(m) | m == 0, NA_real_, sdv / m),
    iqr = IQR(x)
  )
}

calc_eta_sq_safe <- function(df, formula) {
  # devuelve NA si no se puede calcular
  res <- tryCatch({
    aov_model <- aov(formula, data = df)
    aov_tab <- stats::anova(aov_model)
    if (nrow(aov_tab) < 1) return(NA_real_)
    ss_effect <- aov_tab[1, "Sum Sq"]
    ss_total <- sum(aov_tab$"Sum Sq", na.rm = TRUE)
    if (is.na(ss_effect) || ss_total == 0) return(NA_real_)
    as.numeric(ss_effect / ss_total)
  }, error = function(e) NA_real_)
  return(res)
}

# =========================
# 5. CARGA DE DATOS Y CHEQUEOS
# =========================
df_raw <- read_excel(input_file)

if (! target_var %in% colnames(df_raw)) {
  stop(paste0("La variable objetivo '", target_var, "' NO existe en el archivo. Revisa nombres exactos."))
}

# Hacemos copia local
df <- df_raw

# =========================
# 6. ANÁLISIS POR GRUPO con PRINT
# =========================
output_list <- list()
message("Iniciando análisis por Grupo...")

for (g in unique(df$Grupo)) {
  message("\n----------------------------")
  message("GRUPO: ", g)
  message("----------------------------")
  df_g <- df %>% filter(Grupo == g)

  # --- INDIVIDUALES ---
  indiv_tbls <- map_dfr(vars_exploratorias, function(v) {
    # contar por nivel y calcular métricas
    tmp <- df_g %>%
      filter(!is.na(.data[[v]])) %>%
      group_by(level = as.character(.data[[v]])) %>%
      summarise(calc_dispersion(.data[[target_var]]), .groups = "drop") %>%
      rename(Valor = level)
    # filtrar por n mínimo
    tmp <- tmp %>% filter(n >= min_n_indiv)
    if (nrow(tmp) == 0) return(tibble())
    eta <- calc_eta_sq_safe(df_g %>% filter(!is.na(.data[[v]])), as.formula(paste0("`", target_var, "` ~ `", v, "`")))
    tmp %>%
      mutate(
        Grupo = g,
        Variable = v,
        Tipo = "Individual",
        Eta2 = eta
      ) %>%
      select(Grupo, Variable, Valor, n, mean, sd, cv, iqr, Eta2, Tipo)
  })

  if (nrow(indiv_tbls) == 0) {
    message("-> No se encontraron niveles individuales con n >= ", min_n_indiv, " en este Grupo.")
  } else {
    # resumimos por Variable (promedio/ponderado?) — aquí mostramos la media de medias para orden
    summary_indiv <- indiv_tbls %>%
      group_by(Grupo, Variable) %>%
      summarise(
        eta_mean = unique(na.omit(Eta2))[1],
        n_levels = n(),
        mean_mean = mean(mean, na.rm = TRUE),
        sd_mean = mean(sd, na.rm = TRUE),
        cv_mean = mean(cv, na.rm = TRUE),
        iqr_mean = mean(iqr, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(eta_mean))

    message("\nTop variables INDIVIDUALES por Eta2 (resumen por Variable):")
    print(head(summary_indiv, top_print), n = top_print)
  }

  # --- COMBINACIONES DE 2 ---
  comb2 <- combn(vars_exploratorias, 2, simplify = FALSE)
  pairwise_tbls <- map_dfr(comb2, function(vs) {
    v1 <- vs[1]; v2 <- vs[2]
    tmp <- df_g %>%
      filter(!is.na(.data[[v1]]), !is.na(.data[[v2]])) %>%
      group_by(val1 = as.character(.data[[v1]]), val2 = as.character(.data[[v2]])) %>%
      summarise(calc_dispersion(.data[[target_var]]), .groups = "drop") %>%
      rename(Valor1 = val1, Valor2 = val2)
    tmp <- tmp %>% filter(n >= min_n_pair)
    if (nrow(tmp) == 0) return(tibble())
    eta <- calc_eta_sq_safe(df_g %>% filter(!is.na(.data[[v1]]), !is.na(.data[[v2]])),
                             as.formula(paste0("`", target_var, "` ~ `", v1, "` + `", v2, "`")))
    tmp %>%
      mutate(
        Grupo = g,
        Variable = paste(v1, v2, sep = " + "),
        Tipo = "Combinación 2",
        Eta2 = eta
      ) %>%
      select(Grupo, Variable, Valor1, Valor2, n, mean, sd, cv, iqr, Eta2, Tipo)
  })

  if (nrow(pairwise_tbls) == 0) {
    message("-> No se encontraron combinaciones de 2 con celdas con n >= ", min_n_pair, " en este Grupo.")
  } else {
    summary_pair <- pairwise_tbls %>%
      group_by(Grupo, Variable) %>%
      summarise(
        eta_mean = unique(na.omit(Eta2))[1],
        n_cells = n(),
        mean_mean = mean(mean, na.rm = TRUE),
        sd_mean = mean(sd, na.rm = TRUE),
        cv_mean = mean(cv, na.rm = TRUE),
        iqr_mean = mean(iqr, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(eta_mean))

    message("\nTop COMBINACIONES (2) por Eta2 (resumen por combinación):")
    print(head(summary_pair, top_print), n = top_print)
  }

  # Guardamos ambos para output global
  output_list[[paste0("indiv_", g)]] <- indiv_tbls %>% mutate(Grupo = g)
  output_list[[paste0("pair_", g)]] <- pairwise_tbls %>% mutate(Grupo = g)
}

# =========================
# 7. UNIR Y GUARDAR CSV
# =========================
final_results <- bind_rows(output_list, .id = "source") %>%
  select(-source)

if (nrow(final_results) == 0) {
  message("\n=== ATENCIÓN: RESULTADO FINAL VACÍO ===")
  message("Posibles causas: filtros de n mínimos (min_n_indiv / min_n_pair) demasiado estrictos, o datos NA en variables.")
} else {
  out_file <- file.path(output_path, "Exploracion_Dias_Cobertura_Por_Grupo.csv")
  write.csv(final_results, out_file, row.names = FALSE)
  message("\nArchivo guardado en: ", out_file)
  message("Filas guardadas: ", nrow(final_results))
}

message("\nFIN del script. Copia la salida de consola y pégala aquí si quieres que la revisemos.")
