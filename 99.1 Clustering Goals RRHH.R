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
















# =========================================================
# LIBRERÍAS
# =========================================================
library(readxl)
library(dplyr)
library(stringr)
library(purrr)

# =========================================================
# CARGA DE DATOS
# =========================================================
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
datos <- read_excel(file_path)

cat("\n========================================\n")
cat("DIAGNÓSTICO DE CAMPOS NUEVOS (CAPTURA MANUAL)\n")
cat("========================================\n")

# =========================================================
# CAMPOS A ANALIZAR
# =========================================================
campos_nuevos <- c(
  "Escolaridad",
  "Especialización",
  "Software-Avanzado",
  "Software-Intermedio",
  "Software-Básico"
)

# Verificar que existan
campos_existentes <- intersect(campos_nuevos, colnames(datos))
campos_faltantes <- setdiff(campos_nuevos, campos_existentes)

if (length(campos_faltantes) > 0) {
  cat("\n⚠️ Campos no encontrados en el archivo:\n")
  cat(paste(campos_faltantes, collapse = ", "), "\n")
}

cat("\nCampos analizados:\n")
cat(paste(campos_existentes, collapse = ", "), "\n")

# =========================================================
# FUNCIÓN DE PERFILADO DE CAMPO
# =========================================================
perfil_campo <- function(df, campo) {

  cat("\n----------------------------------------\n")
  cat("CAMPO:", campo, "\n")
  cat("----------------------------------------\n")

  x <- df[[campo]]

  total <- length(x)
  na_count <- sum(is.na(x) | trimws(x) == "")
  na_pct <- round(100 * na_count / total, 1)

  cat("Total registros:", total, "\n")
  cat("Registros NA / vacíos:", na_count, "(", na_pct, "% )\n")

  valores <- df %>%
    mutate(valor = as.character(.data[[campo]])) %>%
    mutate(valor = trimws(valor)) %>%
    filter(!is.na(valor), valor != "") %>%
    count(valor, sort = TRUE)

  cat("Valores únicos (no NA):", nrow(valores), "\n")

  # Mostrar top 15 valores
  cat("\nTop valores más frecuentes:\n")
  print(head(valores, 15))

  # Señales de riesgo
  if (nrow(valores) > 30) {
    cat("⚠️ Alta cardinalidad (posible texto libre)\n")
  }

  if (any(str_detect(valores$valor, ","))) {
    cat("⚠️ Detectadas listas separadas por coma (checklist manual)\n")
  }

  if (any(str_detect(valores$valor, "/|\\+| y "))) {
    cat("⚠️ Detectadas combinaciones múltiples en un mismo registro\n")
  }

  if (any(str_detect(valores$valor, "^[Ss]í$|^[Nn]o$"))) {
    cat("ℹ️ Posible campo binario encubierto\n")
  }

  invisible(valores)
}

# =========================================================
# EJECUCIÓN DEL PERFILADO
# =========================================================
walk(campos_existentes, ~ perfil_campo(datos, .x))

cat("\n========================================\n")
cat("FIN DEL DIAGNÓSTICO DE CAMPOS NUEVOS\n")
cat("========================================\n")
