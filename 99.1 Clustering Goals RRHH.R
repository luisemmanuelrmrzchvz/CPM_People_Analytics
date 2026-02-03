# =========================================================
# LIBRERÍAS
# =========================================================
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(rpart)
library(randomForest)
library(stats)
library(cluster)
library(factoextra)

set.seed(123)

# =========================================================
# CONFIGURACIÓN
# =========================================================
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
output_dir <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =========================================================
# CARGA Y LIMPIEZA BÁSICA
# =========================================================
datos <- read_excel(file_path) %>%
  filter(
    !is.na(`Días cobertura con capacitación`),
    !is.na(Grupo)
  )

# =========================================================
# TRANSFORMACIÓN DE VARIABLES NUEVAS (LIMPIAS)
# =========================================================

# Escolaridad
datos <- datos %>%
  mutate(
    Escolaridad_std = str_to_lower(Escolaridad),
    Nivel_Escolaridad = case_when(
      str_detect(Escolaridad_std, "ingenier|licenciatura") ~ "Superior",
      str_detect(Escolaridad_std, "tsu|técnic|tecnica") ~ "Técnica",
      str_detect(Escolaridad_std, "preparatoria|bachiller") ~ "Media",
      TRUE ~ "Otro"
    )
  )

# Especialización
datos <- datos %>%
  mutate(
    Especializacion_std = str_to_lower(Especialización),
    Macro_Especializacion = case_when(
      str_detect(Especializacion_std, "informática|sistemas|ti|tecnolog") ~ "TI",
      str_detect(Especializacion_std, "derecho") ~ "Legal",
      str_detect(Especializacion_std, "contadur|finanza|econom") ~ "Financiero",
      str_detect(Especializacion_std, "administra|mercadotec") ~ "Administrativo",
      str_detect(Especializacion_std, "ingenier") ~ "Ingeniería",
      TRUE ~ "Otro"
    )
  )

# Software
contar_herramientas <- function(x) {
  ifelse(is.na(x) | trimws(x) == "", 0, str_count(x, ",") + 1)
}

datos <- datos %>%
  mutate(
    N_Software_Avanzado = contar_herramientas(`Software-Avanzado`),
    N_Software_Intermedio = contar_herramientas(`Software-Intermedio`),
    N_Software_Basico = contar_herramientas(`Software-Básico`),
    Total_Software = N_Software_Avanzado + N_Software_Intermedio + N_Software_Basico
  )

datos <- datos %>%
  mutate(
    Perfil_TI = ifelse(
      Macro_Especializacion == "TI" | N_Software_Avanzado > 0,
      "TI",
      "No TI"
    ),
    Complejidad_Nivel = case_when(
      Total_Software >= 8 | N_Software_Avanzado >= 2 ~ "Crítica",
      Total_Software >= 3 | Perfil_TI == "TI" ~ "Especializada",
      TRUE ~ "Estándar"
    ),
    Indice_Complejidad =
      Total_Software +
      ifelse(Perfil_TI == "TI", 2, 0) +
      ifelse(Macro_Especializacion %in% c("TI", "Ingeniería"), 1, 0)
  )

# =========================================================
# FUNCIÓN CRÍTICA: ELIMINAR FACTORES SIN VARIABILIDAD
# =========================================================
remove_single_level_factors <- function(df) {
  keep <- sapply(df, function(x) {
    if (is.factor(x) || is.character(x)) {
      length(unique(x[!is.na(x)])) > 1
    } else {
      TRUE
    }
  })
  df[, keep, drop = FALSE]
}

# =========================================================
# FEATURE ENGINEERING
# =========================================================
make_feature_matrix <- function(df, cat_vars, num_vars) {
  df <- df %>% select(all_of(c(cat_vars, num_vars)))
  df <- df %>% mutate(across(where(is.character), as.factor))
  df <- remove_single_level_factors(df)
  df
}

# =========================================================
# FUNCIÓN PRINCIPAL DE ANÁLISIS POR GRUPO
# =========================================================
analyze_group_full <- function(grupo, data, columnas_interes, nuevos_vars, approach_name) {

  cat("\n============================================================\n")
  cat("ANALIZANDO GRUPO:", grupo, "- Aproximación:", approach_name, "\n")
  cat("============================================================\n")

  df <- data %>% filter(Grupo == grupo)
  y <- df$`Días cobertura con capacitación`

  cat_vars <- intersect(columnas_interes, colnames(df))
  num_vars <- intersect(nuevos_vars, colnames(df))

  X <- make_feature_matrix(df, cat_vars, num_vars)

  if (ncol(X) < 2) {
    cat("⚠️ No hay suficientes variables para modelar\n")
    return(NULL)
  }

  idx <- sample(seq_len(nrow(X)), size = floor(0.7 * nrow(X)))
  X_train <- X[idx, ]
  X_test <- X[-idx, ]
  y_train <- y[idx]
  y_test <- y[-idx]

  resultados <- list()

  # ---------------- Lineal
  try({
    lm_fit <- lm(y_train ~ ., data = X_train)
    lm_pred <- predict(lm_fit, X_test)
    resultados$Lineal <- sqrt(mean((y_test - lm_pred)^2))
  }, silent = TRUE)

  # ---------------- Árbol
  try({
    tree_fit <- rpart(y_train ~ ., data = X_train, control = rpart.control(cp = 0.01))
    tree_pred <- predict(tree_fit, X_test)
    resultados$Arbol <- sqrt(mean((y_test - tree_pred)^2))
  }, silent = TRUE)

  # ---------------- Random Forest
  try({
    rf_fit <- randomForest(x = X_train, y = y_train, ntree = 300)
    rf_pred <- predict(rf_fit, X_test)
    resultados$RandomForest <- sqrt(mean((y_test - rf_pred)^2))
  }, silent = TRUE)

  resultados$Baseline_Mediana <- sqrt(mean((y_test - median(y))^2))

  cat("\nRMSE por modelo:\n")
  print(resultados)

  return(resultados)
}

# =========================================================
# EJECUCIÓN GLOBAL
# =========================================================
grupos_obj <- unique(datos$Grupo)

columnas_interes <- setdiff(colnames(datos),
                            c("Días cobertura con capacitación"))

nuevos_vars <- c(
  "Indice_Complejidad",
  "Total_Software",
  "N_Software_Avanzado"
)

resultados_grupos <- list()

for (g in grupos_obj) {

  resA <- analyze_group_full(
    g,
    datos,
    columnas_interes = columnas_interes,
    nuevos_vars = nuevos_vars,
    approach_name = "A_originales"
  )

  resultados_grupos[[paste0(g, "_A")]] <- resA

  resB <- analyze_group_full(
    g,
    datos,
    columnas_interes = columnas_interes,
    nuevos_vars = nuevos_vars,
    approach_name = "B_originales_nuevos"
  )

  resultados_grupos[[paste0(g, "_B")]] <- resB
}

cat("\n==============================\n")
cat("FIN DEL ANÁLISIS COMPLETO\n")
cat("==============================\n")
