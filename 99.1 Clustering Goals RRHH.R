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
# TRANSFORMACIÓN DE VARIABLES NUEVAS
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

# Complejidad
datos <- datos %>%
  mutate(
    Perfil_TI = ifelse(
      Macro_Especializacion == "TI" | N_Software_Avanzado > 0,
      "TI", "No TI"
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
# UTILIDADES
# =========================================================
remove_single_level_factors <- function(df) {
  keep <- sapply(df, function(x) {
    if (is.factor(x) || is.character(x)) {
      length(unique(x[!is.na(x)])) > 1
    } else TRUE
  })
  df[, keep, drop = FALSE]
}

make_feature_matrix <- function(df, cat_vars, num_vars) {
  df <- df %>% select(all_of(c(cat_vars, num_vars)))
  df <- df %>% mutate(across(where(is.character), as.factor))
  df <- remove_single_level_factors(df)
  df
}

rmse <- function(y, p) sqrt(mean((y - p)^2, na.rm = TRUE))

# =========================================================
# FUNCIÓN PRINCIPAL DE ANÁLISIS ROBUSTO
# =========================================================
analyze_group_full <- function(grupo, data, columnas_interes, nuevos_vars,
                               improvement_threshold = 0.05) {

  cat("\n============================================================\n")
  cat("ANALIZANDO GRUPO:", grupo, "\n")
  cat("============================================================\n")

  df <- data %>% filter(Grupo == grupo)
  if (nrow(df) < 15) {
    cat("⚠️ Pocos registros. Omitido.\n")
    return(NULL)
  }

  y <- df$`Días cobertura con capacitación`
  X <- make_feature_matrix(
    df,
    cat_vars = intersect(columnas_interes, names(df)),
    num_vars = intersect(nuevos_vars, names(df))
  )

  if (ncol(X) < 2) {
    cat("⚠️ Insuficientes variables.\n")
    return(NULL)
  }

  set.seed(999)
  idx <- sample(seq_len(nrow(X)), size = floor(0.75 * nrow(X)))
  X_train <- X[idx, ]; y_train <- y[idx]
  X_test  <- X[-idx, ]; y_test <- y[-idx]

  baseline_pred <- rep(median(y_train), length(y_test))
  baseline_rmse <- rmse(y_test, baseline_pred)

  resultados <- data.frame(Modelo = character(), RMSE = numeric(), Mejora = numeric())

  # Árbol
  tree_fit <- rpart(y_train ~ ., data = X_train, control = rpart.control(cp = 0.01))
  tree_pred <- predict(tree_fit, X_test)
  tree_rmse <- rmse(y_test, tree_pred)
  resultados <- rbind(resultados,
                      data.frame(Modelo = "Arbol",
                                 RMSE = tree_rmse,
                                 Mejora = (baseline_rmse - tree_rmse) / baseline_rmse))

  # Random Forest
  if (nrow(X_train) >= 30) {
    rf_fit <- randomForest(x = X_train, y = y_train, ntree = 300)
    rf_pred <- predict(rf_fit, X_test)
    rf_rmse <- rmse(y_test, rf_pred)
    resultados <- rbind(resultados,
                        data.frame(Modelo = "RandomForest",
                                   RMSE = rf_rmse,
                                   Mejora = (baseline_rmse - rf_rmse) / baseline_rmse))
  }

  best <- resultados[which.min(resultados$RMSE), ]
  usar_modelo <- best$Mejora >= improvement_threshold

  cat("\nResultados modelos:\n")
  print(resultados)
  cat("\nBaseline RMSE:", round(baseline_rmse, 2), "\n")

  if (usar_modelo) {
    cat("✅ Usar modelo:", best$Modelo, "\n")
  } else {
    cat("⚠️ Usar percentiles (modelo no mejora lo suficiente)\n")
  }

  write.csv(resultados,
            file.path(output_dir, paste0("metricas_", grupo, ".csv")),
            row.names = FALSE)

  list(
    baseline = median(y_train),
    baseline_rmse = baseline_rmse,
    mejor_modelo = best$Modelo,
    rmse = best$RMSE,
    mejora = best$Mejora,
    usar_modelo = usar_modelo
  )
}

# =========================================================
# EJECUCIÓN GLOBAL
# =========================================================
grupos_obj <- unique(datos$Grupo)

columnas_interes <- setdiff(colnames(datos), "Días cobertura con capacitación")

nuevos_vars <- c(
  "Indice_Complejidad",
  "Total_Software",
  "N_Software_Avanzado"
)

resultados_finales <- list()

for (g in grupos_obj) {
  resultados_finales[[g]] <- analyze_group_full(
    grupo = g,
    data = datos,
    columnas_interes = columnas_interes,
    nuevos_vars = nuevos_vars
  )
}

cat("\n==============================\n")
cat("FIN DEL ANÁLISIS ROBUSTO\n")
cat("==============================\n")





















> resultados_finales <- list()
> 
  > for (g in grupos_obj) {
    +   resultados_finales[[g]] <- analyze_group_full(
      +     grupo = g,
      +     data = datos,
      +     columnas_interes = columnas_interes,
      +     nuevos_vars = nuevos_vars
      +   )
    + }

============================================================
  ANALIZANDO GRUPO: COBRANZA 
============================================================
  Error en randomForest.default(x = X_train, y = y_train, ntree = 300): 
  NA not permitted in predictors
Called from: randomForest.default(x = X_train, y = y_train, ntree = 300)