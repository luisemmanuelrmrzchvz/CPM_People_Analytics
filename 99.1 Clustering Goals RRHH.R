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

set.seed(123)

# =========================================================
# CARGA DE DATOS
# =========================================================
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
datos <- read_excel(file_path)

cat("\n==============================\n")
cat("INICIO MODELO DE ESTIMACIÓN DE GOALS\n")
cat("==============================\n")

# =========================================================
# LIMPIEZA BÁSICA
# =========================================================
datos <- datos %>%
  filter(
    !is.na(`Días cobertura con capacitación`),
    !is.na(Grupo)
  )

# =========================================================
# TRANSFORMACIÓN DE CAMPOS DE COMPLEJIDAD
# =========================================================

# ---- Escolaridad
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

# ---- Especialización
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

# ---- Software (conteo)
contar_herramientas <- function(x) {
  ifelse(
    is.na(x) | trimws(x) == "",
    0,
    str_count(x, ",") + 1
  )
}

datos <- datos %>%
  mutate(
    N_Software_Avanzado = contar_herramientas(`Software-Avanzado`),
    N_Software_Intermedio = contar_herramientas(`Software-Intermedio`),
    N_Software_Basico = contar_herramientas(`Software-Básico`),
    Total_Software = N_Software_Avanzado + N_Software_Intermedio + N_Software_Basico
  )

# ---- Indicadores de complejidad
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

cat("\nDistribución Complejidad_Nivel:\n")
print(table(datos$Complejidad_Nivel))

# =========================================================
# FEATURE ENGINEERING ROBUSTO
# =========================================================

make_feature_matrix <- function(df, cat_vars, num_vars, topN = 30) {
  
  mats <- list()
  
  for (v in cat_vars) {
    vals <- as.character(df[[v]])
    vals[is.na(vals)] <- "NA_MISSING"
    nlevels <- length(unique(vals))
    
    if (nlevels > topN) {
      tab <- sort(table(vals), decreasing = TRUE)
      freq_col <- as.numeric(tab[match(vals, names(tab))]) / length(vals)
      freq_col[is.na(freq_col)] <- 0
      
      mats[[v]] <- data.frame(freq_col)
      colnames(mats[[v]]) <- paste0(v, "_freq")
      
    } else {
      mm <- model.matrix(~ 0 + factor(vals))
      mm <- as.data.frame(mm)
      colnames(mm) <- paste0(v, "__", make.names(colnames(mm)))
      mats[[v]] <- mm
    }
  }
  
  num_df <- df[, num_vars, drop = FALSE]
  res <- cbind(num_df, do.call(cbind, mats))
  
  nzv <- sapply(res, function(x) length(unique(x[!is.na(x)])) > 1)
  res <- res[, nzv, drop = FALSE]
  
  return(res)
}

# =========================================================
# DEFINICIÓN DE VARIABLES
# =========================================================
target <- datos$`Días cobertura con capacitación`

cat_vars <- c(
  "Grupo",
  "Macro_Especializacion",
  "Perfil_TI",
  "Complejidad_Nivel",
  "Nivel_Escolaridad"
)

num_vars <- c(
  "Indice_Complejidad",
  "Total_Software",
  "N_Software_Avanzado"
)

X <- make_feature_matrix(datos, cat_vars, num_vars)

# =========================================================
# PARTICIÓN TRAIN / TEST
# =========================================================
idx <- sample(seq_len(nrow(X)), size = floor(0.7 * nrow(X)))
X_train <- X[idx, ]
X_test  <- X[-idx, ]
y_train <- target[idx]
y_test  <- target[-idx]

# =========================================================
# MODELOS
# =========================================================

# ---- Regresión lineal
lm_fit <- lm(y_train ~ ., data = X_train)
lm_pred <- predict(lm_fit, X_test)

# ---- Árbol
tree_fit <- rpart(y_train ~ ., data = X_train, control = rpart.control(cp = 0.01))
tree_pred <- predict(tree_fit, X_test)

# ---- Random Forest
rf_fit <- randomForest(x = X_train, y = y_train, ntree = 300)
rf_pred <- predict(rf_fit, X_test)

# =========================================================
# MÉTRICAS
# =========================================================
rmse <- function(y, p) sqrt(mean((y - p)^2))

rmse_tab <- data.frame(
  Modelo = c("Lineal", "Árbol", "RandomForest"),
  RMSE = c(
    rmse(y_test, lm_pred),
    rmse(y_test, tree_pred),
    rmse(y_test, rf_pred)
  )
)

cat("\n==============================\n")
cat("RESULTADOS DE MODELOS\n")
cat("==============================\n")
print(rmse_tab)

# =========================================================
# COMPARACIÓN CON PERCENTILES
# =========================================================
percentiles <- quantile(target, probs = c(0.4, 0.5, 0.6, 0.7))

cat("\nPercentiles globales (días cobertura):\n")
print(percentiles)

cat("\nRMSE vs usar mediana global:\n")
cat("RMSE Mediana:", rmse(y_test, median(target)), "\n")

cat("\n==============================\n")
cat("FIN DEL SCRIPT\n")
cat("==============================\n")
