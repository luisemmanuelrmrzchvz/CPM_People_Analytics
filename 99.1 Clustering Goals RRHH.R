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
> # SCRIPT DE ESTIMACIÓN DE GOALS (MULTI-MÉTODO + CAPAS)
  > # - 2 APROXIMACIONES: original vs original + campos nuevos
  > # - Modelos: clustering, lm, rpart, randomForest, xgboost, nnet
  > # - Bootstrap de estabilidad de goals
  > # - Modelo por CAPAS (base + incrementos por complejidad)
  > # =========================================================
> 
  > # ---------------------------
> # 0) Parámetros ejecutables
  > # ---------------------------
> output_dir <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura/Estimaciones MultiMetodo"
> dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
> 
  > # Control de costo computacional
  > CV_folds <- 5
> CV_repeats <- 2
> bootstrap_iter <- 200      # puedes subir a 500-1000 si tu máquina lo soporta
> min_registros_grupo <- 30  # mínimo por Grupo para correr modelos complejos
> seed <- 123
> 
  > # ¿correr modelos pesados? Si FALSE sólo calcula percentiles y clustering.
  > run_heavy_models <- TRUE
> 
  > # ---------------------------
> # 1) Paquetes necesarios
  > # ---------------------------
> pkgs <- c("readxl","dplyr","stringr","tidyr","caret","randomForest",
            +           "xgboost","rpart","nnet","doParallel","purrr","cluster","factoextra","writexl")
> missing_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
> if (length(missing_pkgs) > 0) {
  +   cat("Faltan paquetes:", paste(missing_pkgs, collapse = ", "), "\n")
  +   cat("Instálalos antes de correr este script, por ejemplo:\n")
  +   cat("install.packages(c('", paste(missing_pkgs, collapse = "','"), "'))\n", sep = "")
  +   stop("Instala paquetes faltantes y vuelve a ejecutar.")
  + }
> lapply(pkgs, library, character.only = TRUE)
Cargando paquete requerido: foreach

Adjuntando el paquete: ‘foreach’

The following objects are masked from ‘package:purrr’:
  
  accumulate, when

Cargando paquete requerido: iterators
Cargando paquete requerido: parallel
[[1]]
[1] "stringr"      "randomForest" "caret"        "lattice"      "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"       
[11] "factoextra"   "ggplot2"      "cluster"      "dplyr"        "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"    
[21] "methods"      "base"        

[[2]]
[1] "stringr"      "randomForest" "caret"        "lattice"      "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"       
[11] "factoextra"   "ggplot2"      "cluster"      "dplyr"        "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"    
[21] "methods"      "base"        

[[3]]
[1] "stringr"      "randomForest" "caret"        "lattice"      "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"       
[11] "factoextra"   "ggplot2"      "cluster"      "dplyr"        "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"    
[21] "methods"      "base"        

[[4]]
[1] "stringr"      "randomForest" "caret"        "lattice"      "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"       
[11] "factoextra"   "ggplot2"      "cluster"      "dplyr"        "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"    
[21] "methods"      "base"        

[[5]]
[1] "stringr"      "randomForest" "caret"        "lattice"      "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"       
[11] "factoextra"   "ggplot2"      "cluster"      "dplyr"        "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"    
[21] "methods"      "base"        

[[6]]
[1] "stringr"      "randomForest" "caret"        "lattice"      "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"       
[11] "factoextra"   "ggplot2"      "cluster"      "dplyr"        "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"    
[21] "methods"      "base"        

[[7]]
[1] "xgboost"      "stringr"      "randomForest" "caret"        "lattice"      "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"     
[11] "purrr"        "factoextra"   "ggplot2"      "cluster"      "dplyr"        "readxl"       "stats"        "graphics"     "grDevices"    "utils"       
[21] "datasets"     "methods"      "base"        

[[8]]
[1] "xgboost"      "stringr"      "randomForest" "caret"        "lattice"      "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"     
[11] "purrr"        "factoextra"   "ggplot2"      "cluster"      "dplyr"        "readxl"       "stats"        "graphics"     "grDevices"    "utils"       
[21] "datasets"     "methods"      "base"        

[[9]]
[1] "nnet"         "xgboost"      "stringr"      "randomForest" "caret"        "lattice"      "rpart.plot"   "rpart"        "gridExtra"    "tidyr"       
[11] "writexl"      "purrr"        "factoextra"   "ggplot2"      "cluster"      "dplyr"        "readxl"       "stats"        "graphics"     "grDevices"   
[21] "utils"        "datasets"     "methods"      "base"        

[[10]]
[1] "doParallel"   "parallel"     "iterators"    "foreach"      "nnet"         "xgboost"      "stringr"      "randomForest" "caret"        "lattice"     
[11] "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"        "factoextra"   "ggplot2"      "cluster"      "dplyr"       
[21] "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"     "methods"      "base"        

[[11]]
[1] "doParallel"   "parallel"     "iterators"    "foreach"      "nnet"         "xgboost"      "stringr"      "randomForest" "caret"        "lattice"     
[11] "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"        "factoextra"   "ggplot2"      "cluster"      "dplyr"       
[21] "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"     "methods"      "base"        

[[12]]
[1] "doParallel"   "parallel"     "iterators"    "foreach"      "nnet"         "xgboost"      "stringr"      "randomForest" "caret"        "lattice"     
[11] "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"        "factoextra"   "ggplot2"      "cluster"      "dplyr"       
[21] "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"     "methods"      "base"        

[[13]]
[1] "doParallel"   "parallel"     "iterators"    "foreach"      "nnet"         "xgboost"      "stringr"      "randomForest" "caret"        "lattice"     
[11] "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"        "factoextra"   "ggplot2"      "cluster"      "dplyr"       
[21] "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"     "methods"      "base"        

[[14]]
[1] "doParallel"   "parallel"     "iterators"    "foreach"      "nnet"         "xgboost"      "stringr"      "randomForest" "caret"        "lattice"     
[11] "rpart.plot"   "rpart"        "gridExtra"    "tidyr"        "writexl"      "purrr"        "factoextra"   "ggplot2"      "cluster"      "dplyr"       
[21] "readxl"       "stats"        "graphics"     "grDevices"    "utils"        "datasets"     "methods"      "base"        

Avisos:
  1: package ‘xgboost’ was built under R version 4.5.2 
2: package ‘doParallel’ was built under R version 4.5.2 
> 
  > # Paralelizar caret si está disponible
  > cl <- NULL
> if (run_heavy_models) {
  +   cores <- parallel::detectCores() - 1
  +   cores <- ifelse(cores > 0, cores, 1)
  +   cl <- makePSOCKcluster(cores)
  +   registerDoParallel(cl)
  + }
> 
  > set.seed(seed)
> 
  > # ---------------------------
> # 2) Carga y transformaciones previas (incluye campos nuevos)
  > # ---------------------------
> file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
> datos_raw <- readxl::read_excel(file_path)
> 
  > # Mantener una copia original
  > datos <- datos_raw
> 
  > # Limpiar columnas claves y crear variables de complejidad (las mismas que discutimos)
  > # Manejo robusto de nombres con espacios usando backticks cuando corresponda
  > # Normalizar textos básicos
  > safe_trim <- function(x) {
    +   x <- as.character(x)
    +   x[is.na(x)] <- ""
    +   trimws(x)
    + }
> 
  > # Normalizaciones y variables nuevas
  > datos <- datos %>%
  +   mutate(
    +     `Días_cobertura` = as.numeric(`Días cobertura con capacitación`),
    +     Grupo = as.factor(Grupo),
    +     Escolaridad = ifelse(is.na(Escolaridad), "", as.character(Escolaridad)),
    +     Especializacion = ifelse(is.na(Especialización), "", as.character(Especialización)),
    +     Software_Avanzado = ifelse(is.na(`Software-Avanzado`), "", as.character(`Software-Avanzado`)),
    +     Software_Intermedio = ifelse(is.na(`Software-Intermedio`), "", as.character(`Software-Intermedio`)),
    +     Software_Basico = ifelse(is.na(`Software-Básico`), "", as.character(`Software-Básico`))
    +   )
> 
  > # Funciones auxiliares de transformación (reutilizables)
  > #  - Homologación básica de Escolaridad
  > datos <- datos %>%
  +   mutate(
    +     Escolaridad_std = str_to_lower(Escolaridad),
    +     Nivel_Escolaridad = case_when(
      +       str_detect(Escolaridad_std, "ingenier|licenciatura|lic\\.|licenciatura") ~ "Superior",
      +       str_detect(Escolaridad_std, "tsu|technic|técnic|técnica|t.s.u") ~ "Técnica",
      +       str_detect(Escolaridad_std, "preparatoria|bachiller") ~ "Media",
      +       TRUE ~ "Otro"
      +     )
    +   )
> 
  > # Macro-categorizar Especialización
  > datos <- datos %>%
  +   mutate(
    +     Especializacion_std = str_to_lower(Especializacion),
    +     Macro_Especializacion = case_when(
      +       str_detect(Especializacion_std, "informática|sistemas|ti|tecnolog") ~ "TI",
      +       str_detect(Especializacion_std, "derech") ~ "Legal",
      +       str_detect(Especializacion_std, "contadur|finanz|econom") ~ "Financiero",
      +       str_detect(Especializacion_std, "administ|mercadotec") ~ "Administrativo",
      +       str_detect(Especializacion_std, "ingenier") ~ "Ingeniería",
      +       TRUE ~ "Otro"
      +     )
    +   )
> 
  > # Convertir software lists to counts
  > count_tools <- function(x) {
    +   x <- ifelse(is.na(x) | trimws(x) == "", NA_character_, as.character(x))
    +   sapply(x, function(z) {
      +     if (is.na(z) || z == "") return(0)
      +     # dividir por comas (también considerar ; y /)
        +     parts <- unlist(str_split(z, ",|;|/|\\+| y |\\|"))
        +     parts <- trimws(parts)
        +     parts <- parts[parts != ""]
        +     length(unique(parts))
        +   })
    + }
> 
  > datos$N_Software_Avanzado <- count_tools(datos$Software_Avanzado)
> datos$N_Software_Intermedio <- count_tools(datos$Software_Intermedio)
> datos$N_Software_Basico <- count_tools(datos$Software_Basico)
> datos$Total_Software <- rowSums(cbind(datos$N_Software_Avanzado, datos$N_Software_Intermedio, datos$N_Software_Basico), na.rm = TRUE)
> 
  > # Perfil TI e índice simple de complejidad
  > datos <- datos %>%
  +   mutate(
    +     Perfil_TI = ifelse(Macro_Especializacion == "TI" | N_Software_Avanzado > 0, "TI", "No_TI"),
    +     # Complejidad_Nivel por umbrales ajustables
      +     Complejidad_Nivel = case_when(
        +       Total_Software >= 8 | N_Software_Avanzado >= 3 ~ "Crítica",
        +       Total_Software >= 3 | (Perfil_TI == "TI" & Total_Software >= 1) ~ "Especializada",
        +       TRUE ~ "Estándar"
        +     ),
    +     Indice_Complejidad = Total_Software + ifelse(Perfil_TI == "TI", 2, 0) + ifelse(Macro_Especializacion %in% c("TI","Ingeniería"), 1, 0)
    +   )
> 
  > # ------------------------------------------------------------
> # Variables base (originales) — puedes modificar la lista aquí
  > # ------------------------------------------------------------
> # Usamos las columnas más representativas del script original
  > base_vars_original <- c("DescripcionCC", "Perfil Profesional", "Segmento de puesto",
                            +                         "Tabulador Salarial", "Area de Personal", "Puesto Generico",
                            +                         "Familia de Puesto", "Regional", "Plaza", "Estado",
                            +                         "Nombre Reclutador")
> 
  > # Asegurar que existan en dataset; mantener solo los que sí están
  > base_vars_original <- intersect(base_vars_original, colnames(datos))
> 
  > # Variables nuevas que añadimos
  > vars_nuevas <- c("Nivel_Escolaridad", "Macro_Especializacion", "N_Software_Avanzado",
                     +                  "N_Software_Intermedio", "N_Software_Basico", "Total_Software",
                     +                  "Perfil_TI", "Complejidad_Nivel", "Indice_Complejidad")
> 
  > vars_nuevas <- intersect(vars_nuevas, colnames(datos))
> 
  > # ---------------------------
> # 3) Funciones de preprocesado para modelado
  > #    - Encoding: frecuencia para high-card, one-hot para low-card
  > # ---------------------------
> make_feature_matrix <- function(df, cat_vars, num_vars, topN = 30) {
  +   # df: data.frame
    +   # cat_vars: character vector categorical colnames
    +   # num_vars: numeric colnames to keep
    +   mats <- list()
    +   for (v in cat_vars) {
      +     vals <- df[[v]]
      +     vals[is.na(vals)] <- "NA_MISSING"
      +     vals <- as.character(vals)
      +     nlevels <- length(unique(vals))
      +     if (nlevels > topN) {
        +       # frequency encoding
          +       tab <- sort(table(vals), decreasing = TRUE)
          +       freq <- as.numeric(tab) / length(vals)
          +       freq_map <- as.numeric(match(vals, names(tab)))
          +       mats[[v]] <- data.frame(!!paste0(v, "_freq") := as.numeric(tab[match(vals, names(tab))]) / length(vals))
          +       # If names don't match, NA -> 0
            +       names(mats[[v]]) <- paste0(v, "_freq")
            +     } else {
              +       # one-hot via model.matrix
                +       form <- as.formula(paste0("~ 0 + factor(vals)"))
                +       mm <- as.data.frame(model.matrix(form, data = data.frame(vals = vals)))
                +       # rename columns
                  +       colnames(mm) <- paste0(v, "__", make.names(colnames(mm)))
                  +       mats[[v]] <- mm
                  +     }
      +   }
    +   # numeric vars
      +   num_df <- df[, num_vars, drop = FALSE]
      +   # combine
        +   res <- cbind(num_df, do.call(cbind, mats))
        +   # remove columns with zero variance
          +   nzv <- sapply(res, function(x) length(unique(x[!is.na(x)])) > 1)
          +   res <- res[, nzv, drop = FALSE]
          +   res
          + }
> 
  > # ---------------------------
> # 4) Model training helper (caret wrappers)
  > # ---------------------------
> train_models_cv <- function(X, y, train_control, methods = c("lm","rpart","rf","xgbTree","nnet")) {
  +   # X: data.frame numeric features
    +   # y: numeric vector
    +   # returns: list of trained models and resamples summary
    +   results <- list()
    +   # caret needs data frame
      +   dat_all <- cbind(y = y, X)
      +   for (m in methods) {
        +     tryCatch({
          +       ctrl <- train_control
          +       # For nnet we scale inputs automatically via preProc
            +       if (m == "nnet") {
              +         mod <- caret::train(y ~ ., data = dat_all, method = m,
                                            +                             trControl = ctrl, preProcess = c("center","scale"), trace = FALSE, linout = TRUE)
              +       } else {
                +         mod <- caret::train(y ~ ., data = dat_all, method = m,
                                              +                             trControl = ctrl)
                +       }
          +       results[[m]] <- mod
          +       cat("  Modelo entrenado:", m, "  RMSE(CV):", round(min(mod$results$RMSE, na.rm = TRUE),3),
                      +           " R2(CV):", round(max(mod$results$Rsquared, na.rm = TRUE),3), "\n")
          +     }, error = function(e) {
            +       cat("  Error entrenando", m, ":", e$message, "\n")
            +     })
        +   }
      +   results
      + }
> 
  > # ---------------------------
> # 5) Bootstrap stability of group goals for a model
  > # ---------------------------
> bootstrap_goal_stability_model <- function(df, feature_matrix_fn, model_method = "xgbTree",
                                             +                                            cat_vars, num_vars, group_var, R = 200) {
  +   # Returns proportion of bootstrap iterations that produce same vector of group goals (string compare)
    +   goals_list <- vector("character", R)
    +   for (i in seq_len(R)) {
      +     set.seed(seed + i)
      +     samp_idx <- sample(seq_len(nrow(df)), replace = TRUE)
      +     df_s <- df[samp_idx, , drop = FALSE]
      +     # construct features
        +     Xs <- feature_matrix_fn(df_s, cat_vars, num_vars)
        +     ys <- df_s$Días_cobertura
        +     # minimal model training quick: use xgboost if exists, else lm
          +     if (nrow(Xs) < 20 || ncol(Xs) < 1) {
            +       # fallback: group medians by group_var
              +       g <- df_s %>% group_by_at(group_var) %>% summarise(goal = round(median(Días_cobertura, na.rm = TRUE)), .groups = "drop")
              +       goals_list[i] <- paste0(paste(g[[group_var]], g$goal, sep = ":"), collapse = "|")
              +     } else {
                +       # train small xgboost via caret with fast control
                  +       tr <- trainControl(method = "cv", number = 3)
                  +       # try xgbTree
                    +       tryCatch({
                      +         dat_all <- cbind(y = ys, Xs)
                      +         mod <- caret::train(y ~ ., data = dat_all, method = model_method, trControl = tr)
                      +         pred <- predict(mod, newdata = Xs)
                      +         g <- df_s %>% mutate(.pred = pred) %>%
                        +           group_by_at(group_var) %>%
                        +           summarise(goal = round(median(.pred, na.rm = TRUE)), .groups = "drop")
                      +         goals_list[i] <- paste0(paste(g[[group_var]], g$goal, sep = ":"), collapse = "|")
                      +       }, error = function(e) {
                        +         g <- df_s %>% group_by_at(group_var) %>% summarise(goal = round(median(Días_cobertura, na.rm = TRUE)), .groups = "drop")
                        +         goals_list[i] <- paste0(paste(g[[group_var]], g$goal, sep = ":"), collapse = "|")
                        +       })
                  +     }
        +   }
    +   tab <- table(goals_list)
    +   max(tab) / R
    + }
> 
  > # ---------------------------
> # 6) Method pipeline per Group and per Approach
  > # ---------------------------
> analyze_group_approach <- function(df_group, approach_name, feature_cat_vars, feature_num_vars,
                                     +                                    base_group_var = "Puesto Generico") {
  +   cat("\n--------------------------------------------------\n")
  +   cat("ANALIZANDO GRUPO:", unique(df_group$Grupo), " - APROXIMACIÓN:", approach_name, "\n")
  +   cat("Registros:", nrow(df_group), "\n")
  +   cat("--------------------------------------------------\n")
  + 
    +   # Basic checks
    +   if (nrow(df_group) < 10) {
      +     cat("  Pocos registros; se omite modelado.\n")
      +     return(NULL)
      +   }
  + 
    +   # Build feature matrix
    +   X <- make_feature_matrix(df_group, cat_vars = feature_cat_vars, num_vars = feature_num_vars, topN = 40)
    +   y <- df_group$Días_cobertura
    + 
      +   # Split safe: caret will do CV; but keep a holdout for final evaluation optionally
      +   tr_control <- trainControl(method = "repeatedcv", number = CV_folds, repeats = CV_repeats, allowParallel = TRUE)
      + 
        +   # If heavy models allowed -> train
        +   trained_models <- list()
        +   if (run_heavy_models && nrow(X) >= 20 && ncol(X) >= 1) {
          +     cat("Entrenando modelos (puede tardar)...\n")
          +     methods <- c("lm","rpart","rf","xgbTree","nnet")
          +     trained_models <- train_models_cv(X, y, tr_control, methods = methods)
          +   } else {
            +     cat("Modo rápido: no se entrenan modelos pesados.\n")
            +   }
        + 
          +   # Clustering approach: kmeans on scaled features + mediana per cluster
          +   km_res <- NULL
          +   if (ncol(X) >= 1 && nrow(X) > 5) {
            +     # scale numeric matrix
              +     Xs <- scale(X)
              +     kmax <- min(8, nrow(Xs)-1)
              +     k_try <- ifelse(kmax >= 2, 2:kmax, 1)
              +     wss <- sapply(k_try, function(k) sum(kmeans(Xs, centers = k, nstart = 10)$withinss))
              +     # choose elbow approx: choose k with max relative drop
                +     if (length(wss) >= 2) {
                  +       drops <- -diff(wss)
                  +       rel <- drops / wss[-length(wss)]
                  +       k_opt <- k_try[which.max(rel)]+0
                  +       k_opt <- max(2, min(k_opt, 6))
                  +     } else k_opt <- 1
                  +     if (k_opt >= 2) {
                    +       km <- kmeans(Xs, centers = k_opt, nstart = 25)
                    +       df_group$cluster_km <- factor(km$cluster)
                    +       km_res <- df_group %>% group_by(cluster_km) %>%
                      +         summarise(n = n(), mediana = median(Días_cobertura, na.rm = TRUE), .groups = "drop")
                    +       cat(" Clustering KMeans k_opt:", k_opt, " clusters encontrados:", nrow(km_res), "\n")
                    +     } else {
                      +       cat(" Clustering no aplicable (pocos datos/columnas)\n")
                      +     }
                  +   }
          + 
            +   # Percentiles baseline
            +   baseline <- df_group %>% summarise(
              +     mediana = round(median(Días_cobertura, na.rm = TRUE)),
              +     p60 = round(quantile(Días_cobertura, 0.6, na.rm = TRUE)),
              +     p75 = round(quantile(Días_cobertura, 0.75, na.rm = TRUE)),
              +     p90 = round(quantile(Días_cobertura, 0.90, na.rm = TRUE))
              +   )
            + 
              +   cat(" Baseline percentiles (mediana/p60/p75/p90):", paste(baseline, collapse = " / "), "\n")
            + 
              +   # Bootstrap stability for top model candidate (xgbTree if exists else rf else lm)
              +   model_choice_for_stability <- NULL
              +   if ("xgbTree" %in% names(trained_models)) model_choice_for_stability <- "xgbTree"
              +   else if ("rf" %in% names(trained_models)) model_choice_for_stability <- "rf"
              +   else if ("lm" %in% names(trained_models)) model_choice_for_stability <- "lm"
              + 
                  +   stability_score <- NA
                  +   if (!is.null(model_choice_for_stability)) {
                    +     cat("Calculando estabilidad bootstrap (R=", bootstrap_iter, ") con modelo:", model_choice_for_stability, "...\n")
                    +     stability_score <- bootstrap_goal_stability_model(df_group, make_feature_matrix,
                                                                            +                                                      model_method = model_choice_for_stability,
                                                                            +                                                      cat_vars = feature_cat_vars, num_vars = feature_num_vars,
                                                                            +                                                      group_var = base_group_var, R = bootstrap_iter)
                    +     cat(" Estabilidad (proporción repeticiones idénticas goals):", round(stability_score,3), "\n")
                    +   } else {
                      +     cat("No hay modelo entrenado para bootstrap de estabilidad. Se sugiere usar percentiles.\n")
                      +   }
                  + 
                    +   # Guardar resultados resumidos
                    +   out <- list(
                      +     approach = approach_name,
                      +     baseline = baseline,
                      +     clustering = km_res,
                      +     models = trained_models,
                      +     stability = stability_score
                      +   )
                    +   return(out)
                    + }
> 
  > # ---------------------------
> # 7) Pipeline: por Grupo ejecutar 2 aproximaciones
  > # ---------------------------
> grupos <- unique(datos$Grupo)
> summary_results <- list()
> 
  > for (g in grupos) {
    +   cat("\n###############################################\n")
    +   cat("PROCESANDO GRUPO:", g, "\n")
    +   cat("###############################################\n")
    +   df_g <- datos %>% filter(Grupo == g & !is.na(Días_cobertura))
    +   if (nrow(df_g) < min_registros_grupo) {
      +     cat("  Registros < min_registros_grupo (", nrow(df_g), ") - Se harán percentiles y clustering básicos.\n")
      +   }
    + 
      +   # Define feature sets
      +   # Aproximación A: variables originales (categorical base_vars_original) + numeric basics (none)
      +   cat("\n-> APROXIMACIÓN A: VARIABLES ORIGINALES\n")
    +   cat("   Usando variables:", paste(base_vars_original, collapse = ", "), "\n")
    +   cat_vars_A <- base_vars_original
    +   num_vars_A <- c()  # no numeric predictors other than possibly engineered ones
    + 
      +   resA <- analyze_group_approach(df_g, "Original", feature_cat_vars = cat_vars_A, feature_num_vars = num_vars_A,
                                         +                                  base_group_var = ifelse("Puesto Generico" %in% colnames(df_g), "Puesto Generico", colnames(df_g)[1]))
      +   summary_results[[paste0(g, "_A")]] <- resA
      +   # Guardar breve resumen en excel
        +   tryCatch({
          +     write_xlsx(list(baseline = as.data.frame(t(resA$baseline)), clustering = ifelse(is.null(resA$clustering), data.frame(), resA$clustering)),
                           +                file.path(output_dir, paste0("Resumen_", g, "_Original.xlsx")))
          +   }, error = function(e) {})
      + 
        +   # Aproximación B: originales + nuevas variables
        +   cat("\n-> APROXIMACIÓN B: ORIGINALES + CAMPOS NUEVOS\n")
      +   cat("   Nuevas variables añadidas:", paste(vars_nuevas, collapse = ", "), "\n")
      +   cat_vars_B <- unique(c(base_vars_original, "Nivel_Escolaridad", "Macro_Especializacion", "Perfil_TI", "Complejidad_Nivel"))
      +   num_vars_B <- c("Total_Software", "Indice_Complejidad")  # numeric complexity measures
      + 
        +   resB <- analyze_group_approach(df_g, "Original+Nuevas", feature_cat_vars = cat_vars_B, feature_num_vars = num_vars_B,
                                           +                                  base_group_var = ifelse("Puesto Generico" %in% colnames(df_g), "Puesto Generico", colnames(df_g)[1]))
        +   summary_results[[paste0(g, "_B")]] <- resB
        +   tryCatch({
          +     write_xlsx(list(baseline = as.data.frame(t(resB$baseline)), clustering = ifelse(is.null(resB$clustering), data.frame(), resB$clustering)),
                           +                file.path(output_dir, paste0("Resumen_", g, "_Original_Nuevas.xlsx")))
          +   }, error = function(e) {})
        + 
          +   # -------------------------
        +   # Modelo por CAPAS (dinámico)
          +   # -------------------------
        +   # Base goal por macro-especializacion (o por agrupador principal)
          +   cat("\n-> MODELO POR CAPAS (BASE + INCREMENTOS)\n")
        +   # Elegimos Macro_Especializacion como capa base si existe, sino Perfil Profesional
          +   base_by <- ifelse("Macro_Especializacion" %in% colnames(df_g), "Macro_Especializacion",
                                +                     ifelse("Perfil Profesional" %in% colnames(df_g), "Perfil Profesional", "Puesto Generico"))
          +   df_base <- df_g %>% filter(!is.na(.data[[base_by]]))
          +   base_goals <- df_base %>% group_by_at(base_by) %>% summarise(base_med = round(median(Días_cobertura, na.rm = TRUE)), n = n(), .groups = "drop")
          + 
            +   # Incrementos por Complejidad_Nivel
            +   inc_tbl <- df_g %>% group_by(Complejidad_Nivel) %>% summarise(med = median(Días_cobertura, na.rm = TRUE), n = n(), .groups = "drop")
            +   # Normalize increments relative to 'Estándar'
              +   if (!"Estándar" %in% inc_tbl$Complejidad_Nivel) {
                +     # if not present, pick min as baseline
                  +     baseline_med <- min(inc_tbl$med, na.rm = TRUE)
                  +   } else baseline_med <- inc_tbl$med[inc_tbl$Complejidad_Nivel == "Estándar"]
                  + 
                      +   inc_tbl <- inc_tbl %>% mutate(increment = round(med - baseline_med))
                      + 
                        +   cat(" Base por", base_by, " (muestra):\n")
                      +   print(head(base_goals, 10))
                      +   cat(" Incrementos por Complejidad_Nivel:\n")
                      +   print(inc_tbl)
                      + 
                        +   # Guardar tabla de capas
                        +   tryCatch({
                          +     write_xlsx(list(base_goals = base_goals, inc_tbl = inc_tbl),
                                           +                file.path(output_dir, paste0("Capas_", g, ".xlsx")))
                          +   }, error = function(e) {})
                      + 
                        +   cat("\n-> FIN GRUPO:", g, "\n\n")
                      + }

###############################################
PROCESANDO GRUPO: COBRANZA 
###############################################

-> APROXIMACIÓN A: VARIABLES ORIGINALES
Usando variables: DescripcionCC, Perfil Profesional, Segmento de puesto, Tabulador Salarial, Area de Personal, Puesto Generico, Familia de Puesto, Regional, Plaza, Estado, Nombre Reclutador 

--------------------------------------------------
  ANALIZANDO GRUPO: 1  - APROXIMACIÓN: Original 
Registros: 670 
--------------------------------------------------
  Error en `:=`(!!paste0(v, "_freq"), as.numeric(tab[match(vals, names(tab))])/length(vals)): 
  no se pudo encontrar la función ":="
Called from: data.frame(`:=`(!!paste0(v, "_freq"), as.numeric(tab[match(vals, 
                                                                        names(tab))])/length(vals)))
