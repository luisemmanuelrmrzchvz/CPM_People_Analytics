# =========================================================
# SCRIPT AVANZADO: Estimación robusta de GOALS (original + nuevas)
# - Preserva la lógica y funciones de tu script original
# - Añade limpieza de campos nuevos y modelado avanzado
# - Ejecuta 2 aproximaciones (A: original, B: original + nuevas)
# - Modelos: lm, rpart, randomForest, xgboost, nnet/keras (si disponible)
# - Bootstrap de estabilidad de goals
# - Modelo por CAPAS (base + incrementos por complejidad)
# - Outputs: .xlsx y gráficos en carpetas por grupo
# =========================================================

# ---------------------------
# 0. Parámetros ajustables
# ---------------------------
output_dir <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura/Estimacion_Robusta"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

CV_folds <- 5
CV_repeats <- 2
bootstrap_iter <- 200     # ajusta según capacidad (200-1000)
seed <- 123
min_groups_for_models <- 30  # mínimo por grupo para modelos complejos
run_heavy_models <- TRUE     # si FALSE solo hace percentiles y clustering

# ---------------------------
# 1. Paquetes requeridos
# ---------------------------
required_pkgs <- c(
  "readxl","writexl","dplyr","stringr","tidyr","purrr","ggplot2","gridExtra",
  "cluster","factoextra","caret","randomForest","xgboost","rpart","nnet",
  "doParallel","stats"
)
missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) {
  cat("Faltan paquetes:", paste(missing_pkgs, collapse = ", "), "\n")
  cat("Instala los paquetes faltantes antes de ejecutar este script. Ejemplo:\n")
  cat("install.packages(c('", paste(missing_pkgs, collapse = "','"), "'))\n", sep = "")
  stop("Instala paquetes faltantes y vuelve a ejecutar.")
}
lapply(required_pkgs, library, character.only = TRUE)

# Keras opcional: detecta
use_keras <- requireNamespace("keras", quietly = TRUE)
if (use_keras) cat("Keras disponible: se podrá usar red neuronal profunda.\n") else cat("Keras no encontrado: se usará nnet si se solicita neural nets.\n")

# Configurar paralelo si se entrenan modelos pesados
if (run_heavy_models) {
  cores <- max(1, parallel::detectCores() - 1)
  cl <- makePSOCKcluster(cores)
  registerDoParallel(cl)
  cat("Paralelización activada con", cores, "nucleos.\n")
}

set.seed(seed)

# ---------------------------
# 2. Cargar datos y validaciones
# ---------------------------
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
cat("Cargando datos desde:", file_path, "\n")
datos_raw <- readxl::read_excel(file_path)

if (!"Grupo" %in% colnames(datos_raw)) stop("Error: la columna 'Grupo' no existe.")
cat("Columnas cargadas:", paste(colnames(datos_raw), collapse = ", "), "\n")

# Guardar copia
datos <- datos_raw

# ---------------------------
# 3. Limpieza y homologación de campos nuevos
# ---------------------------

# Normalizadores seguros
safe_char <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

# Perfilado rápido y guardado para revisión
perfil_field <- function(df, field) {
  v <- safe_char(df[[field]])
  total <- length(v)
  na_cnt <- sum(v == "" | is.na(v))
  uniques <- sort(table(v[v != "" & !is.na(v)]), decreasing = TRUE)
  list(total = total, na = na_cnt, unique_count = length(uniques), top = head(uniques, 20))
}

campos_nuevos <- c("Escolaridad", "Especialización", "Software-Avanzado", "Software-Intermedio", "Software-Básico")
campos_existentes <- intersect(campos_nuevos, colnames(datos))
profiling_list <- map(campos_existentes, ~ perfil_field(datos, .x))
names(profiling_list) <- campos_existentes
# Guardar profiling
write_xlsx(profiling_list, file.path(output_dir, "profiling_campos_nuevos.xlsx"))
cat("Perfilado de campos nuevos guardado en profiling_campos_nuevos.xlsx\n")

# ---- Limpiar y convertir campos nuevos
datos <- datos %>%
  mutate(
    Escolaridad = safe_char(Escolaridad),
    Escolaridad_std = str_to_lower(Escolaridad),
    Especializacion = safe_char(Especialización),
    Especializacion_std = str_to_lower(Especialización),
    Software_Avanzado = safe_char(`Software-Avanzado`),
    Software_Intermedio = safe_char(`Software-Intermedio`),
    Software_Basico = safe_char(`Software-Básico`)
  )

# Homologar Escolaridad a niveles
datos <- datos %>%
  mutate(
    Nivel_Escolaridad = case_when(
      str_detect(Escolaridad_std, "ingenier|licenciatura|lic\\.|licenciatura") ~ "Superior",
      str_detect(Escolaridad_std, "tsu|t\\.s\\.u|técnic|tecnica|tecnico") ~ "Técnica",
      str_detect(Escolaridad_std, "preparatoria|bachiller") ~ "Media",
      Escolaridad_std == "" ~ NA_character_,
      TRUE ~ "Otro"
    )
  )

# Macro-categorizar especialización (reglas base; puedes ampliar)
datos <- datos %>%
  mutate(
    Macro_Especializacion = case_when(
      str_detect(Especializacion_std, "informá|informat|sistemas|ti|tecnolog") ~ "TI",
      str_detect(Especializacion_std, "contadur|finanz|econom") ~ "Financiero",
      str_detect(Especializacion_std, "administra|mercadotec") ~ "Administrativo",
      str_detect(Especializacion_std, "derech") ~ "Legal",
      str_detect(Especializacion_std, "ingenier") ~ "Ingeniería",
      Especializacion_std == "" ~ NA_character_,
      TRUE ~ "Otro"
    )
  )

# Conteo normalizado de herramientas en campos Software-*
count_tools <- function(x) {
  x <- safe_char(x)
  sapply(x, function(z) {
    if (z == "" || is.na(z)) return(0)
    parts <- unlist(str_split(z, ",|;|/|\\+|\\|| y | Y |;|:"))
    parts <- trimws(parts)
    parts <- parts[parts != ""]
    length(unique(parts))
  })
}
datos <- datos %>%
  mutate(
    N_Software_Avanzado = count_tools(Software_Avanzado),
    N_Software_Intermedio = count_tools(Software_Intermedio),
    N_Software_Basico = count_tools(Software_Basico),
    Total_Software = N_Software_Avanzado + N_Software_Intermedio + N_Software_Basico
  )

# Indicadores derivadas
datos <- datos %>%
  mutate(
    Perfil_TI = ifelse(Macro_Especializacion == "TI" | N_Software_Avanzado > 0, "TI", "No_TI"),
    Indice_Complejidad = Total_Software + ifelse(Perfil_TI == "TI", 2, 0) + ifelse(Macro_Especializacion %in% c("TI","Ingeniería"), 1, 0),
    Complejidad_Nivel = case_when(
      Total_Software >= 10 | N_Software_Avanzado >= 4 ~ "Crítica",
      Total_Software >= 4 | (Perfil_TI == "TI" & Total_Software >= 1) ~ "Especializada",
      TRUE ~ "Estándar"
    )
  )

# Guardar dataset limpio
write_xlsx(datos, file.path(output_dir, "datos_transformados_completos.xlsx"))
cat("Datos transformados guardados en datos_transformados_completos.xlsx\n")

# ---------------------------
# 4. Funciones base del script original (adaptadas y robustas)
# ---------------------------

# Evaluar agrupador (versión robusta)
evaluar_agrupador_mejorado <- function(columna, datos_subset, min_n = 5) {
  datos_temp <- datos_subset
  # evitar columnas no existentes
  if (!columna %in% colnames(datos_temp)) {
    return(data.frame(agrupador = columna, grupos_n = 0, eta_squared = NA, f_statistic = NA, p_value = NA,
                      cv_promedio = NA, heterogeneidad = NA, rango_medias = NA,
                      mediana_global = median(datos_temp$`Días cobertura con capacitación`, na.rm = TRUE),
                      min_grupo = NA, max_grupo = NA))
  }
  analisis <- datos_temp %>%
    group_by_at(columna) %>%
    summarise(
      n = n(),
      media = mean(`Días cobertura con capacitación`, na.rm = TRUE),
      mediana = median(`Días cobertura con capacitación`, na.rm = TRUE),
      sd = sd(`Días cobertura con capacitación`, na.rm = TRUE),
      cv = ifelse(media > 0, sd / media, NA_real_),
      .groups = "drop"
    ) %>%
    filter(n >= min_n) %>%
    filter(!is.na(cv))

  if (nrow(analisis) < 2) {
    return(data.frame(agrupador = columna, grupos_n = nrow(analisis), eta_squared = NA, f_statistic = NA, p_value = NA,
                      cv_promedio = NA, heterogeneidad = NA, rango_medias = NA,
                      mediana_global = median(datos_temp$`Días cobertura con capacitación`, na.rm = TRUE),
                      min_grupo = ifelse(nrow(analisis) > 0, min(analisis$n), NA),
                      max_grupo = ifelse(nrow(analisis) > 0, max(analisis$n), NA)))
  }

  categorias_validas <- analisis[[columna]]
  datos_anova <- datos_temp %>% filter(.data[[columna]] %in% categorias_validas)

  if (length(unique(datos_anova[[columna]])) > 1) {
    formula_text <- paste0("`Días cobertura con capacitación` ~ `", columna, "`")
    formula_anova <- as.formula(formula_text)
    modelo <- tryCatch(aov(formula_anova, data = datos_anova), error = function(e) NULL)
    if (!is.null(modelo)) {
      resumen_anova <- summary(modelo)
      ss_total <- sum((datos_anova$`Días cobertura con capacitación` - mean(datos_anova$`Días cobertura con capacitación`, na.rm = TRUE))^2, na.rm = TRUE)
      ss_between <- sum(analisis$n * (analisis$media - mean(datos_anova$`Días cobertura con capacitación`, na.rm = TRUE))^2)
      eta_sq <- ifelse(ss_total > 0, ss_between / ss_total, 0)
      f_stat <- tryCatch(resumen_anova[[1]]$`F value`[1], error = function(e) NA_real_)
      p_valor <- tryCatch(resumen_anova[[1]]$`Pr(>F)`[1], error = function(e) NA_real_)
    } else {
      eta_sq <- NA_real_; f_stat <- NA_real_; p_valor <- NA_real_
    }
  } else {
    eta_sq <- NA_real_; f_stat <- NA_real_; p_valor <- NA_real_
  }

  data.frame(
    agrupador = columna,
    grupos_n = nrow(analisis),
    eta_squared = eta_sq,
    f_statistic = f_stat,
    p_value = p_valor,
    cv_promedio = mean(analisis$cv, na.rm = TRUE),
    heterogeneidad = sd(analisis$media, na.rm = TRUE),
    rango_medias = max(analisis$media, na.rm = TRUE) - min(analisis$media, na.rm = TRUE),
    mediana_global = median(datos_temp$`Días cobertura con capacitación`, na.rm = TRUE),
    min_grupo = min(analisis$n),
    max_grupo = max(analisis$n)
  )
}

# Evaluar viabilidad (igual que tu versión, sin cambios conceptuales)
evaluar_viabilidad_agrupador <- function(resultados_grupo) {
  mejor <- resultados_grupo[1, ]
  cat("\n=== EVALUACIÓN DE VIABILIDAD DEL AGRUPADOR ===\n")
  cat("\n1. PODER EXPLICATIVO (eta-squared):", round(mejor$eta_squared, 4), "\n")
  if (is.na(mejor$eta_squared) || mejor$eta_squared < 0.01) cat("  ❌ CRÍTICO: Explica menos del 1% de la variación\n")
  else if (mejor$eta_squared < 0.05) cat("  ⚠️ BAJO: Explica menos del 5% de la variación\n")
  else if (mejor$eta_squared < 0.15) cat("  ⚠️ MODERADO: Explica entre 5-15% de la variación\n")
  else cat("  ✅ ADECUADO: Explica más del 15% de la variación\n")

  cat("\n2. HOMOGENEIDAD (CV promedio):", round(mejor$cv_promedio, 3), "\n")
  if (!is.na(mejor$cv_promedio)) {
    if (mejor$cv_promedio > 1.0) cat("  ❌ MUY ALTA VARIABILIDAD: CV > 1.0\n")
    else if (mejor$cv_promedio > 0.7) cat("  ⚠️ ALTA VARIABILIDAD: CV > 0.7\n")
    else if (mejor$cv_promedio > 0.5) cat("  ⚠️ MODERADA VARIABILIDAD: CV > 0.5\n")
    else cat("  ✅ BUENA HOMOGENEIDAD: CV < 0.5\n")
  }

  cat("\n3. PARSIMONIA (número de grupos):", mejor$grupos_n, "\n")
  cat("  (Tamaño mínimo grupo:", mejor$min_grupo, ", máximo:", mejor$max_grupo, ")\n")
  if (mejor$grupos_n < 3) cat("  ⚠️ MUY POCOS GRUPOS: Puede estar sobresimplificando\n")
  else if (mejor$grupos_n > 50) cat("  ⚠️ MUCHOS GRUPOS: Dificulta la implementación\n")
  else if (mejor$grupos_n > 20) cat("  ⚠️ GRANULARIDAD ALTA: Muchos grupos para gestionar\n")
  else cat("  ✅ NÚMERO ADECUADO DE GRUPOS\n")

  cat("\n4. DIFERENCIACIÓN (rango de medias):", round(mejor$rango_medias, 1), "días\n")
  if (mejor$rango_medias < 10) cat("  ⚠️ POCA DIFERENCIACIÓN: Rangos pequeños entre grupos\n")
  else if (mejor$rango_medias > 50) cat("  ✅ ALTA DIFERENCIACIÓN: Rangos significativos entre grupos\n")
  else cat("  ✅ DIFERENCIACIÓN MODERADA\n")

  score_viabilidad <- (ifelse(is.na(mejor$eta_squared), 0, mejor$eta_squared) * 0.4) +
    ((1 - ifelse(is.na(mejor$cv_promedio), 1, mejor$cv_promedio)) * 0.3) +
    (ifelse(mejor$grupos_n >= 5 & mejor$grupos_n <= 30, 0.2, 0.1)) +
    (pmin(ifelse(is.na(mejor$rango_medias), 0, mejor$rango_medias) / 100, 0.1))

  cat("\n5. SCORE DE VIABILIDAD GLOBAL:", round(score_viabilidad, 3), "/ 1.0\n")
  if (score_viabilidad < 0.3) cat("  ❌ NO VIABLE - Considerar cambio de agrupador\n")
  else if (score_viabilidad < 0.5) cat("  ⚠️ LIMITADAMENTE VIABLE - Necesita mejoras\n")
  else if (score_viabilidad < 0.7) cat("  ⚠️ MODERADAMENTE VIABLE - Aceptable con monitoreo\n")
  else cat("  ✅ ALTAMENTE VIABLE - Excelente agrupador\n")

  recomendaciones <- c()
  if (is.na(mejor$eta_squared) || mejor$eta_squared < 0.05) recomendaciones <- c(recomendaciones, "• CONSIDERAR CAMBIAR DE AGRUPADOR (poder explicativo muy bajo)")
  if (!is.na(mejor$cv_promedio) && mejor$cv_promedio > 0.7) recomendaciones <- c(recomendaciones, "• CONSIDERAR SUB-SEGMENTAR LOS GRUPOS (alta variabilidad interna)")
  if (mejor$grupos_n < 5) recomendaciones <- c(recomendaciones, "• EVALUAR AGRUPADORES MÁS GRANULARES (pocos grupos)")
  if (mejor$grupos_n > 30) recomendaciones <- c(recomendaciones, "• EVALUAR AGRUPADORES MÁS AGRUPADOS (demasiados grupos)")
  if (mejor$rango_medias < 10) recomendaciones <- c(recomendaciones, "• EVALUAR SI VALE LA PENA TENER GRUPOS DIFERENTES (poca diferenciación)")

  cat("\n6. RECOMENDACIÓN ESPECÍFICA:\n")
  if (length(recomendaciones) == 0) cat("  • MANTENER ESTE AGRUPADOR - Buen balance en todas las métricas\n")
  else for (rec in recomendaciones) cat(" ", rec, "\n")

  # alternativas con mejor eta_squared (si existen)
  if (nrow(resultados_grupo) > 1) {
    alternativas <- resultados_grupo %>%
      filter(!is.na(eta_squared)) %>%
      filter(eta_squared > (mejor$eta_squared * 1.2)) %>%
      filter(grupos_n < (mejor$grupos_n * 1.5)) %>%
      arrange(desc(eta_squared)) %>%
      head(3)
    if (nrow(alternativas) > 0) {
      cat("\n7. OPCIONES ALTERNATIVAS CON MEJOR PODER EXPLICATIVO:\n")
      for (i in 1:nrow(alternativas)) {
        alt <- alternativas[i, ]
        cat(sprintf("  %d. %s (η²=%.3f, %d grupos, CV=%.2f)\n", i, alt$agrupador, alt$eta_squared, alt$grupos_n, alt$cv_promedio))
      }
    }
  }

  return(list(score_viabilidad = score_viabilidad, mejor = mejor))
}

# Función de goal (mediana). Se podrá ampliar con reglas robustas.
calcular_goal <- function(x) {
  round(median(x, na.rm = TRUE))
}

# ---------------------------
# 5. Feature engineering para modelos (encoding seguro)
# ---------------------------
make_feature_matrix <- function(df, cat_vars = c(), num_vars = c(), topN = 40) {
  mats <- list()
  for (v in cat_vars) {
    if (!v %in% colnames(df)) next
    vals <- as.character(df[[v]])
    vals[is.na(vals)] <- "NA_MISSING"
    nlevels <- length(unique(vals))
    if (nlevels > topN) {
      tab <- sort(table(vals), decreasing = TRUE)
      freq_col <- as.numeric(tab[match(vals, names(tab))]) / length(vals)
      freq_col[is.na(freq_col)] <- 0
      mats[[v]] <- data.frame(freq_col)
      names(mats[[v]]) <- paste0(v, "_freq")
    } else {
      mm <- model.matrix(~ 0 + factor(vals))
      mm <- as.data.frame(mm)
      colnames(mm) <- paste0(v, "__", make.names(colnames(mm)))
      mats[[v]] <- mm
    }
  }
  num_df <- df[, intersect(num_vars, colnames(df)), drop = FALSE]
  res <- cbind(num_df, do.call(cbind, mats))
  # remove zero variance
  if (ncol(res) == 0) return(data.frame())
  nzv <- sapply(res, function(x) length(unique(x[!is.na(x)])) > 1)
  res <- res[, nzv, drop = FALSE]
  return(res)
}

# ---------------------------
# 6. Bootstrap de estabilidad de goals (para un pipeline/modelo)
# ---------------------------
bootstrap_goal_stability_model <- function(df, feature_cat_vars, feature_num_vars, model_method = "xgbTree",
                                           group_var = "Puesto Generico", R = 200) {
  set.seed(seed)
  goals_list <- vector("character", R)
  for (i in seq_len(R)) {
    samp_idx <- sample(seq_len(nrow(df)), replace = TRUE)
    df_s <- df[samp_idx, , drop = FALSE]
    Xs <- make_feature_matrix(df_s, cat_vars = feature_cat_vars, num_vars = feature_num_vars)
    if (nrow(Xs) < 10 || ncol(Xs) < 1) {
      g <- df_s %>% group_by_at(group_var) %>% summarise(goal = round(median(`Días cobertura con capacitación`, na.rm = TRUE)), .groups = "drop")
      goals_list[i] <- paste(paste(g[[group_var]], g$goal, sep = ":"), collapse = "|")
      next
    }
    ys <- df_s$`Días cobertura con capacitación`
    # use quick caret train with minimal tuning
    tr <- caret::trainControl(method = "cv", number = 3)
    dat_all <- tryCatch(cbind(y = ys, Xs), error = function(e) NULL)
    if (is.null(dat_all)) {
      g <- df_s %>% group_by_at(group_var) %>% summarise(goal = round(median(`Días cobertura con capacitación`, na.rm = TRUE)), .groups = "drop")
      goals_list[i] <- paste(paste(g[[group_var]], g$goal, sep = ":"), collapse = "|")
      next
    }
    model_used <- NULL
    try({
      mod <- caret::train(y ~ ., data = dat_all, method = model_method, trControl = tr)
      pred <- predict(mod, newdata = Xs)
      g <- df_s %>% mutate(.pred = pred) %>%
        group_by_at(group_var) %>%
        summarise(goal = round(median(.pred, na.rm = TRUE)), .groups = "drop")
      goals_list[i] <- paste(paste(g[[group_var]], g$goal, sep = ":"), collapse = "|")
      model_used <- model_method
    }, silent = TRUE)
    if (is.null(model_used)) {
      g <- df_s %>% group_by_at(group_var) %>% summarise(goal = round(median(`Días cobertura con capacitación`, na.rm = TRUE)), .groups = "drop")
      goals_list[i] <- paste(paste(g[[group_var]], g$goal, sep = ":"), collapse = "|")
    }
  }
  tab <- table(goals_list)
  max(tab) / R
}

# ---------------------------
# 7. Función que implementa pipeline por Grupo
# ---------------------------
analyze_group_full <- function(grupo_nombre, datos_df, columnas_interes, nuevos_vars, approach_name = "A") {
  cat("\n", strrep("=", 60), "\n")
  cat("ANALIZANDO GRUPO:", grupo_nombre, "- Aproximación:", approach_name, "\n")
  cat(strrep("=", 60), "\n")
  df_g <- datos_df %>% filter(Grupo == grupo_nombre)
  if (nrow(df_g) < 10) {
    cat("Grupo", grupo_nombre, "tiene menos de 10 observaciones. Omitiendo.\n")
    return(NULL)
  }
  grupo_dir <- file.path(output_dir, grupo_nombre, approach_name)
  dir.create(grupo_dir, recursive = TRUE, showWarnings = FALSE)

  # ---------------------------
  # 7.1 Evaluar agrupadores (incluye combinaciones para ODG)
  # ---------------------------
  agrupadores <- columnas_interes
  # si ODG y existe Segmento de puesto, crear combinaciones
  if (grupo_nombre == "ODG" && "Segmento de puesto" %in% columnas_interes) {
    combos <- setdiff(columnas_interes, c("Días cobertura con capacitación", "Grupo", "Segmento de puesto"))
    comb_names <- c()
    for (v in combos) {
      newname <- paste0(v, "_con_Segmento")
      df_g[[newname]] <- paste0(as.character(df_g[[v]]), " | ", as.character(df_g[["Segmento de puesto"]]))
      comb_names <- c(comb_names, newname)
    }
    agrupadores <- c(agrupadores, comb_names)
  }

  # evaluar cada agrupador
  cat("\nEvaluando agrupadores (esto puede tardar)...\n")
  resultados_eval <- map_dfr(agrupadores, ~ evaluar_agrupador_mejorado(.x, df_g))
  resultados_eval <- resultados_eval %>% filter(!is.na(eta_squared)) %>%
    mutate(
      penalty_grupos = 1 - (grupos_n / nrow(df_g)),
      score_eta = eta_squared * (1 - cv_promedio) * penalty_grupos,
      score_hetero = (heterogeneidad / sd(df_g$`Días cobertura con capacitación`, na.rm = TRUE)) * (1 / (cv_promedio + 0.1)) * penalty_grupos,
      score_combinado = 0.7 * score_eta + 0.3 * score_hetero
    ) %>% arrange(desc(score_combinado))

  write_xlsx(resultados_eval, file.path(grupo_dir, "evaluacion_agrupadores.xlsx"))
  cat("Evaluación agrupadores guardada.\n")
  if (nrow(resultados_eval) == 0) {
    cat("No hay agrupadores con análisis valido.\n")
    return(NULL)
  }
  cat("\nRanking (top 10):\n")
  print(head(resultados_eval %>% select(agrupador, grupos_n, eta_squared, cv_promedio, score_combinado), 10))

  mejor_agrupador <- resultados_eval$agrupador[1]
  cat("\nMejor agrupador sugerido:", mejor_agrupador, "\n")
  eval_viab <- evaluar_viabilidad_agrupador(resultados_eval)

  # ---------------------------
  # 7.2 Generar goals por mejor agrupador (analítico)
  # ---------------------------
  cat("\nGenerando goals por", mejor_agrupador, "...\n")
  goals_tbl <- df_g %>%
    group_by_at(mejor_agrupador) %>%
    summarise(
      n = n(),
      media_dias = mean(`Días cobertura con capacitación`, na.rm = TRUE),
      mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
      sd_dias = sd(`Días cobertura con capacitación`, na.rm = TRUE),
      q1 = quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE),
      q3 = quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE),
      goal = calcular_goal(`Días cobertura con capacitación`),
      goal_exigente = round(quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    filter(n >= 5) %>% arrange(mediana_dias)

  write_xlsx(goals_tbl, file.path(grupo_dir, "goals_recomendados.xlsx"))
  cat("Goals por subgrupo guardados en goals_recomendados.xlsx\n")

  # ---------------------------
  # 7.3 Visualizaciones del mejor agrupador (similar a tu script)
  # ---------------------------
  if (nrow(goals_tbl) > 0) {
    # gráfico de barras mediana + IQR
    p <- goals_tbl %>%
      mutate(cat = reorder(as.character(!!rlang::sym(mejor_agrupador)), mediana_dias)) %>%
      ggplot(aes(x = cat, y = mediana_dias)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "darkred", size = 0.7) +
      geom_text(aes(label = paste("Goal:", goal, "días")), hjust = -0.1, size = 2.8, color = "darkgreen") +
      coord_flip() +
      labs(title = paste0("Goals por ", mejor_agrupador, " - Grupo: ", grupo_nombre),
           subtitle = "Mediana y rango intercuartílico (Q1-Q3)", x = mejor_agrupador, y = "Días") +
      theme_minimal()
    ggsave(filename = file.path(grupo_dir, "mejor_agrupador.png"), plot = p, width = 12, height = max(6, nrow(goals_tbl)*0.25), dpi = 300)
  }

  # ---------------------------
  # 7.4 Clustering (mejor con variables seleccionadas)
  # ---------------------------
  cat("\nEjecutando clustering (PCA + Kmeans) para explorar patrones naturales...\n")
  # preparar variables para clustering: num + dummies de mejores variables
  # usar columnas_interes + nuevos_vars para la aproximación B
  if (approach_name == "A") clustering_vars <- columnas_interes else clustering_vars <- unique(c(columnas_interes, nuevos_vars))
  clustering_vars <- intersect(clustering_vars, colnames(df_g))
  # crear matrix: numeric dias + dummies
  try({
    df_cluster <- df_g %>% select(`Días cobertura con capacitación`, all_of(clustering_vars))
    # eliminar columnas con todo NA o factor problemático
    df_cluster <- df_cluster %>% mutate_if(is.character, as.factor)
    # crear dummies
    model_mat <- model.matrix(~ . - 1, data = df_cluster %>% select(-`Días cobertura con capacitación`))
    mat <- cbind(scale(data.frame(dias = df_cluster$`Días cobertura con capacitación`)), scale(as.data.frame(model_mat)))
    mat <- mat[, colSums(is.na(mat)) == 0, drop = FALSE]
    vargs <- apply(mat, 2, var, na.rm = TRUE)
    mat <- mat[, vargs > 0, drop = FALSE]
    if (ncol(mat) >= 2 && nrow(mat) > 10) {
      # elbow method
      max_k <- min(8, nrow(mat)-1)
      wss <- sapply(1:max_k, function(k) kmeans(mat, k, nstart = 10)$tot.withinss)
      # choose k by relative drop heuristic
      if (length(wss) >= 3) {
        drops <- -diff(wss)
        rel <- drops / wss[-length(wss)]
        k_opt <- which.max(rel) + 1
        k_opt <- max(2, min(k_opt, 6))
      } else k_opt <- 2
      km <- kmeans(mat, centers = k_opt, nstart = 25)
      df_g$cluster_km <- factor(km$cluster)
      km_summary <- df_g %>% group_by(cluster_km) %>%
        summarise(n = n(), mediana = median(`Días cobertura con capacitación`, na.rm = TRUE), .groups = "drop")
      write_xlsx(km_summary, file.path(grupo_dir, "analisis_clusters.xlsx"))
      # plots
      p_elbow <- ggplot(data.frame(k = 1:length(wss), wss = wss), aes(k, wss)) + geom_line() + geom_point() +
        labs(title = paste("Codo -", grupo_nombre, approach_name))
      ggsave(file.path(grupo_dir, "clustering_elbow_plot.png"), p_elbow, width = 10, height = 6)
      # PCA plot
      pca_res <- prcomp(mat, scale = TRUE)
      pca_df <- as.data.frame(pca_res$x[, 1:2])
      pca_df$cluster <- factor(km$cluster)
      p_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) + geom_point(alpha = 0.7) +
        labs(title = paste("PCA Clusters -", grupo_nombre, approach_name))
      ggsave(file.path(grupo_dir, "clustering_pca_plot.png"), p_pca, width = 10, height = 8)
      cat("Clustering guardado.\n")
    } else cat("No hay suficientes variables/datos para clustering en esta aproximación.\n")
  }, silent = TRUE)

  # ---------------------------
  # 7.5 Modelado predictivo (si hay suficientes datos y run_heavy_models=TRUE)
  # ---------------------------
  cat("\nModelado predictivo: comparando modelos y baseline (percentiles)...\n")
  # seleccionar features: para A solo columnas_interes, para B incluir nuevos_vars
  if (approach_name == "A") {
    cat_vars_model <- intersect(columnas_interes, colnames(df_g))
    num_vars_model <- c()
  } else {
    cat_vars_model <- intersect(c(columnas_interes, nuevos_vars), colnames(df_g))
    num_vars_model <- intersect(c("Total_Software","Indice_Complejidad","N_Software_Avanzado"), colnames(df_g))
  }

  X <- make_feature_matrix(df_g, cat_vars = cat_vars_model, num_vars = num_vars_model, topN = 40)
  y <- df_g$`Días cobertura con capacitación`

  if (nrow(X) < 20 || ncol(X) < 1) {
    cat("Datos insuficientes para entrenar modelos complejos. Se entregan percentiles y clustering.\n")
    baseline <- data.frame(mediana = round(median(y, na.rm = TRUE)), p60 = round(quantile(y, 0.6, na.rm = TRUE)), p75 = round(quantile(y, 0.75, na.rm = TRUE)))
    write_xlsx(baseline, file.path(grupo_dir, "baseline_percentiles.xlsx"))
  } else if (run_heavy_models && nrow(X) >= min_groups_for_models) {
    # Train/Test split
    set.seed(seed)
    train_idx <- sample(seq_len(nrow(X)), size = floor(0.75 * nrow(X)))
    X_train <- X[train_idx, , drop = FALSE]; y_train <- y[train_idx]
    X_test  <- X[-train_idx, , drop = FALSE]; y_test <- y[-train_idx]

    tr_ctrl <- trainControl(method = "repeatedcv", number = CV_folds, repeats = CV_repeats, allowParallel = TRUE, verboseIter = FALSE)

    models_trained <- list()
    model_metrics <- list()

    # LM
    try({
      mod_lm <- caret::train(x = X_train, y = y_train, method = "lm", trControl = tr_ctrl)
      pred_lm <- predict(mod_lm, X_test)
      models_trained$lm <- mod_lm
      model_metrics$lm <- c(RMSE = RMSE(pred_lm, y_test), Rsquared = postResample(pred_lm, y_test)[2])
      cat("LM trained. RMSE:", round(model_metrics$lm["RMSE"],3), " R2:", round(model_metrics$lm["Rsquared"],3), "\n")
    }, silent = TRUE)

    # rpart
    try({
      mod_rpart <- caret::train(x = X_train, y = y_train, method = "rpart", trControl = tr_ctrl)
      pred_rpart <- predict(mod_rpart, X_test)
      models_trained$rpart <- mod_rpart
      model_metrics$rpart <- c(RMSE = RMSE(pred_rpart, y_test), Rsquared = postResample(pred_rpart, y_test)[2])
      cat("rpart trained. RMSE:", round(model_metrics$rpart["RMSE"],3), " R2:", round(model_metrics$rpart["Rsquared"],3), "\n")
    }, silent = TRUE)

    # randomForest
    try({
      mod_rf <- caret::train(x = X_train, y = y_train, method = "rf", trControl = tr_ctrl, tuneLength = 3)
      pred_rf <- predict(mod_rf, X_test)
      models_trained$rf <- mod_rf
      model_metrics$rf <- c(RMSE = RMSE(pred_rf, y_test), Rsquared = postResample(pred_rf, y_test)[2])
      cat("RF trained. RMSE:", round(model_metrics$rf["RMSE"],3), " R2:", round(model_metrics$rf["Rsquared"],3), "\n")
    }, silent = TRUE)

    # xgboost (numeric matrix)
    try({
      mod_xgb <- caret::train(x = X_train, y = y_train, method = "xgbTree", trControl = tr_ctrl, tuneLength = 3)
      pred_xgb <- predict(mod_xgb, X_test)
      models_trained$xgbTree <- mod_xgb
      model_metrics$xgbTree <- c(RMSE = RMSE(pred_xgb, y_test), Rsquared = postResample(pred_xgb, y_test)[2])
      cat("XGBoost trained. RMSE:", round(model_metrics$xgbTree["RMSE"],3), " R2:", round(model_metrics$xgbTree["Rsquared"],3), "\n")
    }, silent = TRUE)

    # Neural net: keras if available else nnet
    try({
      if (use_keras) {
        # use caret interface with keras (if configured) or fallback to nnet
        # For safety, try nnet first (less dependencies)
        mod_nnet <- caret::train(x = X_train, y = y_train, method = "nnet", trControl = tr_ctrl, tuneLength = 3, trace = FALSE, linout = TRUE)
        pred_nnet <- predict(mod_nnet, X_test)
        models_trained$nnet <- mod_nnet
        model_metrics$nnet <- c(RMSE = RMSE(pred_nnet, y_test), Rsquared = postResample(pred_nnet, y_test)[2])
        cat("nnet trained. RMSE:", round(model_metrics$nnet["RMSE"],3), " R2:", round(model_metrics$nnet["Rsquared"],3), "\n")
      } else {
        # nnet
        mod_nnet <- caret::train(x = X_train, y = y_train, method = "nnet", trControl = tr_ctrl, tuneLength = 3, trace = FALSE, linout = TRUE)
        pred_nnet <- predict(mod_nnet, X_test)
        models_trained$nnet <- mod_nnet
        model_metrics$nnet <- c(RMSE = RMSE(pred_nnet, y_test), Rsquared = postResample(pred_nnet, y_test)[2])
        cat("nnet trained. RMSE:", round(model_metrics$nnet["RMSE"],3), " R2:", round(model_metrics$nnet["Rsquared"],3), "\n")
      }
    }, silent = TRUE)

    # baseline percentiles
    baseline <- data.frame(
      mediana = round(median(y, na.rm = TRUE)),
      p60 = round(quantile(y, 0.6, na.rm = TRUE)),
      p75 = round(quantile(y, 0.75, na.rm = TRUE)),
      p90 = round(quantile(y, 0.90, na.rm = TRUE))
    )
    write_xlsx(list(models = as.data.frame(do.call(rbind, lapply(model_metrics, t))), baseline = baseline),
               file.path(grupo_dir, "model_summary_and_baseline.xlsx"))
    cat("Resumen de modelos y baseline guardado.\n")

    # Elegir mejor modelo por RMSE si existen
    if (length(model_metrics) > 0) {
      best_name <- names(which.min(sapply(model_metrics, function(x) x["RMSE"])))
      cat("Mejor modelo por RMSE:", best_name, "\n")
      # Calcular estabilidad bootstrap para el mejor modelo
      stab <- tryCatch({
        bootstrap_goal_stability_model(df_g, feature_cat_vars = cat_vars_model, feature_num_vars = num_vars_model,
                                       model_method = best_name, group_var = mejor_agrupador, R = bootstrap_iter)
      }, error = function(e) NA_real_)
      cat("Estabilidad del mejor modelo (proporción):", round(stab, 3), "\n")
      # Save stab to summary
      write_xlsx(list(stability = data.frame(model = best_name, stability = round(stab,3))),
                 file.path(grupo_dir, "stability_best_model.xlsx"))
    }
  } else {
    # fallback: only baseline percentiles
    baseline <- data.frame(mediana = round(median(y, na.rm = TRUE)), p60 = round(quantile(y, 0.6, na.rm = TRUE)), p75 = round(quantile(y, 0.75, na.rm = TRUE)))
    write_xlsx(baseline, file.path(grupo_dir, "baseline_percentiles.xlsx"))
    cat("Solo baseline generado (no modelos pesados).\n")
  }

  # ---------------------------
  # 7.6 Modelo por CAPAS (base + incrementos)
  # ---------------------------
  cat("\nConstruyendo modelo por CAPAS (base + incrementos)...\n")
  # base_by: usar Macro_Especializacion si existe, sino mejor_agrupador
  base_by <- if ("Macro_Especializacion" %in% colnames(df_g)) "Macro_Especializacion" else mejor_agrupador
  base_goals <- df_g %>% filter(!is.na(.data[[base_by]])) %>%
    group_by_at(base_by) %>% summarise(base_med = round(median(`Días cobertura con capacitación`, na.rm = TRUE)), n = n(), .groups = "drop")

  inc_tbl <- df_g %>% group_by(Complejidad_Nivel) %>% summarise(med = round(median(`Días cobertura con capacitación`, na.rm = TRUE)), n = n(), .groups = "drop")
  baseline_med <- if ("Estándar" %in% inc_tbl$Complejidad_Nivel) inc_tbl$med[inc_tbl$Complejidad_Nivel == "Estándar"] else min(inc_tbl$med, na.rm = TRUE)
  inc_tbl <- inc_tbl %>% mutate(increment = med - baseline_med)

  write_xlsx(list(base_goals = base_goals, increments = inc_tbl), file.path(grupo_dir, "capas_model.xlsx"))
  cat("Modelo por capas guardado (capas_model.xlsx)\n")

  # Return summary
  return(list(
    resultados_eval = resultados_eval,
    mejor_agrupador = mejor_agrupador,
    goals_tbl = goals_tbl,
    model_metrics = if (exists("model_metrics")) model_metrics else NULL,
    clustering = if (exists("km_summary")) km_summary else NULL,
    capas = list(base = base_goals, increments = inc_tbl)
  ))
}

# ---------------------------
# 8. Ejecutar pipeline para todos los grupos (dos aproximaciones)
# ---------------------------
# Columnas base originales (tal como en tu script)
columnas_base <- c(
  "Días cobertura con capacitación",
  "Grupo",
  "DescripcionCC",
  "Perfil Profesional",
  "Segmento de puesto",
  "Tabulador Salarial",
  "Area de Personal",
  "Puesto Generico",
  "Familia de Puesto",
  "Regional",
  "Plaza",
  "Estado"
)
# incluir Nombre Reclutador si existe
if ("Nombre Reclutador" %in% colnames(datos)) columnas_interes <- c(columnas_base, "Nombre Reclutador") else columnas_interes <- columnas_base

# nuevos vars que añadimos
nuevos_vars <- c("Nivel_Escolaridad", "Macro_Especializacion", "N_Software_Avanzado",
                 "N_Software_Intermedio", "N_Software_Basico", "Total_Software",
                 "Perfil_TI", "Complejidad_Nivel", "Indice_Complejidad")

grupos_obj <- intersect(c("SUCURSAL","COBRANZA","PLAZA","ODG"), unique(datos$Grupo))
if (length(grupos_obj) == 0) stop("No se encontraron grupos objetivo en datos.")

resultados_grupos <- list()

for (g in grupos_obj) {
  # Aproximación A: originales
  resA <- analyze_group_full(g, datos, columnas_interes = setdiff(columnas_interes, "Días cobertura con capacitación"), nuevos_vars = nuevos_vars, approach_name = "A_originales")
  resultados_grupos[[paste0(g, "_A")]] <- resA

  # Aproximación B: originales + nuevos
  resB <- analyze_group_full(g, datos, columnas_interes = setdiff(columnas_interes, "Días cobertura con capacitación"), nuevos_vars = nuevos_vars, approach_name = "B_originales_nuevos")
  resultados_grupos[[paste0(g, "_B")]] <- resB
}

# ---------------------------
# 9. Consolidado y recomendaciones automáticas
# ---------------------------
cat("\n", strrep("*", 60), "\n")
cat("RESUMEN CONSOLIDADO DE TODOS LOS GRUPOS\n")
cat(strrep("*", 60), "\n")

summary_consolidado <- map(resultados_grupos, function(x) {
  if (is.null(x)) return(NULL)
  data.frame(
    Mejor_Agrupador = x$mejor_agrupador,
    n_goals = if (!is.null(x$goals_tbl)) nrow(x$goals_tbl) else 0,
    model_metrics = if (!is.null(x$model_metrics)) paste(names(x$model_metrics), collapse = ", ") else NA
  )
})
summary_consolidado <- bind_rows(summary_consolidado, .id = "grupo_approach")
write_xlsx(summary_consolidado, file.path(output_dir, "resumen_consolidado_estimation.xlsx"))
cat("Resumen consolidado guardado.\n")
print(summary_consolidado)

# ---------------------------
# 10. Limpieza final (stop cluster)
# ---------------------------
if (run_heavy_models) {
  try({
    stopCluster(cl)
    registerDoSEQ()
  }, silent = TRUE)
}
cat("\nFIN DEL PROCESO. Revisa la carpeta:", output_dir, "\n")
