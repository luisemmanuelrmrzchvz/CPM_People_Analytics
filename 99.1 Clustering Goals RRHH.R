############################################################
# Exploración y Modelado avanzado - DÍAS COBERTURA CON CAPACITACIÓN
# Versión extendida: nuevas variables + pipelines para Deep Learning (Keras/H2O) + XGBoost baseline
# Rutas fijas y convenciones del usuario preservadas
############################################################

## ===== LIBRERÍAS =====
pkgs <- c(
  'readxl','dplyr','tidyr','purrr','stringr','forcats','ggplot2','writexl',
  'recipes','rsample','yardstick','caret','xgboost','h2o','keras','tensorflow'
)

inst <- pkgs[!(pkgs %in% installed.packages()[,1])]
if(length(inst)) install.packages(inst)
# h2o y keras tienen pasos adicionales
if(!'h2o' %in% installed.packages()[,1]) install.packages('h2o')
if(!'keras' %in% installed.packages()[,1]) install.packages('keras')

library(readxl); library(dplyr); library(tidyr); library(purrr); library(stringr);
library(forcats); library(ggplot2); library(writexl)
library(recipes); library(rsample); library(yardstick); library(caret)
library(xgboost); library(h2o); library(keras); library(tensorflow)

# Opcional: inicializar h2o
h2o.init(nthreads = -1, max_mem_size = '8G')

## ===== RUTAS FIJAS =====
input_file <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
output_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura"
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)
models_dir <- file.path(output_path, 'models')
if (!dir.exists(models_dir)) dir.create(models_dir)

## ===== CONFIGURACIÓN =====
TARGET <- "Días cobertura con capacitación"  # EXACTO - no modificar
GRUPOS <- c('SUCURSAL','COBRANZA','PLAZA','ODG')
# Variables fijas, incluyendo las nuevas
vars_all <- c(
  'Año','Mes','IDColaborador','Nombre','Evento','MotivoEvento','FechaEfectiva','IDPosicion',
  'CentroCostos','DescripcionCC','Puesto','Grupo','Regional','Plaza','Estado','Nombre Reclutador',
  'FechaVacante','Fecha término de capacitación', 'Días cobertura con capacitación','Perfil Profesional',
  'Segmento de puesto','Tabulador Salarial','Area de Personal','Puesto Generico','Familia de Puesto',
  'Escolaridad','Especialización','Software-Avanzado','Software-Básico','Software-Intermedio'
)

# Variables que usaremos como predictores categóricas
cat_vars <- c(
  'DescripcionCC','Perfil Profesional','Segmento de puesto','Tabulador Salarial','Area de Personal',
  'Puesto Generico','Familia de Puesto','Regional','Plaza','Estado','Nombre Reclutador',
  'Escolaridad','Especialización','Software-Avanzado','Software-Intermedio','Software-Básico'
)

# Parámetros de modelado
include_reclutador <- FALSE # por defecto FALSE (recomendado). Cambiar a TRUE si quieres probarlo.
test_prop <- 0.2
seed <- 12345
set.seed(seed)

## ===== CARGA Y PREPARACIÓN BÁSICA =====
raw <- read_excel(input_file)
# Mantener solo columnas necesarias (si faltan, no fallar)
keep_cols <- intersect(vars_all, colnames(raw))
df <- raw %>% select(all_of(keep_cols))

# Verificación básica
if(!TARGET %in% colnames(df)) stop(paste0("La variable objetivo '", TARGET, "' no existe. Revisa el Excel."))
if(!'Grupo' %in% colnames(df)) stop("La columna 'Grupo' no existe en el archivo.")

# Filtrar solo grupos de interés
df <- df %>% filter(Grupo %in% GRUPOS)

# Forzar factores en variables categóricas disponibles
cat_vars_present <- intersect(cat_vars, colnames(df))
# opcionalmente excluir reclutador si user pref
if(!include_reclutador) cat_vars_present <- setdiff(cat_vars_present, 'Nombre Reclutador')

df <- df %>% mutate(across(all_of(cat_vars_present), ~ as.factor(.x)))

# Remover filas sin target
df <- df %>% filter(!is.na(.data[[TARGET]]))

# Guardar copia limpia
write_xlsx(df, file.path(output_path, 'datos_limpios_para_modelado.xlsx'))

## ===== UTIL: ENCODING PARA REDES (ENTERO PARA EMBEDDINGS) =====
# Esta función toma un df y devuelve lista con df_int (con columnas integer para embeddings)
encode_for_keras <- function(data, categ_vars, min_count = 5) {
  enc_map <- list()
  data_enc <- data
  for(v in categ_vars) {
    # reemplazar NA por 'NA_level'
    data_enc[[v]] <- as.character(data_enc[[v]])
    data_enc[[v]][is.na(data_enc[[v]])] <- '___NA___'
    # collapse rare levels
    counts <- table(data_enc[[v]])
    rare <- names(counts[counts < min_count])
    if(length(rare) > 0) data_enc[[v]][data_enc[[v]] %in% rare] <- '___RARE___'
    # crear integer encoding (1..n)
    levels_v <- unique(data_enc[[v]])
    enc <- setNames(seq_along(levels_v), levels_v)
    enc_map[[v]] <- enc
    data_enc[[paste0(v, '_idx')]] <- as.integer(enc[data_enc[[v]]])
  }
  list(data = data_enc, map = enc_map)
}

## ===== FUNCIONES DE EVALUACIÓN =====
eval_metrics_df <- function(truth, pred) {
  tibble(
    rmse = rmse_vec(truth, pred),
    mae = mae_vec(truth, pred),
    mape = mean(abs((truth - pred)/pmax(abs(truth),1)), na.rm = TRUE)
  )
}

## ===== MODEL 1: Baseline XGBoost (tabular, one-hot via recipes) =====
run_xgboost <- function(df_train, df_test, predictors, target, out_prefix) {
  cat('Running XGBoost baseline...\n')
  # recipe
  rec <- recipe(as.formula(paste(target, '~ .')), data = df_train %>% select(all_of(c(predictors, target)))) %>%
    step_novel(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())

  prep_rec <- prep(rec)
  X_train <- bake(prep_rec, new_data = df_train)
  X_test <- bake(prep_rec, new_data = df_test)

  y_train <- X_train[[target]]; y_test <- X_test[[target]]
  X_train_mat <- as.matrix(X_train %>% select(-all_of(target)))
  X_test_mat <- as.matrix(X_test %>% select(-all_of(target)))

  dtrain <- xgb.DMatrix(data = X_train_mat, label = y_train)
  dtest  <- xgb.DMatrix(data = X_test_mat, label = y_test)

  params <- list(objective = 'reg:squarederror', eval_metric = 'rmse')
  watchlist <- list(train = dtrain)

  bst <- xgb.train(params = params, data = dtrain, nrounds = 200, watchlist = watchlist, verbose = 0)

  preds <- predict(bst, dtest)
  metrics <- eval_metrics_df(y_test, preds)
  write_xlsx(list(metrics = metrics), file.path(models_dir, paste0(out_prefix, '_xgb_metrics.xlsx')))
  saveRDS(bst, file.path(models_dir, paste0(out_prefix, '_xgb_model.rds')))
  return(list(model = bst, metrics = metrics, preds = preds))
}

## ===== MODEL 2: H2O Deep Learning (scalable) =====
run_h2o_deeplearning <- function(df_train, df_test, features, target, out_prefix) {
  cat('Running H2O DeepLearning...\n')
  # subir a H2O
  h2o_train <- as.h2o(df_train %>% select(all_of(c(features, target))))
  h2o_test  <- as.h2o(df_test %>% select(all_of(c(features, target))))

  x <- features; y <- target
  model_h2o <- h2o.deeplearning(x = x, y = y, training_frame = h2o_train,
                                hidden = c(64,32), epochs = 50, standardize = TRUE, reproducible = TRUE, seed = seed)
  preds_h2o <- as.vector(h2o.predict(model_h2o, h2o_test))
  y_test <- as.vector(h2o_test[[y]])
  metrics <- eval_metrics_df(y_test, preds_h2o)
  write_xlsx(list(metrics = metrics), file.path(models_dir, paste0(out_prefix, '_h2o_metrics.xlsx')))
  h2o.saveModel(model_h2o, path = models_dir, filename = paste0(out_prefix, '_h2o_model'))
  return(list(model = model_h2o, metrics = metrics, preds = preds_h2o))
}

## ===== MODEL 3: Keras with Embeddings for high-cardinality categoricals =====n
run_keras_embeddings <- function(df_train, df_test, categ_vars_idx, numeric_vars, target, out_prefix, emb_dim_fun = function(n) pmin(50, round(1.6 * n^0.56))) {
  cat('Running Keras NN with embeddings...\n')
  # Inputs: df_train contains columns <var>_idx for each categ var
  # Build input layers per categorical variable
  keras::backend()$clear_session()

  inputs <- list(); embeds <- list(); input_names <- list()

  for(v in categ_vars_idx) {
    vocab_size <- max(df_train[[v]], na.rm = TRUE) + 1
    inp <- layer_input(shape = 1, dtype = 'int32', name = paste0(v, '_input'))
    dim_emb <- emb_dim_fun(vocab_size)
    emb <- inp %>% layer_embedding(input_dim = vocab_size + 1, output_dim = dim_emb, name = paste0(v, '_emb')) %>% layer_flatten()
    inputs[[v]] <- inp
    embeds[[v]] <- emb
    input_names <- c(input_names, paste0(v, '_input'))
  }

  # numeric input
  if(length(numeric_vars) > 0) {
    num_inp <- layer_input(shape = length(numeric_vars), dtype = 'float32', name = 'num_input')
    inputs[['num']] <- num_inp
    embeds[['num']] <- num_inp
    input_names <- c(input_names, 'num_input')
  }

  # concatenate
  x <- layer_concatenate(c(embeds), name = 'concat_layer')
  x <- x %>% layer_dropout(rate = 0.2) %>% layer_dense(units = 128, activation = 'relu') %>%
    layer_dropout(rate = 0.2) %>% layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 1, activation = 'linear')

  model <- keras_model(inputs = inputs, outputs = x)
  model %>% compile(optimizer = optimizer_adam(learning_rate = 0.001), loss = 'mse', metrics = list('mae'))
  summary(model)

  # Prepare training lists
  make_input_list <- function(df_local) {
    L <- list()
    for(v in categ_vars_idx) L[[paste0(v, '_input')]] <- as.matrix(df_local[[v]])
    if(length(numeric_vars) > 0) L[['num_input']] <- as.matrix(df_local %>% select(all_of(numeric_vars)))
    return(L)
  }

  x_train <- make_input_list(df_train)
  x_test  <- make_input_list(df_test)
  y_train <- as.matrix(df_train[[target]])
  y_test  <- as.matrix(df_test[[target]])

  history <- model %>% fit(x = x_train, y = y_train, validation_split = 0.15, epochs = 80, batch_size = 128, verbose = 1)
  preds <- as.numeric(model %>% predict(x_test))
  metrics <- eval_metrics_df(as.numeric(y_test), preds)

  # guardar
  save_model_hdf5(model, file.path(models_dir, paste0(out_prefix, '_keras_model.h5')))
  write_xlsx(list(metrics = metrics), file.path(models_dir, paste0(out_prefix, '_keras_metrics.xlsx')))
  return(list(model = model, history = history, metrics = metrics, preds = preds))
}

## ===== MAIN LOOP: por GRUPO =====
resumen_all <- list()
for(g in GRUPOS) {
  cat('\n===== ANALIZANDO GRUPO:', g, '=====\n')
  df_g <- df %>% filter(Grupo == g)
  if(nrow(df_g) < 50) { cat('  - Pocas observaciones (<50). Se recomienda revisar.\n'); }

  # TRAIN/TEST split por strata en target (uso rsample)
  set.seed(seed)
  split <- initial_split(df_g, prop = 1 - test_prop, strata = TARGET)
  train <- training(split); test <- testing(split)

  # Baseline features: usar todas las categóricas presentes + ninguna num extra por ahora
  features <- intersect(cat_vars_present, colnames(df_g))

  # Para XGBoost preprocesaremos con recipe (one-hot)
  run_xgb <- tryCatch({ run_xgboost(train %>% select(all_of(c(features, TARGET))), test %>% select(all_of(c(features, TARGET))), features, TARGET, paste0(g)) }, error = function(e) { cat('XGB error:', e$message, '\n'); NULL })

  # H2O: convertir factors a strings (h2o convertirá)
  features_h2o <- features; # excluding target
  run_h2o <- tryCatch({ run_h2o_deeplearning(train %>% select(all_of(c(features_h2o, TARGET))), test %>% select(all_of(c(features_h2o, TARGET))), features_h2o, TARGET, paste0(g)) }, error = function(e) { cat('H2O error:', e$message, '\n'); NULL })

  # Keras embeddings: preparar encoding (usar min_count para collapse rare levels)
  enc <- encode_for_keras(train %>% select(all_of(features)), features, min_count = 10)
  train_enc <- enc$data
  # aplicar la misma codificación a test (si level desconocido -> NA index guardado como NA -> map to 1)
  for(v in features) {
    map_v <- enc$map[[v]]
    test[[v]] <- as.character(test[[v]])
    test[[v]][is.na(test[[v]])] <- '___NA___'
    # map unknowns
    test[[paste0(v, '_idx')]] <- as.integer(map_v[test[[v]]])
    test[[paste0(v, '_idx')]][is.na(test[[paste0(v, '_idx')]])] <- 1
  }
  # crear train/test numeric vars (ninguna por ahora)
  numeric_vars <- c()
  categ_idx_vars <- paste0(features, '_idx')

  run_keras <- tryCatch({ run_keras_embeddings(train_enc, test, categ_idx_vars, numeric_vars, TARGET, paste0(g)) }, error = function(e) { cat('Keras error:', e$message, '\n'); NULL })

  # Consolidar métricas
  metrics_combined <- tibble(model = c('xgboost','h2o','keras'),
                             rmse = c(ifelse(is.null(run_xgb), NA, run_xgb$metrics$rmse),
                                      ifelse(is.null(run_h2o), NA, run_h2o$metrics$rmse),
                                      ifelse(is.null(run_keras), NA, run_keras$metrics$rmse)),
                             mae = c(ifelse(is.null(run_xgb), NA, run_xgb$metrics$mae),
                                     ifelse(is.null(run_h2o), NA, run_h2o$metrics$mae),
                                     ifelse(is.null(run_keras), NA, run_keras$metrics$mae)))

  write_xlsx(list(metrics = metrics_combined), file.path(models_dir, paste0('metrics_summary_', g, '.xlsx')))

  resumen_all[[g]] <- list(group = g, metrics = metrics_combined)
}

# Guardar resumen completo
saveRDS(resumen_all, file.path(models_dir, 'resumen_modelos_por_grupo.rds'))
write_xlsx(bind_rows(lapply(resumen_all, function(x) mutate(x$metrics, Grupo = x$group))), file.path(models_dir, 'resumen_modelos_por_grupo.xlsx'))

cat('\nFIN: Script de modelado avanzado. Revisa la carpeta:', models_dir, '\n')

# Final: cerrar h2o
h2o.shutdown(prompt = FALSE)
