########################################################################
########################## QUERY SQLite ################################
########################################################################
########################################################################

# Cargar las librerías necesarias
library(DBI)        # Para conectarse a SQLite
library(openxlsx)   # Para crear archivos de Excel

# 1. Conectar a la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(RSQLite::SQLite(), db_path)

# 2. Definir la consulta SQL que deseas ejecutar
query <- "

"

# 3. Ejecutar la consulta y obtener los resultados
resultados <- dbGetQuery(conn, query)

# 4. Cerrar la conexión a la base de datos
dbDisconnect(conn)

# 5. Guardar los resultados en un archivo de Excel
output_path <- "C:/Users/racl26345/Downloads/Resultado Query SQLite.xlsx"
write.xlsx(resultados, file = output_path, rowNames = FALSE)

# 6. Mensaje de confirmación
cat("Los resultados se han guardado en:", output_path, "\n")


########################################################################
########################################################################
########################################################################
########################################################################
############################# PRUEBAS ##################################
########################################################################
########################################################################




  # ---------------------------
  # CONFIGURACIÓN INICIAL (R)
  # ---------------------------
  library(reticulate)
  library(dplyr) 
  library(readxl)
  library(lubridate)
  use_condaenv("base")  # Asegurar que usas el entorno base
  
  # Instalar paquetes de Python (si no están instalados)
  #py_install(c("pandas", "numpy", "scikit-learn", "xgboost", "matplotlib", "seaborn", "shap"))
  
  # Cargar bibliotecas de Python
  pd <- import("pandas")
  np <- import("numpy")
  sklearn <- import("sklearn.ensemble")
  xgb <- import("xgboost")
  plt <- import("matplotlib.pyplot")
  sns <- import("seaborn")
  shap <- import("shap")
  
  # Definir el modelo
  modelo_rf <- sklearn$RandomForestClassifier(
    n_estimators = 200L,     # Entero
    max_depth = 5L,          # Entero
    class_weight = "balanced",
    random_state = 123L      # Entero
  )
  
  # ---------------------------
  # CARGAR DATOS DESDE EXCEL
  # ---------------------------
  data <- read_excel("C:/Users/racl26345/Documents/Modelos Predictivos/Listado Colaboradores Incorporados 2022 a 2024.xlsx")
  
  # ---------------------------
  # PREPARACIÓN DE DATOS (R)
  # ---------------------------
  # Filtrar datos para SUCURSAL y PLAZA
  data_filtrado <- data %>% 
    filter(Alta_nivel_gestion %in% c("SUCURSAL", "PLAZA")) %>%
    mutate(
      Fecha_Contratación = as_date(Fecha_Contratación),  # Convertir a fecha
      Baja_fecha = as_date(Baja_fecha)
    )
  
  # Crear variable objetivo: Supervivencia a 6 meses
  data_filtrado <- data_filtrado %>%
    mutate(
      Supervivencia_6_meses = ifelse(
        is.na(Baja_fecha) | Baja_fecha > Fecha_Contratación %m+% months(6), 
        1, 
        0
      )
    )
  
  # Calcular edades (ejemplo)
  data_filtrado <- data_filtrado %>%
    mutate(
      Edad_Contratación = as.numeric(difftime(Fecha_Contratación, Alta_fecha_nacimiento, units = "days") / 365.25)
    )
  
  # Seleccionar variables para el modelo (¡personaliza según tus columnas!)
  data_modelo <- data_filtrado %>%
    select(
      # Variables demográficas
      Alta_genero, 
      Alta_estado_civil, 
      Edad_Contratación,
      
      # Variables laborales
      Alta_Media_Salario, 
      Alta_area_cobranza,
      
      # Variable objetivo
      Supervivencia_6_meses
    )
  
  # Convertir variables categóricas a factores
  data_modelo <- data_modelo %>%
    mutate(across(where(is.character), as.factor))
  
  # Eliminar filas con NA
  data_modelo <- na.omit(data_modelo)
  
  # ---------------------------
  # PASAR DATOS A PYTHON
  # ---------------------------
  # Convertir a DataFrame de Python
  data_py <- r_to_py(data_modelo)
  
  # Codificar variables categóricas (one-hot encoding)
  X <- pd$get_dummies(data_py$drop("Supervivencia_6_meses", axis = 1))
  y <- data_py["Supervivencia_6_meses"]
  
  # Convertir y a array de numpy (evita errores de formato)
  y <- np$array(y$values) %>% np$ravel()  # Asegurar que es 1D
  
  # Dividir en entrenamiento y prueba (usar 123L para forzar tipo entero)
  split_result <- sklearn$model_selection$train_test_split(
    X, 
    y, 
    test_size = 0.3, 
    random_state = 123L,  # ¡Usar L para indicar entero en R!
    stratify = y
  )
  
  # Extraer componentes
  X_train <- split_result[[1]]
  X_test <- split_result[[2]]
  y_train <- split_result[[3]]
  y_test <- split_result[[4]]
  
  # ---------------------------
  # MODELO 1: RANDOM FOREST
  # ---------------------------
  # Entrenar modelo
  modelo_rf$fit(X_train, y_train)
  
  # Predecir probabilidades
  probas <- modelo_rf$predict_proba(X_test)
  
  # Manejar casos con una sola clase
  if (ncol(probas) == 1) {
    y_proba_rf <- probas[, 1]
  } else {
    y_proba_rf <- probas[, 2]
  }
  
  # Calcular métricas
  library(caret)
  confusionMatrix(factor(y_pred_rf), factor(y_test))
  
  # Importancia de variables
  importancia_rf <- data.frame(
    Variable = colnames(X_train),
    Importancia = modelo_rf$feature_importances_
  ) %>% arrange(desc(Importancia))
  
  # ---------------------------
  # MODELO 2: XGBOOST
  # ---------------------------
  cat("\n[2/2] Entrenando XGBoost...\n")
  
  # Configurar modelo
  modelo_xgb <- xgb$XGBClassifier(
    objective = "binary:logistic",
    n_estimators = 150,
    max_depth = 4,
    learning_rate = 0.1,
    subsample = 0.8,
    scale_pos_weight = sum(y_train == 0) / sum(y_train == 1),  # Balancear clases
    random_state = 123
  )
  
  # Entrenar y predecir
  modelo_xgb$fit(X_train, y_train$values$ravel())
  y_pred_xgb <- modelo_xgb$predict(X_test)
  y_proba_xgb <- modelo_xgb$predict_proba(X_test)[, 2]
  
  # Métricas de evaluación
  cat("\nResultados XGBoost:\n")
  print(sklearn$metrics$classification_report(y_test, y_pred_xgb))
  cat("AUC-ROC:", sklearn$metrics$roc_auc_score(y_test, y_proba_xgb), "\n")
  
  # Importancia de variables
  importancia_xgb <- xgb$plot_importance(modelo_xgb)
  
  # ---------------------------
  # VISUALIZACIÓN
  # ---------------------------
  # Configurar estilo de gráficos
  plt$style$use("seaborn")
  sns$set_palette("viridis")
  
  # Gráfico de importancia (Random Forest)
  plt$figure(figsize = c(12, 8))
  sns$barplot(
    x = "Importancia", 
    y = "Variable", 
    data = importancia_rf[1:15, ]  # Top 15 variables
  )
  plt$title("Importancia de Variables - Random Forest (Top 15)")
  plt$savefig("importancia_rf.png", dpi = 300, bbox_inches = "tight")
  plt$close()
  
  # Gráfico de importancia (XGBoost)
  plt$figure(figsize = c(12, 8))
  xgb$plot_importance(modelo_xgb, max_num_features = 15)
  plt$title("Importancia de Variables - XGBoost (Top 15)")
  plt$savefig("importancia_xgb.png", dpi = 300, bbox_inches = "tight")
  plt$close()
  
  # ---------------------------
  # INTERPRETABILIDAD (SHAP)
  # ---------------------------
  # Calcular valores SHAP (solo para XGBoost)
  cat("\nCalculando valores SHAP...\n")
  explainer <- shap$TreeExplainer(modelo_xgb)
  shap_values <- explainer$shap_values(X_train)
  
  # Gráfico de resumen SHAP
  plt$figure(figsize = c(12, 8))
  shap$summary_plot(shap_values, X_train, plot_type = "dot", max_display = 15)
  plt$title("Impacto de Variables en Predicciones (SHAP)")
  plt$savefig("shap_summary.png", dpi = 300, bbox_inches = "tight")
  plt$close()
  
  # ---------------------------
  # COMPARACIÓN FINAL
  # ---------------------------
  cat("\n[COMPARACIÓN FINAL]\n")
  
  # Crear dataframe comparativo
  comparacion <- data.frame(
    Modelo = c("Random Forest", "XGBoost"),
    Accuracy = c(
      sklearn$metrics$accuracy_score(y_test, y_pred_rf),
      sklearn$metrics$accuracy_score(y_test, y_pred_xgb)
    ),
    AUC_ROC = c(
      sklearn$metrics$roc_auc_score(y_test, y_proba_rf),
      sklearn$metrics$roc_auc_score(y_test, y_proba_xgb)
    )
  )
  
  print(comparacion)