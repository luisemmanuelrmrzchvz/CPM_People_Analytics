# CARGAR LIBRERÍAS NECESARIAS
library(readxl)
library(dplyr)
library(cluster)
library(factoextra)
library(purrr)
library(ggplot2)
library(writexl)
library(tidyr)
library(gridExtra)
library(rpart)          # Para árboles de decisión
library(rpart.plot)     # Para visualizar árboles
library(caret)          # Para modelos predictivos
library(randomForest)   # Para modelos más avanzados
library(corrplot)       # Para matrices de correlación

# DEFINIR DIRECTORIO PARA GUARDAR RESULTADOS
output_dir <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# CARGAR LOS DATOS
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
datos <- read_excel(file_path)

# VERIFICAR QUE LA COLUMNA 'Grupo' EXISTA
if (!"Grupo" %in% colnames(datos)) {
  stop("Error: La columna 'Grupo' no se encuentra en los datos. Verifica el nombre de la columna.")
}

# VERIFICAR QUE LA COLUMNA 'Nombre Reclutador' EXISTA
if (!"Nombre Reclutador" %in% colnames(datos)) {
  cat("ADVERTENCIA: La columna 'Nombre Reclutador' no se encuentra en los datos.\n")
  incluir_reclutador <- FALSE
} else {
  incluir_reclutador <- TRUE
}

# SELECCIONAR Y PREPARAR COLUMNAS DE INTERÉS
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
  "Estado",
  "Escolaridad",
  "Especialización",
  "Software-Avanzado",
  "Software-Básico",
  "Software-Intermedio"
)

if (incluir_reclutador) {
  columnas_interes <- c(columnas_base, "Nombre Reclutador")
} else {
  columnas_interes <- columnas_base
}

# Limpiar datos
datos_limpieza <- datos %>%
  select(all_of(columnas_interes)) %>%
  mutate(
    across(
      c("Software-Avanzado", "Software-Básico", "Software-Intermedio"),
      ~ ifelse(is.na(.), "NO_ESPECIFICADO", .)
    )
  ) %>%
  mutate(across(-c(`Días cobertura con capacitación`), as.factor)) %>%
  filter(!is.na(`Días cobertura con capacitación`), !is.na(Grupo))

# DEFINIR LOS GRUPOS A ANALIZAR
grupos <- c("SUCURSAL", "COBRANZA", "PLAZA", "ODG")

# ======================================================================
# FUNCIÓN PARA EVALUAR AGRUPADORES (SIMPLIFICADA)
# ======================================================================

evaluar_agrupador_simple <- function(columna, datos_subset) {
  
  datos_temp <- datos_subset
  
  analisis <- datos_temp %>%
    group_by_at(columna) %>%
    summarise(
      n = n(),
      media = mean(`Días cobertura con capacitación`, na.rm = TRUE),
      mediana = median(`Días cobertura con capacitación`, na.rm = TRUE),
      sd = sd(`Días cobertura con capacitación`, na.rm = TRUE),
      cv = ifelse(media > 0, sd/media, NA),
      .groups = 'drop'
    ) %>%
    filter(n >= 5) %>%
    filter(!is.na(cv))
  
  if (nrow(analisis) < 2) {
    return(data.frame(
      agrupador = columna,
      grupos_n = nrow(analisis),
      eta_squared = NA,
      cv_promedio = NA,
      rango_medias = NA
    ))
  }
  
  categorias_validas <- analisis[[columna]]
  datos_anova <- datos_temp %>%
    filter(!!sym(columna) %in% categorias_validas)
  
  if (length(unique(datos_anova[[columna]])) > 1) {
    formula_text <- paste0("`Días cobertura con capacitación` ~ `", columna, "`")
    modelo <- aov(as.formula(formula_text), data = datos_anova)
    resumen <- summary(modelo)
    
    ss_total <- sum((datos_anova$`Días cobertura con capacitación` - 
                       mean(datos_anova$`Días cobertura con capacitación`, na.rm = TRUE))^2, na.rm = TRUE)
    ss_between <- sum(analisis$n * (analisis$media - 
                                      mean(datos_anova$`Días cobertura con capacitación`, na.rm = TRUE))^2)
    eta_sq <- ifelse(ss_total > 0, ss_between / ss_total, 0)
  } else {
    eta_sq <- NA
  }
  
  return(data.frame(
    agrupador = columna,
    grupos_n = nrow(analisis),
    eta_squared = eta_sq,
    cv_promedio = mean(analisis$cv, na.rm = TRUE),
    rango_medias = max(analisis$media, na.rm = TRUE) - min(analisis$media, na.rm = TRUE)
  ))
}

# ======================================================================
# FUNCIÓN PARA EVALUAR COMBINACIONES DE VARIABLES CON SEGMENTO DE PUESTO
# ======================================================================

evaluar_combinaciones_variables <- function(datos_grupo, grupo_nombre, grupo_dir) {
  cat("\nANALIZANDO COMBINACIONES DE VARIABLES CON 'Segmento de puesto'...\n")
  
  tryCatch({
    # Lista de variables para combinar con "Segmento de puesto"
    variables_a_combinar <- setdiff(
      columnas_interes,
      c("Días cobertura con capacitación", "Grupo", "Segmento de puesto")
    )
    
    resultados_combinaciones <- purrr::map_dfr(variables_a_combinar, function(var) {
      # Crear variable combinada
      datos_temp <- datos_grupo %>%
        mutate(
          variable_combinada = interaction(!!sym(var), `Segmento de puesto`, sep = " | ")
        )
      
      # Evaluar la combinación
      evaluar_agrupador_simple("variable_combinada", datos_temp) %>%
        mutate(
          variable_original = var,
          combinacion_con = "Segmento de puesto"
        ) %>%
        select(variable_original, combinacion_con, everything())
    })
    
    # Filtrar y ordenar resultados
    resultados_combinaciones <- resultados_combinaciones %>%
      filter(!is.na(eta_squared)) %>%
      mutate(
        score_combinacion = eta_squared * (1 - cv_promedio) * (1 - (grupos_n / nrow(datos_grupo)))
      ) %>%
      arrange(desc(score_combinacion))
    
    if (nrow(resultados_combinaciones) > 0) {
      cat("  Top 5 combinaciones con 'Segmento de puesto':\n")
      for (i in 1:min(5, nrow(resultados_combinaciones))) {
        cat("    ", i, ".", resultados_combinaciones$variable_original[i], 
            " (eta²:", round(resultados_combinaciones$eta_squared[i], 4),
            ", score:", round(resultados_combinaciones$score_combinacion[i], 3), ")\n")
      }
      
      # Guardar resultados de combinaciones
      write_xlsx(resultados_combinaciones, 
                 file.path(grupo_dir, "evaluacion_combinaciones_segmento_puesto.xlsx"))
      
      # Gráfico de comparación de combinaciones
      p_combinaciones <- ggplot(head(resultados_combinaciones, 10), 
                                aes(x = reorder(variable_original, eta_squared), y = eta_squared)) +
        geom_col(fill = "coral", alpha = 0.8) +
        geom_text(aes(label = round(eta_squared, 3)), 
                  hjust = -0.1, size = 3) +
        coord_flip() +
        labs(title = paste("Combinaciones con 'Segmento de puesto' -", grupo_nombre),
             subtitle = "Eta-squared de variables combinadas con Segmento de puesto",
             x = "Variable Original",
             y = "Eta-squared (combinación)") +
        theme_minimal()
      
      ggsave(file.path(grupo_dir, "combinaciones_segmento_puesto.png"), 
             p_combinaciones, width = 12, height = 8, dpi = 300)
      
      return(resultados_combinaciones)
    } else {
      cat("  No se pudieron evaluar combinaciones.\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("  Error en evaluación de combinaciones:", e$message, "\n")
    return(NULL)
  })
}

# ======================================================================
# ANÁLISIS DE CLUSTERING CON MÉTODO DEL CODO Y PCA
# ======================================================================

analizar_clustering <- function(datos_grupo, grupo_nombre, grupo_dir) {
  cat("\nREALIZANDO ANÁLISIS DE CLUSTERING...\n")
  
  tryCatch({
    # Preparar datos para clustering (solo variables numéricas y dummies categóricas)
    datos_numericos <- datos_grupo %>%
      select(`Días cobertura con capacitación`) %>%
      scale()
    
    # 1. MÉTODO DEL CODO PARA DETERMINAR K ÓPTIMO
    wss <- sapply(1:10, function(k) {
      kmeans(datos_numericos, centers = k, nstart = 25)$tot.withinss
    })
    
    # Gráfico del método del codo
    p_codo <- ggplot(data.frame(k = 1:10, wss = wss), aes(x = k, y = wss)) +
      geom_line(color = "steelblue", size = 1.2) +
      geom_point(color = "steelblue", size = 3) +
      geom_vline(xintercept = which.min(diff(wss)/wss[-length(wss)]) + 1, 
                 linetype = "dashed", color = "red", alpha = 0.7) +
      labs(title = paste("Método del Codo para K-Means -", grupo_nombre),
           subtitle = "Determinación del número óptimo de clusters",
           x = "Número de Clusters (k)",
           y = "Suma de Cuadrados Intra-cluster (WSS)") +
      theme_minimal()
    
    ggsave(file.path(grupo_dir, "metodo_codo_clustering.png"), 
           p_codo, width = 10, height = 6, dpi = 300)
    
    # Determinar k óptimo (punto de inflexión)
    k_optimo <- which.min(diff(wss)/wss[-length(wss)]) + 1
    cat("  Número óptimo de clusters sugerido:", k_optimo, "\n")
    
    # 2. APLICAR K-MEANS CON K ÓPTIMO
    set.seed(123)
    modelo_kmeans <- kmeans(datos_numericos, centers = k_optimo, nstart = 25)
    
    # Añadir clusters a los datos
    datos_grupo$cluster_kmeans <- as.factor(modelo_kmeans$cluster)
    
    # 3. GUARDAR DATOS CON CLASIFICACIÓN DE CLUSTERS
    datos_con_clusters <- datos_grupo %>%
      mutate(
        cluster_kmeans = as.character(cluster_kmeans),
        grupo_cluster = paste0(grupo_nombre, "_C", cluster_kmeans)
      )
    
    # Guardar archivo con clasificación completa
    archivo_clasificacion <- file.path(grupo_dir, "datos_clasificados_clusters.xlsx")
    write_xlsx(datos_con_clusters, archivo_clasificacion)
    cat("  Datos clasificados por cluster guardados en:", archivo_clasificacion, "\n")
    
    # 4. ANÁLISIS DE LOS CLUSTERS
    analisis_clusters <- datos_grupo %>%
      group_by(cluster_kmeans) %>%
      summarise(
        n = n(),
        media_dias = mean(`Días cobertura con capacitación`, na.rm = TRUE),
        mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
        sd_dias = sd(`Días cobertura con capacitación`, na.rm = TRUE),
        min_dias = min(`Días cobertura con capacitación`, na.rm = TRUE),
        max_dias = max(`Días cobertura con capacitación`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(mediana_dias)
    
    # 5. GRÁFICO DE DISTRIBUCIÓN POR CLUSTER
    p_clusters <- ggplot(datos_grupo, aes(x = cluster_kmeans, y = `Días cobertura con capacitación`)) +
      geom_boxplot(aes(fill = cluster_kmeans), alpha = 0.7) +
      stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "darkred") +
      labs(title = paste("Distribución por Cluster K-Means -", grupo_nombre),
           subtitle = paste("k =", k_optimo, "clusters óptimos"),
           x = "Cluster",
           y = "Días de Cobertura") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggsave(file.path(grupo_dir, "distribucion_clusters.png"), 
           p_clusters, width = 10, height = 6, dpi = 300)
    
    # 6. ANÁLISIS DE SILUETA
    distancia <- dist(datos_numericos)
    silhouette_score <- silhouette(modelo_kmeans$cluster, distancia)
    
    # Gráfico de silueta
    p_silueta <- fviz_silhouette(silhouette_score) +
      labs(title = paste("Análisis de Silueta -", grupo_nombre),
           subtitle = paste("Silueta promedio:", round(mean(silhouette_score[, 3]), 3))) +
      theme_minimal()
    
    ggsave(file.path(grupo_dir, "analisis_silueta.png"), 
           p_silueta, width = 10, height = 6, dpi = 300)
    
    # 7. GUARDAR ESTADÍSTICAS DETALLADAS DE CLUSTERS
    estadisticas_detalladas <- datos_grupo %>%
      group_by(cluster_kmeans) %>%
      summarise(
        n = n(),
        proporcion = round(n() / nrow(datos_grupo) * 100, 1),
        media_dias = round(mean(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        mediana_dias = round(median(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        sd_dias = round(sd(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        min_dias = round(min(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        max_dias = round(max(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        q25 = round(quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE), 1),
        q75 = round(quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE), 1),
        goal_sugerido = round(quantile(`Días cobertura con capacitación`, 0.4, na.rm = TRUE)),
        cv = round(sd_dias / media_dias * 100, 1),
        .groups = 'drop'
      ) %>%
      mutate(
        grupo = grupo_nombre,
        cluster_id = paste0(grupo_nombre, "_C", cluster_kmeans)
      )
    
    write_xlsx(estadisticas_detalladas, 
               file.path(grupo_dir, "estadisticas_detalladas_clusters.xlsx"))
    
    # 8. GUARDAR CENTROIDES PARA FUTURA CLASIFICACIÓN
    centroides <- as.data.frame(modelo_kmeans$centers)
    centroides$cluster <- rownames(centroides)
    centroides$grupo <- grupo_nombre
    
    write_xlsx(centroides, file.path(grupo_dir, "centroides_clusters.xlsx"))
    
    cat("  Clustering completado con", k_optimo, "clusters\n")
    cat("  Coeficiente de silueta promedio:", round(mean(silhouette_score[, 3]), 3), "\n")
    cat("  Centroides guardados para futura clasificación\n")
    
    return(list(
      analisis = analisis_clusters,
      modelo = modelo_kmeans,
      k_optimo = k_optimo,
      silhouette_score = mean(silhouette_score[, 3]),
      datos_con_clusters = datos_con_clusters,
      estadisticas = estadisticas_detalladas,
      centroides = centroides
    ))
    
  }, error = function(e) {
    cat("  Error en análisis de clustering:", e$message, "\n")
    return(NULL)
  })
}

# ======================================================================
# MÉTODO 1: ÁRBOLES DE DECISIÓN (SEGMENTACIÓN SUPERVISADA)
# ======================================================================

crear_segmentos_arbol <- function(datos_grupo, grupo_nombre, grupo_dir) {
  cat("\nMÉTODO 1: CREANDO SEGMENTOS CON ÁRBOL DE DECISIÓN...\n")
  
  tryCatch({
    # Preparar datos para el árbol
    datos_arbol <- datos_grupo %>%
      select(-Grupo)
    
    # Excluir variables con más de 20 categorías
    n_categorias <- sapply(datos_arbol, function(x) if(is.factor(x)) length(unique(x)) else 0)
    vars_muchos_niveles <- names(n_categorias[n_categorias > 20])
    
    if (length(vars_muchos_niveles) > 0) {
      cat("  Excluyendo variables con >20 categorías:", paste(vars_muchos_niveles, collapse = ", "), "\n")
      datos_arbol <- datos_arbol %>%
        select(-all_of(vars_muchos_niveles))
    }
    
    # Crear árbol de decisión
    set.seed(123)
    formula_arbol <- as.formula("`Días cobertura con capacitación` ~ .")
    arbol <- rpart(formula_arbol, 
                   data = datos_arbol, 
                   method = "anova",
                   control = rpart.control(
                     minsplit = 10,
                     minbucket = 5,
                     cp = 0.01,
                     maxdepth = 5
                   ))
    
    # Obtener segmentos
    datos_grupo$segmento_arbol <- as.factor(arbol$where)
    
    # Análisis de segmentos del árbol
    analisis_segmentos <- datos_grupo %>%
      group_by(segmento_arbol) %>%
      summarise(
        n = n(),
        media_dias = mean(`Días cobertura con capacitación`, na.rm = TRUE),
        mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
        sd_dias = sd(`Días cobertura con capacitación`, na.rm = TRUE),
        min_dias = min(`Días cobertura con capacitación`, na.rm = TRUE),
        max_dias = max(`Días cobertura con capacitación`, na.rm = TRUE),
        q1 = quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE),
        q3 = quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE),
        goal_mediana = round(mediana_dias),
        goal_p75 = round(quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE)),
        goal_p25 = round(quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      arrange(mediana_dias)
    
    # Calcular métricas de calidad del árbol
    r2_arbol <- 1 - sum((datos_grupo$`Días cobertura con capacitación` - predict(arbol))^2) / 
      sum((datos_grupo$`Días cobertura con capacitación` - mean(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE))^2)
    
    # Visualizar el árbol
    png(file.path(grupo_dir, "arbol_decision_segmentos.png"), width = 1200, height = 800)
    rpart.plot(arbol, 
               main = paste("Segmentación por Árbol de Decisión -", grupo_nombre),
               sub = paste("R² =", round(r2_arbol, 3)),
               box.palette = "Blues",
               shadow.col = "gray",
               nn = TRUE)
    dev.off()
    
    # Gráfico de segmentos del árbol
    p_segmentos <- ggplot(analisis_segmentos, aes(x = reorder(segmento_arbol, mediana_dias), y = mediana_dias)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "darkred") +
      geom_text(aes(label = paste("n =", n)), vjust = -0.5, size = 3) +
      geom_text(aes(label = paste("Goal:", goal_mediana)), vjust = 1.5, size = 3, color = "white") +
      labs(title = paste("Segmentos del Árbol de Decisión -", grupo_nombre),
           x = "Segmento (Nodo Terminal)",
           y = "Días de Cobertura (mediana)") +
      theme_minimal()
    
    ggsave(file.path(grupo_dir, "segmentos_arbol.png"), p_segmentos, width = 10, height = 6, dpi = 300)
    
    # Guardar análisis
    write_xlsx(analisis_segmentos, file.path(grupo_dir, "segmentos_arbol.xlsx"))
    
    cat("  Árbol de decisión creado con", nrow(analisis_segmentos), "segmentos\n")
    cat("  R² del árbol:", round(r2_arbol, 3), "\n")
    
    return(list(
      analisis = analisis_segmentos,
      arbol = arbol,
      r2 = r2_arbol
    ))
    
  }, error = function(e) {
    cat("  Error en árbol de decisión:", e$message, "\n")
    return(NULL)
  })
}

# ======================================================================
# MÉTODO 2: MODELO DE REGRESIÓN + SEGMENTACIÓN POR PERCENTILES
# ======================================================================

crear_segmentos_regresion <- function(datos_grupo, grupo_nombre, grupo_dir) {
  cat("\nMÉTODO 2: CREANDO SEGMENTOS CON MODELO DE REGRESIÓN...\n")
  
  tryCatch({
    datos_modelo <- datos_grupo
    
    # Análisis de varianza para cada variable categórica
    vars_categoricas <- names(datos_modelo)[sapply(datos_modelo, is.factor)]
    vars_categoricas <- setdiff(vars_categoricas, c("Grupo", "segmento_arbol"))
    
    # Calcular eta-squared para cada variable
    eta_squared_vars <- purrr::map_dfr(vars_categoricas, function(var) {
      if (length(unique(datos_modelo[[var]])) > 1) {
        modelo <- aov(as.formula(paste("`Días cobertura con capacitación` ~", var)), data = datos_modelo)
        resumen <- summary(modelo)
        ss_between <- resumen[[1]]$`Sum Sq`[1]
        ss_total <- sum(resumen[[1]]$`Sum Sq`)
        eta_sq <- ss_between / ss_total
      } else {
        eta_sq <- 0
      }
      data.frame(variable = var, eta_squared = eta_sq)
    }) %>%
      arrange(desc(eta_squared))
    
    # Seleccionar top 5 variables
    top_vars <- head(eta_squared_vars$variable, 5)
    cat("  Variables más importantes para el modelo:", paste(top_vars, collapse = ", "), "\n")
    
    # Crear fórmula para el modelo
    formula_modelo <- as.formula(paste("`Días cobertura con capacitación` ~", 
                                       paste(top_vars, collapse = " + ")))
    
    # Modelo lineal
    modelo_lm <- lm(formula_modelo, data = datos_modelo)
    
    # Predicciones y residuos
    datos_modelo$prediccion <- predict(modelo_lm, newdata = datos_modelo)
    datos_modelo$residuo <- datos_modelo$`Días cobertura con capacitación` - datos_modelo$prediccion
    
    # Segmentar por percentiles de residuos
    datos_modelo$segmento_residuo <- cut(datos_modelo$residuo,
                                         breaks = quantile(datos_modelo$residuo, 
                                                           probs = seq(0, 1, 0.2), 
                                                           na.rm = TRUE),
                                         labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"),
                                         include.lowest = TRUE)
    
    # Análisis de segmentos por residuos
    analisis_residuos <- datos_modelo %>%
      group_by(segmento_residuo) %>%
      summarise(
        n = n(),
        media_residuo = mean(residuo, na.rm = TRUE),
        media_real = mean(`Días cobertura con capacitación`, na.rm = TRUE),
        media_pred = mean(prediccion, na.rm = TRUE),
        mediana_real = median(`Días cobertura con capacitación`, na.rm = TRUE),
        sd_real = sd(`Días cobertura con capacitación`, na.rm = TRUE),
        goal_ajustado = round(media_pred + media_residuo),
        .groups = 'drop'
      ) %>%
      arrange(media_residuo)
    
    # Métricas del modelo
    r2_modelo <- summary(modelo_lm)$r.squared
    rmse <- sqrt(mean((datos_modelo$residuo)^2, na.rm = TRUE))
    
    cat("  R² del modelo:", round(r2_modelo, 3), "\n")
    cat("  RMSE:", round(rmse, 2), "días\n")
    
    # Gráfico de residuos vs predicciones
    p_residuos <- ggplot(datos_modelo, aes(x = prediccion, y = residuo, color = segmento_residuo)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = paste("Análisis de Residuos -", grupo_nombre),
           subtitle = paste("R² =", round(r2_modelo, 3), "| RMSE =", round(rmse, 2), "días"),
           x = "Días Predichos",
           y = "Residuo (Real - Predicho)",
           color = "Segmento por Residuo") +
      theme_minimal()
    
    ggsave(file.path(grupo_dir, "modelo_residuos.png"), p_residuos, width = 10, height = 6, dpi = 300)
    
    # Gráfico de segmentos de residuos
    p_segmentos_residuos <- ggplot(analisis_residuos, aes(x = segmento_residuo, y = media_real)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_errorbar(aes(ymin = media_real - sd_real/sqrt(n), 
                        ymax = media_real + sd_real/sqrt(n)), 
                    width = 0.2, color = "darkred") +
      geom_text(aes(label = paste("n =", n)), vjust = -0.5, size = 3) +
      geom_text(aes(label = paste("Goal:", goal_ajustado)), vjust = 1.5, size = 3, color = "white") +
      labs(title = paste("Segmentos por Residuos del Modelo -", grupo_nombre),
           x = "Segmento por Residuo",
           y = "Días de Cobertura (media)") +
      theme_minimal()
    
    ggsave(file.path(grupo_dir, "segmentos_residuos.png"), p_segmentos_residuos, width = 10, height = 6, dpi = 300)
    
    # Guardar análisis
    write_xlsx(analisis_residuos, file.path(grupo_dir, "segmentos_modelo.xlsx"))
    
    return(list(
      analisis = analisis_residuos,
      modelo = modelo_lm,
      r2 = r2_modelo,
      rmse = rmse
    ))
    
  }, error = function(e) {
    cat("  Error en modelo de regresión:", e$message, "\n")
    return(NULL)
  })
}

# ======================================================================
# MÉTODO 3: SEGMENTACIÓN POR PERCENTILES DENTRO DE CATEGORÍAS
# ======================================================================

crear_segmentos_percentiles <- function(datos_grupo, grupo_nombre, grupo_dir, mejor_agrupador) {
  cat("\nMÉTODO 3: CREANDO SEGMENTOS POR PERCENTILES...\n")
  
  tryCatch({
    if (missing(mejor_agrupador) || is.null(mejor_agrupador)) {
      mejor_agrupador <- "DescripcionCC"
    }
    
    # Crear segmentos por percentiles
    datos_percentiles <- datos_grupo %>%
      group_by_at(mejor_agrupador) %>%
      mutate(
        percentil_dentro = ntile(`Días cobertura con capacitación`, 4),
        segmento_percentil = case_when(
          percentil_dentro == 1 ~ "Q1 (25% inferior)",
          percentil_dentro == 2 ~ "Q2 (25-50%)",
          percentil_dentro == 3 ~ "Q3 (50-75%)",
          percentil_dentro == 4 ~ "Q4 (75% superior)"
        )
      ) %>%
      ungroup()
    
    # Análisis de segmentos por percentiles
    analisis_percentiles <- datos_percentiles %>%
      group_by(!!sym(mejor_agrupador), segmento_percentil) %>%
      summarise(
        n = n(),
        media_dias = mean(`Días cobertura con capacitación`, na.rm = TRUE),
        mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
        sd_dias = sd(`Días cobertura con capacitación`, na.rm = TRUE),
        q1 = quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE),
        q3 = quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE),
        goal_mediana = round(mediana_dias),
        goal_ambicioso = round(quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE)),
        .groups = 'drop'
      )
    
    # Análisis agregado por percentil
    analisis_percentiles_agregado <- datos_percentiles %>%
      group_by(segmento_percentil) %>%
      summarise(
        n = n(),
        media_dias = mean(`Días cobertura con capacitación`, na.rm = TRUE),
        mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
        sd_dias = sd(`Días cobertura con capacitación`, na.rm = TRUE),
        min_dias = min(`Días cobertura con capacitación`, na.rm = TRUE),
        max_dias = max(`Días cobertura con capacitación`, na.rm = TRUE),
        goal_recomendado = round(quantile(`Días cobertura con capacitación`, 0.4, na.rm = TRUE)),
        .groups = 'drop'
      )
    
    # Gráfico de distribución por percentiles
    p_percentiles <- ggplot(datos_percentiles, aes(x = segmento_percentil, y = `Días cobertura con capacitación`)) +
      geom_boxplot(fill = "lightblue", alpha = 0.7) +
      stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "darkred") +
      labs(title = paste("Distribución por Percentiles -", grupo_nombre),
           subtitle = paste("Agrupador:", mejor_agrupador),
           x = "Segmento por Percentil",
           y = "Días de Cobertura") +
      theme_minimal()
    
    ggsave(file.path(grupo_dir, "segmentos_percentiles.png"), p_percentiles, width = 10, height = 6, dpi = 300)
    
    # Gráfico de metas por percentil
    p_metas_percentiles <- ggplot(analisis_percentiles_agregado, aes(x = reorder(segmento_percentil, -goal_recomendado), y = goal_recomendado)) +
      geom_col(fill = "steelblue", alpha = 0.8) +
      geom_text(aes(label = paste("Meta:", goal_recomendado, "días")), 
                vjust = 1.5, size = 4, color = "white") +
      geom_text(aes(label = paste("n =", n)), vjust = -0.5, size = 3.5) +
      labs(title = paste("Metas Recomendadas por Percentil -", grupo_nombre),
           subtitle = "Meta basada en percentil 40 de cada segmento",
           x = "Segmento por Percentil",
           y = "Meta Recomendada (días)") +
      theme_minimal()
    
    ggsave(file.path(grupo_dir, "metas_percentiles.png"), p_metas_percentiles, width = 10, height = 6, dpi = 300)
    
    # Guardar análisis
    write_xlsx(list(
      por_categoria = analisis_percentiles,
      agregado = analisis_percentiles_agregado
    ), file.path(grupo_dir, "segmentos_percentiles.xlsx"))
    
    cat("  Segmentación creada con 4 percentiles por categoría\n")
    cat("  Total segmentos:", nrow(analisis_percentiles), "\n")
    
    return(list(
      analisis_categoria = analisis_percentiles,
      analisis_agregado = analisis_percentiles_agregado
    ))
    
  }, error = function(e) {
    cat("  Error en segmentación por percentiles:", e$message, "\n")
    return(NULL)
  })
}

# ======================================================================
# FUNCIÓN PARA CREAR GRÁFICO DEL MEJOR AGRUPADOR CON METAS
# ======================================================================

crear_grafico_mejor_agrupador <- function(datos_grupo, mejor_agrupador, grupo_nombre, grupo_dir) {
  cat("\nCREANDO GRÁFICO DEL MEJOR AGRUPADOR CON METAS...\n")
  
  tryCatch({
    # Calcular estadísticas por el mejor agrupador
    datos_agrupados <- datos_grupo %>%
      group_by_at(mejor_agrupador) %>%
      summarise(
        n = n(),
        media = mean(`Días cobertura con capacitación`, na.rm = TRUE),
        mediana = median(`Días cobertura con capacitación`, na.rm = TRUE),
        q25 = quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE),
        q75 = quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE),
        sd = sd(`Días cobertura con capacitación`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(n >= 5) %>%
      arrange(mediana)
    
    if (nrow(datos_agrupados) > 0) {
      # Calcular meta recomendada (percentil 40)
      datos_agrupados <- datos_agrupados %>%
        mutate(
          meta_recomendada = round(0.6 * q25 + 0.4 * mediana)  # Combinación conservadora
        )
      
      # Gráfico del mejor agrupador
      p_mejor_agrupador <- ggplot(datos_agrupados, 
                                  aes(x = reorder(!!sym(mejor_agrupador), mediana), y = mediana)) +
        geom_col(fill = "steelblue", alpha = 0.7, width = 0.6) +
        geom_errorbar(aes(ymin = q25, ymax = q75), width = 0.2, color = "darkred", size = 0.8) +
        geom_point(aes(y = meta_recomendada), color = "darkgreen", size = 3, shape = 17) +
        geom_text(aes(label = paste("n =", n)), 
                  vjust = -0.5, size = 2.8, color = "black") +
        geom_text(aes(y = meta_recomendada, label = paste("Meta:", meta_recomendada)), 
                  vjust = -1.5, size = 2.8, color = "darkgreen") +
        labs(title = paste("Mejor Agrupador:", mejor_agrupador, "-", grupo_nombre),
             subtitle = "Barras: medianas | Líneas: Q25-Q75 | Triángulos: metas recomendadas",
             x = mejor_agrupador,
             y = "Días de Cobertura") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      ggsave(file.path(grupo_dir, "mejor_agrupador_metas.png"), 
             p_mejor_agrupador, width = 14, height = 8, dpi = 300)
      
      # Guardar datos del mejor agrupador
      write_xlsx(datos_agrupados, 
                 file.path(grupo_dir, "analisis_mejor_agrupador.xlsx"))
      
      cat("  Gráfico del mejor agrupador creado\n")
      return(datos_agrupados)
    } else {
      cat("  No hay suficientes datos para crear gráfico del mejor agrupador\n")
      return(NULL)
    }
    
  }, error = function(e) {
    cat("  Error creando gráfico del mejor agrupador:", e$message, "\n")
    return(NULL)
  })
}

# ======================================================================
# FUNCIÓN PARA CREAR ARCHIVO MAESTRO DE CLASIFICACIÓN POR CLUSTER
# ======================================================================

crear_archivo_maestro_clasificacion <- function(datos_limpieza, resultados_detalle, output_dir) {
  cat("\n", strrep("=", 60))
  cat("\nCREANDO ARCHIVO MAESTRO DE CLASIFICACIÓN POR CLUSTER")
  cat("\n", strrep("=", 60), "\n")
  
  # Crear directorio específico para clasificación maestra
  clasificacion_dir <- file.path(output_dir, "Clasificacion_Maestra")
  if (!dir.exists(clasificacion_dir)) {
    dir.create(clasificacion_dir, recursive = TRUE)
  }
  
  # Inicializar lista para datos combinados
  datos_combinados <- data.frame()
  
  # Para cada grupo, aplicar K-Means nuevamente para consistencia
  for (grupo_nombre in names(resultados_detalle)) {
    cat("  Procesando grupo:", grupo_nombre, "\n")
    
    # Filtrar datos del grupo
    datos_grupo <- datos_limpieza %>%
      filter(Grupo == grupo_nombre)
    
    if (nrow(datos_grupo) < 10) {
      cat("    Grupo tiene menos de 10 observaciones. Omitiendo.\n")
      next
    }
    
    # Preparar datos para clustering
    datos_numericos <- datos_grupo %>%
      select(`Días cobertura con capacitación`) %>%
      scale()
    
    # Determinar k óptimo (basado en resultados anteriores)
    # Como ya sabemos que todos los grupos tienen k=2 según el análisis
    k_optimo <- 2
    
    # Aplicar K-Means con misma semilla para consistencia
    set.seed(123)
    modelo_kmeans <- kmeans(datos_numericos, centers = k_optimo, nstart = 25)
    
    # Añadir clasificación a los datos
    datos_grupo <- datos_grupo %>%
      mutate(
        cluster_kmeans = as.character(modelo_kmeans$cluster),
        cluster_id = paste0(grupo_nombre, "_C", cluster_kmeans),
        grupo_cluster = paste0(grupo_nombre, "_Cluster_", cluster_kmeans)
      )
    
    # Calcular estadísticas por cluster para este grupo
    stats_cluster <- datos_grupo %>%
      group_by(cluster_id) %>%
      summarise(
        n_cluster = n(),
        media_cluster = round(mean(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        mediana_cluster = round(median(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        sd_cluster = round(sd(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        q25_cluster = round(quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE), 1),
        q75_cluster = round(quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE), 1),
        goal_sugerido = round(quantile(`Días cobertura con capacitación`, 0.4, na.rm = TRUE)),
        .groups = 'drop'
      )
    
    # Unir estadísticas a los datos
    datos_grupo <- datos_grupo %>%
      left_join(stats_cluster, by = "cluster_id")
    
    # Añadir a los datos combinados
    datos_combinados <- bind_rows(datos_combinados, datos_grupo)
    
    cat("    ", nrow(datos_grupo), "registros clasificados\n")
  }
  
  if (nrow(datos_combinados) > 0) {
    # Crear resumen por grupo y cluster
    resumen_clasificacion <- datos_combinados %>%
      group_by(Grupo, cluster_id, cluster_kmeans) %>%
      summarise(
        n = n(),
        proporcion = round(n() / nrow(datos_combinados) * 100, 2),
        media_dias = round(mean(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        mediana_dias = round(median(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        sd_dias = round(sd(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        min_dias = round(min(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        max_dias = round(max(`Días cobertura con capacitación`, na.rm = TRUE), 1),
        goal_asignado = first(goal_sugerido),
        .groups = 'drop'
      ) %>%
      arrange(Grupo, mediana_dias)
    
    # Crear tabla de referencia para asignación rápida
    tabla_referencia <- resumen_clasificacion %>%
      select(Grupo, cluster_id, cluster_kmeans, goal_asignado, n, mediana_dias) %>%
      mutate(
        Descripcion = case_when(
          cluster_kmeans == "1" ~ "Cluster Bajo",
          cluster_kmeans == "2" ~ "Cluster Alto",
          TRUE ~ "Cluster Otro"
        )
      ) %>%
      rename(
        `Meta (días)` = goal_asignado,
        `Mediana (días)` = mediana_dias,
        `Registros` = n
      )
    
    # Guardar archivos
    archivo_maestro <- file.path(clasificacion_dir, "MAESTRO_Clasificacion_Clusters.xlsx")
    
    # Crear lista de hojas
    hojas_excel <- list(
      "Datos_Clasificados" = datos_combinados,
      "Resumen_Clasificacion" = resumen_clasificacion,
      "Tabla_Referencia_Metas" = tabla_referencia,
      "Instrucciones" = data.frame(
        Instruccion = c(
          "1. Para asignar metas a nuevos reclutamientos:",
          "   - Identifique el Grupo (SUCURSAL, COBRANZA, PLAZA, ODG)",
          "   - Consulte la hoja 'Tabla_Referencia_Metas'",
          "   - Asigne la meta correspondiente al cluster",
          "",
          "2. Metodología de clusters:",
          "   - Cluster 1: Perfiles con menor tiempo de cobertura",
          "   - Cluster 2: Perfiles con mayor tiempo de cobertura",
          "",
          "3. Para reclasificar nuevos datos:",
          "   - Use los centroides guardados en cada carpeta de grupo",
          "   - Calcule la distancia a cada centroide",
          "   - Asigne al cluster más cercano"
        )
      )
    )
    
    write_xlsx(hojas_excel, archivo_maestro)
    
    # Guardar tabla de referencia separada
    write_xlsx(tabla_referencia, 
               file.path(output_dir, "TABLA_REFERENCIA_METAS_CLUSTER.xlsx"))
    
    cat("\n  Archivo maestro creado:", archivo_maestro, "\n")
    cat("  Tabla de referencia guardada: TABLA_REFERENCIA_METAS_CLUSTER.xlsx\n")
    cat("  Total registros clasificados:", nrow(datos_combinados), "\n")
    
    # Mostrar resumen en consola
    cat("\nRESUMEN DE CLASIFICACIÓN:\n")
    print(resumen_clasificacion %>% select(Grupo, cluster_id, n, mediana_dias, goal_asignado))
    
    return(list(
      datos_combinados = datos_combinados,
      resumen_clasificacion = resumen_clasificacion,
      tabla_referencia = tabla_referencia
    ))
  } else {
    cat("  No se pudo crear la clasificación maestra.\n")
    return(NULL)
  }
}

# ======================================================================
# FUNCIÓN PARA ASIGNAR METAS FUTURAS BASADAS EN CLUSTERS
# ======================================================================

asignar_metas_por_cluster <- function(nuevos_datos, output_dir) {
  cat("\nASIGNANDO METAS POR CLUSTER PARA NUEVOS DATOS...\n")
  
  # Cargar tabla de referencia
  archivo_referencia <- file.path(output_dir, "TABLA_REFERENCIA_METAS_CLUSTER.xlsx")
  
  if (!file.exists(archivo_referencia)) {
    cat("  ERROR: No se encontró el archivo de referencia de clusters.\n")
    cat("  Ejecute primero el análisis completo para crear la clasificación.\n")
    return(NULL)
  }
  
  tabla_referencia <- read_excel(archivo_referencia)
  
  # Verificar que los nuevos datos tengan la columna Grupo
  if (!"Grupo" %in% colnames(nuevos_datos)) {
    cat("  ERROR: Los nuevos datos no tienen la columna 'Grupo'.\n")
    return(NULL)
  }
  
  # Para cada registro, asignar meta basada en grupo
  # En una implementación real, aquí clasificarías cada registro en un cluster
  # Pero para simplificar, asignaremos la meta del cluster más común por grupo
  
  # Obtener meta por defecto para cada grupo (cluster más común)
  metas_por_grupo <- tabla_referencia %>%
    group_by(Grupo) %>%
    slice_max(Registros, n = 1) %>%
    select(Grupo, `Meta (días)`)
  
  # Asignar metas a los nuevos datos
  nuevos_datos_con_meta <- nuevos_datos %>%
    left_join(metas_por_grupo, by = "Grupo") %>%
    rename(meta_asignada = `Meta (días)`)
  
  cat("  Metas asignadas a", nrow(nuevos_datos_con_meta), "registros\n")
  
  # Mostrar resumen
  resumen_asignacion <- nuevos_datos_con_meta %>%
    group_by(Grupo, meta_asignada) %>%
    summarise(
      n = n(),
      .groups = 'drop'
    )
  
  cat("\n  Resumen de asignación:\n")
  for (i in 1:nrow(resumen_asignacion)) {
    cat(sprintf("    %s: %d días (%d registros)\n",
                resumen_asignacion$Grupo[i],
                resumen_asignacion$meta_asignada[i],
                resumen_asignacion$n[i]))
  }
  
  return(nuevos_datos_con_meta)
}

# ======================================================================
# FUNCIÓN PRINCIPAL PARA ANALIZAR CADA GRUPO
# ======================================================================

analizar_grupo <- function(grupo_nombre, datos_completos) {
  cat("\n", strrep("=", 50), "\n")
  cat("ANALIZANDO GRUPO:", grupo_nombre, "\n")
  cat(strrep("=", 50), "\n")
  
  # Filtrar datos para el grupo actual
  datos_grupo <- datos_completos %>%
    filter(Grupo == grupo_nombre)
  
  if (nrow(datos_grupo) < 10) {
    cat("Grupo", grupo_nombre, "tiene menos de 10 observaciones. Análisis omitido.\n")
    return(NULL)
  }
  
  # Crear directorio específico para el grupo
  grupo_dir <- file.path(output_dir, grupo_nombre)
  if (!dir.exists(grupo_dir)) {
    dir.create(grupo_dir, recursive = TRUE)
  }
  
  # 1. EVALUAR AGRUPADORES CATEGÓRICOS INDIVIDUALES
  cat("\n1. EVALUANDO AGRUPADORES CATEGÓRICOS INDIVIDUALES...\n")
  
  agrupadores <- setdiff(columnas_interes, c("Días cobertura con capacitación", "Grupo"))
  resultados <- purrr::map_dfr(agrupadores, function(agrupador) {
    evaluar_agrupador_simple(agrupador, datos_grupo)
  })
  
  resultados <- resultados %>%
    filter(!is.na(eta_squared)) %>%
    mutate(
      score = eta_squared * (1 - cv_promedio) * (1 - (grupos_n / nrow(datos_grupo)))
    ) %>%
    arrange(desc(score))
  
  mejor_agrupador <- ifelse(nrow(resultados) > 0, resultados$agrupador[1], "DescripcionCC")
  cat("  Mejor agrupador individual:", mejor_agrupador, "\n")
  cat("  Eta-squared:", round(resultados$eta_squared[1], 4), "\n")
  
  # Mostrar top 10 agrupadores
  cat("  Top 10 agrupadores individuales:\n")
  top_n <- min(10, nrow(resultados))
  for (i in 1:top_n) {
    cat("    ", i, ".", resultados$agrupador[i], 
        " (eta²:", round(resultados$eta_squared[i], 4),
        ", score:", round(resultados$score[i], 3), ")\n")
  }
  
  # Guardar resultados individuales
  write_xlsx(resultados, file.path(grupo_dir, "evaluacion_agrupadores_individuales.xlsx"))
  
  # 2. EVALUAR COMBINACIONES CON SEGMENTO DE PUESTO
  resultado_combinaciones <- evaluar_combinaciones_variables(datos_grupo, grupo_nombre, grupo_dir)
  
  # 3. ANÁLISIS DE CLUSTERING
  resultado_clustering <- analizar_clustering(datos_grupo, grupo_nombre, grupo_dir)
  
  # 4. APLICAR LOS 3 MÉTODOS ALTERNATIVOS
  
  # Método 1: Árbol de decisión
  resultado_arbol <- crear_segmentos_arbol(datos_grupo, grupo_nombre, grupo_dir)
  
  # Método 2: Modelo de regresión
  resultado_modelo <- crear_segmentos_regresion(datos_grupo, grupo_nombre, grupo_dir)
  
  # Método 3: Percentiles
  resultado_percentiles <- crear_segmentos_percentiles(datos_grupo, grupo_nombre, grupo_dir, mejor_agrupador)
  
  # 5. CREAR GRÁFICO DEL MEJOR AGRUPADOR CON METAS
  resultado_grafico_agrupador <- crear_grafico_mejor_agrupador(datos_grupo, mejor_agrupador, grupo_nombre, grupo_dir)
  
  # 6. COMPARAR Y RECOMENDAR EL MEJOR MÉTODO
  cat("\n2. COMPARANDO MÉTODOS Y GENERANDO RECOMENDACIONES...\n")
  
  # Recolectar métricas de cada método
  metricas_metodos <- data.frame(
    Metodo = character(),
    Num_Segmentos = numeric(),
    R2_o_Eta = numeric(),
    CV_Promedio = numeric(),
    Silhouette = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Método tradicional (agrupador categórico individual)
  if (nrow(resultados) > 0) {
    metricas_metodos <- rbind(metricas_metodos, data.frame(
      Metodo = paste("Agrupador Individual:", mejor_agrupador),
      Num_Segmentos = resultados$grupos_n[1],
      R2_o_Eta = resultados$eta_squared[1],
      CV_Promedio = resultados$cv_promedio[1],
      Silhouette = NA
    ))
  }
  
  # Método combinaciones
  if (!is.null(resultado_combinaciones) && nrow(resultado_combinaciones) > 0) {
    metricas_metodos <- rbind(metricas_metodos, data.frame(
      Metodo = paste("Combinación:", resultado_combinaciones$variable_original[1], "+ Segmento de puesto"),
      Num_Segmentos = resultado_combinaciones$grupos_n[1],
      R2_o_Eta = resultado_combinaciones$eta_squared[1],
      CV_Promedio = resultado_combinaciones$cv_promedio[1],
      Silhouette = NA
    ))
  }
  
  # Método árbol
  if (!is.null(resultado_arbol)) {
    cv_arbol <- if (nrow(resultado_arbol$analisis) > 0) {
      mean(resultado_arbol$analisis$sd_dias / resultado_arbol$analisis$media_dias, na.rm = TRUE)
    } else { NA }
    
    metricas_metodos <- rbind(metricas_metodos, data.frame(
      Metodo = "Árbol de Decisión",
      Num_Segmentos = nrow(resultado_arbol$analisis),
      R2_o_Eta = resultado_arbol$r2,
      CV_Promedio = cv_arbol,
      Silhouette = NA
    ))
  }
  
  # Método clustering
  if (!is.null(resultado_clustering)) {
    cv_clustering <- if (nrow(resultado_clustering$analisis) > 0) {
      mean(resultado_clustering$analisis$sd_dias / resultado_clustering$analisis$media_dias, na.rm = TRUE)
    } else { NA }
    
    metricas_metodos <- rbind(metricas_metodos, data.frame(
      Metodo = "Clustering K-Means",
      Num_Segmentos = nrow(resultado_clustering$analisis),
      R2_o_Eta = NA,
      CV_Promedio = cv_clustering,
      Silhouette = resultado_clustering$silhouette_score
    ))
  }
  
  # Método modelo
  if (!is.null(resultado_modelo)) {
    cv_modelo <- if (nrow(resultado_modelo$analisis) > 0) {
      mean(resultado_modelo$analisis$sd_real / resultado_modelo$analisis$media_real, na.rm = TRUE)
    } else { NA }
    
    metricas_metodos <- rbind(metricas_metodos, data.frame(
      Metodo = "Modelo de Regresión",
      Num_Segmentos = nrow(resultado_modelo$analisis),
      R2_o_Eta = resultado_modelo$r2,
      CV_Promedio = cv_modelo,
      Silhouette = NA
    ))
  }
  
  # Método percentiles
  if (!is.null(resultado_percentiles)) {
    cv_percentiles <- mean(resultado_percentiles$analisis_agregado$sd_dias / 
                             resultado_percentiles$analisis_agregado$media_dias, na.rm = TRUE)
    
    metricas_metodos <- rbind(metricas_metodos, data.frame(
      Metodo = "Segmentación por Percentiles",
      Num_Segmentos = nrow(resultado_percentiles$analisis_agregado),
      R2_o_Eta = NA,
      CV_Promedio = cv_percentiles,
      Silhouette = NA
    ))
  }
  
  # Calcular score para cada método (incluyendo silueta para clustering)
  metricas_metodos <- metricas_metodos %>%
    mutate(
      Score = case_when(
        !is.na(Silhouette) ~ Silhouette * (1 - CV_Promedio) * (1 - (Num_Segmentos / nrow(datos_grupo))),
        !is.na(R2_o_Eta) ~ R2_o_Eta * (1 - CV_Promedio) * (1 - (Num_Segmentos / nrow(datos_grupo))),
        TRUE ~ 0.5 * (1 - CV_Promedio) * (1 - (Num_Segmentos / nrow(datos_grupo)))
      )
    ) %>%
    arrange(desc(Score))
  
  # Guardar comparación
  write_xlsx(metricas_metodos, file.path(grupo_dir, "comparacion_metodos.xlsx"))
  
  # Gráfico de comparación de métodos
  p_comparacion <- ggplot(metricas_metodos, aes(x = reorder(Metodo, Score), y = Score, fill = Metodo)) +
    geom_col() +
    geom_text(aes(label = paste("Score:", round(Score, 3))), hjust = -0.1, size = 3) +
    coord_flip() +
    labs(title = paste("Comparación de Métodos de Segmentación -", grupo_nombre),
         x = "Método",
         y = "Score (mayor es mejor)") +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(file.path(grupo_dir, "comparacion_metodos.png"), p_comparacion, width = 12, height = 8, dpi = 300)
  
  # 7. GENERAR RESUMEN EJECUTIVO CON RECOMENDACIÓN
  metodo_recomendado <- metricas_metodos$Metodo[1]
  cat("\nRECOMENDACIÓN FINAL:\n")
  cat("  Método recomendado:", metodo_recomendado, "\n")
  cat("  Score del método:", round(metricas_metodos$Score[1], 3), "\n")
  
  # Determinar goals finales basados en el método recomendado
  if (grepl("Agrupador Individual:", metodo_recomendado)) {
    goals_finales <- datos_grupo %>%
      group_by_at(mejor_agrupador) %>%
      summarise(
        n = n(),
        mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
        goal_recomendado = round(quantile(`Días cobertura con capacitación`, 0.4, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      filter(n >= 5)
    
  } else if (grepl("Combinación:", metodo_recomendado) && !is.null(resultado_combinaciones)) {
    # Extraer la variable de la combinación
    var_combinada <- resultado_combinaciones$variable_original[1]
    datos_grupo_temp <- datos_grupo %>%
      mutate(variable_combinada = interaction(!!sym(var_combinada), `Segmento de puesto`, sep = " | "))
    
    goals_finales <- datos_grupo_temp %>%
      group_by(variable_combinada) %>%
      summarise(
        n = n(),
        mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
        goal_recomendado = round(quantile(`Días cobertura con capacitación`, 0.4, na.rm = TRUE)),
        .groups = 'drop'
      ) %>%
      filter(n >= 5)
    
  } else if (metodo_recomendado == "Árbol de Decisión" && !is.null(resultado_arbol)) {
    goals_finales <- resultado_arbol$analisis %>%
      select(segmento = segmento_arbol, n, mediana_dias = mediana_dias, goal_recomendado = goal_mediana)
    
  } else if (metodo_recomendado == "Clustering K-Means" && !is.null(resultado_clustering)) {
    # Usar las estadísticas detalladas del clustering
    goals_finales <- resultado_clustering$estadisticas %>%
      select(segmento = cluster_id, n, mediana_dias, goal_recomendado = goal_sugerido)
    
  } else if (metodo_recomendado == "Modelo de Regresión" && !is.null(resultado_modelo)) {
    goals_finales <- resultado_modelo$analisis %>%
      select(segmento = segmento_residuo, n, mediana_dias = media_real, goal_recomendado = goal_ajustado)
    
  } else {
    goals_finales <- resultado_percentiles$analisis_agregado %>%
      select(segmento = segmento_percentil, n, mediana_dias, goal_recomendado)
  }
  
  # Guardar goals finales
  write_xlsx(goals_finales, file.path(grupo_dir, "goals_recomendados_finales.xlsx"))
  
  # Gráfico de goals finales
  if (nrow(goals_finales) > 0) {
    p_goals_finales <- ggplot(goals_finales, aes(x = reorder(segmento, goal_recomendado), y = goal_recomendado)) +
      geom_col(fill = "darkgreen", alpha = 0.8) +
      geom_text(aes(label = paste("Goal:", goal_recomendado, "días")), 
                vjust = 1.5, size = 3.5, color = "white") +
      geom_text(aes(label = paste("n =", n)), vjust = -0.5, size = 3) +
      coord_flip() +
      labs(title = paste("Goals Recomendados -", grupo_nombre),
           subtitle = paste("Método:", metodo_recomendado),
           x = "Segmento",
           y = "Meta Recomendada (días)") +
      theme_minimal()
    
    ggsave(file.path(grupo_dir, "goals_finales.png"), p_goals_finales, width = 12, height = 8, dpi = 300)
  }
  
  # 8. CREAR GRÁFICO DE PERCENTILES CON META SUGERIDA
  cat("\n3. CREANDO GRÁFICO DE PERCENTILES CON META SUGERIDA...\n")
  
  tryCatch({
    # Calcular percentiles para el grupo
    percentiles_grupo <- quantile(
      datos_grupo$`Días cobertura con capacitación`,
      probs = c(0.25, 0.5, 0.75, 0.8, 0.9),
      na.rm = TRUE
    )
    
    # Crear dataframe para el gráfico
    datos_percentiles_grafico <- data.frame(
      Percentil = c("P25", "P50", "P75", "P80", "P90"),
      Valor = as.numeric(percentiles_grupo),
      Etiqueta = c("25%", "Mediana\n(50%)", "75%", "80%", "90%")
    )
    
    # Calcular meta sugerida (promedio ponderado)
    meta_sugerida <- round(
      0.25 * percentiles_grupo[1] +  # P25
        0.25 * percentiles_grupo[2] +  # P50
        0.25 * percentiles_grupo[3] +  # P75
        0.15 * percentiles_grupo[4] +  # P80
        0.10 * percentiles_grupo[5]    # P90
    )
    
    # Gráfico de percentiles con meta sugerida
    p_percentiles_meta <- ggplot(datos_percentiles_grafico, aes(x = reorder(Percentil, Valor), y = Valor)) +
      geom_col(fill = "steelblue", alpha = 0.7) +
      geom_hline(yintercept = meta_sugerida, linetype = "dashed", color = "darkred", size = 1.2) +
      geom_text(aes(label = paste(round(Valor, 1), "días")), 
                vjust = -0.5, size = 4, fontface = "bold") +
      annotate("text", x = 3, y = meta_sugerida, 
               label = paste("Meta Sugerida:", meta_sugerida, "días"),
               vjust = -0.5, size = 4.5, color = "darkred", fontface = "bold") +
      labs(title = paste("Percentiles de Días de Cobertura -", grupo_nombre),
           subtitle = "Meta sugerida basada en ponderación 25-25-25-15-10",
           x = "Percentil",
           y = "Días de Cobertura") +
      theme_minimal() +
      theme(plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.title = element_text(size = 12, face = "bold"),
            axis.text = element_text(size = 11))
    
    ggsave(file.path(grupo_dir, "percentiles_meta_sugerida.png"), 
           p_percentiles_meta, width = 12, height = 8, dpi = 300)
    
    cat("  Meta sugerida para", grupo_nombre, ":", meta_sugerida, "días\n")
    
  }, error = function(e) {
    cat("  Error creando gráfico de percentiles:", e$message, "\n")
  })
  
  # RESUMEN EJECUTIVO
  resumen_ejecutivo <- data.frame(
    Grupo = grupo_nombre,
    Metodo_Recomendado = metodo_recomendado,
    Score_Metodo = round(metricas_metodos$Score[1], 3),
    Total_Registros = nrow(datos_grupo),
    Segmentos_Identificados = nrow(goals_finales),
    Mediana_Global = median(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE),
    Rango_Goals = ifelse(nrow(goals_finales) > 0, 
                         paste(min(goals_finales$goal_recomendado), "-", 
                               max(goals_finales$goal_recomendado), "días"),
                         "No disponible"),
    Meta_Sugerida_Ponderada = if(exists("meta_sugerida")) meta_sugerida else NA,
    Silhouette_Promedio = if(!is.null(resultado_clustering)) round(resultado_clustering$silhouette_score, 3) else NA,
    Fecha_Analisis = as.character(Sys.Date())
  )
  
  write_xlsx(resumen_ejecutivo, file.path(grupo_dir, "resumen_ejecutivo.xlsx"))
  
  cat("\nRESUMEN GRUPO", grupo_nombre, ":\n")
  cat("  - Método recomendado:", metodo_recomendado, "\n")
  cat("  - Segmentos identificados:", nrow(goals_finales), "\n")
  cat("  - Mediana global:", round(median(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE)), "días\n")
  cat("  - Rango de goals:", ifelse(nrow(goals_finales) > 0, 
                                    paste(min(goals_finales$goal_recomendado), "-", 
                                          max(goals_finales$goal_recomendado), "días"),
                                    "No disponible"), "\n")
  
  return(list(
    resumen = resumen_ejecutivo,
    metodo_recomendado = metodo_recomendado,
    goals_finales = goals_finales,
    metricas_metodos = metricas_metodos,
    combinaciones = resultado_combinaciones,
    clustering = resultado_clustering,
    percentiles = if(exists("meta_sugerida")) meta_sugerida else NA
  ))
}

# ======================================================================
# FUNCIÓN PARA CREAR ARCHIVO GENERAL DE PERCENTILES
# ======================================================================

crear_tabla_percentiles_grupos <- function(datos_completos, grupos_analizar, output_dir) {
  cat("\n", strrep("=", 60))
  cat("\nCREANDO TABLA GENERAL DE PERCENTILES POR GRUPO")
  cat("\n", strrep("=", 60), "\n")
  
  # Definir los percentiles requeridos
  percentiles <- c(0.25, 0.5, 0.75, 0.8, 0.9)
  nombres_percentiles <- c("P25", "P50", "P75", "P80", "P90")
  
  # Crear lista para almacenar resultados
  resultados_percentiles <- list()
  
  for (grupo in grupos_analizar) {
    cat("\nProcesando grupo:", grupo, "\n")
    
    # Filtrar datos para el grupo actual
    datos_grupo <- datos_completos %>%
      filter(Grupo == grupo)
    
    if (nrow(datos_grupo) < 5) {
      cat("  Grupo tiene menos de 5 observaciones. Omitiendo.\n")
      next
    }
    
    # Calcular percentiles para el grupo
    percentiles_grupo <- quantile(
      datos_grupo$`Días cobertura con capacitación`,
      probs = percentiles,
      na.rm = TRUE
    )
    
    # Calcular estadísticas básicas
    media_grupo <- mean(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE)
    mediana_grupo <- median(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE)
    sd_grupo <- sd(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE)
    
    # Calcular meta sugerida con ponderación
    meta_sugerida <- round(
      0.25 * percentiles_grupo[1] +  # P25
        0.25 * percentiles_grupo[2] +  # P50
        0.25 * percentiles_grupo[3] +  # P75
        0.15 * percentiles_grupo[4] +  # P80
        0.10 * percentiles_grupo[5]    # P90
    )
    
    # Crear fila de resultados
    fila_resultado <- data.frame(
      Grupo = grupo,
      N_Observaciones = nrow(datos_grupo),
      Media = round(media_grupo, 2),
      Mediana = round(mediana_grupo, 2),
      SD = round(sd_grupo, 2),
      Min = round(min(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE), 2),
      Max = round(max(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE), 2)
    )
    
    # Añadir percentiles
    for (i in seq_along(percentiles)) {
      fila_resultado[[nombres_percentiles[i]]] <- round(percentiles_grupo[i], 2)
    }
    
    # Añadir meta sugerida
    fila_resultado$Meta_Sugerida <- meta_sugerida
    fila_resultado$Ponderacion <- "25-25-25-15-10"
    
    resultados_percentiles[[grupo]] <- fila_resultado
  }
  
  # Combinar todos los resultados
  if (length(resultados_percentiles) > 0) {
    tabla_final <- bind_rows(resultados_percentiles)
    
    # Ordenar por grupo
    orden_grupos <- c("ODG", "COBRANZA", "SUCURSAL", "PLAZA")
    tabla_final$Grupo <- factor(tabla_final$Grupo, levels = orden_grupos)
    tabla_final <- tabla_final %>%
      arrange(Grupo)
    
    # Crear hoja adicional con resumen comparativo
    resumen_comparativo <- tabla_final %>%
      select(Grupo, N_Observaciones, Media, SD, Mediana, P25, P50, P75, P80, P90, Meta_Sugerida) %>%
      mutate(
        Coef_Variacion = ifelse(Media > 0, round((SD / Media) * 100, 2), NA),
        Dif_P90_P25 = round(P90 - P25, 2),
        Eficiencia_Relativa = ifelse(Media > 0, round((P75 - P25) / Media * 100, 2), NA),
        Ranking_Variabilidad = rank(Coef_Variacion)
      )
    
    # Crear archivo Excel con múltiples hojas
    file_path <- file.path(output_dir, "percentiles_por_grupo.xlsx")
    
    # Lista de hojas
    hojas_excel <- list(
      "Percentiles_Completos" = tabla_final,
      "Resumen_Comparativo" = resumen_comparativo,
      "Metadatos" = data.frame(
        Descripcion = c(
          "P25: Percentil 25 (primer cuartil) - 25% de los valores están por debajo",
          "P50: Percentil 50 (mediana) - 50% de los valores están por debajo",
          "P75: Percentil 75 (tercer cuartil) - 75% de los valores están por debajo",
          "P80: Percentil 80 - 80% de los valores están por debajo",
          "P90: Percentil 90 - 90% de los valores están por debajo",
          "Meta_Sugerida: Meta calculada con ponderación 25-25-25-15-10",
          "Coef_Variacion: Coeficiente de variación (SD/Media*100) - variabilidad relativa",
          "Eficiencia_Relativa: (P75-P25)/Media*100 - menor valor indica mayor eficiencia"
        )
      )
    )
    
    # Escribir archivo Excel
    write_xlsx(hojas_excel, file_path)
    
    cat("\n", strrep("-", 60))
    cat("\nTABLA DE PERCENTILES CREADA EXITOSAMENTE")
    cat("\nArchivo guardado en:", file_path)
    cat("\n", strrep("-", 60))
    
    # Mostrar resumen en consola
    cat("\n\nRESUMEN DE PERCENTILES POR GRUPO:\n\n")
    print(tabla_final %>% select(Grupo, N_Observaciones, P25, P50, P75, P80, P90, Meta_Sugerida))
    
    # CREAR GRÁFICO GENERAL DE PERCENTILES PARA TODOS LOS GRUPOS
    cat("\nCREANDO GRÁFICO GENERAL DE PERCENTILES...\n")
    
    # Preparar datos para el gráfico general
    datos_grafico_general <- tabla_final %>%
      select(Grupo, P25, P50, P75, P80, P90, Meta_Sugerida) %>%
      pivot_longer(cols = c(P25, P50, P75, P80, P90, Meta_Sugerida), 
                   names_to = "Metrica", values_to = "Valor") %>%
      mutate(
        Tipo = ifelse(Metrica == "Meta_Sugerida", "Meta", "Percentil"),
        Metrica = factor(Metrica, levels = c("P25", "P50", "P75", "P80", "P90", "Meta_Sugerida"))
      )
    
    # Gráfico de comparativa general
    p_comparativa_general <- ggplot(datos_grafico_general, 
                                    aes(x = Grupo, y = Valor, fill = Metrica, alpha = Tipo)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
      scale_alpha_manual(values = c("Percentil" = 0.7, "Meta" = 1)) +
      scale_fill_manual(values = c(
        "P25" = "#4E79A7", "P50" = "#F28E2B", "P75" = "#E15759",
        "P80" = "#76B7B2", "P90" = "#59A14F", "Meta_Sugerida" = "#EDC948"
      )) +
      geom_text(aes(label = round(Valor, 1)), 
                position = position_dodge(width = 0.8), 
                vjust = -0.5, size = 3) +
      labs(title = "Comparativa de Percentiles y Metas por Grupo de Gestión",
           subtitle = "Metas calculadas con ponderación: 25% P25 + 25% P50 + 25% P75 + 15% P80 + 10% P90",
           x = "Grupo de Gestión",
           y = "Días de Cobertura",
           fill = "Métrica",
           alpha = "Tipo") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12))
    
    ggsave(file.path(output_dir, "comparativa_general_percentiles.png"), 
           p_comparativa_general, width = 14, height = 9, dpi = 300)
    
    # Gráfico de líneas para tendencia de percentiles
    p_tendencia_percentiles <- ggplot(datos_grafico_general %>% filter(Tipo == "Percentil"), 
                                      aes(x = Metrica, y = Valor, color = Grupo, group = Grupo)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      geom_text(aes(label = round(Valor, 1)), 
                vjust = -1, size = 3.5, check_overlap = TRUE) +
      labs(title = "Tendencia de Percentiles por Grupo de Gestión",
           subtitle = "Evolución de P25 a P90",
           x = "Percentil",
           y = "Días de Cobertura",
           color = "Grupo") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(size = 16, face = "bold"))
    
    ggsave(file.path(output_dir, "tendencia_percentiles_grupos.png"), 
           p_tendencia_percentiles, width = 14, height = 9, dpi = 300)
    
    cat("\nGráficos generales creados exitosamente\n")
    
    return(list(
      tabla_completa = tabla_final,
      resumen = resumen_comparativo,
      grafico_general = p_comparativa_general
    ))
    
  } else {
    cat("\nNo se pudo crear la tabla de percentiles. Verifique los datos.\n")
    return(NULL)
  }
}

# ======================================================================
# EJECUTAR ANÁLISIS PARA CADA GRUPO
# ======================================================================

cat("\n", strrep("*", 60))
cat("\nINICIANDO ANÁLISIS POR GRUPO DE GESTIÓN")
cat("\n", strrep("*", 60), "\n")

grupos_existentes <- intersect(grupos, unique(datos_limpieza$Grupo))
grupos_no_existentes <- setdiff(grupos, grupos_existentes)

if (length(grupos_no_existentes) > 0) {
  cat("\nADVERTENCIA: Los siguientes grupos no se encontraron en los datos:\n")
  cat(paste(grupos_no_existentes, collapse = ", "), "\n")
}

if (length(grupos_existentes) == 0) {
  stop("Error: No se encontró ninguno de los grupos especificados en los datos.")
}

resultados_grupos <- list()
resultados_detalle <- list()

for (grupo in grupos_existentes) {
  resultado <- analizar_grupo(grupo, datos_limpieza)
  if (!is.null(resultado)) {
    resultados_grupos[[grupo]] <- resultado$resumen
    resultados_detalle[[grupo]] <- resultado
  }
}

# ======================================================================
# CREAR ARCHIVO MAESTRO DE CLASIFICACIÓN POR CLUSTER
# ======================================================================

cat("\n", strrep("*", 60))
cat("\nCREANDO ARCHIVO MAESTRO DE CLASIFICACIÓN POR CLUSTER")
cat("\n", strrep("*", 60), "\n")

# Crear archivo maestro de clasificación
resultado_clasificacion <- crear_archivo_maestro_clasificacion(
  datos_limpieza, 
  resultados_detalle, 
  output_dir
)

# ======================================================================
# RESUMEN CONSOLIDADO
# ======================================================================

cat("\n", strrep("*", 60))
cat("\nRESUMEN CONSOLIDADO DE TODOS LOS GRUPOS")
cat("\n", strrep("*", 60), "\n")

if (length(resultados_grupos) > 0) {
  resumen_consolidado <- bind_rows(resultados_grupos)
  write_xlsx(resumen_consolidado, file.path(output_dir, "resumen_consolidado_grupos.xlsx"))
  
  # Crear tabla de percentiles consolidada
  cat("\nCREANDO TABLA CONSOLIDADA DE PERCENTILES...\n")
  resultado_percentiles <- crear_tabla_percentiles_grupos(datos_limpieza, grupos_existentes, output_dir)
  
  # Crear gráfico de resumen ejecutivo
  cat("\nCREANDO GRÁFICO DE RESUMEN EJECUTIVO...\n")
  
  tryCatch({
    # Gráfico de métodos recomendados por grupo
    p_metodos_recomendados <- ggplot(resumen_consolidado, 
                                     aes(x = reorder(Grupo, -Score_Metodo), y = Score_Metodo, fill = Grupo)) +
      geom_col() +
      geom_text(aes(label = paste("Score:", round(Score_Metodo, 3))), 
                vjust = -0.5, size = 4, fontface = "bold") +
      geom_text(aes(label = gsub(".*: ", "", Metodo_Recomendado)), 
                vjust = 1.5, size = 3.5, color = "white") +
      labs(title = "Métodos Recomendados por Grupo de Gestión",
           subtitle = "Score del método recomendado en cada grupo",
           x = "Grupo de Gestión",
           y = "Score del Método (mayor es mejor)") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(size = 16, face = "bold"))
    
    ggsave(file.path(output_dir, "metodos_recomendados_grupos.png"), 
           p_metodos_recomendados, width = 12, height = 8, dpi = 300)
    
    # Gráfico de medianas globales por grupo
    p_medianas_globales <- ggplot(resumen_consolidado, 
                                  aes(x = reorder(Grupo, -Mediana_Global), y = Mediana_Global, fill = Grupo)) +
      geom_col() +
      geom_text(aes(label = paste(round(Mediana_Global), "días")), 
                vjust = -0.5, size = 5, fontface = "bold") +
      labs(title = "Medianas Globales de Días de Cobertura por Grupo",
           subtitle = "Comparativa del desempeño medio de cada área",
           x = "Grupo de Gestión",
           y = "Mediana de Días de Cobertura") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(size = 16, face = "bold"))
    
    ggsave(file.path(output_dir, "medianas_globales_grupos.png"), 
           p_medianas_globales, width = 12, height = 8, dpi = 300)
    
    cat("Gráficos ejecutivos creados exitosamente\n")
    
  }, error = function(e) {
    cat("Error creando gráficos ejecutivos:", e$message, "\n")
  })
  
  # Crear archivo README con resumen
  readme_content <- paste0(
    "RESULTADOS DEL ANÁLISIS DE GOALS DE DÍAS DE COBERTURA\n",
    "========================================================\n\n",
    "Fecha de análisis: ", Sys.Date(), "\n",
    "Total de observaciones analizadas: ", nrow(datos_limpieza), "\n",
    "Grupos analizados: ", paste(grupos_existentes, collapse = ", "), "\n\n",
    "MÉTODOS IMPLEMENTADOS:\n",
    "1. EVALUACIÓN DE AGRUPADORES INDIVIDUALES - Análisis de cada variable por separado\n",
    "2. COMBINACIONES CON SEGMENTO DE PUESTO - Interacción de variables con Segmento de puesto\n",
    "3. CLUSTERING K-MEANS - Análisis no supervisado con método del codo y silueta\n",
    "4. ÁRBOLES DE DECISIÓN - Segmentación supervisada basada en la variable objetivo\n",
    "5. MODELOS DE REGRESIÓN - Predicción + segmentación por residuos\n",
    "6. SEGMENTACIÓN POR PERCENTILES - División en cuartiles dentro de categorías\n\n",
    "ARCHIVOS GENERADOS EN CADA CARPETA DE GRUPO:\n",
    "- datos_clasificados_clusters.xlsx: Datos completos con clasificación por cluster\n",
    "- estadisticas_detalladas_clusters.xlsx: Estadísticas detalladas por cluster\n",
    "- centroides_clusters.xlsx: Centroides para futura clasificación\n",
    "- evaluacion_agrupadores_individuales.xlsx: Ranking de variables individuales\n",
    "- evaluacion_combinaciones_segmento_puesto.xlsx: Combinaciones con Segmento de puesto\n",
    "- metodo_codo_clustering.png: Gráfico del método del codo para K-Means\n",
    "- distribucion_clusters.png: Distribución de días por cluster\n",
    "- analisis_silueta.png: Análisis de coeficiente de silueta\n",
    "- arbol_decision_segmentos.png: Visualización del árbol de decisión\n",
    "- modelo_residuos.png: Análisis de residuos del modelo\n",
    "- segmentos_percentiles.png: Distribución por percentiles\n",
    "- mejor_agrupador_metas.png: Gráfico del mejor agrupador con metas\n",
    "- percentiles_meta_sugerida.png: Percentiles con meta sugerida ponderada\n",
    "- comparacion_metodos.xlsx: Comparación de todos los métodos\n",
    "- goals_recomendados_finales.xlsx: Goals finales recomendados\n",
    "- resumen_ejecutivo.xlsx: Resumen estadístico\n\n",
    "ARCHIVOS GENERALES EN LA CARPETA PRINCIPAL:\n",
    "- Clasificacion_Maestra/MAESTRO_Clasificacion_Clusters.xlsx: Archivo maestro con clasificación completa\n",
    "- TABLA_REFERENCIA_METAS_CLUSTER.xlsx: Tabla de referencia para asignar metas por cluster\n",
    "- percentiles_por_grupo.xlsx: Tabla consolidada de percentiles\n",
    "- comparativa_general_percentiles.png: Gráfico comparativo general\n",
    "- tendencia_percentiles_grupos.png: Tendencia de percentiles\n",
    "- metodos_recomendados_grupos.png: Métodos recomendados por grupo\n",
    "- medianas_globales_grupos.png: Medianas globales por grupo\n",
    "- resumen_consolidado_grupos.xlsx: Resumen ejecutivo consolidado\n\n",
    "INSTRUCCIONES PARA USAR LA CLASIFICACIÓN POR CLUSTERS:\n",
    "1. Para asignar metas a nuevos reclutamientos:\n",
    "   - Identifique el Grupo (SUCURSAL, COBRANZA, PLAZA, ODG)\n",
    "   - Consulte TABLA_REFERENCIA_METAS_CLUSTER.xlsx\n",
    "   - Asigne la meta correspondiente al cluster\n",
    "\n",
    "2. Clusters identificados:\n",
    "   - Cluster 1: Perfiles con menor tiempo de cobertura\n",
    "   - Cluster 2: Perfiles con mayor tiempo de cobertura\n",
    "\n",
    "3. Para reclasificar nuevos datos automáticamente:\n",
    "   - Use los centroides guardados en cada carpeta de grupo\n",
    "   - Calcule la distancia de cada nuevo registro a los centroides\n",
    "   - Asigne al cluster más cercano\n",
    "\n",
    "PONDERACIÓN UTILIZADA PARA METAS SUGERIDAS:\n",
    "- 25% P25 (percentil 25)\n",
    "- 25% P50 (mediana)\n",
    "- 25% P75 (percentil 75)\n",
    "- 15% P80 (percentil 80)\n",
    "- 10% P90 (percentil 90)\n"
  )
  
  writeLines(readme_content, file.path(output_dir, "README.txt"))
  
  cat("\n", strrep("=", 60))
  cat("\nANÁLISIS COMPLETADO EXITOSAMENTE")
  cat("\n", strrep("=", 60))
  cat("\n\nARCHIVOS GENERADOS:")
  cat("\n✓ Clasificación completa por clusters guardada")
  cat("\n✓ Tabla de referencia para asignación de metas")
  cat("\n✓ Tablas de percentiles por grupo")
  cat("\n✓ Análisis de combinaciones con Segmento de puesto")
  cat("\n✓ Análisis de clustering con método del codo y silueta")
  cat("\n✓ Gráficos del mejor agrupador con metas")
  cat("\n✓ Gráficos de percentiles con metas sugeridas")
  cat("\n✓ Comparativa general de todos los grupos")
  cat("\n✓ Resúmenes ejecutivos completos")
  cat("\n✓ Archivo README con documentación completa")
  cat("\n\nARCHIVOS CLAVE PARA FUTURAS METAS:")
  cat("\n• TABLA_REFERENCIA_METAS_CLUSTER.xlsx - Tabla de referencia rápida")
  cat("\n• Clasificacion_Maestra/MAESTRO_Clasificacion_Clusters.xlsx - Datos completos clasificados")
  cat("\n• En cada carpeta de grupo: centroides_clusters.xlsx - Para reclasificación automática")
  cat("\n")
  
} else {
  cat("\nNo se pudo realizar análisis para ningún grupo.\n")
}

# ======================================================================
# MOSTRAR RESUMEN FINAL
# ======================================================================

cat("\n", strrep("=", 60))
cat("\nRESUMEN EJECUTIVO FINAL")
cat("\n", strrep("=", 60), "\n")

if (exists("resultado_clasificacion") && !is.null(resultado_clasificacion)) {
  cat("\nMETAS POR CLUSTER PARA CADA GRUPO:\n\n")
  
  tabla_metas <- resultado_clasificacion$tabla_referencia %>%
    select(Grupo, cluster_id, `Meta (días)`, Registros, `Mediana (días)`) %>%
    arrange(Grupo, `Meta (días)`)
  
  for (grupo in unique(tabla_metas$Grupo)) {
    cat(sprintf("%s:\n", grupo))
    subset_metas <- tabla_metas %>% filter(Grupo == grupo)
    for (i in 1:nrow(subset_metas)) {
      cat(sprintf("  %s: %d días (n=%d, mediana=%.1f)\n", 
                  subset_metas$cluster_id[i],
                  subset_metas$`Meta (días)`[i],
                  subset_metas$Registros[i],
                  subset_metas$`Mediana (días)`[i]))
    }
    cat("\n")
  }
  
  cat("\nTOTAL REGISTROS CLASIFICADOS:", sum(tabla_metas$Registros), "\n")
  cat("ARCHIVO DE REFERENCIA: TABLA_REFERENCIA_METAS_CLUSTER.xlsx\n")
  cat("ARCHIVO MAESTRO: Clasificacion_Maestra/MAESTRO_Clasificacion_Clusters.xlsx\n")
}

cat("\n", strrep("*", 60))
cat("\nPROCESO COMPLETADO")
cat("\n", strrep("*", 60), "\n")
