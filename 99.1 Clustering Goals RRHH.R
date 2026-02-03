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
  cat("El análisis continuará sin esta variable.\n")
  incluir_reclutador <- FALSE
} else {
  incluir_reclutador <- TRUE
}

# SELECCIONAR Y PREPARAR COLUMNAS DE INTERÉS - CON NUEVAS VARIABLES
# Lista base de columnas
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

# Agregar Nombre Reclutador si existe
if (incluir_reclutador) {
  columnas_interes <- c(columnas_base, "Nombre Reclutador")
} else {
  columnas_interes <- columnas_base
}

datos_limpieza <- datos %>%
  select(all_of(columnas_interes)) %>%
  mutate(across(-c(`Días cobertura con capacitación`), as.factor)) %>%
  filter(!is.na(`Días cobertura con capacitación`), !is.na(Grupo))

# DEFINIR LOS GRUPOS A ANALIZAR
grupos <- c("SUCURSAL", "COBRANZA", "PLAZA", "ODG")

# FUNCIÓN PARA EVALUAR AGRUPADORES MEJORADA CON EVALUACIÓN DE VIABILIDAD
evaluar_agrupador_mejorado <- function(columna, datos_subset) {
  
  # Crear una copia de los datos para evitar problemas de evaluación
  datos_temp <- datos_subset
  
  # Filtrar grupos con al menos 5 observaciones
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
    filter(n >= 5) %>%  # Mínimo 5 observaciones por grupo
    filter(!is.na(cv))  # Eliminar CV no válidos
  
  if (nrow(analisis) < 2) {
    # No hay suficientes grupos para comparar
    return(data.frame(
      agrupador = columna,
      grupos_n = nrow(analisis),
      eta_squared = NA,
      f_statistic = NA,
      p_value = NA,
      cv_promedio = NA,
      heterogeneidad = NA,
      rango_medias = NA,
      mediana_global = median(datos_temp$`Días cobertura con capacitación`, na.rm = TRUE),
      min_grupo = ifelse(nrow(analisis) > 0, min(analisis$n), NA),
      max_grupo = ifelse(nrow(analisis) > 0, max(analisis$n), NA)
    ))
  }
  
  # ANOVA para probar diferencias significativas (solo si hay datos suficientes)
  # Filtrar solo los grupos que cumplen con el mínimo
  categorias_validas <- analisis[[columna]]
  datos_anova <- datos_temp %>%
    filter(!!sym(columna) %in% categorias_validas)
  
  if (length(unique(datos_anova[[columna]])) > 1) {
    # Crear fórmula para ANOVA usando as.formula para manejar nombres con espacios
    formula_text <- paste0("`Días cobertura con capacitación` ~ `", columna, "`")
    formula_anova <- as.formula(formula_text)
    
    modelo <- aov(formula_anova, data = datos_anova)
    resumen_anova <- summary(modelo)
    
    # Calcular eta-squared (tamaño del efecto)
    ss_total <- sum((datos_anova$`Días cobertura con capacitación` - 
                       mean(datos_anova$`Días cobertura con capacitación`, na.rm = TRUE))^2, na.rm = TRUE)
    ss_between <- sum(analisis$n * (analisis$media - 
                                      mean(datos_anova$`Días cobertura con capacitación`, na.rm = TRUE))^2)
    eta_sq <- ifelse(ss_total > 0, ss_between / ss_total, 0)
    
    f_stat <- resumen_anova[[1]]$`F value`[1]
    p_valor <- resumen_anova[[1]]$`Pr(>F)`[1]
  } else {
    eta_sq <- NA
    f_stat <- NA
    p_valor <- NA
  }
  
  return(data.frame(
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
  ))
}

# FUNCIÓN PARA EVALUAR VIABILIDAD DEL AGRUPADOR
evaluar_viabilidad_agrupador <- function(resultados_grupo) {
  
  # Extraer métricas del mejor agrupador
  mejor <- resultados_grupo[1, ]
  
  cat("\n=== EVALUACIÓN DE VIABILIDAD DEL AGRUPADOR ===\n")
  
  # Evaluar eta_squared
  cat("\n1. PODER EXPLICATIVO (eta-squared):", round(mejor$eta_squared, 4), "\n")
  if (mejor$eta_squared < 0.01) {
    cat("   ❌ CRÍTICO: Explica menos del 1% de la variación\n")
  } else if (mejor$eta_squared < 0.05) {
    cat("   ⚠️ BAJO: Explica menos del 5% de la variación\n")
  } else if (mejor$eta_squared < 0.15) {
    cat("   ⚠️ MODERADO: Explica entre 5-15% de la variación\n")
  } else {
    cat("   ✅ ADECUADO: Explica más del 15% de la variación\n")
  }
  
  # Evaluar homogeneidad
  cat("\n2. HOMOGENEIDAD (CV promedio):", round(mejor$cv_promedio, 3), "\n")
  if (mejor$cv_promedio > 1.0) {
    cat("   ❌ MUY ALTA VARIABILIDAD: CV > 1.0\n")
  } else if (mejor$cv_promedio > 0.7) {
    cat("   ⚠️ ALTA VARIABILIDAD: CV > 0.7\n")
  } else if (mejor$cv_promedio > 0.5) {
    cat("   ⚠️ MODERADA VARIABILIDAD: CV > 0.5\n")
  } else {
    cat("   ✅ BUENA HOMOGENEIDAD: CV < 0.5\n")
  }
  
  # Evaluar número de grupos
  cat("\n3. PARSIMONIA (número de grupos):", mejor$grupos_n, "\n")
  cat("   (Tamaño mínimo grupo:", mejor$min_grupo, ", máximo:", mejor$max_grupo, ")\n")
  
  if (mejor$grupos_n < 3) {
    cat("   ⚠️ MUY POCOS GRUPOS: Puede estar sobresimplificando\n")
  } else if (mejor$grupos_n > 50) {
    cat("   ⚠️ MUCHOS GRUPOS: Dificulta la implementación\n")
  } else if (mejor$grupos_n > 20) {
    cat("   ⚠️ GRANULARIDAD ALTA: Muchos grupos para gestionar\n")
  } else {
    cat("   ✅ NÚMERO ADECUADO DE GRUPOS\n")
  }
  
  # Evaluar rango de medias
  cat("\n4. DIFERENCIACIÓN (rango de medias):", round(mejor$rango_medias, 1), "días\n")
  if (mejor$rango_medias < 10) {
    cat("   ⚠️ POCA DIFERENCIACIÓN: Rangos pequeños entre grupos\n")
  } else if (mejor$rango_medias > 50) {
    cat("   ✅ ALTA DIFERENCIACIÓN: Rangos significativos entre grupos\n")
  } else {
    cat("   ✅ DIFERENCIACIÓN MODERADA\n")
  }
  
  # Calcular score de viabilidad
  score_viabilidad <- (mejor$eta_squared * 0.4) + 
    ((1 - mejor$cv_promedio) * 0.3) +
    (ifelse(mejor$grupos_n >= 5 & mejor$grupos_n <= 30, 0.2, 0.1)) +
    (pmin(mejor$rango_medias / 100, 0.1))  # Hasta 10% adicional por rango
  
  cat("\n5. SCORE DE VIABILIDAD GLOBAL:", round(score_viabilidad, 3), "/ 1.0\n")
  if (score_viabilidad < 0.3) {
    cat("   ❌ NO VIABLE - Considerar cambio de agrupador\n")
  } else if (score_viabilidad < 0.5) {
    cat("   ⚠️ LIMITADAMENTE VIABLE - Necesita mejoras\n")
  } else if (score_viabilidad < 0.7) {
    cat("   ⚠️ MODERADAMENTE VIABLE - Aceptable con monitoreo\n")
  } else {
    cat("   ✅ ALTAMENTE VIABLE - Excelente agrupador\n")
  }
  
  # Recomendación final basada en métricas
  cat("\n6. RECOMENDACIÓN ESPECÍFICA:\n")
  recomendaciones <- c()
  
  if (mejor$eta_squared < 0.05) {
    recomendaciones <- c(recomendaciones, 
                         "• CONSIDERAR CAMBIAR DE AGRUPADOR (poder explicativo muy bajo)")
  }
  
  if (mejor$cv_promedio > 0.7) {
    recomendaciones <- c(recomendaciones,
                         "• CONSIDERAR SUB-SEGMENTAR LOS GRUPOS (alta variabilidad interna)")
  }
  
  if (mejor$grupos_n < 5) {
    recomendaciones <- c(recomendaciones,
                         "• EVALUAR AGRUPADORES MÁS GRANULARES (pocos grupos)")
  }
  
  if (mejor$grupos_n > 30) {
    recomendaciones <- c(recomendaciones,
                         "• EVALUAR AGRUPADORES MÁS AGRUPADOS (demasiados grupos)")
  }
  
  if (mejor$rango_medias < 10) {
    recomendaciones <- c(recomendaciones,
                         "• EVALUAR SI VALE LA PENA TENER GRUPOS DIFERENTES (poca diferenciación)")
  }
  
  if (length(recomendaciones) == 0) {
    cat("   • MANTENER ESTE AGRUPADOR - Buen balance en todas las métricas\n")
  } else {
    for (rec in recomendaciones) {
      cat("   ", rec, "\n")
    }
  }
  
  # Si hay opciones alternativas con mejor eta_squared
  if (nrow(resultados_grupo) > 1) {
    # Buscar alternativas con mejor eta_squared pero menos grupos
    alternativas <- resultados_grupo %>%
      filter(eta_squared > mejor$eta_squared * 1.2,  # 20% mejor
             grupos_n < mejor$grupos_n * 1.5) %>%    # No mucho más grupos
      arrange(desc(eta_squared)) %>%
      head(3)
    
    if (nrow(alternativas) > 0) {
      cat("\n7. OPCIONES ALTERNATIVAS CON MEJOR PODER EXPLICATIVO:\n")
      for (i in 1:nrow(alternativas)) {
        alt <- alternativas[i, ]
        cat(sprintf("   %d. %s (η²=%.3f, %d grupos, CV=%.2f)\n", 
                    i, alt$agrupador, alt$eta_squared, alt$grupos_n, alt$cv_promedio))
      }
    }
  }
  
  return(list(
    score_viabilidad = score_viabilidad,
    mejor = mejor
  ))
}

# FUNCIÓN PARA CALCULAR GOAL (MEJORADA)
calcular_goal <- function(x) {
  # Usar la mediana como default (más robusta a outliers)
  goal <- round(median(x, na.rm = TRUE))
  return(goal)
}

# FUNCIÓN PARA CREAR COMBINACIONES CON SEGMENTO DE PUESTO (SOLO PARA ODG)
crear_combinaciones_odg <- function(datos_odg, agrupadores_base) {
  # Excluir 'Segmento de puesto' y la variable de respuesta
  agrupadores_para_combinar <- setdiff(agrupadores_base, 
                                       c("Días cobertura con capacitación", "Grupo", "Segmento de puesto"))
  
  datos_extendidos <- datos_odg
  
  # Crear combinaciones con Segmento de puesto
  combinaciones <- list()
  
  for (agrupador in agrupadores_para_combinar) {
    nombre_combinado <- paste0(agrupador, "_con_Segmento")
    
    # Crear combinación única
    datos_extendidos[[nombre_combinado]] <- paste(
      as.character(datos_odg[[agrupador]]),
      as.character(datos_odg[["Segmento de puesto"]]),
      sep = " | "
    ) %>% as.factor()
    
    combinaciones <- c(combinaciones, nombre_combinado)
  }
  
  return(list(
    datos = datos_extendidos,
    combinaciones = unlist(combinaciones)
  ))
}

# FUNCIÓN PARA REALIZAR CLUSTERING Y GRÁFICOS
realizar_clustering <- function(datos_grupo, grupo_dir, grupo_nombre) {
  cat("\n6. REALIZANDO ANÁLISIS DE CLUSTERING...\n")
  
  tryCatch({
    # Preparar datos para clustering (solo variables numéricas o convertidas a dummy)
    datos_numericos <- datos_grupo %>%
      select(`Días cobertura con capacitación`)
    
    # Convertir variables categóricas a dummy (excluir nombre reclutador si hay muchos niveles)
    datos_categoricos <- datos_grupo %>%
      select(-c(`Días cobertura con capacitación`, Grupo))
    
    # Si hay muchas categorías en alguna variable, consideramos excluirla o reducir dimensionalidad
    n_categorias <- sapply(datos_categoricos, function(x) length(unique(x)))
    
    # Excluir variables con demasiadas categorías (> 50) para evitar explosión dimensional
    vars_muchas_categorias <- names(n_categorias[n_categorias > 50])
    if (length(vars_muchas_categorias) > 0) {
      cat("  Excluyendo variables con muchas categorías (>50):", 
          paste(vars_muchas_categorias, collapse = ", "), "\n")
      datos_categoricos <- datos_categoricos %>%
        select(-all_of(vars_muchas_categorias))
    }
    
    # Convertir a matriz dummy
    datos_categoricos_mat <- model.matrix(~ . - 1, data = datos_categoricos) %>%
      as.data.frame()
    
    # Combinar datos
    datos_clustering <- cbind(
      dias_cobertura_scale = scale(datos_numericos),
      scale(datos_categoricos_mat)
    )
    
    # Remover columnas con NA o varianza cero
    datos_clustering <- datos_clustering[, colSums(is.na(datos_clustering)) == 0]
    varianzas <- apply(datos_clustering, 2, var, na.rm = TRUE)
    datos_clustering <- datos_clustering[, varianzas > 0]
    
    if (ncol(datos_clustering) >= 2 && nrow(datos_clustering) > 10) {
      # Determinar número óptimo de clusters usando método del codo
      set.seed(123)
      max_k <- min(10, nrow(datos_clustering) - 1)
      
      if (max_k >= 2) {
        wss <- map_dbl(1:max_k, function(k) {
          kmeans(datos_clustering, k, nstart = 25, iter.max = 50)$tot.withinss
        })
        
        # Crear gráfico del codo
        elbow_data <- data.frame(k = 1:length(wss), wss = wss)
        elbow_plot <- ggplot(elbow_data, aes(x = k, y = wss)) +
          geom_line(color = "steelblue", size = 1.2) + 
          geom_point(color = "darkred", size = 3) +
          labs(title = paste("Método del Codo para Número Óptimo de Clusters -", grupo_nombre),
               subtitle = "Selección visual del punto donde la curva se dobla",
               x = "Número de Clusters (k)",
               y = "Suma de Cuadrados Within (WSS)") +
          theme_minimal() +
          theme(plot.title = element_text(face = "bold", size = 14),
                plot.subtitle = element_text(color = "gray40"))
        
        # Guardar gráfico del codo
        ggsave(file.path(grupo_dir, "clustering_elbow_plot.png"), elbow_plot, 
               width = 10, height = 6, dpi = 300)
        
        # Elegir número de clusters usando método del codo simplificado
        # Encontrar el "codo" donde la reducción en WSS se estabiliza
        reducciones <- -diff(wss)
        reducciones_relativas <- reducciones / wss[1:(length(wss)-1)]
        k_optimo <- which.max(reducciones_relativas < mean(reducciones_relativas) * 0.5) + 1
        k_optimo <- max(2, min(k_optimo, 5))  # Entre 2 y 5 clusters
        
        cat("  Número óptimo de clusters identificado:", k_optimo, "\n")
        
        # Aplicar k-means
        kmeans_result <- kmeans(datos_clustering, centers = k_optimo, nstart = 25)
        datos_grupo$cluster <- as.factor(kmeans_result$cluster)
        
        # Gráfico de clusters (solo si tenemos al menos 2 dimensiones)
        if (ncol(datos_clustering) >= 2) {
          # Reducción de dimensionalidad con PCA para visualización
          pca_result <- prcomp(datos_clustering, scale = TRUE)
          pca_data <- as.data.frame(pca_result$x[, 1:2])
          pca_data$cluster <- as.factor(kmeans_result$cluster)
          pca_data$dias_cobertura <- datos_grupo$`Días cobertura con capacitación`
          
          # Gráfico de clusters en espacio PCA
          cluster_pca_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
            geom_point(size = 3, alpha = 0.7) +
            stat_ellipse(level = 0.95, size = 1) +
            labs(title = paste("Visualización de Clusters -", grupo_nombre),
                 subtitle = "Reducción PCA (Primeras 2 componentes principales)",
                 x = paste("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)", sep = ""),
                 y = paste("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)", sep = "")) +
            theme_minimal() +
            theme(legend.position = "right")
          
          ggsave(file.path(grupo_dir, "clustering_pca_plot.png"), cluster_pca_plot, 
                 width = 10, height = 8, dpi = 300)
          
          # Gráfico de silueta para evaluar calidad de clusters
          if (require("cluster", quietly = TRUE)) {
            dist_matrix <- dist(datos_clustering)
            sil_width <- silhouette(as.numeric(kmeans_result$cluster), dist_matrix)
            sil_data <- data.frame(
              cluster = as.factor(sil_width[, 1]),
              sil_width = sil_width[, 3]
            )
            
            silhouette_plot <- ggplot(sil_data, aes(x = cluster, y = sil_width, fill = cluster)) +
              geom_boxplot(alpha = 0.7) +
              geom_hline(yintercept = mean(sil_width[, 3]), linetype = "dashed", color = "red") +
              annotate("text", x = Inf, y = mean(sil_width[, 3]), 
                       label = paste("Promedio:", round(mean(sil_width[, 3]), 3)),
                       hjust = 1.1, vjust = -0.5, color = "red") +
              labs(title = paste("Análisis de Silueta -", grupo_nombre),
                   subtitle = paste("Coeficiente de silueta promedio:", round(mean(sil_width[, 3]), 3)),
                   x = "Cluster",
                   y = "Ancho de Silueta") +
              theme_minimal() +
              theme(legend.position = "none")
            
            ggsave(file.path(grupo_dir, "clustering_silhouette_plot.png"), silhouette_plot, 
                   width = 10, height = 6, dpi = 300)
            
            # Evaluar calidad del clustering
            sil_promedio <- mean(sil_width[, 3])
            cat("  Coeficiente de silueta promedio:", round(sil_promedio, 3), "\n")
            if (sil_promedio > 0.5) {
              cat("  ✅ Calidad de clustering: ALTA\n")
            } else if (sil_promedio > 0.25) {
              cat("  ⚠️ Calidad de clustering: MODERADA\n")
            } else {
              cat("  ❌ Calidad de clustering: BAJA\n")
            }
          }
        }
        
        # Análisis de clusters
        analisis_clusters <- datos_grupo %>%
          group_by(cluster) %>%
          summarise(
            n = n(),
            media_dias = mean(`Días cobertura con capacitación`, na.rm = TRUE),
            mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
            sd_dias = sd(`Días cobertura con capacitación`, na.rm = TRUE),
            min_dias = min(`Días cobertura con capacitación`, na.rm = TRUE),
            max_dias = max(`Días cobertura con capacitación`, na.rm = TRUE),
            goal = calcular_goal(`Días cobertura con capacitación`),
            .groups = 'drop'
          ) %>%
          arrange(mediana_dias)
        
        # Guardar análisis de clusters
        write_xlsx(analisis_clusters, file.path(grupo_dir, "analisis_clusters.xlsx"))
        
        # Gráfico comparativo de clusters
        cluster_comparison_plot <- ggplot(datos_grupo, aes(x = cluster, y = `Días cobertura con capacitación`, fill = cluster)) +
          geom_boxplot(alpha = 0.7) +
          stat_summary(fun = median, geom = "point", shape = 18, size = 4, color = "darkred") +
          labs(title = paste("Distribución de Días de Cobertura por Cluster -", grupo_nombre),
               subtitle = "Los puntos rojos indican la mediana de cada cluster",
               x = "Cluster",
               y = "Días de Cobertura") +
          theme_minimal() +
          theme(legend.position = "none")
        
        ggsave(file.path(grupo_dir, "clustering_boxplot.png"), cluster_comparison_plot, 
               width = 10, height = 6, dpi = 300)
        
        # Gráfico de perfil de clusters
        cluster_profile <- datos_grupo %>%
          group_by(cluster) %>%
          summarise(
            n = n(),
            mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
            cv = sd(`Días cobertura con capacitación`, na.rm = TRUE) / 
              mean(`Días cobertura con capacitación`, na.rm = TRUE),
            .groups = 'drop'
          )
        
        profile_plot <- ggplot(cluster_profile, aes(x = cluster, y = mediana_dias, fill = cluster)) +
          geom_col(width = 0.7) +
          geom_text(aes(label = paste("n =", n)), vjust = -0.5, size = 3.5) +
          geom_text(aes(label = paste(round(mediana_dias), "días")), vjust = 1.5, size = 3.5, color = "white") +
          labs(title = paste("Perfil de Clusters -", grupo_nombre),
               x = "Cluster",
               y = "Mediana de Días de Cobertura") +
          theme_minimal() +
          theme(legend.position = "none")
        
        ggsave(file.path(grupo_dir, "clustering_profile_plot.png"), profile_plot, 
               width = 10, height = 6, dpi = 300)
        
        cat("  Clustering completado exitosamente.\n")
        return(list(
          analisis = analisis_clusters,
          k_optimo = k_optimo
        ))
      }
    } else {
      cat("  No hay suficientes datos para clustering.\n")
    }
  }, error = function(e) {
    cat("  Error en clustering:", e$message, "\n")
    return(NULL)
  })
  
  return(NULL)
}

# FUNCIÓN PRINCIPAL PARA ANALIZAR CADA GRUPO
analizar_grupo <- function(grupo_nombre, datos_completos) {
  cat("\n", strrep("=", 50), "\n")
  cat("ANALIZANDO GRUPO:", grupo_nombre, "\n")
  cat(strrep("=", 50), "\n")
  
  # Filtrar datos para el grupo actual
  datos_grupo <- datos_completos %>%
    filter(Grupo == grupo_nombre)
  
  # Verificar que hay suficientes datos
  if (nrow(datos_grupo) < 10) {
    cat("Grupo", grupo_nombre, "tiene menos de 10 observaciones. Análisis omitido.\n")
    return(NULL)
  }
  
  # Crear directorio específico para el grupo
  grupo_dir <- file.path(output_dir, grupo_nombre)
  if (!dir.exists(grupo_dir)) {
    dir.create(grupo_dir, recursive = TRUE)
  }
  
  # MANEJO ESPECIAL PARA ODG: CREAR COMBINACIONES
  if (grupo_nombre == "ODG") {
    cat("\nCreando combinaciones con 'Segmento de puesto' para ODG...\n")
    
    # Crear combinaciones
    resultado_combinaciones <- crear_combinaciones_odg(datos_grupo, columnas_interes)
    datos_grupo_extendido <- resultado_combinaciones$datos
    combinaciones <- resultado_combinaciones$combinaciones
    
    cat("Combinaciones creadas:", length(combinaciones), "\n")
    
    # Usar datos extendidos para ODG
    datos_grupo_actual <- datos_grupo_extendido
    agrupadores_odg <- setdiff(c(columnas_interes, combinaciones), 
                               c("Días cobertura con capacitación", "Grupo"))
  } else {
    # Para otros grupos, usar datos normales
    datos_grupo_actual <- datos_grupo
    agrupadores_odg <- setdiff(columnas_interes, c("Días cobertura con capacitación", "Grupo"))
  }
  
  # EVALUACIÓN DE AGRUPADORES
  cat("\n1. EVALUANDO AGRUPADORES...\n")
  
  # Evaluar todos los agrupadores (incluyendo combinaciones para ODG)
  resultados <- purrr::map_dfr(agrupadores_odg, function(agrupador) {
    evaluar_agrupador_mejorado(agrupador, datos_grupo_actual)
  })
  
  # Calcular score mejorado
  resultados <- resultados %>%
    filter(!is.na(eta_squared)) %>%  # Eliminar agrupadores sin análisis válido
    mutate(
      # Penalizar por muchos grupos (favor de parsimonia)
      penalty_grupos = 1 - (grupos_n / nrow(datos_grupo)),
      # Score que considera tamaño del efecto, homogeneidad y parsimonia
      score_eta = eta_squared * (1 - cv_promedio) * penalty_grupos,
      # Score alternativo que considera heterogeneidad
      score_hetero = (heterogeneidad / sd(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE)) *
        (1 / (cv_promedio + 0.1)) *  # Evitar división por 0
        penalty_grupos,
      # Score combinado
      score_combinado = 0.7 * score_eta + 0.3 * score_hetero
    ) %>%
    arrange(desc(score_combinado))
  
  # Mostrar resultados
  cat("\nRanking de agrupadores (top 10):\n")
  print(resultados %>% 
          select(agrupador, grupos_n, eta_squared, cv_promedio, score_combinado) %>%
          head(10))
  
  # Guardar resultados de evaluación completos
  write_xlsx(resultados, file.path(grupo_dir, "evaluacion_agrupadores.xlsx"))
  
  # IDENTIFICAR MEJOR AGRUPADOR
  mejor_agrupador <- ifelse(nrow(resultados) > 0, 
                            resultados$agrupador[1], 
                            "DescripcionCC")  # Default si no hay resultados
  
  cat("\n2. MEJOR AGRUPADOR IDENTIFICADO:", mejor_agrupador, "\n")
  
  # INFORMACIÓN SOBRE SI ES UNA COMBINACIÓN (solo para ODG)
  if (grupo_nombre == "ODG" && grepl("_con_Segmento", mejor_agrupador)) {
    cat("  (Esta es una combinación con 'Segmento de puesto')\n")
  }
  
  # EVALUACIÓN DE VIABILIDAD
  cat("\n2.5. EVALUANDO VIABILIDAD DEL AGRUPADOR...\n")
  evaluacion_viabilidad <- evaluar_viabilidad_agrupador(resultados)
  
  # GENERAR GOALS POR MEJOR AGRUPADOR
  cat("\n3. GENERANDO GOALS...\n")
  
  # Calcular goals usando group_by_at para evitar problemas con !!sym()
  goals_agrupador <- datos_grupo_actual %>%
    group_by_at(mejor_agrupador) %>%
    summarise(
      n = n(),
      media_dias = mean(`Días cobertura con capacitación`, na.rm = TRUE),
      mediana_dias = median(`Días cobertura con capacitación`, na.rm = TRUE),
      sd_dias = sd(`Días cobertura con capacitación`, na.rm = TRUE),
      q1 = quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE),
      q3 = quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE),
      # Usar mediana para el goal (más robusto)
      goal = calcular_goal(`Días cobertura con capacitación`),
      # Alternativa: percentil 75 para metas más exigentes
      goal_exigente = round(quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE)),
      .groups = 'drop'
    ) %>%
    filter(n >= 5) %>%  # Solo grupos con al menos 5 observaciones
    arrange(mediana_dias)
  
  # Guardar goals
  write_xlsx(goals_agrupador, file.path(grupo_dir, "goals_recomendados.xlsx"))
  
  # VISUALIZACIÓN DEL MEJOR AGRUPADOR
  cat("\n4. CREANDO VISUALIZACIONES...\n")
  
  if (nrow(goals_agrupador) > 0) {
    # Para el gráfico, necesitamos manejar nombres con espacios o guiones bajos
    necesita_backticks <- grepl(" |_", mejor_agrupador)
    
    if (necesita_backticks) {
      # Si tiene espacios o guiones bajos, usar backticks
      mejor_agrupador_expr <- parse(text = paste0("`", mejor_agrupador, "`"))
      
      # Gráfico de barras con mediana
      p1 <- goals_agrupador %>%
        mutate(categoria = reorder(eval(mejor_agrupador_expr), mediana_dias)) %>%
        ggplot(aes(x = categoria, y = mediana_dias)) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        geom_errorbar(aes(ymin = q1, ymax = q3), 
                      width = 0.2, color = "darkred", size = 0.7) +
        geom_text(aes(label = paste("Goal:", goal, "días")), 
                  hjust = -0.1, size = 2.5, color = "darkgreen") +
        coord_flip() +
        labs(title = paste("Goal de Días de Cobertura por", mejor_agrupador, "- Grupo:", grupo_nombre),
             subtitle = "Mediana con rango intercuartílico (Q1-Q3)",
             x = mejor_agrupador,
             y = "Días de Cobertura (mediana)") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12),
              axis.text.y = element_text(size = 8))
    } else {
      # Si no tiene espacios ni guiones bajos, usar sym normalmente
      mejor_agrupador_sym <- sym(mejor_agrupador)
      
      # Gráfico de barras con mediana
      p1 <- goals_agrupador %>%
        mutate(categoria = reorder(!!mejor_agrupador_sym, mediana_dias)) %>%
        ggplot(aes(x = categoria, y = mediana_dias)) +
        geom_col(fill = "steelblue", alpha = 0.8) +
        geom_errorbar(aes(ymin = q1, ymax = q3), 
                      width = 0.2, color = "darkred", size = 0.7) +
        geom_text(aes(label = paste("Goal:", goal, "días")), 
                  hjust = -0.1, size = 2.5, color = "darkgreen") +
        coord_flip() +
        labs(title = paste("Goal de Días de Cobertura por", mejor_agrupador, "- Grupo:", grupo_nombre),
             subtitle = "Mediana con rango intercuartílico (Q1-Q3)",
             x = mejor_agrupador,
             y = "Días de Cobertura (mediana)") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 12),
              axis.text.y = element_text(size = 8))
    }
    
    ggsave(file.path(grupo_dir, "mejor_agrupador.png"), p1, 
           width = 12, height = max(8, nrow(goals_agrupador) * 0.25), dpi = 300)
  }
  
  # CLUSTERING
  clustering_result <- realizar_clustering(datos_grupo, grupo_dir, grupo_nombre)
  
  # RESUMEN EJECUTIVO
  cat("\n5. GENERANDO RESUMEN EJECUTIVO...\n")
  
  resumen_ejecutivo <- data.frame(
    Grupo = grupo_nombre,
    Mejor_Agrupador = mejor_agrupador,
    Score_Viabilidad = round(evaluacion_viabilidad$score_viabilidad, 3),
    Total_Registros = nrow(datos_grupo),
    Grupos_Identificados = ifelse(nrow(goals_agrupador) > 0, nrow(goals_agrupador), 0),
    Eta_Squared = round(evaluacion_viabilidad$mejor$eta_squared, 4),
    CV_Promedio = round(evaluacion_viabilidad$mejor$cv_promedio, 3),
    Mediana_Global = median(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE),
    Media_Global = mean(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE),
    SD_Global = sd(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE),
    Rango_Goals = ifelse(nrow(goals_agrupador) > 0, 
                         paste(min(goals_agrupador$goal), "-", max(goals_agrupador$goal), "días"),
                         "No disponible"),
    Fecha_Analisis = as.character(Sys.Date()),
    Es_Combinacion = ifelse(grupo_nombre == "ODG" && grepl("_con_Segmento", mejor_agrupador), "Sí", "No")
  )
  
  write_xlsx(resumen_ejecutivo, file.path(grupo_dir, "resumen_ejecutivo.xlsx"))
  
  # Imprimir resumen en consola
  cat("\nRESUMEN GRUPO", grupo_nombre, ":\n")
  cat("  - Total registros:", nrow(datos_grupo), "\n")
  cat("  - Mejor agrupador:", mejor_agrupador, "\n")
  cat("  - Score de viabilidad:", round(evaluacion_viabilidad$score_viabilidad, 3), "\n")
  if (grupo_nombre == "ODG" && grepl("_con_Segmento", mejor_agrupador)) {
    cat("     (Combinación con 'Segmento de puesto')\n")
  }
  cat("  - Grupos identificados:", ifelse(nrow(goals_agrupador) > 0, nrow(goals_agrupador), 0), "\n")
  cat("  - Mediana global:", round(median(datos_grupo$`Días cobertura con capacitación`, na.rm = TRUE)), "días\n")
  cat("  - Rango de goals:", ifelse(nrow(goals_agrupador) > 0, 
                                    paste(min(goals_agrupador$goal), "-", max(goals_agrupador$goal), "días"),
                                    "No disponible"), "\n")
  
  return(list(
    resumen = resumen_ejecutivo,
    mejor_agrupador = mejor_agrupador,
    es_combinacion = ifelse(grupo_nombre == "ODG" && grepl("_con_Segmento", mejor_agrupador), TRUE, FALSE),
    datos_grupo = datos_grupo,
    goals = goals_agrupador,
    evaluacion = evaluacion_viabilidad
  ))
}

# EJECUTAR ANÁLISIS PARA CADA GRUPO
cat("\n", strrep("*", 60))
cat("\nINICIANDO ANÁLISIS POR GRUPO DE GESTIÓN")
cat("\n", strrep("*", 60), "\n")

# Verificar que todos los grupos existen en los datos
grupos_existentes <- intersect(grupos, unique(datos_limpieza$Grupo))
grupos_no_existentes <- setdiff(grupos, grupos_existentes)

if (length(grupos_no_existentes) > 0) {
  cat("\nADVERTENCIA: Los siguientes grupos no se encontraron en los datos:\n")
  cat(paste(grupos_no_existentes, collapse = ", "), "\n")
}

if (length(grupos_existentes) == 0) {
  stop("Error: No se encontró ninguno de los grupos especificados en los datos.")
}

# Inicializar listas para resultados
resultados_grupos <- list()
resultados_detalle <- list()

# Ejecutar análisis para cada grupo existente
for (grupo in grupos_existentes) {
  resultado <- analizar_grupo(grupo, datos_limpieza)
  if (!is.null(resultado)) {
    resultados_grupos[[grupo]] <- resultado$resumen
    resultados_detalle[[grupo]] <- resultado
  }
}

# CREAR RESUMEN CONSOLIDADO DE TODOS LOS GRUPOS
cat("\n", strrep("*", 60))
cat("\nRESUMEN CONSOLIDADO DE TODOS LOS GRUPOS")
cat("\n", strrep("*", 60), "\n")

if (length(resultados_grupos) > 0) {
  # Consolidar todos los resúmenes
  resumen_consolidado <- bind_rows(resultados_grupos)
  
  # Guardar resumen consolidado
  write_xlsx(resumen_consolidado, file.path(output_dir, "resumen_consolidado_grupos.xlsx"))
  
  # Crear gráfico comparativo entre grupos
  datos_comparativo <- datos_limpieza %>%
    filter(Grupo %in% grupos_existentes) %>%
    group_by(Grupo) %>%
    summarise(
      mediana = median(`Días cobertura con capacitación`, na.rm = TRUE),
      media = mean(`Días cobertura con capacitación`, na.rm = TRUE),
      q1 = quantile(`Días cobertura con capacitación`, 0.25, na.rm = TRUE),
      q3 = quantile(`Días cobertura con capacitación`, 0.75, na.rm = TRUE),
      n = n(),
      .groups = 'drop'
    )
  
  p_comparativo <- ggplot(datos_comparativo, aes(x = reorder(Grupo, mediana), y = mediana)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    geom_errorbar(aes(ymin = q1, ymax = q3), width = 0.2, color = "darkred", size = 0.8) +
    geom_text(aes(label = paste("n =", n)), vjust = -0.5, size = 3.5) +
    geom_text(aes(label = paste(round(mediana), "días")), vjust = 1.5, size = 3.5, color = "white") +
    labs(title = "Comparativa de Días de Cobertura entre Grupos de Gestión",
         subtitle = "Mediana con rango intercuartílico (Q1-Q3)",
         x = "Grupo de Gestión",
         y = "Días de Cobertura (mediana)") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 16),
          axis.text = element_text(size = 11))
  
  ggsave(file.path(output_dir, "comparativa_grupos.png"), p_comparativo, width = 10, height = 7, dpi = 300)
  
  # Crear gráfico de viabilidad por grupo
  viabilidad_plot <- ggplot(resumen_consolidado, aes(x = reorder(Grupo, -Score_Viabilidad), y = Score_Viabilidad, fill = Score_Viabilidad)) +
    geom_col() +
    scale_fill_gradient(low = "red", high = "green", limits = c(0, 1)) +
    geom_text(aes(label = paste(round(Score_Viabilidad, 2), "\nη²=", Eta_Squared)), 
              vjust = -0.3, size = 3.5) +
    labs(title = "Viabilidad de Agrupadores por Grupo",
         subtitle = "Score de 0 (no viable) a 1 (altamente viable)",
         x = "Grupo de Gestión",
         y = "Score de Viabilidad") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold", size = 16),
          axis.text = element_text(size = 11))
  
  ggsave(file.path(output_dir, "viabilidad_agrupadores.png"), viabilidad_plot, width = 10, height = 7, dpi = 300)
  
  # Mostrar resumen en consola
  cat("\nRESUMEN CONSOLIDADO:\n")
  print(resumen_consolidado)
  
  # Análisis especial de combinaciones para ODG
  if ("ODG" %in% grupos_existentes) {
    resultado_odg <- resultados_detalle[["ODG"]]
    if (!is.null(resultado_odg) && resultado_odg$es_combinacion) {
      cat("\nANÁLISIS ESPECIAL PARA ODG:\n")
      cat("El mejor agrupador para ODG es una combinación con 'Segmento de puesto'.\n")
      cat("Esto sugiere que segmentar por categoría + segmento de puesto mejora la homogeneidad.\n")
      cat("Revisa el archivo 'evaluacion_agrupadores.xlsx' en la carpeta ODG para ver todas las combinaciones.\n")
    }
  }
  
  # Guardar datos limpios completos
  write_xlsx(datos_limpieza, file.path(output_dir, "datos_limpios_completos.xlsx"))
  
  # Crear archivo README con explicaciones
  readme_content <- paste0(
    "RESULTADOS DEL ANÁLISIS DE GOALS DE DÍAS DE COBERTURA\n",
    "Fecha: ", Sys.Date(), "\n",
    "\nNUEVA VARIABLE INCLUIDA: 'Nombre Reclutador'\n",
    ifelse(incluir_reclutador, 
           "- Se incluyó 'Nombre Reclutador' como variable agrupadora adicional\n",
           "- 'Nombre Reclutador' no se encontró en los datos\n"),
    "\nESTRUCTURA DE CARPETAS:\n",
    paste("- ", output_dir, "/", sep = ""), "\n",
    paste(sapply(grupos_existentes, function(g) paste("    ├── ", g, "/ (análisis específico del grupo)", sep = "")), collapse = "\n"), "\n",
    "    ├── comparativa_grupos.png (gráfico comparativo)\n",
    "    ├── viabilidad_agrupadores.png (viabilidad por grupo)\n",
    "    ├── resumen_consolidado_grupos.xlsx\n",
    "    └── datos_limpios_completos.xlsx\n",
    "\nEXPLICACIÓN DE ARCHIVOS EN CADA CARPETA DE GRUPO:\n",
    "- evaluacion_agrupadores.xlsx: Ranking de todos los agrupadores evaluados\n",
    "- goals_recomendados.xlsx: Goals específicos por categoría del mejor agrupador\n",
    "- resumen_ejecutivo.xlsx: Resumen estadístico del grupo con evaluación de viabilidad\n",
    "- mejor_agrupador.png: Gráfico de barras con los goals\n",
    "- clustering_elbow_plot.png: Método del codo para determinar número óptimo de clusters\n",
    "- clustering_pca_plot.png: Visualización de clusters en espacio PCA\n",
    "- clustering_silhouette_plot.png: Análisis de silueta para calidad de clusters\n",
    "- clustering_boxplot.png: Distribución de días por cluster\n",
    "- clustering_profile_plot.png: Perfil de cada cluster\n",
    "- analisis_clusters.xlsx: Estadísticas detalladas por cluster\n",
    "\nINTERPRETACIÓN DEL SCORE DE VIABILIDAD:\n",
    "- < 0.3: NO VIABLE - Considerar cambio de agrupador\n",
    "- 0.3-0.5: LIMITADAMENTE VIABLE - Necesita mejoras\n",
    "- 0.5-0.7: MODERADAMENTE VIABLE - Aceptable con monitoreo\n",
    "- > 0.7: ALTAMENTE VIABLE - Excelente agrupador\n",
    "\nNOTAS IMPORTANTES:\n",
    "1. Se utilizó la MEDIANA para calcular los goals (más robusta a outliers)\n",
    "2. Solo se consideraron categorías con al menos 5 observaciones\n",
    "3. Para el grupo ODG, se evaluaron combinaciones con 'Segmento de puesto'\n",
    "4. El score combina: tamaño del efecto (eta-squared), homogeneidad (CV) y parsimonia\n",
    "5. El clustering ayuda a identificar patrones naturales en los datos\n"
  )
  
  writeLines(readme_content, file.path(output_dir, "README.txt"))
  
  cat("\n", strrep("=", 60))
  cat("\nANÁLISIS COMPLETADO EXITOSAMENTE")
  cat("\n", strrep("=", 60))
  cat("\n\nRESULTADOS GUARDADOS EN:", output_dir)
  cat("\n\nRECOMENDACIONES:")
  cat("\n  1. Revisar los goals recomendados en cada carpeta de grupo")
  cat("\n  2. Para ODG, ver si las combinaciones con 'Segmento de puesto' son útiles")
  cat("\n  3. Validar los grupos con menos de 5 observaciones manualmente")
  cat("\n  4. Revisar los gráficos de clustering para entender patrones naturales")
  cat("\n  5. El análisis de silueta ayuda a evaluar la calidad de los clusters")
  cat("\n  6. Revisar el score de viabilidad para tomar decisiones informadas\n")
} else {
  cat("\nNo se pudo realizar análisis para ningún grupo.\n")
}