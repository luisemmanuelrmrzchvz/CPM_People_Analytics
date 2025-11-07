# =============================================================================
# SCRIPT CORREGIDO - PERFILAMIENTO DE CONSEJOS Y MIEMBROS
# =============================================================================

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(openxlsx)
library(stringr)
library(tidyr)
library(purrr)

# Configurar opciones
options(stringsAsFactors = FALSE)

# =============================================================================
# 1. DEFINICIÓN DE PARÁMETROS Y FUNCIONES BASE
# =============================================================================

# Definir generaciones con rangos actualizados
generaciones <- data.frame(
  Generacion = c("Tradicionalistas", "Boomers - Tempranos", "Boomers - Tardíos",
                 "Gen X - Tempranos", "Gen X - Tardíos", "Millennials - Tempranos",
                 "Millennials - Tardíos", "Gen Z - Tempranos", "Gen Z - Tardíos"),
  Inicio = c(-Inf, 1946, 1956, 1965, 1974, 1981, 1989, 1997, 2005),
  Fin = c(1945, 1955, 1964, 1973, 1980, 1988, 1996, 2004, 2012)
)

# Función para asignar generación
asignar_generacion <- function(anio_nac) {
  case_when(
    anio_nac <= 1945 ~ "Tradicionalistas",
    between(anio_nac, 1946, 1955) ~ "Boomers - Tempranos",
    between(anio_nac, 1956, 1964) ~ "Boomers - Tardíos",
    between(anio_nac, 1965, 1973) ~ "Gen X - Tempranos",
    between(anio_nac, 1974, 1980) ~ "Gen X - Tardíos",
    between(anio_nac, 1981, 1988) ~ "Millennials - Tempranos",
    between(anio_nac, 1989, 1996) ~ "Millennials - Tardíos",
    between(anio_nac, 1997, 2004) ~ "Gen Z - Tempranos",
    between(anio_nac, 2005, 2012) ~ "Gen Z - Tardíos",
    TRUE ~ "Fuera de rango"
  )
}

# =============================================================================
# 2. CARGA Y PREPARACIÓN DE DATOS
# =============================================================================

# Leer archivo de datos
cat("Cargando datos...\n")
datos <- read_excel("C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Listado_Dirigentes.xlsx")

# Estandarizar nombres de columnas
names_originales <- names(datos)
names_estandarizados <- str_replace_all(names_originales, " ", "_")
names(datos) <- names_estandarizados

# Estandarizar campos de texto
datos <- datos %>%
  mutate(
    across(where(is.character), ~ str_to_title(.x)),
    Órgano_dirigencial = case_when(
      str_detect(Órgano_dirigencial, "(?i)coprosu") ~ "Coprosu",
      str_detect(Órgano_dirigencial, "(?i)cap") ~ "CAP",
      str_detect(Órgano_dirigencial, "(?i)administración") ~ "Consejo_De_Administracion",
      str_detect(Órgano_dirigencial, "(?i)vigilancia") ~ "Consejo_De_Vigilancia",
      TRUE ~ Órgano_dirigencial
    ),
    Periodo = case_when(
      str_detect(Periodo, "(?i)primer") ~ "Primer_Periodo",
      str_detect(Periodo, "(?i)segundo") ~ "Segundo_Periodo",
      TRUE ~ Periodo
    ),
    Género = case_when(
      str_detect(Género, "(?i)mujer") ~ "Mujer",
      str_detect(Género, "(?i)hombre") ~ "Hombre",
      TRUE ~ Género
    ),
    Exdirigente_o_excolaborador = case_when(
      str_detect(Exdirigente_o_excolaborador, "(?i)ex.colaborador") ~ "Ex_Colaborador",
      str_detect(Exdirigente_o_excolaborador, "(?i)ex.dirigente") ~ "Ex_Dirigente",
      str_detect(Exdirigente_o_excolaborador, "(?i)no.aplica") ~ "No_Aplica",
      TRUE ~ Exdirigente_o_excolaborador
    )
  )

# Calcular edad y años en el consejo
hoy <- as.Date(Sys.Date())
datos <- datos %>%
  mutate(
    Fecha_nacimiento = as.Date(Fecha_de_nacimiento),
    Fecha_ingreso = as.Date(Fecha_de_ingreso),
    Edad = as.numeric(difftime(hoy, Fecha_nacimiento, units = "days")) / 365.25,
    Anios_en_consejo = as.numeric(difftime(hoy, Fecha_ingreso, units = "days")) / 365.25,
    Anio_nacimiento = as.numeric(format(Fecha_nacimiento, "%Y")),
    Generacion = asignar_generacion(Anio_nacimiento)
  )

# =============================================================================
# 3. PERFILAMIENTO INDIVIDUAL - VERSIÓN SIMPLIFICADA Y ROBUSTA
# =============================================================================

cat("Calculando perfiles individuales...\n")

calcular_perfil_individual <- function(datos) {
  datos %>%
    mutate(
      # Puntuación de predisposición al cambio (0-100)
      puntos_edad = case_when(
        Edad <= 35 ~ 30,
        Edad <= 45 ~ 20,
        Edad <= 55 ~ 10,
        Edad <= 65 ~ 0,
        TRUE ~ -10
      ),
      
      puntos_experiencia = case_when(
        Periodo == "Primer_Periodo" ~ 25,
        TRUE ~ 0
      ),
      
      puntos_area = case_when(
        Clasificación_por_Profesión %in% c("Ingenierías", "Ciencias Físicas, Químicas y de la Tierra") ~ 20,
        Clasificación_por_Profesión %in% c("Negocios Y Administración") ~ 15,
        Clasificación_por_Profesión %in% c("Ciencias Exactas", "Ciencias De La Comunicación", "Ciencias Naturales") ~ 10,
        Clasificación_por_Profesión %in% c("Ciencias Sociales", "Humanidades", "Ciencias De La Educación") ~ 5,
        Clasificación_por_Profesión == "Sin Profesión" ~ -5,
        TRUE ~ 0
      ),
      
      puntos_background = case_when(
        Exdirigente_o_excolaborador == "Ex_Colaborador" ~ 15,
        Exdirigente_o_excolaborador == "No_Aplica" ~ 5,
        TRUE ~ 0
      ),
      
      puntos_genero = case_when(
        Género == "Mujer" ~ 10,
        TRUE ~ 5
      ),
      
      # Puntuación total
      puntuacion_cambio = puntos_edad + puntos_experiencia + puntos_area + 
        puntos_background + puntos_genero,
      
      # Segmentación por predisposición al cambio
      segmento_cambio = case_when(
        puntuacion_cambio >= 80 ~ "Innovador",
        puntuacion_cambio >= 60 ~ "Adaptador",
        puntuacion_cambio >= 40 ~ "Moderado",
        puntuacion_cambio >= 20 ~ "Cauteloso",
        TRUE ~ "Tradicional"
      ),
      
      # Capacidad técnica
      capacidad_tecnica = case_when(
        Clasificación_por_Profesión %in% c("Ingenierías", "Negocios Y Administración", "Ciencias Físicas, Químicas Y De La Tierra",
                                           "Ciencias Exactas", "Ciencias Naturales", "Ciencias de la Salud", "Agronomía Y Veterinaria") ~ "Alta",
        Clasificación_por_Profesión %in% c("Ciencias Sociales", "Ciencias De La Educación", "Ciencias de la Comunicación",
                                           "Humanidades", "Artísticas") ~ "Media",
        TRUE ~ "Baja"
      ),
      
      # Influencia institucional
      influencia_institucional = case_when(
        Periodo == "Segundo_Periodo" & Exdirigente_o_excolaborador == "Ex_Dirigente" ~ "Alta",
        Periodo == "Segundo_Periodo" | Exdirigente_o_excolaborador == "Ex_Colaborador" ~ "Media",
        TRUE ~ "Baja"
      ),
      
      # Perfil individual consolidado
      perfil_individual = paste(
        segmento_cambio,
        capacidad_tecnica,
        influencia_institucional,
        sep = " | "
      ),
      
      # Valor estratégico
      valor_estrategico = case_when(
        segmento_cambio %in% c("Innovador", "Adaptador") & 
          capacidad_tecnica == "Alta" ~ "Alto",
        segmento_cambio %in% c("Innovador", "Adaptador") |
          (capacidad_tecnica == "Alta" & influencia_institucional == "Alta") ~ "Medio_Alto",
        TRUE ~ "Medio"
      )
    )
}

# Aplicar perfilamiento individual
datos_individual <- calcular_perfil_individual(datos)

# =============================================================================
# 4. IDENTIFICACIÓN DE CONSEJOS
# =============================================================================

cat("Identificando consejos...\n")

datos_individual <- datos_individual %>%
  mutate(
    ID_Consejo = case_when(
      Órgano_dirigencial == "Coprosu" ~ paste("Coprosu", Sucursal, sep = "_"),
      Órgano_dirigencial == "CAP" ~ paste("CAP", Plaza, sep = "_"),
      Órgano_dirigencial == "Consejo_De_Administracion" ~ "Consejo_Administracion_Nacional",
      Órgano_dirigencial == "Consejo_De_Vigilancia" ~ "Consejo_Vigilancia_Nacional",
      TRUE ~ "Otro"
    ),
    Tipo_Consejo = Órgano_dirigencial
  )

# =============================================================================
# 5. PERFILAMIENTO DE CONSEJOS - VERSIÓN CORREGIDA
# =============================================================================

cat("Calculando perfiles de consejos...\n")

# Función simplificada y robusta para calcular dinámicas de consejo
calcular_dinamica_consejo <- function(consejo_data) {
  
  # Si no hay datos, retornar valores por defecto
  if (nrow(consejo_data) == 0) {
    return(data.frame(
      perfil_consejo = "Sin_Datos",
      dinamica_consejo = "Sin_Datos",
      capacidad_tecnica_consejo = "Sin_Datos",
      sensibilidad_cambio = "Sin_Datos",
      generacion_primaria = "Sin_Datos",
      n_miembros = 0,
      porcentaje_innovadores = 0,
      porcentaje_adaptadores = 0,
      porcentaje_moderados = 0,
      porcentaje_cautelosos = 0,
      porcentaje_tradicionales = 0,
      puntuacion_cambio_promedio = 0,
      recomendacion_seguimiento = "No_aplica",
      miembros_clave = "No_identificados",
      miembros_alta_influencia = 0
    ))
  }
  
  n_miembros <- nrow(consejo_data)
  
  # Calcular métricas básicas del consejo
  porcentaje_innovadores <- sum(consejo_data$segmento_cambio == "Innovador", na.rm = TRUE) / n_miembros * 100
  porcentaje_adaptadores <- sum(consejo_data$segmento_cambio == "Adaptador", na.rm = TRUE) / n_miembros * 100
  porcentaje_moderados <- sum(consejo_data$segmento_cambio == "Moderado", na.rm = TRUE) / n_miembros * 100
  porcentaje_cautelosos <- sum(consejo_data$segmento_cambio == "Cauteloso", na.rm = TRUE) / n_miembros * 100
  porcentaje_tradicionales <- sum(consejo_data$segmento_cambio == "Tradicional", na.rm = TRUE) / n_miembros * 100
  
  puntuacion_cambio_promedio <- mean(consejo_data$puntuacion_cambio, na.rm = TRUE)
  
  # Determinar capacidad técnica predominante
  capacidad_counts <- table(consejo_data$capacidad_tecnica)
  capacidad_tecnica_consejo <- ifelse(length(capacidad_counts) > 0, 
                                      names(which.max(capacidad_counts)), "Sin_Datos")
  
  # Determinar generación predominante
  generacion_counts <- table(consejo_data$Generacion)
  generacion_primaria <- ifelse(length(generacion_counts) > 0,
                                names(which.max(generacion_counts)), "Sin_Datos")
  
  # Determinar sensibilidad al cambio basada en puntuación promedio
  sensibilidad_cambio <- case_when(
    puntuacion_cambio_promedio > 75 ~ "Receptividad_Muy_Alta",
    puntuacion_cambio_promedio > 60 ~ "Receptividad_Alta",
    puntuacion_cambio_promedio > 50 ~ "Receptividad_Media_Alta",
    puntuacion_cambio_promedio > 40 ~ "Receptividad_Media_Baja",
    puntuacion_cambio_promedio > 25 ~ "Receptividad_Baja",
    TRUE ~ "Resistencia_Muy_Alta"
  )
  
  # Determinar dinámica basada en composición de segmentos
  total_pro_cambio <- porcentaje_innovadores + porcentaje_adaptadores
  total_anti_cambio <- porcentaje_cautelosos + porcentaje_tradicionales
  
  if (total_pro_cambio >= 60) {
    dinamica_consejo <- "Consenso_Innovador"
  } else if (total_anti_cambio >= 60) {
    dinamica_consejo <- "Consenso_Conservador"
  } else if (abs(total_pro_cambio - total_anti_cambio) < 20) {
    dinamica_consejo <- "Equilibrado"
  } else if (porcentaje_moderados >= 40) {
    dinamica_consejo <- "Centro_Moderado"
  } else {
    dinamica_consejo <- "Mixto_Complejo"
  }
  
  # Identificar miembros clave (los 3 con mayor puntuación de cambio)
  miembros_clave_df <- consejo_data %>%
    arrange(desc(puntuacion_cambio)) %>%
    head(3)
  
  miembros_clave <- paste(miembros_clave_df$Nombre, "(", 
                          miembros_clave_df$segmento_cambio, ")", 
                          sep = "", collapse = "; ")
  
  # Recomendaciones basadas en la dinámica
  recomendacion_seguimiento <- case_when(
    dinamica_consejo == "Consenso_Innovador" ~ "Facilitar_implementacion_seguimiento_trimestral",
    dinamica_consejo == "Consenso_Conservador" ~ "Cambios_incrementales_justificacion_solida_seguimiento_mensual",
    dinamica_consejo == "Equilibrado" ~ "Identificar_votantes_clave_mensajes_consenso_seguimiento_bimestral",
    dinamica_consejo == "Centro_Moderado" ~ "Evidencia_solida_beneficios_claros_seguimiento_bimestral",
    TRUE ~ "Segmentacion_mensajes_seguimiento_mensual"
  )
  
  # Crear perfil consolidado
  perfil_consejo <- paste(
    dinamica_consejo,
    capacidad_tecnica_consejo,
    sensibilidad_cambio,
    generacion_primaria,
    sep = " | "
  )
  
  return(data.frame(
    perfil_consejo = perfil_consejo,
    dinamica_consejo = dinamica_consejo,
    capacidad_tecnica_consejo = capacidad_tecnica_consejo,
    sensibilidad_cambio = sensibilidad_cambio,
    generacion_primaria = generacion_primaria,
    n_miembros = n_miembros,
    porcentaje_innovadores = round(porcentaje_innovadores, 1),
    porcentaje_adaptadores = round(porcentaje_adaptadores, 1),
    porcentaje_moderados = round(porcentaje_moderados, 1),
    porcentaje_cautelosos = round(porcentaje_cautelosos, 1),
    porcentaje_tradicionales = round(porcentaje_tradicionales, 1),
    puntuacion_cambio_promedio = round(puntuacion_cambio_promedio, 1),
    recomendacion_seguimiento = recomendacion_seguimiento,
    miembros_clave = miembros_clave,
    miembros_alta_influencia = nrow(miembros_clave_df)
  ))
}

# Aplicar perfilamiento a todos los consejos
perfiles_consejos <- datos_individual %>%
  group_by(ID_Consejo, Tipo_Consejo) %>%
  group_modify(~ calcular_dinamica_consejo(.x))

# =============================================================================
# 6. INTEGRACIÓN DE RESULTADOS
# =============================================================================

cat("Integrando resultados...\n")

# Unir perfiles de consejo con datos individuales
datos_completos <- datos_individual %>%
  left_join(perfiles_consejos, by = c("ID_Consejo", "Tipo_Consejo"))

# =============================================================================
# 7. GENERACIÓN DE ARCHIVOS DE SALIDA
# =============================================================================

cat("Generando archivos de salida...\n")

# Crear directorio de salida si no existe
dir_output <- "C:/Users/racl26345/Documents/Reportes Automatizados/Perfil Consejo"
if (!dir.exists(dir_output)) {
  dir.create(dir_output, recursive = TRUE)
}

# Archivo 1: Datos completos con perfiles individuales y de consejo
write.xlsx(
  datos_completos %>% 
    select(
      ID, Nombre, Regional, Plaza, Sucursal, Órgano_dirigencial,
      Generacion, Edad, Género, Estado_civil, Escolaridad, 
      Clasificación_por_Profesión, Periodo, Año_en_que_concluye_periodo,
      segmento_cambio, capacidad_tecnica, influencia_institucional,
      perfil_individual, valor_estrategico, puntuacion_cambio,
      ID_Consejo, perfil_consejo, dinamica_consejo, sensibilidad_cambio,
      recomendacion_seguimiento
    ),
  paste0(dir_output, "/Perfil_Completo_Consejeros_CORREGIDO.xlsx"),
  overwrite = TRUE
)

# Archivo 2: Resumen ejecutivo de consejos
write.xlsx(
  perfiles_consejos,
  paste0(dir_output, "/Resumen_Ejecutivo_Consejos_CORREGIDO.xlsx"),
  overwrite = TRUE
)

# Archivo 3: Estadísticas agregadas
estadisticas_generales <- list(
  "Resumen_General" = data.frame(
    Metric = c(
      "Total Miembros", "Total Consejos", "Consejos Pro-Cambio", 
      "Consejos Conservadores", "Consejos Balanceados", "Edad Promedio",
      "Años Experiencia Promedio", "Porcentaje Mujeres"
    ),
    Value = c(
      nrow(datos_completos),
      nrow(perfiles_consejos),
      sum(perfiles_consejos$dinamica_consejo == "Consenso_Innovador", na.rm = TRUE),
      sum(perfiles_consejos$dinamica_consejo == "Consenso_Conservador", na.rm = TRUE),
      sum(perfiles_consejos$dinamica_consejo %in% c("Equilibrado", "Centro_Moderado"), na.rm = TRUE),
      round(mean(datos_completos$Edad, na.rm = TRUE), 1),
      round(mean(datos_completos$Anios_en_consejo, na.rm = TRUE), 1),
      round(mean(datos_completos$Género == "Mujer", na.rm = TRUE) * 100, 1)
    )
  ),
  "Distribucion_Segmentos" = as.data.frame(table(datos_completos$segmento_cambio)),
  "Distribucion_Dinamicas" = as.data.frame(table(perfiles_consejos$dinamica_consejo))
)

write.xlsx(
  estadisticas_generales,
  paste0(dir_output, "/Estadisticas_Generales_CORREGIDO.xlsx"),
  overwrite = TRUE
)

# =============================================================================
# 8. MENSAJE FINAL
# =============================================================================

cat("¡Proceso completado exitosamente!\n\n")
cat("Archivos generados:\n")
cat("1. Perfil_Completo_Consejeros_CORREGIDO.xlsx\n")
cat("2. Resumen_Ejecutivo_Consejos_CORREGIDO.xlsx\n")
cat("3. Estadisticas_Generales_CORREGIDO.xlsx\n\n")

cat("Resumen del análisis:\n")
cat("- Total miembros procesados:", nrow(datos_completos), "\n")
cat("- Total consejos identificados:", nrow(perfiles_consejos), "\n")

if (exists("perfiles_consejos")) {
  cat("\nDistribución de dinámicas de consejos:\n")
  print(table(perfiles_consejos$dinamica_consejo))
}

cat("\n¡Análisis completado correctamente!\n")