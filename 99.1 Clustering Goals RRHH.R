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
> # LIBRERÍAS
  > # =========================================================
> library(readxl)
> library(dplyr)
> library(stringr)
> library(purrr)
> 
  > # =========================================================
> # CARGA DE DATOS
  > # =========================================================
> file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
> datos <- read_excel(file_path)
> 
  > cat("\n========================================\n")

========================================
  > cat("DIAGNÓSTICO DE CAMPOS NUEVOS (CAPTURA MANUAL)\n")
DIAGNÓSTICO DE CAMPOS NUEVOS (CAPTURA MANUAL)
> cat("========================================\n")
========================================
  > 
  > # =========================================================
> # CAMPOS A ANALIZAR
  > # =========================================================
> campos_nuevos <- c(
  +   "Escolaridad",
  +   "Especialización",
  +   "Software-Avanzado",
  +   "Software-Intermedio",
  +   "Software-Básico"
  + )
> 
  > # Verificar que existan
  > campos_existentes <- intersect(campos_nuevos, colnames(datos))
> campos_faltantes <- setdiff(campos_nuevos, campos_existentes)
> 
  > if (length(campos_faltantes) > 0) {
    +   cat("\n⚠️ Campos no encontrados en el archivo:\n")
    +   cat(paste(campos_faltantes, collapse = ", "), "\n")
    + }
> 
  > cat("\nCampos analizados:\n")

Campos analizados:
  > cat(paste(campos_existentes, collapse = ", "), "\n")
Escolaridad, Especialización, Software-Avanzado, Software-Intermedio, Software-Básico 
> 
  > # =========================================================
> # FUNCIÓN DE PERFILADO DE CAMPO
  > # =========================================================
> perfil_campo <- function(df, campo) {
  + 
    +   cat("\n----------------------------------------\n")
  +   cat("CAMPO:", campo, "\n")
  +   cat("----------------------------------------\n")
  + 
    +   x <- df[[campo]]
    + 
      +   total <- length(x)
      +   na_count <- sum(is.na(x) | trimws(x) == "")
      +   na_pct <- round(100 * na_count / total, 1)
      + 
        +   cat("Total registros:", total, "\n")
      +   cat("Registros NA / vacíos:", na_count, "(", na_pct, "% )\n")
      + 
        +   valores <- df %>%
          +     mutate(valor = as.character(.data[[campo]])) %>%
          +     mutate(valor = trimws(valor)) %>%
          +     filter(!is.na(valor), valor != "") %>%
          +     count(valor, sort = TRUE)
        + 
          +   cat("Valores únicos (no NA):", nrow(valores), "\n")
        + 
          +   # Mostrar top 15 valores
          +   cat("\nTop valores más frecuentes:\n")
        +   print(head(valores, 15))
        + 
          +   # Señales de riesgo
          +   if (nrow(valores) > 30) {
            +     cat("⚠️ Alta cardinalidad (posible texto libre)\n")
            +   }
        + 
          +   if (any(str_detect(valores$valor, ","))) {
            +     cat("⚠️ Detectadas listas separadas por coma (checklist manual)\n")
            +   }
        + 
          +   if (any(str_detect(valores$valor, "/|\\+| y "))) {
            +     cat("⚠️ Detectadas combinaciones múltiples en un mismo registro\n")
            +   }
        + 
          +   if (any(str_detect(valores$valor, "^[Ss]í$|^[Nn]o$"))) {
            +     cat("ℹ️ Posible campo binario encubierto\n")
            +   }
        + 
          +   invisible(valores)
        + }
> 
  > # =========================================================
> # EJECUCIÓN DEL PERFILADO
  > # =========================================================
> walk(campos_existentes, ~ perfil_campo(datos, .x))

----------------------------------------
  CAMPO: Escolaridad 
----------------------------------------
  Total registros: 2403 
Registros NA / vacíos: 0 ( 0 % )
Valores únicos (no NA): 32 

Top valores más frecuentes:
  # A tibble: 15 × 2
  valor                                                                                   n
<chr>                                                                               <int>
  1 Preparatoria / Bachillerato, TSU, Licenciatura trunca, Licenciatura concluida         642
2 Preparatoria, bachillerato, Carrera técnica, TSU, Licenciatura trunca                 452
3 TSU, Licenciatura trunca, Licenciatura concluida                                      335
4 Preparatoria / Bachillerato, Carrera técnica, TSU, Licenciatura trunca.               266
5 Licenciatura concluida                                                                212
6 Preparatoria, bachillerato, Carrera técnica, TSU, Licenciatura trunc                  152
7 TSU, Licenciatura trunca, Licenciatura concluida.                                      72
8 Preparatoria/ Bachillerato, Carrera Técnica, TSU, Pasante.                             46
9 Preparatoria, Carrera técnica, TSU, Licenciatura trunca                                42
10 TSU, Licenciatura trunca, Licenciatura Concluida                                       36
11 Ingeniería / Licenciatura concluida                                                    24
12 TSU, Licenciatura trunca,                                                              21
13 Licenciatura / Ingeniería                                                              16
14 Preparatoria, bachillerato, Carrera técnica, TSU, Licenciatura e Ingeniería trunca     16
15 Preparatoria / bachillerato, Carrera técnica, TSU, Licenciatura trunca o concluida.    14
⚠️ Alta cardinalidad (posible texto libre)
⚠️ Detectadas listas separadas por coma (checklist manual)
⚠️ Detectadas combinaciones múltiples en un mismo registro

----------------------------------------
  CAMPO: Especialización 
----------------------------------------
  Total registros: 2403 
Registros NA / vacíos: 0 ( 0 % )
Valores únicos (no NA): 120 

Top valores más frecuentes:
  # A tibble: 15 × 2
  valor                                                                                                      n
<chr>                                                                                                  <int>
  1 Administración de Empresas, Contaduría Pública o carrera afín                                            688
2 Áreas Administrativas                                                                                    521
3 Administración de Empresas, Contaduría Pública o carrera afín.                                           268
4 Económico-Administrativas, Ciencias ambientales, Sustentabilidad, Responsabilidad y Desarrollo Social.   191
5 Derecho                                                                                                  119
6 Áreas administrativas                                                                                    101
7 Contaduría Pública                                                                                        78
8 Informática, Ingeniería en Sistemas                                                                       29
9 Mercadotecnia, Administración de empresas o carrera afín.                                                 28
10 Ingeniero Agrónomo                                                                                        24
11 Agronomía, Administración o carrera afín                                                                  21
12 Informática Administrativa, Ingeniería en Sistemas, Tecnologías de la información o carrera afín.         20
13 Administración                                                                                            17
14 Informática, Ing. en Sistemas.                                                                            16
15 Mantenimiento de inmuebles                                                                                15
⚠️ Alta cardinalidad (posible texto libre)
⚠️ Detectadas listas separadas por coma (checklist manual)
⚠️ Detectadas combinaciones múltiples en un mismo registro

----------------------------------------
  CAMPO: Software-Avanzado 
----------------------------------------
  Total registros: 2403 
Registros NA / vacíos: 2351 ( 97.8 % )
Valores únicos (no NA): 15 

Top valores más frecuentes:
  # A tibble: 15 × 2
  valor                                                                                                                                                       n
<chr>                                                                                                                                                   <int>
  1 Visual C# (.Net/Net Core), Visual Studio                                                                                                                   19
2 CRS, SCACS                                                                                                                                                  9
3 Office                                                                                                                                                      4
4 n/a                                                                                                                                                         4
5 Conocimiento en sistemas Operativos Windows, Linux, Manejo de Paquetería Office                                                                             3
6 Frameworks de automatización de pruebas (Selenium, Cypress, Playwright, Appium), Automatización de pruebas de API (Postman, RestAssured, Karate, SOAP …     2
                                                                                                                     7 Impresión de Documentos, Microsoft Office, carpetas y archivos compartidos, Adobe PDF, IM (mensajero instataneo), Navegador web                             2
                                                                                                                     8 OFFICE                                                                                                                                                      2
                                                                                                                     9 Construcción de matrices de pruebas (UI, aceptación, regresión, smoke tests)                                                                                1
                                                                                                                     10 Directorio Activo Entrega de Haberes a Ex-Socios e Tesorería Exchange - Correo Electrónico iSeries ICBS Infórmate Mensajeria Instantánea - Microsoft L…     1
                                                                                                                     11 ESB/Event.Driven/ API-Led/ Microservicios, REST/ GraphQL/ SOAP, EIP, OAuth, JWT, OpenID Connect                                                             1
                                                                                                                     12 EXCELL                                                                                                                                                      1
                                                                                                                     13 Frameworks de automatización (Selenium, Cypress, Playwright), Gestión de pruebas y defectos (JIRA, TestRail, Xray, Azure Test Plans), Integración de p…     1
                                                                                                                     14 Microsoft Office,  Carpetas y archivos compartidos ,Adobe PDF,  Microsoft Teams (mensajero instantáneo) , Navegador Web , Microsoft Project, Microsoft…     1
                                                                                                                     15 Microsoft Office, Carpetas y archivos compartidos, Adobe PDF, IM (mensajero instantáneo), Navegador Web, Impresión de Documentos (nivel experto).           1
                                                                                                                     ⚠️ Detectadas listas separadas por coma (checklist manual)
                                                                                                                     ⚠️ Detectadas combinaciones múltiples en un mismo registro
                                                                                                                     
                                                                                                                     ----------------------------------------
                                                                                                                       CAMPO: Software-Intermedio 
                                                                                                                     ----------------------------------------
                                                                                                                       Total registros: 2403 
                                                                                                                     Registros NA / vacíos: 1423 ( 59.2 % )
                                                                                                                     Valores únicos (no NA): 113 
                                                                                                                     
                                                                                                                     Top valores más frecuentes:
                                                                                                                       # A tibble: 15 × 2
                                                                                                                       valor                                                                                                                                                       n
                                                                                                                     <chr>                                                                                                                                                   <int>
                                                                                                                       1 ABT                                                                                                                                                       268
                                                                                                                     2 Directorio Activo, Correo Electrónico, Infórmate, Mensajería Instantánea - Microsoft Teams, SAP SuccesFactors, PROFORCOOP - Sistema de Formación Coope…   191
                                                                                                                     3 Directorio activo eTesoreria Exchange - Correo Electronico iSeries ICBS Infórmate SAFI-SAT3 / CPM-DIOT PeopleSoft Finanzas SAP SuccesFactors Sistema d…    69
                                                                                                                     4 Office, CPMóvil                                                                                                                                            69
                                                                                                                     5 Office                                                                                                                                                     38
                                                                                                                     6 Directorio Activo Exchange - Correo Electrónico Infórmate Mensajería Instantánea - Microsoft Lync SAP SuccesFactors Sistema de Gestión Integral de Obj…    28
                                                                                                                     7 Iseries ICBS  SISCO                                                                                                                                        21
                                                                                                                     8 Office, Javascript, IIS, Postman /soap, SQL Server, Sharepoint,                                                                                            19
                                                                                                                     9 Office, Exchange-Correo Electronico, Informate, PeoplesoftFinanzas, SAP Success Factors                                                                    17
                                                                                                                     10 Office, Footprints                                                                                                                                         16
                                                                                                                     11 Office FTP                                                                                                                                                 14
                                                                                                                     12 Office  Inform@te ICBS Emulador Footprints Gestor de pagos y servicios Webtopo                                                                             12
                                                                                                                     13 Office, ICBS Emulador, Visual Studio, SQL Server,  Servicios WEB / API                                                                                     11
                                                                                                                     14 EMULADOR, WEBTOP                                                                                                                                            9
                                                                                                                     15 OFFICE VISIO BPM PROJECT                                                                                                                                    8
                                                                                                                     ⚠️ Alta cardinalidad (posible texto libre)
                                                                                                                     ⚠️ Detectadas listas separadas por coma (checklist manual)
                                                                                                                     ⚠️ Detectadas combinaciones múltiples en un mismo registro
                                                                                                                     
                                                                                                                     ----------------------------------------
                                                                                                                       CAMPO: Software-Básico 
                                                                                                                     ----------------------------------------
                                                                                                                       Total registros: 2403 
                                                                                                                     Registros NA / vacíos: 930 ( 38.7 % )
                                                                                                                     Valores únicos (no NA): 37 
                                                                                                                     
                                                                                                                     Top valores más frecuentes:
                                                                                                                       # A tibble: 15 × 2
                                                                                                                       valor                                                                                                                                                       n
                                                                                                                     <chr>                                                                                                                                                   <int>
                                                                                                                       1 Directorio Activo SAP SuccesFactors Sistema de Gestión Integral de Objetivos CPM Sistema de Impresión                                                     642
                                                                                                                     2 CPMovil - CRM Dynamics Directorio Activo iSeries ICBS SAP SuccesFactors VF Investigaciones Mixtas Sistema de Gestión Integral de Objetivos CRM Investi…   259
                                                                                                                     3 CPMovil - CRM Dynamics Directorio Activo Documentum (WebTop) Exchange - Correo Electronico Infórmate SAP SuccesFactors VF Investigaciones Mixtas Siste…   152
                                                                                                                     4 Directorio Activo Documentum (WebTop) Exchange - Correo Electronico iSeries ICBS Informate SAP SuccesFactors VF Investigaciones Mixtas Sistema de Gest…   110
                                                                                                                     5 Directorio Activo Exchange - Correo Electrónico Mensajeria Instantanea - Microsoft Lync PeopleSoft Finanzas SAP SuccesFactors Sistema de Gestión Integ…    56
                                                                                                                     6 Office CPMóvil                                                                                                                                             46
                                                                                                                     7 Cartera Eliminada Office CPMovil - CRM Dynamics Directorio Activo Documentum (WebTop) Exchange - Correo Electronico iSeries ICBS SAP SuccesFactors Sis…    42
                                                                                                                     8 Office                                                                                                                                                     41
                                                                                                                     9 DevOps, Git, Teams                                                                                                                                         19
                                                                                                                     10 Office, Exchance-Correo Electronico, SAP SuccessFactors                                                                                                    15
                                                                                                                     11 Directorio Activo Exchange-correo electrónico Infórmate Mensajería instantánea-Lync PeopleSoft Finanzas SAP Succes Factor Sistema integral de objetivo…    14
                                                                                                                     12 Visual C#,  .Net / Net Core                                                                                                                                11
                                                                                                                     13 Office, ICBS Emulador, Power Center.                                                                                                                        8
                                                                                                                     14 Cartera Eliminada CPMovil - CRM Dynamics Directorio Activo Exchange - Correo Electrónico iSeries ICBS Informate SAP SuccesFactors SIGMA CONDONACIONES …     7
                                                                                                                     15 EMULADOR                                                                                                                                                    6
                                                                                                                     ⚠️ Alta cardinalidad (posible texto libre)
                                                                                                                     ⚠️ Detectadas listas separadas por coma (checklist manual)
                                                                                                                     ⚠️ Detectadas combinaciones múltiples en un mismo registro
                                                                                                                     > 
                                                                                                                       > cat("\n========================================\n")
                                                                                                                     
                                                                                                                     ========================================
                                                                                                                       > cat("FIN DEL DIAGNÓSTICO DE CAMPOS NUEVOS\n")
                                                                                                                     FIN DEL DIAGNÓSTICO DE CAMPOS NUEVOS
                                                                                                                     > cat("========================================\n")
                                                                                                                     ========================================
