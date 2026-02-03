> ############################################################
> #  EXPLORACIÓN AVANZADA – DÍAS DE COBERTURA CON CAPACITACIÓN
  > #  Versión: imprime resumen en consola + guarda CSV
  > ############################################################
> 
  > # =========================
> # 1. LIBRERÍAS
  > # =========================
> library(readxl)
> library(dplyr)
> library(tidyr)
> library(purrr)
> library(stringr)
> library(broom)
> library(forcats)
> 
  > # =========================
> # 2. RUTAS FIJAS (NO CAMBIAR)
  > # =========================
> input_file <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
> output_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura"
> 
  > # =========================
> # 3. PARÁMETROS (ajustables)
  > # =========================
> target_var <- "Días cobertura con capacitación"  # EXACTO
> vars_exploratorias <- c(
  +   "Regional", "Plaza", "DescripcionCC", "Estado",
  +   "Nombre Reclutador", "Perfil Profesional",
  +   "Segmento de puesto", "Area de Personal",
  +   "Puesto Generico", "Familia de Puesto",
  +   "Escolaridad", "Especialización",
  +   "Software-Avanzado", "Software-Intermedio", "Software-Básico"
  + )
> min_n_indiv <- 15   # mínimo por nivel para considerar variable individual
> min_n_pair  <- 20   # mínimo por celda para combinaciones de 2
> top_print   <- 10   # cuántas filas mostrar por tabla en consola
> 
  > # =========================
> # 4. FUNCIONES AUXILIARES
  > # =========================
> calc_dispersion <- function(x) {
  +   x <- x[!is.na(x)]
  +   n <- length(x)
  +   if (n == 0) return(tibble(n = 0, mean = NA_real_, sd = NA_real_, cv = NA_real_, iqr = NA_real_))
  +   m <- mean(x)
  +   sdv <- sd(x)
  +   tibble(
    +     n = n,
    +     mean = m,
    +     sd = sdv,
    +     cv = ifelse(is.na(m) | m == 0, NA_real_, sdv / m),
    +     iqr = IQR(x)
    +   )
  + }
> 
  > calc_eta_sq_safe <- function(df, formula) {
    +   # devuelve NA si no se puede calcular
      +   res <- tryCatch({
        +     aov_model <- aov(formula, data = df)
        +     aov_tab <- stats::anova(aov_model)
        +     if (nrow(aov_tab) < 1) return(NA_real_)
        +     ss_effect <- aov_tab[1, "Sum Sq"]
        +     ss_total <- sum(aov_tab$"Sum Sq", na.rm = TRUE)
        +     if (is.na(ss_effect) || ss_total == 0) return(NA_real_)
        +     as.numeric(ss_effect / ss_total)
        +   }, error = function(e) NA_real_)
      +   return(res)
      + }
  > 
    > # =========================
  > # 5. CARGA DE DATOS Y CHEQUEOS
    > # =========================
  > df_raw <- read_excel(input_file)
  Hubo 50 o más avisos (use warnings() para ver los primeros 50)
  > 
    > if (! target_var %in% colnames(df_raw)) {
      +   stop(paste0("La variable objetivo '", target_var, "' NO existe en el archivo. Revisa nombres exactos."))
      + }
  > 
    > # Hacemos copia local
    > df <- df_raw
  > 
    > # =========================
  > # 6. ANÁLISIS POR GRUPO con PRINT
    > # =========================
  > output_list <- list()
  > message("Iniciando análisis por Grupo...")
  Iniciando análisis por Grupo...
  > 
    > for (g in unique(df$Grupo)) {
      +   message("\n----------------------------")
      +   message("GRUPO: ", g)
      +   message("----------------------------")
      +   df_g <- df %>% filter(Grupo == g)
      + 
        +   # --- INDIVIDUALES ---
        +   indiv_tbls <- map_dfr(vars_exploratorias, function(v) {
          +     # contar por nivel y calcular métricas
            +     tmp <- df_g %>%
              +       filter(!is.na(.data[[v]])) %>%
              +       group_by(level = as.character(.data[[v]])) %>%
              +       summarise(calc_dispersion(.data[[target_var]]), .groups = "drop") %>%
              +       rename(Valor = level)
            +     # filtrar por n mínimo
              +     tmp <- tmp %>% filter(n >= min_n_indiv)
              +     if (nrow(tmp) == 0) return(tibble())
              +     eta <- calc_eta_sq_safe(df_g %>% filter(!is.na(.data[[v]])), as.formula(paste0("`", target_var, "` ~ `", v, "`")))
              +     tmp %>%
                +       mutate(
                  +         Grupo = g,
                  +         Variable = v,
                  +         Tipo = "Individual",
                  +         Eta2 = eta
                  +       ) %>%
                +       select(Grupo, Variable, Valor, n, mean, sd, cv, iqr, Eta2, Tipo)
              +   })
        + 
          +   if (nrow(indiv_tbls) == 0) {
            +     message("-> No se encontraron niveles individuales con n >= ", min_n_indiv, " en este Grupo.")
            +   } else {
              +     # resumimos por Variable (promedio/ponderado?) — aquí mostramos la media de medias para orden
                +     summary_indiv <- indiv_tbls %>%
                  +       group_by(Grupo, Variable) %>%
                  +       summarise(
                    +         eta_mean = unique(na.omit(Eta2))[1],
                    +         n_levels = n(),
                    +         mean_mean = mean(mean, na.rm = TRUE),
                    +         sd_mean = mean(sd, na.rm = TRUE),
                    +         cv_mean = mean(cv, na.rm = TRUE),
                    +         iqr_mean = mean(iqr, na.rm = TRUE),
                    +         .groups = "drop"
                    +       ) %>%
                  +       arrange(desc(eta_mean))
                + 
                  +     message("\nTop variables INDIVIDUALES por Eta2 (resumen por Variable):")
                +     print(head(summary_indiv, top_print), n = top_print)
                +   }
        + 
          +   # --- COMBINACIONES DE 2 ---
          +   comb2 <- combn(vars_exploratorias, 2, simplify = FALSE)
          +   pairwise_tbls <- map_dfr(comb2, function(vs) {
            +     v1 <- vs[1]; v2 <- vs[2]
            +     tmp <- df_g %>%
              +       filter(!is.na(.data[[v1]]), !is.na(.data[[v2]])) %>%
              +       group_by(val1 = as.character(.data[[v1]]), val2 = as.character(.data[[v2]])) %>%
              +       summarise(calc_dispersion(.data[[target_var]]), .groups = "drop") %>%
              +       rename(Valor1 = val1, Valor2 = val2)
            +     tmp <- tmp %>% filter(n >= min_n_pair)
            +     if (nrow(tmp) == 0) return(tibble())
            +     eta <- calc_eta_sq_safe(df_g %>% filter(!is.na(.data[[v1]]), !is.na(.data[[v2]])),
                                          +                              as.formula(paste0("`", target_var, "` ~ `", v1, "` + `", v2, "`")))
            +     tmp %>%
              +       mutate(
                +         Grupo = g,
                +         Variable = paste(v1, v2, sep = " + "),
                +         Tipo = "Combinación 2",
                +         Eta2 = eta
                +       ) %>%
              +       select(Grupo, Variable, Valor1, Valor2, n, mean, sd, cv, iqr, Eta2, Tipo)
            +   })
          + 
            +   if (nrow(pairwise_tbls) == 0) {
              +     message("-> No se encontraron combinaciones de 2 con celdas con n >= ", min_n_pair, " en este Grupo.")
              +   } else {
                +     summary_pair <- pairwise_tbls %>%
                  +       group_by(Grupo, Variable) %>%
                  +       summarise(
                    +         eta_mean = unique(na.omit(Eta2))[1],
                    +         n_cells = n(),
                    +         mean_mean = mean(mean, na.rm = TRUE),
                    +         sd_mean = mean(sd, na.rm = TRUE),
                    +         cv_mean = mean(cv, na.rm = TRUE),
                    +         iqr_mean = mean(iqr, na.rm = TRUE),
                    +         .groups = "drop"
                    +       ) %>%
                  +       arrange(desc(eta_mean))
                + 
                  +     message("\nTop COMBINACIONES (2) por Eta2 (resumen por combinación):")
                +     print(head(summary_pair, top_print), n = top_print)
                +   }
          + 
            +   # Guardamos ambos para output global
            +   output_list[[paste0("indiv_", g)]] <- indiv_tbls %>% mutate(Grupo = g)
            +   output_list[[paste0("pair_", g)]] <- pairwise_tbls %>% mutate(Grupo = g)
            + }
  
  ----------------------------
    GRUPO: SUCURSAL
  ----------------------------
    
    Top variables INDIVIDUALES por Eta2 (resumen por Variable):
    # A tibble: 10 × 8
    Grupo    Variable           eta_mean n_levels mean_mean sd_mean cv_mean iqr_mean
  <chr>    <chr>                 <dbl>    <int>     <dbl>   <dbl>   <dbl>    <dbl>
    1 SUCURSAL DescripcionCC       0.279         25      27.4    16.6   0.589     17.7
  2 SUCURSAL Nombre Reclutador   0.226         47      27.2    16.3   0.594     17.6
  3 SUCURSAL Escolaridad         0.0844         2      36.6    28.8   0.748     30.5
  4 SUCURSAL Plaza               0.0775        27      26.4    17.0   0.628     17.0
  5 SUCURSAL Especialización     0.0549         2      37.1    31.1   0.796     30.2
  6 SUCURSAL Estado              0.0549        21      27.0    17.4   0.633     18.1
  7 SUCURSAL Puesto Generico     0.0421         3      35.2    23.9   0.679     27.3
  8 SUCURSAL Area de Personal    0.0146         2      28.8    23.4   0.804     20.5
  9 SUCURSAL Regional            0.0109         6      27.5    19.7   0.719     17.9
  10 SUCURSAL Perfil Profesional  0.00811        1      27.3    19.6   0.717     18  
  
  Top COMBINACIONES (2) por Eta2 (resumen por combinación):
    # A tibble: 10 × 8
    Grupo    Variable                            eta_mean n_cells mean_mean sd_mean cv_mean iqr_mean
  <chr>    <chr>                                  <dbl>   <int>     <dbl>   <dbl>   <dbl>    <dbl>
    1 SUCURSAL DescripcionCC + Area de Personal       0.279      21      26.9    15.4   0.561     16.8
  2 SUCURSAL DescripcionCC + Perfil Profesional     0.279      21      27.3    16.2   0.585     17.1
  3 SUCURSAL DescripcionCC + Segmento de puesto     0.279      21      27.3    16.2   0.585     17.1
  4 SUCURSAL DescripcionCC + Estado                 0.279      19      27.5    15.4   0.548     16.8
  5 SUCURSAL DescripcionCC + Escolaridad            0.279      20      26.9    15.4   0.561     16.8
  6 SUCURSAL DescripcionCC + Especialización        0.279      21      27.3    16.3   0.586     17.1
  7 SUCURSAL DescripcionCC + Puesto Generico        0.279      19      26.8    15.1   0.548     17  
  8 SUCURSAL DescripcionCC + Nombre Reclutador      0.277      18      28.0    14.2   0.494     16.8
  9 SUCURSAL Nombre Reclutador + Escolaridad        0.226      36      25.3    13.6   0.529     15.7
  10 SUCURSAL Nombre Reclutador + Especialización    0.226      37      25.7    14.2   0.542     15.8
  
  ----------------------------
    GRUPO: ODG
  ----------------------------
    
    Top variables INDIVIDUALES por Eta2 (resumen por Variable):
    # A tibble: 10 × 8
    Grupo Variable           eta_mean n_levels mean_mean sd_mean cv_mean iqr_mean
  <chr> <chr>                 <dbl>    <int>     <dbl>   <dbl>   <dbl>    <dbl>
    1 ODG   DescripcionCC        0.248        10      34.7    27.0   0.776     28.6
  2 ODG   Nombre Reclutador    0.186         9      44.9    35.8   0.809     33.8
  3 ODG   Familia de Puesto    0.162         9      36.8    31.5   0.853     34.4
  4 ODG   Puesto Generico      0.159         5      41.0    35.8   0.861     34.7
  5 ODG   Software-Básico      0.130         3      33.9    24.8   0.746     28.4
  6 ODG   Segmento de puesto   0.0859        3      40.7    34.4   0.845     37.1
  7 ODG   Escolaridad          0.0731        3      31.7    27.8   0.855     28.5
  8 ODG   Software-Avanzado    0.0696        2      48.9    40.3   0.835     40.8
  9 ODG   Perfil Profesional   0.0679        6      47.7    41.1   0.880     34.9
  10 ODG   Especialización      0.0550        4      37.8    36.1   0.954     36  
  
  Top COMBINACIONES (2) por Eta2 (resumen por combinación):
    # A tibble: 10 × 8
    Grupo Variable                              eta_mean n_cells mean_mean sd_mean cv_mean iqr_mean
  <chr> <chr>                                    <dbl>   <int>     <dbl>   <dbl>   <dbl>    <dbl>
    1 ODG   DescripcionCC + Software-Avanzado        0.349       1      52.3    35.6   0.682     34.5
  2 ODG   DescripcionCC + Software-Básico          0.322       4      29.8    21.5   0.781     22  
  3 ODG   Familia de Puesto + Software-Básico      0.292       4      39.6    27.1   0.693     29.4
  4 ODG   Software-Intermedio + Software-Básico    0.269       1      52.7    34.7   0.659     35.5
  5 ODG   Familia de Puesto + Software-Avanzado    0.257       1      51.7    35.3   0.684     34  
  6 ODG   DescripcionCC + Nombre Reclutador        0.252       2      45.6    35.8   0.764     19.5
  7 ODG   DescripcionCC + Escolaridad              0.248      10      33.9    26.2   0.772     27.2
  8 ODG   DescripcionCC + Area de Personal         0.248       8      34.0    27.4   0.802     27.8
  9 ODG   DescripcionCC + Perfil Profesional       0.248      10      37.1    30.3   0.822     29.2
  10 ODG   DescripcionCC + Puesto Generico          0.248      11      36.8    28.7   0.789     31.7
  
  ----------------------------
    GRUPO: COBRANZA
  ----------------------------
    
    Top variables INDIVIDUALES por Eta2 (resumen por Variable):
    # A tibble: 10 × 8
    Grupo    Variable            eta_mean n_levels mean_mean sd_mean cv_mean iqr_mean
  <chr>    <chr>                  <dbl>    <int>     <dbl>   <dbl>   <dbl>    <dbl>
    1 COBRANZA Nombre Reclutador    0.167         45      45.3    34.6   0.744     32.7
  2 COBRANZA Estado               0.0902        22      44.3    35.0   0.760     32.4
  3 COBRANZA DescripcionCC        0.0860        26      43.6    34.7   0.778     33.1
  4 COBRANZA Plaza                0.0833        26      43.6    34.7   0.777     33.1
  5 COBRANZA Regional             0.0326         6      45.0    38.6   0.854     34.5
  6 COBRANZA Software-Básico      0.0112         2      35.1    31.8   0.924     26.5
  7 COBRANZA Puesto Generico      0.00664        6      43.3    38.7   0.901     32.5
  8 COBRANZA Escolaridad          0.00236        3      41.8    36.5   0.873     29.7
  9 COBRANZA Software-Intermedio  0.00178        2      44.8    41.6   0.921     37.2
  10 COBRANZA Especialización      0.00134        2      43.1    41.7   0.974     32  
  
  Top COMBINACIONES (2) por Eta2 (resumen por combinación):
    # A tibble: 10 × 8
    Grupo    Variable                               eta_mean n_cells mean_mean sd_mean cv_mean iqr_mean
  <chr>    <chr>                                     <dbl>   <int>     <dbl>   <dbl>   <dbl>    <dbl>
    1 COBRANZA Estado + Software-Intermedio              0.326       2      49.4    40.0   0.749     33  
  2 COBRANZA Nombre Reclutador + Software-Básico       0.178      36      46.8    34.5   0.715     32.5
  3 COBRANZA Nombre Reclutador + Puesto Generico       0.167      25      43.8    30.3   0.649     30.3
  4 COBRANZA Nombre Reclutador + Area de Personal      0.167      35      43.4    29.6   0.662     29.9
  5 COBRANZA Nombre Reclutador + Segmento de puesto    0.167      37      45.6    34.1   0.723     31.9
  6 COBRANZA Nombre Reclutador + Escolaridad           0.167      36      47.0    34.6   0.713     32.5
  7 COBRANZA Nombre Reclutador + Especialización       0.167      36      46.9    34.6   0.715     32.5
  8 COBRANZA Nombre Reclutador + Familia de Puesto     0.167      39      45.3    34.9   0.744     32.3
  9 COBRANZA Nombre Reclutador + Perfil Profesional    0.167      37      45.6    34.0   0.721     31.9
  10 COBRANZA Regional + Software-Intermedio            0.122       4      44.4    34.3   0.708     37.2
  
  ----------------------------
    GRUPO: PLAZA
  ----------------------------
    
    Top variables INDIVIDUALES por Eta2 (resumen por Variable):
    # A tibble: 10 × 8
    Grupo Variable           eta_mean n_levels mean_mean sd_mean cv_mean iqr_mean
  <chr> <chr>                 <dbl>    <int>     <dbl>   <dbl>   <dbl>    <dbl>
    1 PLAZA DescripcionCC       0.202          9      27.0    22.1   0.799     22.1
  2 PLAZA Nombre Reclutador   0.197         18      28.8    21.7   0.747     22.4
  3 PLAZA Plaza               0.148         20      31.4    25.6   0.806     27.2
  4 PLAZA Estado              0.0637         9      34.3    29.0   0.856     31.5
  5 PLAZA Area de Personal    0.0322         2      35.0    31.2   0.889     31.5
  6 PLAZA Puesto Generico     0.0309         3      31.7    28.3   0.885     28.3
  7 PLAZA Regional            0.0181         6      31.8    29.0   0.916     29.2
  8 PLAZA Perfil Profesional  0.0123         4      31.7    27.2   0.858     24.2
  9 PLAZA Escolaridad         0.0113         3      32.5    28.5   0.875     23.2
  10 PLAZA Segmento de puesto  0.00444        2      36.5    27.6   0.779     29.4
  
  Top COMBINACIONES (2) por Eta2 (resumen por combinación):
    # A tibble: 10 × 8
    Grupo Variable                                eta_mean n_cells mean_mean sd_mean cv_mean iqr_mean
  <chr> <chr>                                      <dbl>   <int>     <dbl>   <dbl>   <dbl>    <dbl>
    1 PLAZA Nombre Reclutador + Software-Intermedio    0.207       1      22.6    25.1   1.11      14  
  2 PLAZA DescripcionCC + Nombre Reclutador          0.203       2      28.0    18.9   0.672     21.8
  3 PLAZA DescripcionCC + Especialización            0.202       5      27.3    21.5   0.778     24.8
  4 PLAZA DescripcionCC + Area de Personal           0.202       4      24.3    19.6   0.789     20.1
  5 PLAZA DescripcionCC + Estado                     0.202       6      28.2    21.9   0.771     23.9
  6 PLAZA DescripcionCC + Escolaridad                0.202       4      24.4    18.9   0.756     21.1
  7 PLAZA DescripcionCC + Perfil Profesional         0.202       3      28.5    23.6   0.831     25.8
  8 PLAZA DescripcionCC + Segmento de puesto         0.202       5      26.9    21.0   0.770     23.8
  9 PLAZA DescripcionCC + Puesto Generico            0.202       4      24.4    18.8   0.754     20.6
  10 PLAZA Nombre Reclutador + Area de Personal       0.197       2      21.8    18.2   0.820     18.9
  > 
    > # =========================
  > # 7. UNIR Y GUARDAR CSV
    > # =========================
  > final_results <- bind_rows(output_list, .id = "source") %>%
    +   select(-source)
  > 
    > if (nrow(final_results) == 0) {
      +   message("\n=== ATENCIÓN: RESULTADO FINAL VACÍO ===")
      +   message("Posibles causas: filtros de n mínimos (min_n_indiv / min_n_pair) demasiado estrictos, o datos NA en variables.")
      + } else {
        +   out_file <- file.path(output_path, "Exploracion_Dias_Cobertura_Por_Grupo.csv")
        +   write.csv(final_results, out_file, row.names = FALSE)
        +   message("\nArchivo guardado en: ", out_file)
        +   message("Filas guardadas: ", nrow(final_results))
        + }
  
  Archivo guardado en: C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura/Exploracion_Dias_Cobertura_Por_Grupo.csv
  Filas guardadas: 4061
  > 
    > message("\nFIN del script. Copia la salida de consola y pégala aquí si quieres que la revisemos.")
  
  FIN del script. Copia la salida de consola y pégala aquí si quieres que la revisemos.