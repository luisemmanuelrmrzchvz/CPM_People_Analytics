# =========================================================
# SCRIPT: Estimación de incrementos por complejidad (Opción B)
# =========================================================
# Requisitos: readxl, dplyr, stringr, tidyr, purrr, ggplot2, writexl, boot
# =========================================================

# -----------------------
# 0. Parámetros y librerías
# -----------------------
required <- c("readxl","dplyr","stringr","tidyr","purrr","ggplot2","writexl","boot")
missing <- required[!(required %in% installed.packages()[, "Package"])]
if(length(missing)>0){
  cat("Faltan paquetes:", paste(missing, collapse=", "), "\nInstálalos con install.packages().\n")
  stop("Instala paquetes faltantes y vuelve a ejecutar.")
}
library(readxl); library(dplyr); library(stringr); library(tidyr); library(purrr)
library(ggplot2); library(writexl); library(boot)

set.seed(123)

# Ajusta rutas si es necesario
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
output_dir <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura/Incrementos_Complejidad"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Parámetros
min_n <- 8          # mínimo observaciones en nivel para reportar estimado
boot_R <- 1000      # iteraciones bootstrap (1000 es razonable; aumenta si tu equipo lo permite)
alpha <- 0.05       # nivel de confianza (95% CI)
vars_of_interest <- c("Perfil_TI", "Complejidad_Nivel", "Nivel_Escolaridad",
                      "Macro_Especializacion") # variables categóricas
numeric_interest <- c("Indice_Complejidad","Total_Software") # variables numéricas a analizar distinto

# -----------------------
# 1. Cargar y recrear transformaciones (asegurarnos de tener campos)
# -----------------------
datos <- readxl::read_excel(file_path)

# Asegurar variable objetivo y Grupo
if (!("Días cobertura con capacitación" %in% colnames(datos))) stop("No existe la columna 'Días cobertura con capacitación'")
if (!("Grupo" %in% colnames(datos))) stop("No existe la columna 'Grupo'")

# Reproducir las transformaciones previas (por si ejecutas este script en sesión nueva)
safe_char <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; trimws(x) }

datos <- datos %>%
  mutate(
    Escolaridad = safe_char(Escolaridad),
    Escolaridad_std = str_to_lower(Escolaridad),
    Nivel_Escolaridad = case_when(
      str_detect(Escolaridad_std, "ingenier|licenciatura") ~ "Superior",
      str_detect(Escolaridad_std, "tsu|técnic|tecnica") ~ "Técnica",
      str_detect(Escolaridad_std, "preparatoria|bachiller") ~ "Media",
      Escolaridad_std == "" ~ NA_character_,
      TRUE ~ "Otro"
    ),
    Especializacion_std = safe_char(Especialización) %>% str_to_lower(),
    Macro_Especializacion = case_when(
      str_detect(Especializacion_std, "informá|informat|sistemas|ti|tecnolog") ~ "TI",
      str_detect(Especializacion_std, "contadur|finanz|econom") ~ "Financiero",
      str_detect(Especializacion_std, "administra|mercadotec") ~ "Administrativo",
      str_detect(Especializacion_std, "derech") ~ "Legal",
      str_detect(Especializacion_std, "ingenier") ~ "Ingeniería",
      Especializacion_std == "" ~ NA_character_,
      TRUE ~ "Otro"
    ),
    `Software-Avanzado` = safe_char(`Software-Avanzado`),
    `Software-Intermedio` = safe_char(`Software-Intermedio`),
    `Software-Básico` = safe_char(`Software-Básico`)
  )

# conteo simple de herramientas (similar a lo usado antes)
count_tools <- function(x) {
  x <- safe_char(x)
  sapply(x, function(z) {
    if (z == "" || is.na(z)) return(0)
    parts <- unlist(str_split(z, ",|;|/|\\+|\\|| y | Y |:"))
    parts <- trimws(parts); parts <- parts[parts!=""]
    length(unique(parts))
  })
}
datos <- datos %>%
  mutate(
    N_Software_Avanzado = count_tools(`Software-Avanzado`),
    N_Software_Intermedio = count_tools(`Software-Intermedio`),
    N_Software_Basico = count_tools(`Software-Básico`),
    Total_Software = N_Software_Avanzado + N_Software_Intermedio + N_Software_Basico,
    Perfil_TI = ifelse(Macro_Especializacion == "TI" | N_Software_Avanzado > 0, "TI", "No TI"),
    Complejidad_Nivel = case_when(
      Total_Software >= 8 | N_Software_Avanzado >= 2 ~ "Crítica",
      Total_Software >= 3 | Perfil_TI == "TI" ~ "Especializada",
      TRUE ~ "Estándar"
    ),
    Indice_Complejidad = Total_Software + ifelse(Perfil_TI == "TI", 2, 0) + ifelse(Macro_Especializacion %in% c("TI","Ingeniería"), 1, 0)
  )

# Convertir a factores las categoricas de interés
for (v in vars_of_interest) {
  if (v %in% colnames(datos)) datos[[v]] <- as.factor(datos[[v]])
}

# -----------------------
# 2. Funciones auxiliares
# -----------------------
# función bootstrap que calcula diferencia de medianas (nivel vs todo el grupo) dentro del grupo
boot_median_diff <- function(data_vec, group_index_vec, level_value, R = 1000) {
  # data_vec = numeric vector of days for the group
  # group_index_vec = factor vector with levels (same length)
  # level_value = the level for which compute median(level) - median(all)
  idx_level <- which(group_index_vec == level_value)
  if (length(idx_level) < 2) return(list(est = NA_real_, ci = c(NA_real_, NA_real_)))
  # bootstrap resampling indices within the group
  boot_fun <- function(indices, data_vec, group_index_vec, level_value) {
    sampled_data <- data_vec[indices]
    sampled_group_index <- group_index_vec[indices]
    med_level <- median(sampled_data[sampled_group_index == level_value], na.rm = TRUE)
    med_all <- median(sampled_data, na.rm = TRUE)
    med_level - med_all
  }
  # set up indices 1:n for boot()
  n <- length(data_vec)
  b <- tryCatch({
    boot_obj <- boot::boot(data = seq_len(n),
                          statistic = function(ind, idx) boot_fun(ind, data_vec, group_index_vec, level_value),
                          R = R)
    c(est = as.numeric(boot_obj$t0), ci = boot::boot.ci(boot_obj, type = "perc")$percent[4:5])
  }, error = function(e) rep(NA_real_, 3))
  list(est = b[1], ci = c(b[2], b[3]))
}

# Safe wilcoxon p-value (level vs rest of group)
wilcox_p <- function(x, group_flag) {
  tryCatch({
    g1 <- x[group_flag]
    g2 <- x[!group_flag]
    if (length(unique(g1))<=1 && length(unique(g2))<=1) return(NA_real_)
    wilcox.test(g1, g2, exact = FALSE)$p.value
  }, error = function(e) NA_real_)
}

# -----------------------
# 3. Cálculo incrementos por grupo y variable
# -----------------------
groups <- sort(unique(datos$Grupo))

results_list <- list()

for (grp in groups) {
  cat("\n--- Grupo:", grp, " ---\n")
  df_g <- datos %>% filter(Grupo == grp)
  med_group <- median(df_g$`Días cobertura con capacitación`, na.rm = TRUE)
  n_group <- nrow(df_g)
  cat("Registros:", n_group, " Mediana grupo:", med_group, "\n")

  # placeholder for this group's results
  grp_results <- list()
  # Para cada variable categórica
  for (v in vars_of_interest) {
    if (! (v %in% colnames(df_g))) next
    levels_v <- levels(droplevels(df_g[[v]]))
    res_rows <- list()
    for (lev in levels_v) {
      n_level <- sum(df_g[[v]] == lev, na.rm = TRUE)
      if (n_level < 1) next
      med_level <- median(df_g$`Días cobertura con capacitación`[df_g[[v]] == lev], na.rm = TRUE)
      diff_med <- med_level - med_group
      # bootstrap CI only if enough observations
      if (n_level >= min_n) {
        bres <- boot_median_diff(df_g$`Días cobertura con capacitación`, df_g[[v]], lev, R = boot_R)
        ci_low <- bres$ci[1]; ci_high <- bres$ci[2]; est <- bres$est
      } else {
        ci_low <- NA; ci_high <- NA; est <- NA
      }
      pval <- wilcox_p(df_g$`Días cobertura con capacitación`, df_g[[v]] == lev)
      res_rows[[lev]] <- data.frame(
        Grupo = grp, Variable = v, Nivel = lev,
        n = n_level, Mediana_nivel = med_level,
        Mediana_grupo = med_group,
        Incremento = diff_med,
        Est_boot = est,
        CI_low = ci_low, CI_high = ci_high,
        p_val = pval,
        stringsAsFactors = FALSE
      )
    } # lev
    if (length(res_rows)>0) {
      df_res_v <- bind_rows(res_rows)
      grp_results[[v]] <- df_res_v
    }
  } # v

  # Para variables numéricas: usar correlación y efecto por unidad (median by bins)
  num_results <- list()
  for (nv in numeric_interest) {
    if (!(nv %in% colnames(df_g))) next
    # crear 3 bins (terciles) para ver efecto por rango
    q <- quantile(df_g[[nv]], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    df_g[[paste0(nv, "_bin")]] <- cut(df_g[[nv]], breaks = unique(q), include.lowest = TRUE, labels = c("low","mid","high"))
    # compute medians by bin
    bins <- levels(droplevels(df_g[[paste0(nv, "_bin")]]))
    bin_rows <- list()
    for (b in bins) {
      n_bin <- sum(df_g[[paste0(nv,"_bin")]] == b, na.rm = TRUE)
      if (n_bin < 1) next
      med_bin <- median(df_g$`Días cobertura con capacitación`[df_g[[paste0(nv,"_bin")]] == b], na.rm = TRUE)
      diff_med <- med_bin - med_group
      # bootstrap if enough
      if (n_bin >= min_n) {
        bres <- boot_median_diff(df_g$`Días cobertura con capacitación`, df_g[[paste0(nv,"_bin")]], b, R = boot_R)
        ci_low <- bres$ci[1]; ci_high <- bres$ci[2]; est <- bres$est
      } else {
        ci_low <- NA; ci_high <- NA; est <- NA
      }
      bin_rows[[b]] <- data.frame(
        Grupo = grp, Variable = nv, Nivel = b,
        n = n_bin, Mediana_nivel = med_bin,
        Mediana_grupo = med_group,
        Incremento = diff_med,
        Est_boot = est,
        CI_low = ci_low, CI_high = ci_high,
        stringsAsFactors = FALSE
      )
    }
    if (length(bin_rows)>0) num_results[[nv]] <- bind_rows(bin_rows)
  }

  # Bind results for group
  all_v <- bind_rows(grp_results)
  all_num <- bind_rows(num_results)
  results_list[[as.character(grp)]] <- list(cat = all_v, num = all_num)
  # Save intermediate files
  if (!is.null(all_v) && nrow(all_v)>0) write_xlsx(all_v, file.path(output_dir, paste0("incrementos_", grp, "_categoricas.xlsx")))
  if (!is.null(all_num) && nrow(all_num)>0) write_xlsx(all_num, file.path(output_dir, paste0("incrementos_", grp, "_numericas.xlsx")))
}

# -----------------------
# 4. Consolidado global (pool)
# -----------------------
cat("\nGenerando consolidado global (pool) ...\n")
# categorical pooled: compute medians by level across full dataset, vs global median
global_med <- median(datos$`Días cobertura con capacitación`, na.rm = TRUE)
pooled_rows <- list()
for (v in vars_of_interest) {
  if (!(v %in% colnames(datos))) next
  levels_v <- levels(droplevels(datos[[v]]))
  for (lev in levels_v) {
    n_level <- sum(datos[[v]] == lev, na.rm = TRUE)
    med_level <- median(datos$`Días cobertura con capacitación`[datos[[v]] == lev], na.rm = TRUE)
    diff_med <- med_level - global_med
    pval <- wilcox_p(datos$`Días cobertura con capacitación`, datos[[v]] == lev)
    pooled_rows[[paste(v,lev,sep="_")]] <- data.frame(
      Variable = v, Nivel = lev, n = n_level, Mediana_nivel = med_level,
      Mediana_global = global_med, Incremento = diff_med, p_val = pval, stringsAsFactors = FALSE
    )
  }
}
df_pooled_cat <- bind_rows(pooled_rows)
write_xlsx(df_pooled_cat, file.path(output_dir, "incrementos_pooled_categoricas.xlsx"))

# numeric pooled: bins across full dataset
pooled_num_rows <- list()
for (nv in numeric_interest) {
  if (!(nv %in% colnames(datos))) next
  q <- quantile(datos[[nv]], probs = c(0,1/3,2/3,1), na.rm = TRUE)
  datos[[paste0(nv,"_bin")]] <- cut(datos[[nv]], breaks = unique(q), include.lowest = TRUE, labels = c("low","mid","high"))
  bins <- levels(droplevels(datos[[paste0(nv,"_bin")]]))
  for (b in bins) {
    n_bin <- sum(datos[[paste0(nv,"_bin")]] == b, na.rm = TRUE)
    med_bin <- median(datos$`Días cobertura con capacitación`[datos[[paste0(nv,"_bin")]] == b], na.rm = TRUE)
    diff_med <- med_bin - global_med
    pooled_num_rows[[paste(nv,b,sep="_")]] <- data.frame(Variable = nv, Nivel = b, n = n_bin, Mediana_nivel = med_bin,
                                                        Mediana_global = global_med, Incremento = diff_med, stringsAsFactors = FALSE)
  }
}
df_pooled_num <- bind_rows(pooled_num_rows)
write_xlsx(df_pooled_num, file.path(output_dir, "incrementos_pooled_numeric_bins.xlsx"))

# -----------------------
# 5. Graficas (por variable, global)
# -----------------------
cat("\nGenerando gráficas de incrementos (global y por grupo)...\n")

# helper to plot a df (with CI if available)
plot_increment <- function(df, title, file) {
  if (is.null(df) || nrow(df)==0) return(NULL)
  df2 <- df %>% filter(!is.na(Incremento))
  df2$Nivel2 <- with(df2, paste0(Nivel, "\n(n=", n, ")"))
  p <- ggplot(df2, aes(x = reorder(Nivel2, Incremento), y = Incremento)) +
    geom_col(fill = "steelblue", alpha = 0.8) +
    coord_flip() +
    labs(title = title, x = "", y = "Incremento (días) vs mediana referencia") +
    theme_minimal()
  # add CI if exists
  if ("CI_low" %in% colnames(df2) && any(!is.na(df2$CI_low))) {
    p <- p + geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "darkred")
  }
  ggsave(filename = file, plot = p, width = 10, height = max(3, nrow(df2)*0.3), dpi = 300)
  invisible(p)
}

# global plots for categorical pooled
for (v in vars_of_interest) {
  df_plot <- df_pooled_cat %>% filter(Variable == v)
  if (nrow(df_plot)>0) {
    plot_increment(df_plot, paste0("Incrementos globales - ", v), file.path(output_dir, paste0("plot_pooled_", v, ".png")))
  }
}

# plots by group (categorical)
for (grp in names(results_list)) {
  res_cat <- results_list[[grp]]$cat
  if (!is.null(res_cat) && nrow(res_cat)>0) {
    for (v in unique(res_cat$Variable)) {
      df_plot <- res_cat %>% filter(Variable == v)
      plot_increment(df_plot, paste0("Incrementos ", grp, " - ", v), file.path(output_dir, paste0("plot_", grp, "_", v, ".png")))
    }
  }
}

cat("\nArchivos generados en:", output_dir, "\n")
cat(" - Excel con incrementos por grupo y pooled\n")
cat(" - Gráficos PNG por variable\n")
cat("\nEJECUCIÓN FINALIZADA.\n")























+   # Bind results for group
  +   all_v <- bind_rows(grp_results)
+   all_num <- bind_rows(num_results)
+   results_list[[as.character(grp)]] <- list(cat = all_v, num = all_num)
+   # Save intermediate files
  +   if (!is.null(all_v) && nrow(all_v)>0) write_xlsx(all_v, file.path(output_dir, paste0("incrementos_", grp, "_categoricas.xlsx")))
+   if (!is.null(all_num) && nrow(all_num)>0) write_xlsx(all_num, file.path(output_dir, paste0("incrementos_", grp, "_numericas.xlsx")))
+ }

--- Grupo: COBRANZA  ---
  Registros: 670  Mediana grupo: 41 
Error en cut.default(df_g[[nv]], breaks = unique(q), include.lowest = TRUE, : 
                       El número de intervalos y la longitud de las 'etiquetas' difieren