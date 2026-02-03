# =========================================================
# OPCIÓN B - INCREMENTOS POR COMPLEJIDAD (SCRIPT CORREGIDO)
# =========================================================

library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(purrr)
library(ggplot2)
library(writexl)
library(boot)

set.seed(123)

# =========================
# CONFIGURACIÓN
# =========================
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Detalle Días de Coberturas.xlsx"
output_dir <- "C:/Users/racl26345/Documents/Reportes Automatizados/Goal Días Cobertura/Incrementos_Complejidad"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

min_n   <- 8
boot_R <- 1000

vars_of_interest <- c(
  "Perfil_TI",
  "Complejidad_Nivel",
  "Nivel_Escolaridad",
  "Macro_Especializacion"
)

numeric_interest <- c(
  "Indice_Complejidad",
  "Total_Software"
)

# =========================
# CARGA Y TRANSFORMACIÓN
# =========================
datos <- read_excel(file_path) %>%
  filter(
    !is.na(`Días cobertura con capacitación`),
    !is.na(Grupo)
  )

safe_char <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

count_tools <- function(x) {
  x <- safe_char(x)
  sapply(x, function(z) {
    if (z == "") return(0)
    length(unique(trimws(unlist(str_split(z, ",|;|/|\\+|\\|| y | Y ")))))
  })
}

datos <- datos %>%
  mutate(
    Escolaridad_std = str_to_lower(safe_char(Escolaridad)),
    Nivel_Escolaridad = case_when(
      str_detect(Escolaridad_std, "ingenier|licenciatura") ~ "Superior",
      str_detect(Escolaridad_std, "tsu|técnic|tecnica") ~ "Técnica",
      str_detect(Escolaridad_std, "preparatoria|bachiller") ~ "Media",
      TRUE ~ "Otro"
    ),
    Especializacion_std = str_to_lower(safe_char(Especialización)),
    Macro_Especializacion = case_when(
      str_detect(Especializacion_std, "ti|sistemas|inform") ~ "TI",
      str_detect(Especializacion_std, "finanz|contadur|econom") ~ "Financiero",
      str_detect(Especializacion_std, "administra|mercadotec") ~ "Administrativo",
      str_detect(Especializacion_std, "ingenier") ~ "Ingeniería",
      str_detect(Especializacion_std, "derech") ~ "Legal",
      TRUE ~ "Otro"
    ),
    N_Software_Avanzado   = count_tools(`Software-Avanzado`),
    N_Software_Intermedio = count_tools(`Software-Intermedio`),
    N_Software_Basico     = count_tools(`Software-Básico`),
    Total_Software = N_Software_Avanzado + N_Software_Intermedio + N_Software_Basico,
    Perfil_TI = ifelse(Macro_Especializacion == "TI" | N_Software_Avanzado > 0, "TI", "No TI"),
    Complejidad_Nivel = case_when(
      Total_Software >= 8 | N_Software_Avanzado >= 2 ~ "Crítica",
      Total_Software >= 3 | Perfil_TI == "TI" ~ "Especializada",
      TRUE ~ "Estándar"
    ),
    Indice_Complejidad = Total_Software +
      ifelse(Perfil_TI == "TI", 2, 0) +
      ifelse(Macro_Especializacion %in% c("TI", "Ingeniería"), 1, 0)
  )

# =========================
# BOOTSTRAP DIFERENCIA MEDIANAS
# =========================
boot_median_diff <- function(y, g, lvl, R = 1000) {
  idx <- which(g == lvl)
  if (length(idx) < min_n) return(c(NA, NA, NA))

  stat <- function(i, y, g, lvl) {
    yb <- y[i]
    gb <- g[i]
    median(yb[gb == lvl], na.rm = TRUE) - median(yb, na.rm = TRUE)
  }

  b <- boot(
    data = seq_along(y),
    statistic = function(i, d) stat(i, y, g, lvl),
    R = R
  )

  ci <- tryCatch(boot.ci(b, type = "perc")$percent[4:5], error = function(e) c(NA, NA))
  c(b$t0, ci)
}

# =========================
# ANÁLISIS POR GRUPO
# =========================
results_list <- list()

for (grp in sort(unique(datos$Grupo))) {

  cat("\n--- Grupo:", grp, "---\n")

  df_g <- datos %>% filter(Grupo == grp)
  y <- df_g$`Días cobertura con capacitación`
  med_group <- median(y)

  res_cat <- list()

  for (v in vars_of_interest) {
    df_g[[v]] <- as.factor(df_g[[v]])
    for (lvl in levels(df_g[[v]])) {

      idx <- df_g[[v]] == lvl
      n_lvl <- sum(idx)

      inc <- median(y[idx], na.rm = TRUE) - med_group
      boot_res <- boot_median_diff(y, df_g[[v]], lvl)

      res_cat[[paste(v, lvl)]] <- data.frame(
        Grupo = grp,
        Variable = v,
        Nivel = lvl,
        n = n_lvl,
        Incremento = inc,
        Est_boot = boot_res[1],
        CI_low = boot_res[2],
        CI_high = boot_res[3]
      )
    }
  }

  # ========= NUMÉRICAS (BINNING SEGURO) =========
  res_num <- list()

  for (nv in numeric_interest) {

    q <- quantile(df_g[[nv]], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
    q <- unique(q)

    if (length(q) < 3) {
      cat("  ⚠️", nv, "sin suficiente variabilidad para bins\n")
      next
    }

    labels <- paste0("Q", seq_len(length(q) - 1))

    df_g$bin <- cut(df_g[[nv]], breaks = q, include.lowest = TRUE, labels = labels)

    for (lvl in levels(df_g$bin)) {
      idx <- df_g$bin == lvl
      n_lvl <- sum(idx)

      inc <- median(y[idx], na.rm = TRUE) - med_group
      boot_res <- boot_median_diff(y, df_g$bin, lvl)

      res_num[[paste(nv, lvl)]] <- data.frame(
        Grupo = grp,
        Variable = nv,
        Nivel = lvl,
        n = n_lvl,
        Incremento = inc,
        Est_boot = boot_res[1],
        CI_low = boot_res[2],
        CI_high = boot_res[3]
      )
    }
  }

  df_cat <- bind_rows(res_cat)
  df_num <- bind_rows(res_num)

  results_list[[grp]] <- list(cat = df_cat, num = df_num)

  if (nrow(df_cat) > 0)
    write_xlsx(df_cat, file.path(output_dir, paste0("incrementos_", grp, "_categoricas.xlsx")))

  if (nrow(df_num) > 0)
    write_xlsx(df_num, file.path(output_dir, paste0("incrementos_", grp, "_numericas.xlsx")))
}

cat("\n✔ Análisis completado sin errores\n")
cat("Resultados en:", output_dir, "\n")


















CAMPO	|	CONTIENE	|	RETO
Grupo	|	Nivel de Gestión (ODG, COBRANZA, PLAZA, SUCURSAL)	|	Macro Niveles de Gestión (ODG -> Posiciones Corporativas)
Regional	|	Subagrupador de zonas operativas	|	6 regiones de México + ODG
Plaza	|	Centros de subnivel de regionales que administran un grupo de sucursales	|	27 plazas + ODG
DescripcionCC	|	Centros de Costos	|	En ODG los centros de costos son Departamento, pero en PLAZA y COBRANZA son los centros regionales de administración de sucursales, SUCURSAL cada centro de costos es una sucursal de más de 500
Estado	|	Entidad Federativa de México	|	Una regional puede agrupar varios estados
Nombre Reclutador	|	Colaborador que hizo proceso de contratación	|	92 reclutadores registrados, aunque no se tiene registro de 132 contrataciones quien fue el reclutador (NULL); de un total de 5,148 contrataciones que se tuvieron en el periodo
Perfil Profesional	|	Agrupador de Perfiles por su enfoque de actividades	|	8 combinaciones
Segmento de puesto	|	Agrupador de Enfoque de Puesto para la Organización	|	4 grupos, partiendo de operativo, especializado, táctivo hasta estrátegico
Area de Personal	|	Agrupador de tipo de colaborador (contrato)	|	Si el colaborador es de confianza o sindicaliza, como un nivel funcionario (superior a Confianza)
Puesto Generico	|	Agrupador de puestos por niveles genéricos de puestos	|	
  Familia de Puesto	|	División de unidades de negocio, superior a departamentos	|	
  Escolaridad	|	Grados de Estudio Mínimo	|	
  Especialización	|	Área de Estudio enfocada	|	
  Software-Avanzado	|	Software indipensable nivel experto	|	
  Software-Básico	|	Software indipensable nivel básico	|	
  Software-Intermedio	|	Software indipensable nivel intermedio	|	
  









