################# REPORTE SVL C4C MEJORADO ############################

# Cargar las librerías necesarias
library(DBI)
library(RSQLite)
library(openxlsx)
library(blastula)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

# Definir el query SQL
query <- "
-- CURRENT MONTH
WITH
tickets_mes_actual AS (
    SELECT
        codigo_tickets.id_ticket,
        codigo_tickets.fecha_creado,
        codigo_tickets.fecha_interaccion,
        codigo_tickets.hora_interaccion,
        codigo_tickets.id_catalog,
        catalog_tickets.tipo_atencion,
        catalog_tickets.prioridad,
        catalog_tickets.tipo_ticket,
        catalog_tickets.nivel_atencion,
        catalog_tickets.categoria,
        catalog_tickets.subcategoria,
        codigo_tickets.agente_servicio,
        CASE WHEN catalog_tickets.prioridad = 'Prioridad 1 - Inmediata' THEN 28800
            WHEN catalog_tickets.prioridad = 'Prioridad 2 - Normal' THEN 86400
            ELSE 144000 END AS tiempo_objetivo
    FROM codigo_tickets 
    LEFT JOIN catalog_tickets
        ON codigo_tickets.id_catalog = catalog_tickets.id_catalog
    WHERE STRFTIME('%Y', codigo_tickets.fecha_creado) = STRFTIME('%Y', 'now')
    AND CAST(STRFTIME('%m', codigo_tickets.fecha_creado) AS INTEGER) BETWEEN 
        (CAST(STRFTIME('%m', 'now') AS INTEGER) - 1) / 3 * 3 + 1 
        AND (CAST(STRFTIME('%m', 'now') AS INTEGER) - 1) / 3 * 3 + 3
    AND catalog_tickets.tipo_atencion = 'Ticket Válido'
    AND codigo_tickets.agente_servicio IS NOT NULL
),

status_nuevos AS (
    SELECT 
        hist_status_tickets_sw.id_ticket,
        SUM(hist_status_tickets_sw.seg_duracion) AS timing
    FROM hist_status_tickets_sw
    WHERE hist_status_tickets_sw.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets_sw.code_estado_ticket = 1 -- Nuevo (1)
    GROUP BY 1
),

status_proceso_agente AS (
    SELECT 
        hist_status_tickets_sw.id_ticket,
        SUM(hist_status_tickets_sw.seg_duracion) AS timing
    FROM hist_status_tickets_sw
    WHERE hist_status_tickets_sw.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets_sw.code_estado_ticket = 2 -- En proceso - Agente (2)
    GROUP BY 1
),

status_proceso_cliente AS (
    SELECT 
        hist_status_tickets_sw.id_ticket,
        SUM(hist_status_tickets_sw.seg_duracion) AS timing
    FROM hist_status_tickets_sw
    WHERE hist_status_tickets_sw.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets_sw.code_estado_ticket = 4 -- En proceso - Cliente (4)
    GROUP BY 1
),

status_propuesta_solucion AS (
    SELECT 
        hist_status_tickets_sw.id_ticket,
        SUM(hist_status_tickets_sw.seg_duracion) AS timing
    FROM hist_status_tickets_sw
    WHERE hist_status_tickets_sw.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets_sw.code_estado_ticket = 5 -- Solución propuesta (5)
    GROUP BY 1
),

status_solucion_confirmada AS (
    SELECT 
        hist_status_tickets_sw.id_ticket,
        SUM(hist_status_tickets_sw.seg_duracion) AS timing
    FROM hist_status_tickets_sw
    WHERE hist_status_tickets_sw.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets_sw.code_estado_ticket = 'Z6' -- Solución Confirmada (Z6)
    GROUP BY 1
),

status_cerrados AS (
    SELECT 
        hist_status_tickets_sw.id_ticket,
        SUM(hist_status_tickets_sw.seg_duracion) AS timing
    FROM hist_status_tickets_sw
    WHERE hist_status_tickets_sw.id_ticket IN (SELECT tickets_mes_actual.id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets_sw.code_estado_ticket = 6 --Cerrado (6)
    GROUP BY 1
    ORDER BY 1
),

colaboradores AS (
    SELECT 
        tickets.id_ticket,
        tickets.id_colaborador AS id_cliente,
        creador.nombre_completo AS nombre_cliente,
        creador.puesto AS puesto_cliente,
        creador.id_centro_costos AS id_centro_costos_cliente,
        creador.centro_costos AS centro_costos_cliente,
        creador.departamento AS departamento_cliente,
        CASE WHEN creador.area_cobranza = 'No Aplica' THEN creador.nivel_gestion
            ELSE 'COBRANZA' END AS nivel_gestion_cliente,
        creador.area_personal AS area_personal_cliente,
        creador.plaza AS plaza_cliente,
        creador.regional AS regional_cliente,
        creador.horario AS horario_cliente,
        tickets.id_procesador AS id_procesador,
        procesador.nombre_completo AS nombre_procesador,
        procesador.puesto AS puesto_procesador,
        procesador.id_centro_costos AS id_centro_costos_procesador,
        procesador.centro_costos AS centro_costos_procesador,
        procesador.departamento AS departamento_procesador,
        CASE WHEN procesador.area_cobranza = 'No Aplica' THEN procesador.nivel_gestion
            WHEN tickets.id_procesador = 'F2APIS2' THEN NULL
            ELSE 'COBRANZA' END AS nivel_gestion_procesador,
        procesador.plaza AS plaza_procesador,
        procesador.regional AS regional_procesador,
        procesador.horario AS horario_procesador
    FROM colaboradores_tickets tickets
    LEFT JOIN datos_colaboradores creador
        ON tickets.id_colaborador = creador.id_colaborador
    LEFT JOIN datos_colaboradores procesador
        ON tickets.id_procesador = procesador.id_colaborador
)

SELECT
    tm.id_ticket,
    col.id_cliente,
    col.nombre_cliente,
    col.puesto_cliente,
    col.id_centro_costos_cliente,
    col.centro_costos_cliente,
    col.departamento_cliente,
    col.nivel_gestion_cliente,
    col.area_personal_cliente,
    col.plaza_cliente,
    col.regional_cliente,
    col.horario_cliente,
    col.id_procesador,
    col.nombre_procesador,
    col.puesto_procesador,
    col.id_centro_costos_procesador,
    col.centro_costos_procesador,
    col.departamento_procesador,
    col.nivel_gestion_procesador,
    col.plaza_procesador,
    col.regional_procesador,
    col.horario_procesador,
    tm.fecha_creado,
    tm.id_catalog,
    tm.fecha_interaccion,
    tm.prioridad,
    tm.tipo_ticket,
    tm.nivel_atencion,
    tm.categoria,
    tm.subcategoria,
    tm.tiempo_objetivo,
    COALESCE(sn.timing, 0) AS tiempo_nuevo,
    COALESCE(spa.timing, 0) AS tiempo_proceso_agente,
    COALESCE(spc.timing, 0) AS tiempo_proceso_cliente,
    COALESCE(sps.timing, 0) AS tiempo_propuesta_solucion,
    COALESCE(ssc.timing, 0) AS tiempo_solucion_confirmada,
    (
        COALESCE(sn.timing, 0) + 
        COALESCE(spa.timing, 0) + 
        COALESCE(spc.timing, 0) + 
        COALESCE(sps.timing, 0) + 
        COALESCE(ssc.timing, 0)
    ) AS total_tiempo,
    CASE WHEN (
        COALESCE(sn.timing, 0) + 
        COALESCE(spa.timing, 0) + 
        COALESCE(spc.timing, 0) + 
        COALESCE(sps.timing, 0) + 
        COALESCE(ssc.timing, 0)
        ) <= tm.tiempo_objetivo THEN 'Yes'
        ELSE 'No' END AS cumple_svl,
    (
        COALESCE(sn.timing, 0) + 
        COALESCE(spa.timing, 0)
    ) AS total_tiempo_procesador,
    (
        COALESCE(spc.timing, 0) + 
        COALESCE(sps.timing, 0) + 
        COALESCE(ssc.timing, 0)
    ) AS total_tiempo_cliente,
    CASE WHEN (
        COALESCE(sn.timing, 0) + 
        COALESCE(spa.timing, 0)
        ) <= tm.tiempo_objetivo THEN 'Yes'
        ELSE 'No' END AS cumple_svl_procesador,
    CASE WHEN tm.id_ticket IN (SELECT status_cerrados.id_ticket FROM status_cerrados) THEN 'Cerrado'
        WHEN tm.id_ticket IN (SELECT status_solucion_confirmada.id_ticket FROM status_solucion_confirmada) THEN 'Solución Confirmada'
        WHEN tm.id_ticket IN (SELECT status_propuesta_solucion.id_ticket FROM status_propuesta_solucion) THEN 'Completado - Solución propuesta'
        WHEN tm.id_ticket IN (SELECT status_proceso_cliente.id_ticket FROM status_proceso_cliente) THEN 'En proceso - Acción del cliente'
        WHEN tm.id_ticket IN (SELECT status_proceso_agente.id_ticket FROM status_proceso_agente) THEN 'En proceso - Agente'
        ELSE 'Nuevo' END AS estado_actual_ticket
FROM tickets_mes_actual tm
LEFT JOIN status_nuevos sn ON tm.id_ticket = sn.id_ticket
LEFT JOIN status_proceso_agente spa ON tm.id_ticket = spa.id_ticket
LEFT JOIN status_proceso_cliente spc ON tm.id_ticket = spc.id_ticket
LEFT JOIN status_propuesta_solucion sps ON tm.id_ticket = sps.id_ticket
LEFT JOIN status_solucion_confirmada ssc ON tm.id_ticket = ssc.id_ticket
LEFT JOIN colaboradores col ON tm.id_ticket = col.id_ticket
;
"

# Conectar a la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(SQLite(), db_path)

# Ejecutar el query y obtener los datos
datos <- dbGetQuery(conn, query)
dbDisconnect(conn)

# Definir columnas de tiempo
columnas_tiempo <- c("tiempo_nuevo", "tiempo_proceso_agente", "tiempo_proceso_cliente", 
                     "tiempo_propuesta_solucion", "tiempo_solucion_confirmada", 
                     "total_tiempo", "tiempo_objetivo", "total_tiempo_procesador", 
                     "total_tiempo_cliente")

# Mantener datos numéricos para análisis y crear versión para Excel
datos_analysis <- datos %>%
  mutate(across(all_of(columnas_tiempo), ~ as.numeric(.)))

# Obtener fecha anterior
fecha_anterior <- format(Sys.Date() - 1, "%Y-%m-%d")

# Crear dataset para Excel
datos_excel <- datos_analysis %>%
  mutate(across(all_of(columnas_tiempo), ~ sprintf("%02d:%02d:%02d", 
                                                   floor(./3600),
                                                   floor((.%%3600)/60),
                                                   .%%60)))

# Guardar el archivo Excel
output_path <- paste0("C:/Users/racl26345/Documents/Reportes Automatizados/Detalle Mensual Tickets C4C ", fecha_anterior, ".xlsx")
write.xlsx(datos_excel, output_path, rowNames = FALSE)

# --------------------------
# Cálculo de KPIs
# --------------------------
total_tickets <- nrow(datos_analysis)
tickets_cerrados <- sum(datos_analysis$estado_actual_ticket == "Cerrado")
porc_cerrados <- round(tickets_cerrados / total_tickets * 100, 1)
porc_cumple_sla <- round(mean(datos_analysis$cumple_svl_procesador == "Yes") * 100, 1)
avg_resolucion <- round(mean(datos_analysis$total_tiempo_procesador[datos_analysis$estado_actual_ticket == "Cerrado"] / 3600), 1)

kpi_text <- paste0(
  "<b>KPIs Principales del Mes:</b><br>",
  "• Total Tickets: ", total_tickets, "<br>",
  "• Tickets Cerrados: ", tickets_cerrados, " (", porc_cerrados, "%)<br>",
  "• Cumplimiento SLA General: ", porc_cumple_sla, "%<br>",
  "• Tiempo Medio Resolución (Cerrados): ", avg_resolucion, " hrs<br><br>"
)

# --------------------------
# Generación de Gráficos Mejorados
# --------------------------

# 1. Tendencia Diaria de Cumplimiento SLA
daily_sla <- datos_analysis %>%
  mutate(fecha = as.Date(fecha_creado)) %>%
  group_by(fecha) %>%
  summarise(
    total = n(),
    cumplidos = sum(cumple_svl_procesador == "Yes"),
    porc_cumplido = cumplidos/total * 100
  )

grafico_tendencia_sla <- ggplot(daily_sla, aes(x = fecha, y = porc_cumplido)) +
  geom_line(color = "#1f77b4", size = 1) +
  geom_point(color = "#1f77b4", size = 3) +
  geom_text(aes(label = paste0(round(porc_cumplido, 1), "%")), 
            vjust = -1, color = "#1f77b4", size = 3.5) +  # Etiquetas de porcentaje
  geom_hline(yintercept = 90, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Tendencia Diaria de Cumplimiento SLA",
       x = "Fecha", y = "% Tickets Dentro de SLA") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day") +  # Formato de fechas
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    panel.grid.minor = element_blank()  # Eliminar líneas de cuadrícula menores
  )

# 2. Tiempo por Etapa de Proceso
tiempo_etapas <- datos_analysis %>%
  select(prioridad, tiempo_nuevo:tiempo_solucion_confirmada) %>%
  pivot_longer(-prioridad, names_to = "etapa", values_to = "segundos") %>%
  mutate(
    etapa = gsub("tiempo_", "", etapa),
    etapa = factor(etapa, levels = c("nuevo", "proceso_agente", "proceso_cliente",
                                     "propuesta_solucion", "solucion_confirmada")),
    horas = segundos / 3600
  )

tiempo_etapas_agg <- tiempo_etapas %>%
  group_by(prioridad, etapa) %>%
  summarise(horas_promedio = mean(horas, na.rm = TRUE))  # Calcular promedios

grafico_etapas <- ggplot(tiempo_etapas_agg, aes(x = prioridad, y = horas_promedio, fill = etapa)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(horas_promedio, 1), "h")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +  # Etiquetas de tiempo
  labs(title = "Tiempo Promedio por Etapa y Prioridad",
       x = "Prioridad", y = "Horas Promedio", fill = "Etapa") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotar etiquetas del eje X
    legend.position = "bottom"  # Leyenda en la parte inferior
  )

# 3. Top 5 Categorías Fuera de SLA
top_categorias <- datos_analysis %>%
  filter(cumple_svl_procesador == "No") %>%
  count(categoria, sort = TRUE) %>%
  head(5)

grafico_categorias <- ggplot(top_categorias, aes(x = reorder(categoria, n), y = n)) +
  geom_col(fill = "#d62728") +
  geom_text(aes(label = n), hjust = 1.2, color = "white", size = 4) +  # Etiquetas dentro de las barras
  coord_flip() +
  labs(title = "Top 5 Categorías con Mayor Incumplimiento SLA",
       x = "", y = "N° Tickets Fuera de SLA") +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),  # Eliminar líneas de cuadrícula horizontales
    axis.text.y = element_text(size = 12)  # Aumentar tamaño de etiquetas del eje Y
  )

# 4. Distribución de Tiempos vs SLA
datos_cerrados <- datos_analysis %>% 
  filter(estado_actual_ticket == "Cerrado") %>%
  mutate(
    total_horas = total_tiempo_procesador / 3600,
    prioridad = factor(prioridad,
                       levels = c("Prioridad 1 - Inmediata", 
                                  "Prioridad 2 - Normal", 
                                  "Prioridad 3 - Baja"))
  )

sla_targets <- data.frame(
  prioridad = c("Prioridad 1 - Inmediata", "Prioridad 2 - Normal", "Prioridad 3 - Baja"),
  target = c(8, 24, 40)
)

grafico_distribucion <- ggplot(datos_cerrados, aes(x = total_horas)) +
  geom_histogram(binwidth = 2, fill = "#2ca02c", alpha = 0.7) +
  geom_vline(data = sla_targets, aes(xintercept = target), 
             color = "red", linetype = "dashed", size = 1) +
  geom_text(data = sla_targets, aes(x = target, y = 0, label = paste0("Objetivo: ", target, "h")), 
            vjust = -1, hjust = 1.1, color = "red", size = 3.5) +  # Etiquetas de objetivos SLA
  facet_wrap(~prioridad, scales = "free") +
  labs(title = "Distribución de Tiempos de Resolución vs Objetivo SLA",
       x = "Horas Totales", y = "N° Tickets") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold")  # Aumentar tamaño de títulos de facetas
  )

# 5. Cumplimiento SLA por Nivel de Atención
sla_nivel <- datos_analysis %>%
  group_by(nivel_atencion) %>%
  summarise(
    total = n(),
    cumplidos = sum(cumple_svl_procesador == "Yes"),
    porc = cumplidos/total * 100
  )

grafico_nivel <- ggplot(sla_nivel, aes(x = reorder(nivel_atencion, porc), y = porc)) +
  geom_col(fill = "#9467bd") +
  geom_text(aes(label = paste0(round(porc), "%")), hjust = -0.1, size = 4) +  # Etiquetas de porcentaje
  coord_flip() +
  labs(title = "Cumplimiento SLA por Nivel de Atención",
       x = "", y = "% Dentro de SLA") +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),  # Aumentar tamaño de etiquetas del eje Y
    panel.grid.major.y = element_blank()  # Eliminar líneas de cuadrícula horizontales
  )

# --------------------------
# Guardar todos los gráficos
# --------------------------
guardar_grafico <- function(grafico, nombre) {
  path <- tempfile(fileext = ".png")
  ggsave(path, grafico, width = 8, height = 5, dpi = 100)
  return(path)
}

paths <- list(
  tendencia = guardar_grafico(grafico_tendencia_sla),
  etapas = guardar_grafico(grafico_etapas),
  categorias = guardar_grafico(grafico_categorias),
  distribucion = guardar_grafico(grafico_distribucion),
  nivel = guardar_grafico(grafico_nivel)
)

# --------------------------
# Construcción del Correo
# --------------------------
cuerpo_correo <- paste0(
  "<html><body style='font-family: Arial, sans-serif;'>",
  "<h2 style='color: #1f77b4;'>Reporte Diario Tickets SSCC</h2>",
  "<h3>", format(Sys.Date(), "%d %B %Y"), "</h3>",
  kpi_text,
  "<h3>Análisis Detallado</h3>",
  "1. <b>Tendencia Cumplimiento SLA:</b><br>", add_image(paths$tendencia, width = 650), "<br><br>",
  "2. <b>Eficiencia por Etapas:</b><br>", add_image(paths$etapas, width = 650), "<br><br>",
  "3. <b>Áreas Críticas:</b><br>", add_image(paths$categorias, width = 650), "<br><br>",
  "4. <b>Distribución de Tiempos:</b><br>", add_image(paths$distribucion, width = 650), "<br><br>",
  "5. <b>Desempeño por Nivel:</b><br>", add_image(paths$nivel, width = 650), "<br><br>",
  "<p style='color: #666;'>* Datos correspondientes al mes en curso<br>",
  "** SLA calculado sobre tickets cerrados<br>",
  "*** SLA solo considera tiempos de atención agentes<br>",
  "**** Estimación de Tiempos en Horas Laborales</p>",
  "</body></html>"
)

# Crear y enviar correo
email <- compose_email(
  body = md(cuerpo_correo),
  footer = md("Reporte Automatizado - Analítica SSCC")
) %>% add_attachment(output_path)

# Configurar las credenciales de Microsoft 365
creds <- creds(
  user = "racl26345@cpm.coop",  # Tu correo institucional
  host = "smtp.office365.com",  # Servidor SMTP de Microsoft 365
  port = 587,                   # Puerto SMTP para Microsoft 365
  use_ssl = TRUE                # Usar SSL
)

# Enviar el correo electrónico
smtp_send(
  email,
  from = "luis_ramirezC@cpm.coop",  # Tu correo institucional
  to = c("gerardo_nahum@cpm.coop", "bibiana_rico@cpm.coop"),  # Correos de los destinatarios
  cc = "luis_ramirezC@cpm.coop",  # Correos de destinatarios-copias
  subject = paste("Reporte Mensual SVL C4C -", fecha_anterior),
  credentials = creds
)

# Mensaje de confirmación
cat("El correo electrónico ha sido enviado.\n")

# Limpiar archivos temporales
file.remove(unlist(paths))





##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################
##################################################################################



