################# REPORTE SVL C4C MEJORADO ############################


# --------------------------------------------------------------
# A. CARGAR LAS LIBRERIAS y DEFINIR FUNCIONES
# --------------------------------------------------------------

library(DBI)
library(RSQLite)
library(openxlsx)
library(blastula)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(gridExtra)
library(gtable)
library(stringr)
library(xml2)
library(ggrepel)
library(purrr)

#Definir la función para guardar gráficos
guardar_grafico <- function(grafico, nombre, ancho = 8, alto = 5, dpi = 100) {
  path <- tempfile(fileext = ".png")
  ggsave(path, grafico, width = ancho, height = alto, dpi = dpi)
  return(path)
}

# --------------------------------------------------------------
# B. EXTRAER Y TRANSFORMAR DATOS PARA REPORTE
# --------------------------------------------------------------

#Definir el query SQL
query <- "
-- MODIFIED QUERY (filter by date > 1/1/2025)
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
        CASE WHEN catalog_tickets.categoria = 'Beneficios Económicos Sindicalizados' THEN 259200
            WHEN catalog_tickets.subcategoria = 'Reposición de tarjeta de Nómina' THEN 460800
            WHEN catalog_tickets.subcategoria = 'Constancia Laboral' THEN 86400
            WHEN catalog_tickets.prioridad = 'Prioridad 1 - Inmediata' THEN 28800
            WHEN catalog_tickets.prioridad = 'Prioridad 2 - Normal' THEN 86400
            ELSE 144000 END AS tiempo_objetivo
    FROM codigo_tickets 
    LEFT JOIN catalog_tickets
        ON codigo_tickets.id_catalog = catalog_tickets.id_catalog
    WHERE codigo_tickets.fecha_creado BETWEEN '2025-10-01' AND '2025-12-31'
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
        agnt.id_colaborador AS id_procesador,
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
    LEFT JOIN agentes_tickets agnt
        ON tickets.agente_servicio = agnt.agente_nombre
    LEFT JOIN datos_colaboradores procesador
        ON agnt.id_colaborador = procesador.id_colaborador
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
output_path <- paste0("C:/Users/racl26345/Documents/Reportes Automatizados/Detalle Trimestral Tickets C4C ", fecha_anterior, ".xlsx")
write.xlsx(datos_excel, output_path, rowNames = FALSE)

# --------------------------------------------------------------
# C. CÁLCULO DE KPIS Y MÉTRICAS
# --------------------------------------------------------------

# --------------------------------------------------------------
# C. CÁLCULO DE KPIS Y MÉTRICAS
# --------------------------------------------------------------

# C.1. KPIs del Trimestre
kpis_trimestre <- datos_analysis %>%
  summarise(
    total_tickets = n(),
    tickets_por_resolver = sum(estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    tickets_espera_confirmacion = sum(estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    tickets_cerrados = sum(estado_actual_ticket == "Cerrado"),
    porc_por_resolver = round(tickets_por_resolver / total_tickets * 100, 2),
    porc_espera_confirmacion = round(tickets_espera_confirmacion / total_tickets * 100, 2),
    porc_cerrados = round(tickets_cerrados / total_tickets * 100, 2),
    count_in_sla = sum(cumple_svl_procesador == "Yes" & estado_actual_ticket == "Cerrado"),
    porc_sla_general = round(count_in_sla / tickets_cerrados * 100, 2),
    tiempo_medio_resolucion = round(mean(total_tiempo_procesador[estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2)
  )

# C.2. Métricas de Referencia del Trimestre
metricas_trimestre <- datos_analysis %>%
  summarise(
    total_tickets = n(),  # Calcular total_tickets dentro del mismo summarise
    
    # Consultas
    consultas = sum(tipo_ticket == "Tipo 1 - Consulta"),
    consultas_por_resolver = sum(tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    consultas_espera_confirmacion = sum(tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    consultas_cerrados = sum(tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket == "Cerrado"),
    porc_consultas = round(consultas / total_tickets * 100, 1),
    porc_consultas_por_resolver = round(consultas_por_resolver / consultas * 100, 1),
    porc_consultas_espera_confirmacion = round(consultas_espera_confirmacion / consultas * 100, 1),
    porc_consultas_cerrados = round(consultas_cerrados / consultas * 100, 1),
    count_consultas_sla = sum(cumple_svl_procesador == "Yes" & tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket == "Cerrado"),
    sla_consultas = round(count_consultas_sla / consultas_cerrados * 100, 2),
    tiempo_medio_consultas = round(mean(total_tiempo_procesador[tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2),
    
    # Requerimientos
    requerimientos = sum(tipo_ticket == "Tipo 2 - Requerimiento"),
    requerimientos_por_resolver = sum(tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    requerimientos_espera_confirmacion = sum(tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    requerimientos_cerrados = sum(tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket == "Cerrado"),
    porc_requerimientos = round(requerimientos / total_tickets * 100, 2),
    porc_requerimientos_por_resolver = round(requerimientos_por_resolver / requerimientos * 100, 1),
    porc_requerimientos_espera_confirmacion = round(requerimientos_espera_confirmacion / requerimientos * 100, 1),
    porc_requerimientos_cerrados = round(requerimientos_cerrados / requerimientos * 100, 1),
    count_requerimientos_sla = sum(cumple_svl_procesador == "Yes" & tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket == "Cerrado"),  
    sla_requerimientos = round(count_requerimientos_sla / requerimientos_cerrados * 100, 2),
    tiempo_medio_requerimientos = round(mean(total_tiempo_procesador[tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2),
    
    # Rhadar vs Escalamientos
    rhadar = sum(nivel_atencion != "Nivel 3 - Escalamiento"),
    rhadar_por_resolver = sum(nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    rhadar_espera_confirmacion = sum(nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    rhadar_cerrados = sum(nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"),
    porc_rhadar = round(rhadar / total_tickets * 100, 2),
    porc_rhadar_por_resolver = round(rhadar_por_resolver / rhadar * 100, 1),
    porc_rhadar_espera_confirmacion = round(rhadar_espera_confirmacion / rhadar * 100, 1),
    porc_rhadar_cerrados = round(rhadar_cerrados / rhadar * 100, 1),
    count_rhadar_sla = sum(cumple_svl_procesador == "Yes" & nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"),  
    sla_rhadar = round(count_rhadar_sla / rhadar_cerrados * 100, 2),
    tiempo_medio_rhadar = round(mean(total_tiempo_procesador[nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2),
    
    escalamientos = sum(nivel_atencion == "Nivel 3 - Escalamiento"),
    escalamientos_por_resolver = sum(nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    escalamientos_espera_confirmacion = sum(nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    escalamientos_cerrados = sum(nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"),
    porc_escalamientos = round(escalamientos / total_tickets * 100, 2),
    porc_escalamientos_por_resolver = round(escalamientos_por_resolver / escalamientos * 100, 1),
    porc_escalamientos_espera_confirmacion = round(escalamientos_espera_confirmacion / escalamientos * 100, 1),
    porc_escalamientos_cerrados = round(escalamientos_cerrados / escalamientos * 100, 1),
    count_escalamientos_sla = sum(cumple_svl_procesador == "Yes" & nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"),    
    sla_escalamientos = round(count_escalamientos_sla / escalamientos_cerrados * 100, 2),
    tiempo_medio_escalamientos = round(mean(total_tiempo_procesador[nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2)
  )


# --------------------------------------------------------------
# D. GENERACIÓN DE GRÁFICOS
# --------------------------------------------------------------

# D.1. Tendencia Diaria de Cumplimiento SLA
daily_sla <- datos_analysis %>%
  mutate(fecha = as.Date(fecha_creado),
         dia_semana = weekdays(fecha),
         es_fin_de_semana = ifelse(dia_semana %in% c("sábado", "domingo"), "Fin de Semana", "Día de Semana")) %>%
  group_by(fecha, es_fin_de_semana) %>%
  summarise(
    total = n(),
    cerrados = sum(estado_actual_ticket == "Cerrado"),
    cumplidos = sum(cumple_svl_procesador == "Yes" & estado_actual_ticket == "Cerrado"),
    porc_cumplido = ifelse(cerrados > 0, cumplidos / cerrados * 100, 0)
  )

grafico_tendencia_sla <- ggplot(daily_sla, aes(x = fecha)) +
  # Barras para el volumen de tickets creados y cerrados
  geom_bar(aes(y = total, fill = "Tickets Creados"), stat = "identity", alpha = 0.6) +
  geom_bar(aes(y = cerrados, fill = "Tickets Cerrados"), stat = "identity", alpha = 0.6) +
  
  # Línea y puntos para el porcentaje de cumplimiento SLA
  geom_line(aes(y = porc_cumplido * max(total) / 100), color = "#1f77b4", size = 1) +
  geom_point(aes(y = porc_cumplido * max(total) / 100), color = "#1f77b4", size = 3) +
  
  # Etiquetas de %SLA solo para días por debajo del 80%
  geom_text(
    aes(
      y = porc_cumplido * max(total) / 100,
      label = ifelse(porc_cumplido < 80, paste0(round(porc_cumplido, 1), "%"), "")
    ),
    vjust = -1, color = "#1f77b4", size = 3.5
  ) +
  
  # Línea horizontal para el goal del 80%
  geom_hline(yintercept = 80 * max(daily_sla$total) / 100, linetype = "dashed", color = "red", size = 1) +
  
  # Escalas y ejes
  scale_y_continuous(
    name = "Volumen de Tickets",
    sec.axis = sec_axis(~ . / max(daily_sla$total) * 100, name = "% Cumplimiento SLA")
  ) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day") +
  scale_fill_manual(
    name = "Leyenda",
    values = c("Tickets Creados" = "#1f77b4", "Tickets Cerrados" = "#2ca02c", "Fin de Semana" = "#ff7f0e", "Día de Semana" = "#1f77b4"),
    breaks = c("Tickets Creados", "Tickets Cerrados", "Fin de Semana", "Día de Semana")
  ) +
  
  # Títulos y tema
  labs(
    title = "Tendencia Diaria de Cumplimiento SLA",
    x = "Fecha",
    y = "Volumen de Tickets",
    fill = "Leyenda"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Guardar el gráfico
path_tendencia_mejorada <- guardar_grafico(grafico_tendencia_sla, "tendencia_mejorada.png", ancho = 16, alto = 6)

# D.2. Tiempo por Etapa de Proceso
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
  summarise(horas_promedio = mean(horas, na.rm = TRUE))

grafico_etapas <- ggplot(tiempo_etapas_agg, aes(x = prioridad, y = horas_promedio, fill = etapa)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(horas_promedio, 1), "h")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "Tiempo Promedio por Etapa y Prioridad",
       x = "Prioridad", y = "Horas Promedio", fill = "Etapa") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# D.3. Áreas Críticas | Top 5 Categorías Fuera de SLA con porcentaje
# Obtener las top 5 categorías críticas
top_categorias <- datos_analysis %>%
  group_by(categoria) %>%
  summarise(
    total_tickets = n(),
    fuera_sla = sum(cumple_svl_procesador == "No"),
    porc_fuera_sla = round(fuera_sla / total_tickets * 100, 1)
  ) %>%
  arrange(desc(fuera_sla)) %>%
  head(5)

# Obtener las top 3 subcategorías de cada categoría crítica
top_subcategorias <- datos_analysis %>%
  filter(categoria %in% top_categorias$categoria) %>%
  group_by(categoria, subcategoria) %>%
  summarise(
    total_tickets = n(),
    fuera_sla = sum(cumple_svl_procesador == "No"),
    porc_fuera_sla = round(fuera_sla / total_tickets * 100, 1)
  ) %>%
  arrange(categoria, desc(fuera_sla)) %>%
  group_by(categoria) %>%
  slice_head(n = 3) %>%
  ungroup()

# Combinar las categorías y subcategorías en un solo dataframe
datos_grafico <- bind_rows(
  top_categorias %>% mutate(tipo = "Categoría"),
  top_subcategorias %>% mutate(tipo = "Subcategoría")
) %>%
  mutate(
    etiqueta = paste0(fuera_sla, " (", porc_fuera_sla, "%)"),
    nombre = ifelse(tipo == "Categoría", categoria, subcategoria)
  )

# Calcular el valor máximo de "fuera_sla" para ajustar la escala
max_fuera_sla <- max(datos_grafico$fuera_sla, na.rm = TRUE)

# Ajustar el límite superior del eje X (horizontal en el gráfico volteado)
limite_superior <- max_fuera_sla * 1.2  # Aumentar un 20% para dar espacio a las etiquetas

# Crear el gráfico jerárquico con la escala ajustada
grafico_jerarquico <- ggplot(datos_grafico, aes(x = reorder(nombre, fuera_sla), y = fuera_sla, fill = tipo)) +
  geom_col(alpha = 0.7) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.1,  # Etiquetas a la derecha de las barras
    size = 3.5,
    color = "black"
  ) +
  coord_flip() +
  facet_wrap(
    ~categoria, 
    scales = "free_y", 
    ncol = 1, 
    strip.position = "top"
  ) +
  scale_y_continuous(limits = c(0, limite_superior)) +  # Ajustar el límite superior del eje X
  labs(
    title = "Top 5 Categorías Críticas y sus Top 3 Subcategorías con Mayor Incumplimiento SLA",
    x = "",
    y = "N° Tickets Fuera de SLA",
    fill = "Tipo",
    caption = "Entre paréntesis se muestra el % de tickets fuera de SLA respecto al total de tickets de la categoría/subcategoría."
  ) +
  scale_fill_manual(values = c("Categoría" = "#1f77b4", "Subcategoría" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold", hjust = 0),  # Estilo de las etiquetas de categoría
    plot.caption = element_text(hjust = 0, size = 9, color = "gray50", margin = ggplot2::margin(t = 10)),
    plot.margin = ggplot2::margin(r = 20)
  )

# Guardar el gráfico
path_grafico_jerarquico <- guardar_grafico(grafico_jerarquico, "grafico_jerarquico.png", ancho = 10, alto = 12)

# D.4. Distribución de Tiempos vs SLA
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
            vjust = -1, hjust = 1.1, color = "red", size = 3.5) +
  facet_wrap(~prioridad, scales = "free") +
  labs(title = "Distribución de Tiempos de Resolución vs Objetivo SLA",
       x = "Horas Totales", y = "N° Tickets") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold")
  )

# D.5. Desempeño por Nivel | Cumplimiento SLA por Nivel de Atención
sla_nivel <- datos_analysis %>%
  group_by(nivel_atencion) %>%
  summarise(
    total = n(),
    cumplidos = sum(cumple_svl_procesador == "Yes"),
    porc = cumplidos / total * 100,
    tiempo_medio_respuesta = mean(total_tiempo_procesador / 3600, na.rm = TRUE)
  )

grafico_nivel <- ggplot(sla_nivel, aes(x = reorder(nivel_atencion, porc), y = porc)) +
  geom_col(fill = "#9467bd") +
  geom_text(
    aes(label = paste0("SLA: ", round(porc,1), "%\n", "Tiempo: ", round(tiempo_medio_respuesta, 2), "h\n", "Tickets: ", total)), 
    hjust = -0.1, size = 4, vjust = 0.5
  ) +
  coord_flip() +
  labs(
    title = "Cumplimiento SLA por Nivel de Atención",
    x = "", 
    y = "% Dentro de SLA"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank()
  )

# D.6. Comparativo de Tiempos por Nivel de Gestión y Área de Personal
tiempos_nivel_area <- datos_analysis %>%
  group_by(nivel_gestion_cliente, area_personal_cliente) %>%
  summarise(
    tiempo_procesador_promedio = mean(total_tiempo_procesador / 3600, na.rm = TRUE),
    tiempo_cliente_promedio = mean(total_tiempo_cliente / 3600, na.rm = TRUE),
    n_tickets = n(),  # Agregamos el conteo de tickets
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(tiempo_procesador_promedio, tiempo_cliente_promedio),
    names_to = "tipo_tiempo",
    values_to = "horas_promedio"
  )

# Calcula el máximo valor de horas_promedio con un 10% de margen
max_horas <- max(tiempos_nivel_area$horas_promedio, na.rm = TRUE) * 1.1

# Crear el gráfico mejorado
grafico_nivel_area <- ggplot(tiempos_nivel_area, aes(x = nivel_gestion_cliente, y = horas_promedio, fill = tipo_tiempo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  
  # Texto dentro de las barras (valores de tiempo)
  geom_text(
    aes(label = sprintf("%.1f h", horas_promedio)),
    position = position_dodge(width = 0.9),
    angle = 90,
    hjust = 1.1,
    vjust = 0.5,
    size = 4.5,
    color = "black"
  ) +
  
  # Texto encima de las barras (número de tickets)
  geom_text(
    aes(
      y = max_horas * 0.95,  # Posición cerca del tope del gráfico
      label = paste("n =", n_tickets),
      group = nivel_gestion_cliente  # Agrupar por nivel de gestión
    ),
    position = position_dodge(width = 0.9),
    size = 3.5,
    color = "black",
    vjust = -0.5
  ) +
  
  facet_wrap(~area_personal_cliente, scales = "fixed") +
  labs(
    title = "Comparativo de Tiempos por Nivel de Gestión y Área de Personal",
    subtitle = "Valores dentro de barras: Horas promedio | Valores superiores: Número de tickets",
    x = "Nivel de Gestión", 
    y = "Horas Promedio", 
    fill = "Tipo de Tiempo"
  ) +
  scale_fill_manual(
    values = c("tiempo_procesador_promedio" = "#1f77b4", "tiempo_cliente_promedio" = "#ff7f0e"),
    labels = c("Tiempo Procesador", "Tiempo Cliente")
  ) +
  scale_y_continuous(limits = c(0, max_horas), expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    panel.spacing = unit(1, "lines")
  )

# D.7. Tipos de Tickets y Niveles de Atención | Volúmenes
grafico_volumen_tipo_nivel <- datos_analysis %>%
  group_by(tipo_ticket, nivel_atencion) %>%
  summarise(volumen = n(), .groups = "drop") %>%
  ggplot(aes(x = tipo_ticket, y = volumen, fill = nivel_atencion)) +
  geom_col(position = "dodge") +
  geom_text_repel(
    aes(label = volumen), 
    position = position_dodge(width = 0.9),
    size = 5.25,
    box.padding = 0.75,
    point.padding = 0.75,
    min.segment.length = 0.3
  ) +
  labs(
    title = "Volumen de Tickets por Tipo y Nivel de Atención",
    x = "Tipo de Ticket",
    y = "Volumen",
    fill = "Nivel de Atención"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )

# D.8. Tipos de Tickets y Niveles de Atención | Tiempos
grafico_tiempos_tipo_nivel <- datos_analysis %>%
  group_by(tipo_ticket, nivel_atencion) %>%
  summarise(tiempo_promedio = mean(total_tiempo_procesador / 3600, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = tipo_ticket, y = tiempo_promedio, fill = nivel_atencion)) +
  geom_col(position = "dodge") +
  geom_text_repel(
    aes(label = sprintf("%.1f h", tiempo_promedio)), 
    position = position_dodge(width = 0.9),
    size = 5.25,
    box.padding = 0.75,
    point.padding = 0.75,
    min.segment.length = 0.3
  ) +
  labs(
    title = "Tiempos de Procesador por Tipo y Nivel de Atención",
    x = "Tipo de Ticket",
    y = "Horas Promedio",
    fill = "Nivel de Atención"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )

# --------------------------------------------------------------
# Alpha. Análisis de Tiempos por Departamento y Procesador
# --------------------------------------------------------------

# Función para calcular percentiles 5 y 95
calcular_limites <- function(x) {
  quantile(x, probs = c(0.05, 0.95), na.rm = TRUE)
}

# D.9. Tiempos por Departamento (Violines truncados)
grafico_tiempos_departamento <- datos_analysis %>%
  mutate(horas_procesador = total_tiempo_procesador/3600) %>%
  group_by(departamento_procesador) %>%
  mutate(
    limite_inferior = calcular_limites(horas_procesador)[1],
    limite_superior = calcular_limites(horas_procesador)[2]
  ) %>%
  filter(horas_procesador >= limite_inferior & horas_procesador <= limite_superior) %>%
  ggplot(aes(x = departamento_procesador, y = horas_procesador)) +
  geom_violin(aes(fill = departamento_procesador), trim = TRUE, scale = "width") +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  stat_summary(fun = "median", geom = "point", size = 3, color = "black") +
  labs(
    title = "Distribución de Tiempos por Departamento (P5-P95)",
    subtitle = "Ámbito visual: Percentiles 5-95 | Punto negro: Mediana",
    x = "Departamento",
    y = "Horas de Procesamiento"
  ) +
  scale_y_continuous(labels = scales::number_format(suffix = " h")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# D.9.1 Tabla de tickets extremos por departamento
tabla_extremos <- datos_analysis %>%
  mutate(horas_procesador = total_tiempo_procesador/3600) %>%
  group_by(departamento_procesador) %>%
  summarise(
    Ticket_Max = paste0("TKT-", id_ticket[which.max(horas_procesador)]),
    Horas_Max = sprintf("%.1f h", max(horas_procesador)),
    Ticket_Min = paste0("TKT-", id_ticket[which.min(horas_procesador)]),
    Horas_Min = sprintf("%.1f h", min(horas_procesador))
  ) %>%
  mutate(
    departamento_procesador = str_wrap(departamento_procesador, width = 20)
  )

# D.9.2 Convertir a tabla grob
tabla_grob <- gridExtra::tableGrob(
  tabla_extremos,
  theme = gridExtra::ttheme_minimal(
    base_size = 8,
    padding = grid::unit(c(4, 4), "mm"),
    core = list(fg_params = list(hjust = 0, x = 0.05))
  )
)


# D.10. Tiempos Promedio por Procesador (Barras agrupadas)
grafico_tiempos_procesador <- datos_analysis %>%
  group_by(departamento_procesador, nombre_procesador) %>%
  summarise(
    horas_promedio = mean(total_tiempo_procesador/3600, na.rm = TRUE),
    cumplimiento_sla = mean(cumple_svl_procesador == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    nombre_procesador = str_wrap(nombre_procesador, width = 20),
    departamento_procesador = str_to_upper(departamento_procesador) %>% 
      str_wrap(width = 15)
  ) %>%
  ggplot(aes(x = reorder(nombre_procesador, horas_promedio), y = horas_promedio)) +
  geom_col(aes(fill = cumplimiento_sla), width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f h", horas_promedio)), 
    hjust = -0.1, 
    size = 4.5,
    color = "black"
  ) +
  coord_flip() +
  facet_grid(
    departamento_procesador ~ ., 
    scales = "free_y", 
    space = "free",
    switch = "y"
  ) +
  labs(
    title = "Tiempo Promedio por Procesador",
    subtitle = "Color: % Cumplimiento SLA | Barras: Horas promedio",
    x = "Procesador",
    y = "Horas Promedio",
    fill = "% Cumplimiento SLA:"
  ) +
  scale_fill_gradient(
    name = "% Cumplimiento SLA:",
    labels = scales::percent,
    low = "#FF5252", 
    high = "#4CAF50",
    limits = c(0, 1),
    guide = guide_colorbar(
      title.position = "top",
      barwidth = unit(8, "cm"),
      direction = "horizontal"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    strip.text.y = element_text(
      angle = 0,
      size = 12,
      face = "bold",
      hjust = 0.5,
      margin = ggplot2::margin(l = 10)
    ),
    legend.position = "bottom",
    plot.margin = ggplot2::margin(r = 3, unit = "cm"),
    axis.text.y = element_text(size = 10)
  )

# D.11. Volumen de Tickets por Plaza (Agrupados por Regional)
grafico_plazas_regional <- datos_analysis %>%
  group_by(regional_cliente, plaza_cliente) %>%
  summarise(
    tickets_creados = n(),
    sla_cumplido = mean(cumple_svl_procesador == "Yes" & estado_actual_ticket == "Cerrado", na.rm = TRUE) * 100,
    tiempo_promedio = mean(total_tiempo_procesador/3600, na.rm = TRUE),
    porcentaje_escalados = mean(nivel_atencion == "Nivel 3 - Escalamiento", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    etiqueta = paste0(  # Reorganizar en dos líneas
      "Tickets: ", tickets_creados, "  SLA: ", sprintf("%.1f%%", sla_cumplido), "\n",
      "Tiempo: ", sprintf("%.1f h", tiempo_promedio), "  Escalados: ", sprintf("%.1f%%", porcentaje_escalados)
    )
  ) %>%
  ggplot(aes(x = reorder(plaza_cliente, tickets_creados), y = tickets_creados, fill = regional_cliente)) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = etiqueta, y = tickets_creados),
    hjust = -0.1,
    size = 4,
    color = "black",
    position = position_nudge(y = 0),
    vjust = 0.5
  ) +
  coord_flip() +
  facet_grid(regional_cliente ~ ., scales = "free_y", space = "free") +
  labs(
    title = "Volumen de Tickets por Plaza (Agrupados por Regional)",
    subtitle = "Etiquetas: Tickets Creados, % SLA, Tiempo Promedio y % Escalados",
    x = "Plaza",
    y = "Tickets Creados",
    fill = "Regional"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    strip.text.y = element_text(angle = 0, size = 10, face = "bold"),
    legend.position = "bottom",
    plot.margin = ggplot2::margin(r = 8, unit = "cm")
  )


# --------------------------------------------------------------
# E. GUARDAR TODOS LOS GRÁFICOS
# --------------------------------------------------------------
guardar_grafico <- function(grafico, nombre, ancho = 8, alto = 5, dpi = 100) {
  path <- tempfile(fileext = ".png")
  ggsave(path, grafico, width = ancho, height = alto, dpi = dpi)
  return(path)
}

combo_departamento <- grid.arrange(
  grafico_tiempos_departamento,
  tabla_grob,
  nrow = 2,
  heights = c(3, 1.2)  # Más espacio para la tabla
)

paths <- list(
  etapas = guardar_grafico(grafico_etapas, "etapas"),
  distribucion = guardar_grafico(grafico_distribucion, "distribucion"),
  nivel = guardar_grafico(grafico_nivel, "nivel", ancho = 10, alto = 6),
  area = guardar_grafico(grafico_nivel_area, "area", ancho = 10, alto = 8),
  vol_tipo = guardar_grafico(grafico_volumen_tipo_nivel, "vol_tipo", ancho = 10, alto = 6),
  time_tipo = guardar_grafico(grafico_tiempos_tipo_nivel, "time_tipo", ancho = 10, alto = 6),
  departamento = guardar_grafico(combo_departamento, "departamento", ancho = 14, alto = 14),
  procesador = guardar_grafico(grafico_tiempos_procesador, "procesador", ancho = 18, alto = 22),
  plazas_regional = guardar_grafico(grafico_plazas_regional, "plazas_regional", ancho = 18, alto = 22)
)


# --------------------------------------------------------------
# G. CONSRUCCION Y ENVIO DEL CORREO
# --------------------------------------------------------------
# F.1. Texto de KPIs del Trimestre
kpi_text_trimestre <- paste0(
  "<b>KPIs del Trimestre Previo:</b><br>",
  "• Tickets Creados: ", kpis_trimestre$total_tickets, "<br>",
  "--- Tickets por Resolver: ", kpis_trimestre$tickets_por_resolver, " (", kpis_trimestre$porc_por_resolver, "%)<br>",
  "--- Tickets Espera Confirmación: ", kpis_trimestre$tickets_espera_confirmacion, " (", kpis_trimestre$porc_espera_confirmacion, "%)<br>",
  "--- Tickets Cerrados: ", kpis_trimestre$tickets_cerrados, " (", kpis_trimestre$porc_cerrados, "%)<br>",
  "• % SLA General: ", kpis_trimestre$porc_sla_general, "%<br>",
  "• Tiempo Medio Resolución (Cerrados): ", kpis_trimestre$tiempo_medio_resolucion, " hrs<br><br>"
)

# F.2. Texto de Métricas de Referencia del Trimestre
metricas_text_trimestre <- paste0(
  "<b>Métricas de Referencia del Trimestre Previo:</b><br>",
  "<b>[Consultas vs Requerimientos]</b><br>",
  "• <b>Consultas:</b><br>",
  "--- Total Consultas: ", metricas_trimestre$consultas, " (", metricas_trimestre$porc_consultas, "%)<br>",
  "▪▪▪▪▪ Por Resolver: ", metricas_trimestre$consultas_por_resolver, " (", metricas_trimestre$porc_consultas_por_resolver, "%)<br>",
  "▪▪▪▪▪ En Espera Confirmación: ", metricas_trimestre$consultas_espera_confirmacion, " (", metricas_trimestre$porc_consultas_espera_confirmacion, "%)<br>",
  "▪▪▪▪▪ Cerradas: ", metricas_trimestre$consultas_cerrados, " (", metricas_trimestre$porc_consultas_cerrados, "%)<br>",
  "--- % SLA (Consultas Cerradas): ", metricas_trimestre$sla_consultas, "%<br>",
  "--- Tiempo Medio Resolución: ", metricas_trimestre$tiempo_medio_consultas, " hrs<br>",
  "• <b>Requerimientos:</b><br>",
  "--- Total Requerimientos: ", metricas_trimestre$requerimientos, " (", metricas_trimestre$porc_requerimientos, "%)<br>",
  "▪▪▪▪▪ Por Resolver: ", metricas_trimestre$requerimientos_por_resolver, " (", metricas_trimestre$porc_requerimientos_por_resolver, "%)<br>",
  "▪▪▪▪▪ En Espera Confirmación: ", metricas_trimestre$requerimientos_espera_confirmacion, " (", metricas_trimestre$porc_requerimientos_espera_confirmacion, "%)<br>",
  "▪▪▪▪▪ Cerrados: ", metricas_trimestre$requerimientos_cerrados, " (", metricas_trimestre$porc_requerimientos_cerrados, "%)<br>",
  "--- % SLA (Requerimientos Cerrados): ", metricas_trimestre$sla_requerimientos, "%<br>",
  "--- Tiempo Medio Resolución: ", metricas_trimestre$tiempo_medio_requerimientos, " hrs<br>",
  "<b>[Rhadar vs Escalamientos]</b><br>",
  "• <b>Rhadar:</b><br>",
  "--- Total Rhadar: ", metricas_trimestre$rhadar, " (", metricas_trimestre$porc_rhadar, "%)<br>",
  "▪▪▪▪▪ Por Resolver: ", metricas_trimestre$rhadar_por_resolver, " (", metricas_trimestre$porc_rhadar_por_resolver, "%)<br>",
  "▪▪▪▪▪ En Espera Confirmación: ", metricas_trimestre$rhadar_espera_confirmacion, " (", metricas_trimestre$porc_rhadar_espera_confirmacion, "%)<br>",
  "▪▪▪▪▪ Cerrados: ", metricas_trimestre$rhadar_cerrados, " (", metricas_trimestre$porc_rhadar_cerrados, "%)<br>",
  "--- % SLA (Rhadar Cerrados): ", metricas_trimestre$sla_rhadar, "%<br>",
  "--- Tiempo Medio Resolución: ", metricas_trimestre$tiempo_medio_rhadar, " hrs<br>",
  "• <b>Escalamientos:</b><br>",
  "--- Total Escalamientos: ", metricas_trimestre$escalamientos, " (", metricas_trimestre$porc_escalamientos, "%)<br>",
  "▪▪▪▪▪ Por Resolver: ", metricas_trimestre$escalamientos_por_resolver, " (", metricas_trimestre$porc_escalamientos_por_resolver, "%)<br>",
  "▪▪▪▪▪ En Espera Confirmación: ", metricas_trimestre$escalamientos_espera_confirmacion, " (", metricas_trimestre$porc_escalamientos_espera_confirmacion, "%)<br>",
  "▪▪▪▪▪ Cerrados: ", metricas_trimestre$escalamientos_cerrados, " (", metricas_trimestre$porc_escalamientos_cerrados, "%)<br>",
  "--- % SLA (Escalamientos Cerrados): ", metricas_trimestre$sla_escalamientos, "%<br>",
  "--- Tiempo Medio Resolución: ", metricas_trimestre$tiempo_medio_escalamientos, " hrs<br><br>"
)


# G.4. Cuerpo del Correo
cuerpo_correo <- paste0(
  "<html><body style='font-family: Arial, sans-serif;'>",
  "<h2 style='color: #1f77b4;'>Reporte Trimestre Previo | Tickets C4C</h2>",
  "<h3>", format(Sys.Date(), "%d %B %Y"), "</h3>",
  kpi_text_trimestre,
  metricas_text_trimestre,
  "<h3>Análisis Detallado</h3>",
  "1. <b>Tendencia Cumplimiento SLA:</b><br>", add_image(path_tendencia_mejorada, width = 1000), "<br><br>",
  "2. <b>Eficiencia por Etapas:</b><br>", add_image(paths$etapas, width = 650), "<br><br>",
  "3. <b>Áreas Críticas:</b><br>", add_image(path_grafico_jerarquico, width = 650), "<br><br>",
  "4. <b>Distribución de Tiempos:</b><br>", add_image(paths$distribucion, width = 800), "<br><br>",
  "5. <b>Desempeño por Nivel:</b><br>", add_image(paths$nivel, width = 650), "<br><br>",
  "6. <b>Tiempos por Nivel de Gestión y Área de Personal:</b><br>", add_image(paths$area, width = 800), "<br><br>",
  "7. <b>Tipos de Tickets y Niveles de Atención | Volumenes:</b><br>", add_image(paths$vol_tipo, width = 650), "<br><br>",
  "8. <b>Tipos de Tickets y Niveles de Atención | Tiempos:</b><br>", add_image(paths$time_tipo, width = 650), "<br><br>",
  "9. <b>Tiempos por Departamento:</b><br>", add_image(paths$departamento, width = 1000), "<br><br>",
  "10. <b>Tiempos por Procesador:</b><br>", add_image(paths$procesador, width = 1000), "<br><br>",
  "11. <b>Volumen de Tickets por Plaza (Agrupados por Regional):</b><br>", add_image(paths$plazas_regional, width = 1000), "<br><br>",
  "<p style='color: #666;'>* Datos correspondientes al mes en curso<br>",
  "** SLA calculado sobre tickets cerrados<br>",
  "*** SLA solo considera tiempos de atención agentes<br>",
  "**** Estimación de Tiempos en Horas Laborales</p>",
  "</body></html>"
)

# G.5. Crear y enviar correo
email <- compose_email(
  body = md(cuerpo_correo),
  footer = md("Reporte Automatizado - Analítica")
) %>% 
  add_attachment(output_path)

# G.6. Configurar las credenciales de Microsoft 365
creds <- creds(
  user = "racl26345@cpm.coop",  # Tu correo institucional
  host = "smtp.office365.com",  # Servidor SMTP de Microsoft 365
  port = 587,                   # Puerto SMTP para Microsoft 365
  use_ssl = TRUE                # Usar SSL
)

# G.7. Enviar el correo electrónico
smtp_send(
  email,
  from = "luis_ramirezC@cpm.coop",  # Tu correo institucional
  to = c("gerardo_nahum@cpm.coop", "bibiana_rico@cpm.coop"),  # Correos de los destinatarios
  cc = "luis_ramirezC@cpm.coop",  # Correos de destinatarios-copias
  subject = paste("Reporte Trimestre Previo SLA C4C -", paste(fecha_anterior, collapse = " y ")),
  credentials = creds
)

# --------------------------------------------------------------
# H. CIERRE DE PROCESO
# --------------------------------------------------------------

# H.1. Mensaje de confirmación
cat("El correo electrónico ha sido enviado.\n")

# H.2. Limpiar archivos temporales
file.remove(unlist(paths))




################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################
################################################################################################################















################# REPORTE SVL C4C MEJORADO ############################


# --------------------------------------------------------------
# A. CARGAR LAS LIBRERIAS y DEFINIR FUNCIONES
# --------------------------------------------------------------

library(DBI)
library(RSQLite)
library(openxlsx)
library(blastula)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)
library(gridExtra)
library(gtable)
library(stringr)
library(xml2)
library(ggrepel)
library(purrr)

#Definir la función para guardar gráficos
guardar_grafico <- function(grafico, nombre, ancho = 8, alto = 5, dpi = 100) {
  path <- tempfile(fileext = ".png")
  ggsave(path, grafico, width = ancho, height = alto, dpi = dpi)
  return(path)
}

# --------------------------------------------------------------
# B. EXTRAER Y TRANSFORMAR DATOS PARA REPORTE
# --------------------------------------------------------------

#Definir el query SQL
query <- "
-- CURRENT QUARTER
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
        CASE WHEN catalog_tickets.categoria = 'Beneficios Económicos Sindicalizados' THEN 259200
            WHEN catalog_tickets.subcategoria = 'Reposición de tarjeta de Nómina' THEN 460800
            WHEN catalog_tickets.subcategoria = 'Constancia Laboral' THEN 86400
            WHEN catalog_tickets.prioridad = 'Prioridad 1 - Inmediata' THEN 28800
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
        agnt.id_colaborador AS id_procesador,
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
    LEFT JOIN agentes_tickets agnt
        ON tickets.agente_servicio = agnt.agente_nombre
    LEFT JOIN datos_colaboradores procesador
        ON agnt.id_colaborador = procesador.id_colaborador
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
output_path <- paste0("C:/Users/racl26345/Documents/Reportes Automatizados/Detalle Trimestral Tickets C4C ", fecha_anterior, ".xlsx")
write.xlsx(datos_excel, output_path, rowNames = FALSE)

# --------------------------------------------------------------
# C. CÁLCULO DE KPIS Y MÉTRICAS
# --------------------------------------------------------------

# --------------------------------------------------------------
# C. CÁLCULO DE KPIS Y MÉTRICAS
# --------------------------------------------------------------

# C.1. KPIs del Trimestre
kpis_trimestre <- datos_analysis %>%
  summarise(
    total_tickets = n(),
    tickets_por_resolver = sum(estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    tickets_espera_confirmacion = sum(estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    tickets_cerrados = sum(estado_actual_ticket == "Cerrado"),
    porc_por_resolver = round(tickets_por_resolver / total_tickets * 100, 2),
    porc_espera_confirmacion = round(tickets_espera_confirmacion / total_tickets * 100, 2),
    porc_cerrados = round(tickets_cerrados / total_tickets * 100, 2),
    count_in_sla = sum(cumple_svl_procesador == "Yes" & estado_actual_ticket == "Cerrado"),
    porc_sla_general = round(count_in_sla / tickets_cerrados * 100, 2),
    tiempo_medio_resolucion = round(mean(total_tiempo_procesador[estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2)
  )

# C.2. Métricas de Referencia del Trimestre
metricas_trimestre <- datos_analysis %>%
  summarise(
    total_tickets = n(),  # Calcular total_tickets dentro del mismo summarise
    
    # Consultas
    consultas = sum(tipo_ticket == "Tipo 1 - Consulta"),
    consultas_por_resolver = sum(tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    consultas_espera_confirmacion = sum(tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    consultas_cerrados = sum(tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket == "Cerrado"),
    porc_consultas = round(consultas / total_tickets * 100, 1),
    porc_consultas_por_resolver = round(consultas_por_resolver / consultas * 100, 1),
    porc_consultas_espera_confirmacion = round(consultas_espera_confirmacion / consultas * 100, 1),
    porc_consultas_cerrados = round(consultas_cerrados / consultas * 100, 1),
    count_consultas_sla = sum(cumple_svl_procesador == "Yes" & tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket == "Cerrado"),
    sla_consultas = round(count_consultas_sla / consultas_cerrados * 100, 2),
    tiempo_medio_consultas = round(mean(total_tiempo_procesador[tipo_ticket == "Tipo 1 - Consulta" & estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2),
    
    # Requerimientos
    requerimientos = sum(tipo_ticket == "Tipo 2 - Requerimiento"),
    requerimientos_por_resolver = sum(tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    requerimientos_espera_confirmacion = sum(tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    requerimientos_cerrados = sum(tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket == "Cerrado"),
    porc_requerimientos = round(requerimientos / total_tickets * 100, 2),
    porc_requerimientos_por_resolver = round(requerimientos_por_resolver / requerimientos * 100, 1),
    porc_requerimientos_espera_confirmacion = round(requerimientos_espera_confirmacion / requerimientos * 100, 1),
    porc_requerimientos_cerrados = round(requerimientos_cerrados / requerimientos * 100, 1),
    count_requerimientos_sla = sum(cumple_svl_procesador == "Yes" & tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket == "Cerrado"),  
    sla_requerimientos = round(count_requerimientos_sla / requerimientos_cerrados * 100, 2),
    tiempo_medio_requerimientos = round(mean(total_tiempo_procesador[tipo_ticket == "Tipo 2 - Requerimiento" & estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2),
    
    # Rhadar vs Escalamientos
    rhadar = sum(nivel_atencion != "Nivel 3 - Escalamiento"),
    rhadar_por_resolver = sum(nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    rhadar_espera_confirmacion = sum(nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    rhadar_cerrados = sum(nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"),
    porc_rhadar = round(rhadar / total_tickets * 100, 2),
    porc_rhadar_por_resolver = round(rhadar_por_resolver / rhadar * 100, 1),
    porc_rhadar_espera_confirmacion = round(rhadar_espera_confirmacion / rhadar * 100, 1),
    porc_rhadar_cerrados = round(rhadar_cerrados / rhadar * 100, 1),
    count_rhadar_sla = sum(cumple_svl_procesador == "Yes" & nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"),  
    sla_rhadar = round(count_rhadar_sla / rhadar_cerrados * 100, 2),
    tiempo_medio_rhadar = round(mean(total_tiempo_procesador[nivel_atencion != "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2),
    
    escalamientos = sum(nivel_atencion == "Nivel 3 - Escalamiento"),
    escalamientos_por_resolver = sum(nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    escalamientos_espera_confirmacion = sum(nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    escalamientos_cerrados = sum(nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"),
    porc_escalamientos = round(escalamientos / total_tickets * 100, 2),
    porc_escalamientos_por_resolver = round(escalamientos_por_resolver / escalamientos * 100, 1),
    porc_escalamientos_espera_confirmacion = round(escalamientos_espera_confirmacion / escalamientos * 100, 1),
    porc_escalamientos_cerrados = round(escalamientos_cerrados / escalamientos * 100, 1),
    count_escalamientos_sla = sum(cumple_svl_procesador == "Yes" & nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"),    
    sla_escalamientos = round(count_escalamientos_sla / escalamientos_cerrados * 100, 2),
    tiempo_medio_escalamientos = round(mean(total_tiempo_procesador[nivel_atencion == "Nivel 3 - Escalamiento" & estado_actual_ticket == "Cerrado"] / 3600, na.rm = TRUE), 2)
  )

# C.3. Métricas de Referencia del Día Previo
fecha_anterior <- if (weekdays(Sys.Date()) == "lunes") {
  # Si es lunes, considerar viernes y sábado
  seq(Sys.Date() - 3, Sys.Date() - 2, by = "day")
} else {
  # Si es de martes a viernes, considerar el día anterior
  Sys.Date() - 1
}

metricas_dia_previo <- datos_analysis %>%
  filter(as.Date(fecha_creado) %in% fecha_anterior) %>%
  summarise(
    tickets_creados_dia_previo = n(),
    tickets_por_resolver_dia_previo = sum(estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")),
    tickets_espera_confirmacion_dia_previo = sum(estado_actual_ticket %in% c("En proceso - Acción del cliente", "Completado - Solución propuesta", "Solución Confirmada")),
    tickets_cerrados_dia_previo = sum(estado_actual_ticket == "Cerrado"),
    porc_por_resolver_dia_previo = round(tickets_por_resolver_dia_previo / tickets_creados_dia_previo * 100, 1),
    porc_espera_confirmacion_dia_previo = round(tickets_espera_confirmacion_dia_previo / tickets_creados_dia_previo * 100, 1),
    porc_cerrados_dia_previo = round(tickets_cerrados_dia_previo / tickets_creados_dia_previo * 100, 1)
  )

# --------------------------------------------------------------
# D. GENERACIÓN DE GRÁFICOS
# --------------------------------------------------------------

# D.1. Tendencia Diaria de Cumplimiento SLA
daily_sla <- datos_analysis %>%
  mutate(fecha = as.Date(fecha_creado),
         dia_semana = weekdays(fecha),
         es_fin_de_semana = ifelse(dia_semana %in% c("sábado", "domingo"), "Fin de Semana", "Día de Semana")) %>%
  group_by(fecha, es_fin_de_semana) %>%
  summarise(
    total = n(),
    cerrados = sum(estado_actual_ticket == "Cerrado"),
    cumplidos = sum(cumple_svl_procesador == "Yes" & estado_actual_ticket == "Cerrado"),
    porc_cumplido = ifelse(cerrados > 0, cumplidos / cerrados * 100, 0)
  )

grafico_tendencia_sla <- ggplot(daily_sla, aes(x = fecha)) +
  # Barras para el volumen de tickets creados y cerrados
  geom_bar(aes(y = total, fill = "Tickets Creados"), stat = "identity", alpha = 0.6) +
  geom_bar(aes(y = cerrados, fill = "Tickets Cerrados"), stat = "identity", alpha = 0.6) +
  
  # Línea y puntos para el porcentaje de cumplimiento SLA
  geom_line(aes(y = porc_cumplido * max(total) / 100), color = "#1f77b4", size = 1) +
  geom_point(aes(y = porc_cumplido * max(total) / 100), color = "#1f77b4", size = 3) +
  
  # Etiquetas de %SLA solo para días por debajo del 80%
  geom_text(
    aes(
      y = porc_cumplido * max(total) / 100,
      label = ifelse(porc_cumplido < 80, paste0(round(porc_cumplido, 1), "%"), "")
    ),
    vjust = -1, color = "#1f77b4", size = 3.5
  ) +
  
  # Línea horizontal para el goal del 80%
  geom_hline(yintercept = 80 * max(daily_sla$total) / 100, linetype = "dashed", color = "red", size = 1) +
  
  # Escalas y ejes
  scale_y_continuous(
    name = "Volumen de Tickets",
    sec.axis = sec_axis(~ . / max(daily_sla$total) * 100, name = "% Cumplimiento SLA")
  ) +
  scale_x_date(date_labels = "%d %b", date_breaks = "1 day") +
  scale_fill_manual(
    name = "Leyenda",
    values = c("Tickets Creados" = "#1f77b4", "Tickets Cerrados" = "#2ca02c", "Fin de Semana" = "#ff7f0e", "Día de Semana" = "#1f77b4"),
    breaks = c("Tickets Creados", "Tickets Cerrados", "Fin de Semana", "Día de Semana")
  ) +
  
  # Títulos y tema
  labs(
    title = "Tendencia Diaria de Cumplimiento SLA",
    x = "Fecha",
    y = "Volumen de Tickets",
    fill = "Leyenda"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

# Guardar el gráfico
path_tendencia_mejorada <- guardar_grafico(grafico_tendencia_sla, "tendencia_mejorada.png", ancho = 12, alto = 6)

# D.2. Tiempo por Etapa de Proceso
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
  summarise(horas_promedio = mean(horas, na.rm = TRUE))

grafico_etapas <- ggplot(tiempo_etapas_agg, aes(x = prioridad, y = horas_promedio, fill = etapa)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = paste0(round(horas_promedio, 1), "h")), 
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3.5) +
  labs(title = "Tiempo Promedio por Etapa y Prioridad",
       x = "Prioridad", y = "Horas Promedio", fill = "Etapa") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# D.3. Áreas Críticas | Top 5 Categorías Fuera de SLA con porcentaje
# Obtener las top 5 categorías críticas
top_categorias <- datos_analysis %>%
  group_by(categoria) %>%
  summarise(
    total_tickets = n(),
    fuera_sla = sum(cumple_svl_procesador == "No"),
    porc_fuera_sla = round(fuera_sla / total_tickets * 100, 1)
  ) %>%
  arrange(desc(fuera_sla)) %>%
  head(5)

# Obtener las top 3 subcategorías de cada categoría crítica
top_subcategorias <- datos_analysis %>%
  filter(categoria %in% top_categorias$categoria) %>%
  group_by(categoria, subcategoria) %>%
  summarise(
    total_tickets = n(),
    fuera_sla = sum(cumple_svl_procesador == "No"),
    porc_fuera_sla = round(fuera_sla / total_tickets * 100, 1)
  ) %>%
  arrange(categoria, desc(fuera_sla)) %>%
  group_by(categoria) %>%
  slice_head(n = 3) %>%
  ungroup()

# Combinar las categorías y subcategorías en un solo dataframe
datos_grafico <- bind_rows(
  top_categorias %>% mutate(tipo = "Categoría"),
  top_subcategorias %>% mutate(tipo = "Subcategoría")
) %>%
  mutate(
    etiqueta = paste0(fuera_sla, " (", porc_fuera_sla, "%)"),
    nombre = ifelse(tipo == "Categoría", categoria, subcategoria)
  )

# Calcular el valor máximo de "fuera_sla" para ajustar la escala
max_fuera_sla <- max(datos_grafico$fuera_sla, na.rm = TRUE)

# Ajustar el límite superior del eje X (horizontal en el gráfico volteado)
limite_superior <- max_fuera_sla * 1.2  # Aumentar un 20% para dar espacio a las etiquetas

# Crear el gráfico jerárquico con la escala ajustada
grafico_jerarquico <- ggplot(datos_grafico, aes(x = reorder(nombre, fuera_sla), y = fuera_sla, fill = tipo)) +
  geom_col(alpha = 0.7) +
  geom_text(
    aes(label = etiqueta),
    hjust = -0.1,  # Etiquetas a la derecha de las barras
    size = 3.5,
    color = "black"
  ) +
  coord_flip() +
  facet_wrap(
    ~categoria, 
    scales = "free_y", 
    ncol = 1, 
    strip.position = "top"
  ) +
  scale_y_continuous(limits = c(0, limite_superior)) +  # Ajustar el límite superior del eje X
  labs(
    title = "Top 5 Categorías Críticas y sus Top 3 Subcategorías con Mayor Incumplimiento SLA",
    x = "",
    y = "N° Tickets Fuera de SLA",
    fill = "Tipo",
    caption = "Entre paréntesis se muestra el % de tickets fuera de SLA respecto al total de tickets de la categoría/subcategoría."
  ) +
  scale_fill_manual(values = c("Categoría" = "#1f77b4", "Subcategoría" = "#ff7f0e")) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(size = 12, face = "bold", hjust = 0),  # Estilo de las etiquetas de categoría
    plot.caption = element_text(hjust = 0, size = 9, color = "gray50", margin = ggplot2::margin(t = 10)),
    plot.margin = ggplot2::margin(r = 20)
  )

# Guardar el gráfico
path_grafico_jerarquico <- guardar_grafico(grafico_jerarquico, "grafico_jerarquico.png", ancho = 10, alto = 12)

# D.4. Distribución de Tiempos vs SLA
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
            vjust = -1, hjust = 1.1, color = "red", size = 3.5) +
  facet_wrap(~prioridad, scales = "free") +
  labs(title = "Distribución de Tiempos de Resolución vs Objetivo SLA",
       x = "Horas Totales", y = "N° Tickets") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold")
  )

# D.5. Desempeño por Nivel | Cumplimiento SLA por Nivel de Atención
sla_nivel <- datos_analysis %>%
  group_by(nivel_atencion) %>%
  summarise(
    total = n(),
    cumplidos = sum(cumple_svl_procesador == "Yes"),
    porc = cumplidos / total * 100,
    tiempo_medio_respuesta = mean(total_tiempo_procesador / 3600, na.rm = TRUE)
  )

grafico_nivel <- ggplot(sla_nivel, aes(x = reorder(nivel_atencion, porc), y = porc)) +
  geom_col(fill = "#9467bd") +
  geom_text(
    aes(label = paste0("SLA: ", round(porc,1), "%\n", "Tiempo: ", round(tiempo_medio_respuesta, 2), "h\n", "Tickets: ", total)), 
    hjust = -0.1, size = 4, vjust = 0.5
  ) +
  coord_flip() +
  labs(
    title = "Cumplimiento SLA por Nivel de Atención",
    x = "", 
    y = "% Dentro de SLA"
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank()
  )

# D.6. Comparativo de Tiempos por Nivel de Gestión y Área de Personal
tiempos_nivel_area <- datos_analysis %>%
  group_by(nivel_gestion_cliente, area_personal_cliente) %>%
  summarise(
    tiempo_procesador_promedio = mean(total_tiempo_procesador / 3600, na.rm = TRUE),
    tiempo_cliente_promedio = mean(total_tiempo_cliente / 3600, na.rm = TRUE),
    n_tickets = n(),  # Agregamos el conteo de tickets
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(tiempo_procesador_promedio, tiempo_cliente_promedio),
    names_to = "tipo_tiempo",
    values_to = "horas_promedio"
  )

# Calcula el máximo valor de horas_promedio con un 10% de margen
max_horas <- max(tiempos_nivel_area$horas_promedio, na.rm = TRUE) * 1.1

# Crear el gráfico mejorado
grafico_nivel_area <- ggplot(tiempos_nivel_area, aes(x = nivel_gestion_cliente, y = horas_promedio, fill = tipo_tiempo)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  
  # Texto dentro de las barras (valores de tiempo)
  geom_text(
    aes(label = sprintf("%.1f h", horas_promedio)),
    position = position_dodge(width = 0.9),
    angle = 90,
    hjust = 1.1,
    vjust = 0.5,
    size = 4.5,
    color = "black"
  ) +
  
  # Texto encima de las barras (número de tickets)
  geom_text(
    aes(
      y = max_horas * 0.95,  # Posición cerca del tope del gráfico
      label = paste("n =", n_tickets),
      group = nivel_gestion_cliente  # Agrupar por nivel de gestión
    ),
    position = position_dodge(width = 0.9),
    size = 3.5,
    color = "black",
    vjust = -0.5
  ) +
  
  facet_wrap(~area_personal_cliente, scales = "fixed") +
  labs(
    title = "Comparativo de Tiempos por Nivel de Gestión y Área de Personal",
    subtitle = "Valores dentro de barras: Horas promedio | Valores superiores: Número de tickets",
    x = "Nivel de Gestión", 
    y = "Horas Promedio", 
    fill = "Tipo de Tiempo"
  ) +
  scale_fill_manual(
    values = c("tiempo_procesador_promedio" = "#1f77b4", "tiempo_cliente_promedio" = "#ff7f0e"),
    labels = c("Tiempo Procesador", "Tiempo Cliente")
  ) +
  scale_y_continuous(limits = c(0, max_horas), expand = expansion(mult = c(0, 0.1))) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10, face = "bold"),
    plot.subtitle = element_text(size = 9, color = "gray40"),
    panel.spacing = unit(1, "lines")
  )

# D.7. Tipos de Tickets y Niveles de Atención | Volúmenes
grafico_volumen_tipo_nivel <- datos_analysis %>%
  group_by(tipo_ticket, nivel_atencion) %>%
  summarise(volumen = n(), .groups = "drop") %>%
  ggplot(aes(x = tipo_ticket, y = volumen, fill = nivel_atencion)) +
  geom_col(position = "dodge") +
  geom_text_repel(
    aes(label = volumen), 
    position = position_dodge(width = 0.9),
    size = 5.25,
    box.padding = 0.75,
    point.padding = 0.75,
    min.segment.length = 0.3
  ) +
  labs(
    title = "Volumen de Tickets por Tipo y Nivel de Atención",
    x = "Tipo de Ticket",
    y = "Volumen",
    fill = "Nivel de Atención"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )

# D.8. Tipos de Tickets y Niveles de Atención | Tiempos
grafico_tiempos_tipo_nivel <- datos_analysis %>%
  group_by(tipo_ticket, nivel_atencion) %>%
  summarise(tiempo_promedio = mean(total_tiempo_procesador / 3600, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = tipo_ticket, y = tiempo_promedio, fill = nivel_atencion)) +
  geom_col(position = "dodge") +
  geom_text_repel(
    aes(label = sprintf("%.1f h", tiempo_promedio)), 
    position = position_dodge(width = 0.9),
    size = 5.25,
    box.padding = 0.75,
    point.padding = 0.75,
    min.segment.length = 0.3
  ) +
  labs(
    title = "Tiempos de Procesador por Tipo y Nivel de Atención",
    x = "Tipo de Ticket",
    y = "Horas Promedio",
    fill = "Nivel de Atención"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(
    text = element_text(size = 15),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14),
    plot.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 15)
  )

# --------------------------------------------------------------
# Alpha. Análisis de Tiempos por Departamento y Procesador
# --------------------------------------------------------------

# Función para calcular percentiles 5 y 95
calcular_limites <- function(x) {
  quantile(x, probs = c(0.05, 0.95), na.rm = TRUE)
}

# D.9. Tiempos por Departamento (Violines truncados)
grafico_tiempos_departamento <- datos_analysis %>%
  mutate(horas_procesador = total_tiempo_procesador/3600) %>%
  group_by(departamento_procesador) %>%
  mutate(
    limite_inferior = calcular_limites(horas_procesador)[1],
    limite_superior = calcular_limites(horas_procesador)[2]
  ) %>%
  filter(horas_procesador >= limite_inferior & horas_procesador <= limite_superior) %>%
  ggplot(aes(x = departamento_procesador, y = horas_procesador)) +
  geom_violin(aes(fill = departamento_procesador), trim = TRUE, scale = "width") +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  stat_summary(fun = "median", geom = "point", size = 3, color = "black") +
  labs(
    title = "Distribución de Tiempos por Departamento (P5-P95)",
    subtitle = "Ámbito visual: Percentiles 5-95 | Punto negro: Mediana",
    x = "Departamento",
    y = "Horas de Procesamiento"
  ) +
  scale_y_continuous(labels = scales::number_format(suffix = " h")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

# D.9.1 Tabla de tickets extremos por departamento
tabla_extremos <- datos_analysis %>%
  mutate(horas_procesador = total_tiempo_procesador/3600) %>%
  group_by(departamento_procesador) %>%
  summarise(
    Ticket_Max = paste0("TKT-", id_ticket[which.max(horas_procesador)]),
    Horas_Max = sprintf("%.1f h", max(horas_procesador)),
    Ticket_Min = paste0("TKT-", id_ticket[which.min(horas_procesador)]),
    Horas_Min = sprintf("%.1f h", min(horas_procesador))
  ) %>%
  mutate(
    departamento_procesador = str_wrap(departamento_procesador, width = 20)
  )

# D.9.2 Convertir a tabla grob
tabla_grob <- gridExtra::tableGrob(
  tabla_extremos,
  theme = gridExtra::ttheme_minimal(
    base_size = 8,
    padding = grid::unit(c(4, 4), "mm"),
    core = list(fg_params = list(hjust = 0, x = 0.05))
  )
)


# D.10. Tiempos Promedio por Procesador (Barras agrupadas)
grafico_tiempos_procesador <- datos_analysis %>%
  group_by(departamento_procesador, nombre_procesador) %>%
  summarise(
    horas_promedio = mean(total_tiempo_procesador/3600, na.rm = TRUE),
    cumplimiento_sla = mean(cumple_svl_procesador == "Yes", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    nombre_procesador = str_wrap(nombre_procesador, width = 20),
    departamento_procesador = str_to_upper(departamento_procesador) %>% 
      str_wrap(width = 15)
  ) %>%
  ggplot(aes(x = reorder(nombre_procesador, horas_promedio), y = horas_promedio)) +
  geom_col(aes(fill = cumplimiento_sla), width = 0.7) +
  geom_text(
    aes(label = sprintf("%.1f h", horas_promedio)), 
    hjust = -0.1, 
    size = 4.5,
    color = "black"
  ) +
  coord_flip() +
  facet_grid(
    departamento_procesador ~ ., 
    scales = "free_y", 
    space = "free",
    switch = "y"
  ) +
  labs(
    title = "Tiempo Promedio por Procesador",
    subtitle = "Color: % Cumplimiento SLA | Barras: Horas promedio",
    x = "Procesador",
    y = "Horas Promedio",
    fill = "% Cumplimiento SLA:"
  ) +
  scale_fill_gradient(
    name = "% Cumplimiento SLA:",
    labels = scales::percent,
    low = "#FF5252", 
    high = "#4CAF50",
    limits = c(0, 1),
    guide = guide_colorbar(
      title.position = "top",
      barwidth = unit(8, "cm"),
      direction = "horizontal"
    )
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    strip.text.y = element_text(
      angle = 0,
      size = 12,
      face = "bold",
      hjust = 0.5,
      margin = ggplot2::margin(l = 10)
    ),
    legend.position = "bottom",
    plot.margin = ggplot2::margin(r = 3, unit = "cm"),
    axis.text.y = element_text(size = 10)
  )

# D.11. Volumen de Tickets por Plaza (Agrupados por Regional)
grafico_plazas_regional <- datos_analysis %>%
  group_by(regional_cliente, plaza_cliente) %>%
  summarise(
    tickets_creados = n(),
    sla_cumplido = mean(cumple_svl_procesador == "Yes" & estado_actual_ticket == "Cerrado", na.rm = TRUE) * 100,
    tiempo_promedio = mean(total_tiempo_procesador/3600, na.rm = TRUE),
    porcentaje_escalados = mean(nivel_atencion == "Nivel 3 - Escalamiento", na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    etiqueta = paste0(  # Reorganizar en dos líneas
      "Tickets: ", tickets_creados, "  SLA: ", sprintf("%.1f%%", sla_cumplido), "\n",
      "Tiempo: ", sprintf("%.1f h", tiempo_promedio), "  Escalados: ", sprintf("%.1f%%", porcentaje_escalados)
    )
  ) %>%
  ggplot(aes(x = reorder(plaza_cliente, tickets_creados), y = tickets_creados, fill = regional_cliente)) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = etiqueta, y = tickets_creados),
    hjust = -0.1,
    size = 4,
    color = "black",
    position = position_nudge(y = 0),
    vjust = 0.5
  ) +
  coord_flip() +
  facet_grid(regional_cliente ~ ., scales = "free_y", space = "free") +
  labs(
    title = "Volumen de Tickets por Plaza (Agrupados por Regional)",
    subtitle = "Etiquetas: Tickets Creados, % SLA, Tiempo Promedio y % Escalados",
    x = "Plaza",
    y = "Tickets Creados",
    fill = "Regional"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  theme_minimal() +
  theme(
    panel.spacing = unit(1.5, "lines"),
    strip.text.y = element_text(angle = 0, size = 10, face = "bold"),
    legend.position = "bottom",
    plot.margin = ggplot2::margin(r = 8, unit = "cm")
  )


# --------------------------------------------------------------
# E. GUARDAR TODOS LOS GRÁFICOS
# --------------------------------------------------------------
guardar_grafico <- function(grafico, nombre, ancho = 8, alto = 5, dpi = 100) {
  path <- tempfile(fileext = ".png")
  ggsave(path, grafico, width = ancho, height = alto, dpi = dpi)
  return(path)
}

combo_departamento <- grid.arrange(
  grafico_tiempos_departamento,
  tabla_grob,
  nrow = 2,
  heights = c(3, 1.2)  # Más espacio para la tabla
)

paths <- list(
  etapas = guardar_grafico(grafico_etapas, "etapas"),
  distribucion = guardar_grafico(grafico_distribucion, "distribucion"),
  nivel = guardar_grafico(grafico_nivel, "nivel", ancho = 10, alto = 6),
  area = guardar_grafico(grafico_nivel_area, "area", ancho = 10, alto = 8),
  vol_tipo = guardar_grafico(grafico_volumen_tipo_nivel, "vol_tipo", ancho = 10, alto = 6),
  time_tipo = guardar_grafico(grafico_tiempos_tipo_nivel, "time_tipo", ancho = 10, alto = 6),
  departamento = guardar_grafico(combo_departamento, "departamento", ancho = 12, alto = 14),
  procesador = guardar_grafico(grafico_tiempos_procesador, "procesador", ancho = 18, alto = 22),
  plazas_regional = guardar_grafico(grafico_plazas_regional, "plazas_regional", ancho = 18, alto = 22)
)

# --------------------------------------------------------------
# F. IDENTIFICACIÓN DE TICKETS PRIORITARIOS
# --------------------------------------------------------------

# Función para calcular la hora límite considerando la ventana de servicio
calcular_hora_limite <- function(tiempo_restante_sla, fecha_inicio) {
  # Definir la ventana de servicio
  ventana_lunes_viernes <- c(9, 18)  # 9:00 a 18:00
  ventana_sabado <- c(9, 14)         # 9:00 a 14:00
  
  # Convertir la fecha de inicio a POSIXct
  fecha_inicio <- as.POSIXct(fecha_inicio, tz = "America/Mexico_City")
  
  # Inicializar la hora límite
  hora_limite <- fecha_inicio
  
  # Mientras haya tiempo restante
  while (tiempo_restante_sla > 0) {
    # Obtener el día de la semana (1 = lunes, 7 = domingo)
    dia_semana <- as.numeric(format(hora_limite, "%u"))
    
    # Obtener la hora actual
    hora_actual <- as.numeric(format(hora_limite, "%H"))
    
    # Definir la ventana de servicio según el día
    if (dia_semana %in% 1:5) {  # Lunes a Viernes
      ventana <- ventana_lunes_viernes
    } else if (dia_semana == 6) {  # Sábado
      ventana <- ventana_sabado
    } else {  # Domingo (no es día laborable)
      # Saltar al siguiente día (lunes) a las 9:00
      hora_limite <- as.POSIXct(paste(format(hora_limite + 86400, "%Y-%m-%d"), "09:00:00"), tz = "America/Mexico_City")
      next
    }
    
    # Calcular las horas restantes en el día actual
    horas_restantes_dia <- ventana[2] - hora_actual
    
    # Si el tiempo restante es menor que las horas restantes en el día
    if (tiempo_restante_sla <= horas_restantes_dia * 3600) {
      hora_limite <- hora_limite + tiempo_restante_sla
      break
    } else {
      # Restar las horas restantes del día y avanzar al siguiente día
      tiempo_restante_sla <- tiempo_restante_sla - (horas_restantes_dia * 3600)
      hora_limite <- as.POSIXct(paste(format(hora_limite + 86400, "%Y-%m-%d"), "09:00:00"), tz = "America/Mexico_City")
    }
  }
  
  return(hora_limite)
}

# Filtrar tickets que están en estado "Nuevo" o "En proceso - Agente"
tickets_prioritarios <- datos_analysis %>%
  filter(estado_actual_ticket %in% c("Nuevo", "En proceso - Agente")) %>%
  mutate(
    tiempo_restante_sla = tiempo_objetivo - total_tiempo_procesador,
    hora_limite_sla = map2(tiempo_restante_sla, paste(Sys.Date(), "09:00:00"), ~ calcular_hora_limite(.x, .y))
  ) %>%
  filter(tiempo_restante_sla > 0) %>%  # Solo tickets que aún están dentro del SLA
  select(
    id_ticket,
    id_procesador,
    nombre_procesador,
    departamento_procesador,
    estado_actual_ticket,
    prioridad,  # Incluir el campo "prioridad"
    hora_limite_sla
  ) %>%
  arrange(hora_limite_sla)  # Ordenar por "hora_limite_sla" (de menor a mayor)

# Guardar el archivo en formato XLSX
xlsx_output_path <- paste0("C:/Users/racl26345/Documents/Reportes Automatizados/Tickets_Prioritarios_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx")
write.xlsx(tickets_prioritarios, xlsx_output_path, rowNames = FALSE)


# --------------------------------------------------------------
# G. CONSRUCCION Y ENVIO DEL CORREO
# --------------------------------------------------------------
# F.1. Texto de KPIs del Trimestre
kpi_text_trimestre <- paste0(
  "<b>KPIs del Trimestre:</b><br>",
  "• Tickets Creados: ", kpis_trimestre$total_tickets, "<br>",
  "--- Tickets por Resolver: ", kpis_trimestre$tickets_por_resolver, " (", kpis_trimestre$porc_por_resolver, "%)<br>",
  "--- Tickets Espera Confirmación: ", kpis_trimestre$tickets_espera_confirmacion, " (", kpis_trimestre$porc_espera_confirmacion, "%)<br>",
  "--- Tickets Cerrados: ", kpis_trimestre$tickets_cerrados, " (", kpis_trimestre$porc_cerrados, "%)<br>",
  "• % SLA General: ", kpis_trimestre$porc_sla_general, "%<br>",
  "• Tiempo Medio Resolución (Cerrados): ", kpis_trimestre$tiempo_medio_resolucion, " hrs<br><br>"
)

# F.2. Texto de Métricas de Referencia del Trimestre
metricas_text_trimestre <- paste0(
  "<b>Métricas de Referencia del Trimestre:</b><br>",
  "<b>[Consultas vs Requerimientos]</b><br>",
  "• <b>Consultas:</b><br>",
  "--- Total Consultas: ", metricas_trimestre$consultas, " (", metricas_trimestre$porc_consultas, "%)<br>",
  "▪▪▪▪▪ Por Resolver: ", metricas_trimestre$consultas_por_resolver, " (", metricas_trimestre$porc_consultas_por_resolver, "%)<br>",
  "▪▪▪▪▪ En Espera Confirmación: ", metricas_trimestre$consultas_espera_confirmacion, " (", metricas_trimestre$porc_consultas_espera_confirmacion, "%)<br>",
  "▪▪▪▪▪ Cerradas: ", metricas_trimestre$consultas_cerrados, " (", metricas_trimestre$porc_consultas_cerrados, "%)<br>",
  "--- % SLA (Consultas Cerradas): ", metricas_trimestre$sla_consultas, "%<br>",
  "--- Tiempo Medio Resolución: ", metricas_trimestre$tiempo_medio_consultas, " hrs<br>",
  "• <b>Requerimientos:</b><br>",
  "--- Total Requerimientos: ", metricas_trimestre$requerimientos, " (", metricas_trimestre$porc_requerimientos, "%)<br>",
  "▪▪▪▪▪ Por Resolver: ", metricas_trimestre$requerimientos_por_resolver, " (", metricas_trimestre$porc_requerimientos_por_resolver, "%)<br>",
  "▪▪▪▪▪ En Espera Confirmación: ", metricas_trimestre$requerimientos_espera_confirmacion, " (", metricas_trimestre$porc_requerimientos_espera_confirmacion, "%)<br>",
  "▪▪▪▪▪ Cerrados: ", metricas_trimestre$requerimientos_cerrados, " (", metricas_trimestre$porc_requerimientos_cerrados, "%)<br>",
  "--- % SLA (Requerimientos Cerrados): ", metricas_trimestre$sla_requerimientos, "%<br>",
  "--- Tiempo Medio Resolución: ", metricas_trimestre$tiempo_medio_requerimientos, " hrs<br>",
  "<b>[Rhadar vs Escalamientos]</b><br>",
  "• <b>Rhadar:</b><br>",
  "--- Total Rhadar: ", metricas_trimestre$rhadar, " (", metricas_trimestre$porc_rhadar, "%)<br>",
  "▪▪▪▪▪ Por Resolver: ", metricas_trimestre$rhadar_por_resolver, " (", metricas_trimestre$porc_rhadar_por_resolver, "%)<br>",
  "▪▪▪▪▪ En Espera Confirmación: ", metricas_trimestre$rhadar_espera_confirmacion, " (", metricas_trimestre$porc_rhadar_espera_confirmacion, "%)<br>",
  "▪▪▪▪▪ Cerrados: ", metricas_trimestre$rhadar_cerrados, " (", metricas_trimestre$porc_rhadar_cerrados, "%)<br>",
  "--- % SLA (Rhadar Cerrados): ", metricas_trimestre$sla_rhadar, "%<br>",
  "--- Tiempo Medio Resolución: ", metricas_trimestre$tiempo_medio_rhadar, " hrs<br>",
  "• <b>Escalamientos:</b><br>",
  "--- Total Escalamientos: ", metricas_trimestre$escalamientos, " (", metricas_trimestre$porc_escalamientos, "%)<br>",
  "▪▪▪▪▪ Por Resolver: ", metricas_trimestre$escalamientos_por_resolver, " (", metricas_trimestre$porc_escalamientos_por_resolver, "%)<br>",
  "▪▪▪▪▪ En Espera Confirmación: ", metricas_trimestre$escalamientos_espera_confirmacion, " (", metricas_trimestre$porc_escalamientos_espera_confirmacion, "%)<br>",
  "▪▪▪▪▪ Cerrados: ", metricas_trimestre$escalamientos_cerrados, " (", metricas_trimestre$porc_escalamientos_cerrados, "%)<br>",
  "--- % SLA (Escalamientos Cerrados): ", metricas_trimestre$sla_escalamientos, "%<br>",
  "--- Tiempo Medio Resolución: ", metricas_trimestre$tiempo_medio_escalamientos, " hrs<br><br>"
)

# F.3. Texto de Métricas de Referencia del Día Previo
metricas_text_dia_previo <- paste0(
  "<b>Métricas de Referencia del Día Previo:</b><br>",
  "• Tickets Creados: ", metricas_dia_previo$tickets_creados_dia_previo, "<br>",
  "--- Por Resolver: ", metricas_dia_previo$tickets_por_resolver_dia_previo, " (", metricas_dia_previo$porc_por_resolver_dia_previo, "%)<br>",
  "--- En Espera Confirmación: ", metricas_dia_previo$tickets_espera_confirmacion_dia_previo, " (", metricas_dia_previo$porc_espera_confirmacion_dia_previo, "%)<br>",
  "--- Tickets Cerrados: ", metricas_dia_previo$tickets_cerrados_dia_previo, " (", metricas_dia_previo$porc_cerrados_dia_previo, "%)<br>"
)

# G.4. Cuerpo del Correo
cuerpo_correo <- paste0(
  "<html><body style='font-family: Arial, sans-serif;'>",
  "<h2 style='color: #1f77b4;'>Reporte Trimestral Tickets C4C</h2>",
  "<h3>", format(Sys.Date(), "%d %B %Y"), "</h3>",
  kpi_text_trimestre,
  metricas_text_trimestre,
  metricas_text_dia_previo,
  "<h3>Análisis Detallado</h3>",
  "1. <b>Tendencia Cumplimiento SLA:</b><br>", add_image(path_tendencia_mejorada, width = 1000), "<br><br>",
  "2. <b>Eficiencia por Etapas:</b><br>", add_image(paths$etapas, width = 650), "<br><br>",
  "3. <b>Áreas Críticas:</b><br>", add_image(path_grafico_jerarquico, width = 650), "<br><br>",
  "4. <b>Distribución de Tiempos:</b><br>", add_image(paths$distribucion, width = 800), "<br><br>",
  "5. <b>Desempeño por Nivel:</b><br>", add_image(paths$nivel, width = 650), "<br><br>",
  "6. <b>Tiempos por Nivel de Gestión y Área de Personal:</b><br>", add_image(paths$area, width = 800), "<br><br>",
  "7. <b>Tipos de Tickets y Niveles de Atención | Volumenes:</b><br>", add_image(paths$vol_tipo, width = 650), "<br><br>",
  "8. <b>Tipos de Tickets y Niveles de Atención | Tiempos:</b><br>", add_image(paths$time_tipo, width = 650), "<br><br>",
  "9. <b>Tiempos por Departamento:</b><br>", add_image(paths$departamento, width = 1000), "<br><br>",
  "10. <b>Tiempos por Procesador:</b><br>", add_image(paths$procesador, width = 1000), "<br><br>",
  "11. <b>Volumen de Tickets por Plaza (Agrupados por Regional):</b><br>", add_image(paths$plazas_regional, width = 1000), "<br><br>",
  "<p style='color: #666;'>* Datos correspondientes al mes en curso<br>",
  "** SLA calculado sobre tickets cerrados<br>",
  "*** SLA solo considera tiempos de atención agentes<br>",
  "**** Estimación de Tiempos en Horas Laborales</p>",
  "</body></html>"
)

# G.5. Crear y enviar correo
email <- compose_email(
  body = md(cuerpo_correo),
  footer = md("Reporte Automatizado - Analítica")
) %>% 
  add_attachment(output_path) %>%
  add_attachment(xlsx_output_path)

# G.6. Configurar las credenciales de Microsoft 365
creds <- creds(
  user = "racl26345@cpm.coop",  # Tu correo institucional
  host = "smtp.office365.com",  # Servidor SMTP de Microsoft 365
  port = 587,                   # Puerto SMTP para Microsoft 365
  use_ssl = TRUE                # Usar SSL
)

# G.7. Enviar el correo electrónico
smtp_send(
  email,
  from = "luis_ramirezC@cpm.coop",  # Tu correo institucional
  to = c("gerardo_nahum@cpm.coop", "bibiana_rico@cpm.coop","miriam_ramireza@cpm.coop", 
         "alejandra_tavares@cpm.coop", "ana_banuelos@cpm.coop"),  # Correos de los destinatarios
  cc = "luis_ramirezC@cpm.coop",  # Correos de destinatarios-copias
  subject = paste("Reporte Trimestral SLA C4C -", paste(fecha_anterior, collapse = " y ")),
  credentials = creds
)

# --------------------------------------------------------------
# H. CIERRE DE PROCESO
# --------------------------------------------------------------

# H.1. Mensaje de confirmación
cat("El correo electrónico ha sido enviado.\n")

# H.2. Limpiar archivos temporales
file.remove(unlist(paths))
file.remove(xlsx_output_path) 