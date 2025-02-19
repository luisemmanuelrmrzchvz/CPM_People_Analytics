################# REPORTE SVL C4C ############################

# Cargar las librerías necesarias
library(DBI)
library(RSQLite)
library(openxlsx)
library(blastula)  # Para enviar correos electrónicos
library(lubridate) # Para manejar fechas

# Conectar a la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(SQLite(), db_path)

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
    WHERE STRFTIME('%Y-%m', codigo_tickets.fecha_creado) = STRFTIME('%Y-%m', 'now')
    AND catalog_tickets.tipo_atencion = 'Ticket Válido'
    AND codigo_tickets.agente_servicio IS NOT NULL
),

status_nuevos AS (
    SELECT 
        hist_status_tickets.id_ticket,
        SUM(hist_status_tickets.seg_duracion) AS timing
    FROM hist_status_tickets
    WHERE hist_status_tickets.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets.code_estado_ticket = 1 -- Nuevo (1)
    GROUP BY 1
),

status_proceso_agente AS (
    SELECT 
        hist_status_tickets.id_ticket,
        SUM(hist_status_tickets.seg_duracion) AS timing
    FROM hist_status_tickets
    WHERE hist_status_tickets.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets.code_estado_ticket = 2 -- En proceso - Agente (2)
    GROUP BY 1
),

status_proceso_cliente AS (
    SELECT 
        hist_status_tickets.id_ticket,
        SUM(hist_status_tickets.seg_duracion) AS timing
    FROM hist_status_tickets
    WHERE hist_status_tickets.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets.code_estado_ticket = 4 -- En proceso - Cliente (4)
    GROUP BY 1
),

status_propuesta_solucion AS (
    SELECT 
        hist_status_tickets.id_ticket,
        SUM(hist_status_tickets.seg_duracion) AS timing
    FROM hist_status_tickets
    WHERE hist_status_tickets.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets.code_estado_ticket = 5 -- Solución propuesta (5)
    GROUP BY 1
),

status_solucion_confirmada AS (
    SELECT 
        hist_status_tickets.id_ticket,
        SUM(hist_status_tickets.seg_duracion) AS timing
    FROM hist_status_tickets
    WHERE hist_status_tickets.id_ticket IN (SELECT id_ticket FROM tickets_mes_actual)
        AND hist_status_tickets.code_estado_ticket = 'Z6' -- Solución Confirmada (Z6)
    GROUP BY 1
),

status_cerrados AS (
SELECT 
    hist_status_tickets.id_ticket,
    SUM(hist_status_tickets.seg_duracion) AS timing
FROM hist_status_tickets
WHERE hist_status_tickets.id_ticket IN (SELECT tickets_mes_actual.id_ticket FROM tickets_mes_actual)
    AND hist_status_tickets.code_estado_ticket = 6 --Cerrado (6)
GROUP BY 1
ORDER BY 1
)

SELECT
    tm.id_ticket,
    tm.fecha_creado,
    tm.id_catalog,
    tm.fecha_interaccion,
    tm.prioridad,
    tm.tipo_ticket,
    tm.nivel_atencion,
    tm.categoria,
    tm.subcategoria,
    tm.agente_servicio,
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
    tm.tiempo_objetivo,
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
;
"

# Ejecutar el query y obtener los datos
datos <- dbGetQuery(conn, query)

# Desconectar de la base de datos
dbDisconnect(conn)

# Obtener la fecha del día anterior
fecha_anterior <- format(Sys.Date() - 1, "%Y-%m-%d")

# Definir la ruta y el nombre del archivo de Excel con la fecha del día anterior
output_path <- paste0("C:/Users/racl26345/Documents/Reportes Automatizados/Monthly SVL C4C ", fecha_anterior, ".xlsx")

# Guardar los datos en un archivo de Excel
write.xlsx(datos, output_path, rowNames = FALSE)

# Mensaje de confirmación
cat("El reporte ha sido guardado en:", output_path, "\n")

# --------------------------
# Generación de gráficos
# --------------------------

# 1. Volúmenes de tickets creados por día del mes en curso
datos$fecha_creado <- as.Date(datos$fecha_creado)
tickets_por_dia <- datos %>%
  group_by(fecha_creado) %>%
  summarise(volumen = n()) %>%
  mutate(dia_semana = weekdays(fecha_creado),
         tipo_dia = ifelse(dia_semana %in% c("sábado", "domingo"), "Fin de Semana", "Lunes a Viernes"))

grafico_tickets_dia <- ggplot(tickets_por_dia, aes(x = fecha_creado, y = volumen, fill = tipo_dia)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = volumen), vjust = -0.5, color = "black", size = 3) +  # Número de tickets encima de cada barra
  labs(title = "Tickets Creados por Día",
       x = "Fecha",
       y = "Volumen de Tickets",
       fill = "Tipo de Día") +
  scale_fill_manual(values = c("Lunes a Viernes" = "steelblue", "Fin de Semana" = "orange")) +
  theme_minimal()

# Guardar el gráfico en un archivo temporal
grafico_tickets_dia_path <- tempfile(fileext = ".png")
ggsave(grafico_tickets_dia_path, grafico_tickets_dia, width = 8, height = 4)

# 2. Volúmenes de tickets por status (ordenados según el flujo)
orden_estados <- c("Nuevo", "En proceso - Agente", "En proceso - Acción del cliente", 
                   "Completado - Solución propuesta", "Solución Confirmada", "Cerrado")

tickets_por_status <- datos %>%
  group_by(estado_actual_ticket) %>%
  summarise(volumen = n()) %>%
  mutate(estado_actual_ticket = factor(estado_actual_ticket, levels = orden_estados))

grafico_tickets_status <- ggplot(tickets_por_status, aes(x = estado_actual_ticket, y = volumen)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = volumen), vjust = -0.5, color = "black", size = 3) +  # Número de tickets encima de cada barra
  labs(title = "Tickets por Estado Actual",
       x = "Estado",
       y = "Volumen de Tickets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Guardar el gráfico en un archivo temporal
grafico_tickets_status_path <- tempfile(fileext = ".png")
ggsave(grafico_tickets_status_path, grafico_tickets_status, width = 8, height = 4)

# 3. % de tickets dentro de nivel de servicio por prioridad (solo tickets cerrados)
tickets_cerrados <- datos %>%
  filter(estado_actual_ticket == "Cerrado")

tickets_cerrados_svl <- tickets_cerrados %>%
  group_by(prioridad, cumple_svl) %>%
  summarise(volumen = n()) %>%
  mutate(porcentaje = volumen / sum(volumen) * 100)

grafico_svl_prioridad <- ggplot(tickets_cerrados_svl, aes(x = prioridad, y = porcentaje, fill = cumple_svl)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(porcentaje, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +  # Etiquetas de porcentaje
  labs(title = "% de Tickets Dentro de Nivel de Servicio por Prioridad (Cerrados)",
       x = "Prioridad",
       y = "Porcentaje",
       fill = "Cumple SVL") +
  theme_minimal()

# Guardar el gráfico en un archivo temporal
grafico_svl_prioridad_path <- tempfile(fileext = ".png")
ggsave(grafico_svl_prioridad_path, grafico_svl_prioridad, width = 8, height = 4)

# 4. Volumen de tickets no cerrados dentro y fuera de nivel de servicio
tickets_no_cerrados <- datos %>%
  filter(estado_actual_ticket != "Cerrado")

tickets_no_cerrados_svl <- tickets_no_cerrados %>%
  group_by(cumple_svl) %>%
  summarise(volumen = n()) %>%
  mutate(porcentaje = volumen / sum(volumen) * 100)

grafico_no_cerrados_svl <- ggplot(tickets_no_cerrados_svl, aes(x = "", y = volumen, fill = cumple_svl)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(volumen, " (", round(porcentaje, 1), "%)")), 
            position = position_stack(vjust = 0.5), size = 4) +  # Etiquetas de volumen y porcentaje
  labs(title = "Tickets No Cerrados Dentro/Fuera de Nivel de Servicio",
       x = "",
       y = "",
       fill = "Cumple SVL") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Guardar el gráfico en un archivo temporal
grafico_no_cerrados_svl_path <- tempfile(fileext = ".png")
ggsave(grafico_no_cerrados_svl_path, grafico_no_cerrados_svl, width = 6, height = 4)

# --------------------------
# Configurar el correo electrónico
# --------------------------

# Crear el cuerpo del correo con los gráficos
cuerpo_correo <- paste0(
  "Hola,<br><br>",
  "Adjunto encontrarás el reporte Monthly SVL C4C correspondiente al ", fecha_anterior, ".<br><br>",
  "A continuación, se presentan algunos gráficos relevantes:<br><br>",
  "1. <b>Tickets Creados por Día:</b><br>",
  add_image(grafico_tickets_dia_path, width = 600), "<br><br>",
  "2. <b>Tickets por Estado Actual:</b><br>",
  add_image(grafico_tickets_status_path, width = 600), "<br><br>",
  "3. <b>% de Tickets Dentro de Nivel de Servicio por Prioridad (Cerrados):</b><br>",
  add_image(grafico_svl_prioridad_path, width = 600), "<br><br>",
  "4. <b>Tickets No Cerrados Dentro/Fuera de Nivel de Servicio:</b><br>",
  add_image(grafico_no_cerrados_svl_path, width = 600), "<br><br>",
  "Saludos,<br>",
  "Proceso Automatizado SSCC"
)

# Crear el correo electrónico
email <- compose_email(
  body = md(cuerpo_correo),
  footer = md("Este es un correo generado automáticamente.")
)

# Adjuntar el archivo de Excel
email <- add_attachment(email, output_path)

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
  subject = paste("Reporte Monthly SVL C4C -", fecha_anterior),
  credentials = creds
)

# Mensaje de confirmación
cat("El correo electrónico ha sido enviado.\n")