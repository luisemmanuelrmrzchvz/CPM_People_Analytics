# Cargar librerías necesarias
library(DBI)
library(RSQLite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(DT)

# --- PASO 1: Extraer y procesar los datos desde SQLite ---

# Ruta de la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Extraer los datos necesarios de la tabla hist_posiciones
hist_posiciones <- dbGetQuery(conn, "
    SELECT 
        id_posicion, 
        id_colaborador, 
        status, 
        area_de_cobranza, 
        nivel_gestion, 
        vacante, 
        fecha_daily, 
        fecha_inicio
    FROM hist_posiciones
    WHERE DATE(fecha_daily) BETWEEN '2025-01-01' AND '2025-01-24'
")

# Cerrar la conexión
dbDisconnect(conn)

# Convertir fechas a formato Date
hist_posiciones <- hist_posiciones %>%
  mutate(
    fecha_daily = as.Date(fecha_daily),
    fecha_inicio = as.Date(fecha_inicio)
  )

# Crear una secuencia de fechas para el mes de enero
fechas <- seq(as.Date("2025-01-01"), as.Date("2025-01-24"), by = "day")

# Función para comparar dos días consecutivos
comparar_dias <- function(fecha_actual, fecha_anterior, datos) {
  # Filtrar datos para el día actual y el día anterior
  hoy <- datos %>% filter(fecha_daily == fecha_actual)
  ayer <- datos %>% filter(fecha_daily == fecha_anterior)
  
  # Identificar posiciones inactivas
  down_positions <- datos %>%
    filter(status == 'I' & fecha_inicio <= fecha_actual) %>%
    pull(id_posicion) %>%
    unique()
  
  # Filtrar posiciones activas
  hoy <- hoy %>% filter(!id_posicion %in% down_positions)
  ayer <- ayer %>% filter(!id_posicion %in% down_positions)
  
  # Realizar las comparaciones
  comparacion <- hoy %>%
    left_join(ayer, by = "id_posicion", suffix = c("_hoy", "_ayer")) %>%
    mutate(
      nivel_gestion = case_when(
        area_de_cobranza_hoy == 'Cobranza administrativa' ~ 'COBRANZA',
        area_de_cobranza_hoy == 'Cobranza en campo' ~ 'COBRANZA',
        TRUE ~ nivel_gestion_hoy
      ),
      Cambios = case_when(
        status_hoy == 'I' & status_ayer == 'A' ~ 'Posicion Inactivada',
        is.na(status_ayer) & status_hoy == 'A' ~ 'Posicion Creada',
        status_hoy == 'A' & is.na(id_colaborador_hoy) & !is.na(id_colaborador_ayer) ~ 'Posicion Vacante',
        status_hoy == 'A' & !is.na(id_colaborador_hoy) & is.na(id_colaborador_ayer) ~ 'Posicion Cubierta',
        status_hoy == 'I' ~ 'Sin Cambios - Posiciones Inactivas',
        vacante_hoy == 'True' ~ 'Sin Cambios - Posicion Activa Vacante',
        TRUE ~ 'Sin Cambios - Posicion Activa Ocupada'
      )
    ) %>%
    select(fecha = fecha_daily_hoy, id_posicion, nivel_gestion, Cambios)
  
  return(comparacion)
}

# Aplicar la función a todas las fechas
resultados <- lapply(fechas[-1], function(fecha) {
  comparar_dias(fecha, fecha - 1, hist_posiciones)
}) %>% bind_rows()

# Resumir los resultados por fecha, nivel_gestion y Cambios
resumen <- resultados %>%
  group_by(fecha, nivel_gestion, Cambios) %>%
  summarise(Total_Posiciones = n(), .groups = 'drop')

# --- PASO 2: Crear el Dashboard Interactivo con Shiny ---

# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Evolución Diaria de Posiciones en la Compañía"),
  
  sidebarLayout(
    sidebarPanel(
      # Filtro por Nivel de Gestión
      selectInput("nivel_gestion", "Seleccionar Nivel de Gestión:",
                  choices = unique(resumen$nivel_gestion),
                  multiple = TRUE,
                  selected = unique(resumen$nivel_gestion)),
      
      # Filtro por Cambios
      selectInput("cambios", "Seleccionar Cambios:",
                  choices = unique(resumen$Cambios),
                  multiple = TRUE,
                  selected = unique(resumen$Cambios)),
      
      # Botón para actualizar los filtros
      actionButton("actualizar", "Aplicar Filtros")
    ),
    
    mainPanel(
      # Gráfico de evolución diaria
      plotOutput("grafico"),
      
      # Tabla dinámica
      DTOutput("tabla")
    )
  )
)

# Definir el servidor (Server)
server <- function(input, output) {
  # Filtrar los datos según los filtros seleccionados
  datos_filtrados <- reactive({
    resumen %>%
      filter(nivel_gestion %in% input$nivel_gestion &
               Cambios %in% input$cambios)
  })
  
  # Gráfico de evolución diaria
  output$grafico <- renderPlot({
    datos_filtrados() %>%
      ggplot(aes(x = fecha, y = Total_Posiciones, color = Cambios, group = Cambios)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      facet_wrap(~ nivel_gestion, scales = "free_y") +
      labs(title = "Evolución Diaria de Posiciones",
           x = "Fecha",
           y = "Número de Posiciones",
           color = "Cambios") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Tabla dinámica
  output$tabla <- renderDT({
    datos_filtrados() %>%
      pivot_wider(names_from = fecha, values_from = Total_Posiciones, values_fill = list(Total_Posiciones = 0)) %>%
      datatable(rownames = FALSE,
                extensions = 'Buttons',
                options = list(
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  pageLength = 10
                ))
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)