> # Cargar librerías necesarias
  > library(DBI)
> library(RSQLite)
> library(dplyr)
> library(openxlsx)
> 
  > # Paso 1: Conectar a la base de datos SQLite y extraer datos en bruto
  > db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
  > 
    > # Conectar a la base de datos
    > conn <- dbConnect(SQLite(), db_path)
    > 
      > # Query para extraer los datos de enero 2025
      > query <- "
+ SELECT 
+     id_posicion, 
+     id_colaborador, 
+     status, 
+     area_de_cobranza, 
+     nivel_gestion, 
+     vacante, 
+     fecha_daily
+ FROM hist_posiciones
+ WHERE fecha_daily BETWEEN '2024-12-31' AND '2025-01-31';
+ "
      > 
        > # Obtener los datos
        > data <- dbGetQuery(conn, query)
        > 
          > # Cerrar conexión a la base de datos
          > dbDisconnect(conn)
        > 
          > # Paso 2: Procesar los datos en R
          > # Convertir la columna fecha_daily a formato Date
          > data <- data %>%
            +   mutate(fecha_daily = as.Date(fecha_daily))
          > 
            > # Identificar todas las posiciones que existían ayer y hoy
            > data_yesterday <- data %>%
              +   filter(fecha_daily == as.Date("2024-12-31")) %>%
              +   select(id_posicion, id_colaborador) %>%
              +   mutate(existia_ayer = TRUE)
            > 
              > data_today <- data %>%
                +   filter(fecha_daily == as.Date("2025-01-01"))
              > 
                > # Unir para verificar cambios entre ayer y hoy
                > daily_comparison <- data_today %>%
                  +   left_join(data_yesterday, by = "id_posicion")
                > 
                  > # Definir cambios de estado, incluyendo nuevos escenarios
                  > status <- daily_comparison %>%
                    +   mutate(
                      +     Cambios = case_when(
                        +       !existia_ayer & is.na(id_colaborador) ~ 'Nueva Posicion Vacante',
                        +       !existia_ayer & !is.na(id_colaborador) ~ 'Nueva Posicion Ocupada',
                        +       status == 'I' & existia_ayer & is.na(id_colaborador.y) ~ 'Posicion Inactivada Vacante',
                        +       status == 'I' & existia_ayer & !is.na(id_colaborador.y) ~ 'Posicion Inactivada Ocupada',
                        +       is.na(id_colaborador.y) & !is.na(id_colaborador.x) ~ 'Posicion Cubierta',
                        +       !is.na(id_colaborador.y) & is.na(id_colaborador.x) ~ 'Posicion Vacante',
                        +       status == "I" ~ 'Sin Cambios - Posiciones Inactivas',
                        +       vacante == "True" ~ 'Sin Cambios - Posicion Activa Vacante',
                        +       TRUE ~ 'Sin Cambios - Posicion Activa Ocupada'
                        +     )
                      +   ) %>%
                    +   group_by(fecha_daily, nivel_gestion, Cambios) %>%
                    +   summarise(Total_Posiciones = n(), .groups = "drop") %>%
                    +   rename(fecha = fecha_daily)
                  Error in `mutate()`:
                    ℹ In argument: `Cambios = case_when(...)`.
                  Caused by error in `case_when()`:
                    ! Failed to evaluate the left-hand side of formula 1.
                  Caused by error:
                    ! objeto 'id_colaborador' no encontrado
                  Run `rlang::last_trace()` to see where the error occurred.
                  > rlang::last_trace()
                  <error/dplyr:::mutate_error>
                    Error in `mutate()`:
                    ℹ In argument: `Cambios = case_when(...)`.
                  Caused by error in `case_when()`:
                    ! Failed to evaluate the left-hand side of formula 1.
                  Caused by error:
                    ! objeto 'id_colaborador' no encontrado
                  ---
                    Backtrace:
                    ▆
                  1. ├─... %>% rename(fecha = fecha_daily)
                  2. ├─dplyr::rename(., fecha = fecha_daily)
                  3. ├─dplyr::summarise(., Total_Posiciones = n(), .groups = "drop")
                  4. ├─dplyr::group_by(., fecha_daily, nivel_gestion, Cambios)
                  5. ├─dplyr::mutate(...)
                  6. ├─dplyr:::mutate.data.frame(...)
                  7. │ └─dplyr:::mutate_cols(.data, dplyr_quosures(...), by)
                  8. │   ├─base::withCallingHandlers(...)
                  9. │   └─dplyr:::mutate_col(dots[[i]], data, mask, new_columns)
                  10. │     └─mask$eval_all_mutate(quo)
                  11. │       └─dplyr (local) eval()
                  12. └─dplyr::case_when(...)
                  13.   └─dplyr:::case_formula_evaluate(...)
                  14.     ├─base::withCallingHandlers(...)
                  15.     └─rlang::eval_tidy(pair$lhs, env = default_env)