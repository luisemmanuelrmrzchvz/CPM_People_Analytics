# Cargar librerías necesarias
library(readxl)
library(DBI)
library(RSQLite)

# Ruta del archivo de entrada y base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
archivo_excel <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Hist_Movimientos.xlsx"

# Leer el archivo XLSX ignorando la primera fila (títulos de columnas)
datos <- read_excel(archivo_excel, skip = 1, col_names = FALSE)

# Definir las columnas a formatear como fechas (5, 6, 14, 18, 19, 20, 28, 64)
columnas_fecha <- c(5, 6, 14, 18, 19, 20, 28, 64)

# Convertir las columnas seleccionadas a formato de fecha "YYYY-MM-DD"
for (col in columnas_fecha) {
  datos[[col]] <- format(as.Date(datos[[col]], origin = "1899-12-30"), "%Y-%m-%d")
}

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Definir las columnas de la tabla de base de datos
columnas_db <- c(
  "id_colaborador", "nombre", "evento_asociado", "razon_evento", "fecha_efectiva_movimiento",
  "fecha_vencimiento_contrato", "puesto_anterior", "trans_sequence_mov_dia", "movimiento_creado_por",
  "evento_asociado_etiqueta", "area_personal", "dias_laborales_por_semana", "familia_puestos",
  "fecha_entrada_posicion", "nivel_escala_remuneracion", "etiqueta_plan_horario", "tabulador_salarial",
  "fecha_original_contratacion", "fecha_baja", "fecha_ultimo_dia_laborado", "causa_baja", "motivo_baja",
  "detalle_baja", "id_posicion", "posicion_tipo", "posicion_regional", "posicion_plaza",
  "posicion_fecha_vacante", "posicion_comunidad_estrategia", "posicion_motivo", "posicion_motivo_especifico",
  "posiciondepartamento", "posicion_centro_costos", "posicion_estado", "posicion_localidad", "posicion_division",
  "posicion_municipio", "posicion_ubicacion", "posicion_puesto", "id_centro_costos", "nivel_gestion",
  "nivel_1", "nivel_2", "nivel_3", "tipo_contrato", "nombre_puesto", "tipo_reclutamiento", "area_cobranza",
  "puesto_generico", "area_especialidad_deseada", "clasificacion_riesgo", "clasificacion_liderazgo",
  "disponibilidad_viajar", "escolaridad_deseada", "grupo_personal", "limitante", "modalidad_puesto",
  "perfil_profesional", "pruebas_psicometricas", "segmento_puesto", "genero", "estado_civil", "estado_nacimiento",
  "fecha_nacimiento", "lengua_nativa", "limitante_fisica", "nacionalidad", "pais_nacimiento",
  "segunda_nacionalidad"
)

# Renombrar las columnas de los datos para que coincidan con la tabla de SQLite
colnames(datos) <- columnas_db

# Insertar los datos en la tabla hist_movimientos
dbWriteTable(conn, "hist_movimientos", datos, append = TRUE, row.names = FALSE)

# Cerrar la conexión
dbDisconnect(conn)

print("Datos insertados en la tabla hist_movimientos correctamente.")
