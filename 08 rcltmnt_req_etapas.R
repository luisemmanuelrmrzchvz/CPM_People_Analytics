# Cargar librerías necesarias
library(readxl)
library(DBI)
library(RSQLite)

# Ruta del archivo de entrada y base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
archivo_excel <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Requisiciones Etapas.xlsx"

# Leer el archivo XLSX ignorando la primera fila (títulos de columnas)
datos <- read_excel(archivo_excel, skip = 1, col_names = FALSE)

# Verificar la estructura de los datos
print("Estructura de datos leídos:")
print(dim(datos))
print(head(datos))

# Definir las columnas que necesitan formato de fecha según tu estructura
# Columnas en el Excel: 
# 11 = Fecha de creación, 16 = Fecha de aprobación, 17 = Fecha de cierre
columnas_fecha_excel <- c(11, 16, 17)

# Función para convertir fechas con diferentes formatos
convertir_fecha <- function(x) {
  # Si es NA, retornar NA
  if (is.na(x)) return(NA)
  
  # Intentar diferentes formatos de fecha
  formatos <- c("%d/%m/%Y", "%d/%m/%Y %I:%M:%S %p", "%Y-%m-%d")
  
  for (fmt in formatos) {
    fecha_convertida <- as.Date(x, format = fmt)
    if (!is.na(fecha_convertida)) {
      return(format(fecha_convertida, "%Y-%m-%d"))
    }
  }
  return(NA) # Si no se puede convertir, retornar NA
}

# Convertir las columnas seleccionadas a formato de fecha "YYYY-MM-DD"
for (col in columnas_fecha_excel) {
  datos[[col]] <- sapply(datos[[col]], convertir_fecha)
}

# SELECCIÓN CORRECTA DE COLUMNAS - EXCLUYENDO LAS QUE NO NECESITAS
# Columnas que SÍ necesitas del Excel (basado en tu CREATE TABLE):
# 1: ID de requisición, 2: Nombre del puesto, 3: Nombre(s), 4: Apellido materno,
# 5: Es interno, 6: Categoría de estado, 7: Estado de solicitud, 8: Es estado actual,
# 11: Fecha de creación, 12: Estado omitido, 13: Progreso del candidato,
# 14: Nombre del Ejecutivo, 15: Apellidos del Ejecutivo,
# 16: Fecha de aprobación, 17: Fecha de cierre, 18: ID de candidato, 19: ID de empleado

columnas_necesarias <- c(1, 2, 3, 4, 5, 6, 7, 8, 11, 12, 13, 14, 15, 16, 17, 18, 19)

# Filtrar solo las columnas que necesitas (excluyendo columnas 9 y 10)
datos <- datos[, columnas_necesarias]

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Verificar si existen registros en la tabla
registros_existentes <- dbGetQuery(conn, "SELECT COUNT(*) FROM rcltmnt_req_etapas")$`COUNT(*)`

# Si existen registros, eliminarlos
if (registros_existentes > 0) {
  dbExecute(conn, "DELETE FROM rcltmnt_req_etapas")
}

# Definir las columnas de la tabla de base de datos (sin id_key que es AUTOINCREMENT)
columnas_db <- c("id_requisicion", "puesto_nombre", "nombre_candidato", "apellido_candidato", "interno", 
                 "categoria_estado_requisicion", "estado_requisicion", "es_ultimo_estado", "fecha_creacion", "estado_omitido", "progreso_candidato", 
                 "nombre_ejecutivo_rcltmnt", "apellidos_ejecutivo_rcltmnt", "fecha_aprobacion", "fecha_cierre", 
                 "id_candidato", "id_colaborador")

# Renombrar las columnas de los datos para que coincidan con la tabla de SQLite
colnames(datos) <- columnas_db

# Insertar los datos en la tabla rcltmnt_req_etapas
dbWriteTable(conn, "rcltmnt_req_etapas", datos, append = TRUE, row.names = FALSE)

# Cerrar la conexión
dbDisconnect(conn)

print("Datos insertados en la base de datos correctamente.")