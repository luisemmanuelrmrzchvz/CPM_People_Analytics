# Cargar librerías necesarias
library(readxl)
library(DBI)
library(RSQLite)

# Ruta del archivo de entrada y base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
archivo_excel <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Datos_Colaboradores.xlsx"

# Leer el archivo XLSX ignorando la primera fila (títulos de columnas)
datos <- read_excel(archivo_excel, skip = 1, col_names = FALSE)

# Definir las columnas a formatear como fechas (14, 15, 16, 19)
columnas_fecha <- c(14, 15, 16, 19)

# Convertir las columnas seleccionadas a formato de fecha "YYYY-MM-DD"
for (col in columnas_fecha) {
  datos[[col]] <- format(as.Date(datos[[col]], origin = "1899-12-30"), "%Y-%m-%d")
}

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Verificar si existen registros en la tabla
registros_existentes <- dbGetQuery(conn, "SELECT COUNT(*) FROM datos_colaboradores")$`COUNT(*)`

# Si existen registros, eliminarlos
if (registros_existentes > 0) {
  dbExecute(conn, "DELETE FROM datos_colaboradores")
}

# Definir las columnas de la tabla de base de datos
columnas_db <- c("id_colaborador", "nombre_completo", "puesto", "id_centro_costos", "centro_costos", 
                 "id_departamento", "departamento", "id_ubicacion", "ubicacion", "plaza", "regional", 
                 "regional_hr", "status_colaborador", "fecha_entrada_puesto", "fecha_baja", 
                 "fecha_ultimo_dia_laborado", "causa_baja", "motivo_baja", "fecha_nacimiento", 
                 "estado_civil", "genero", "area_personal", "area_cobranza", "tipo_reclutamiento", 
                 "tipo_contrato", "uniforme", "nivel_gestion")

# Renombrar las columnas de los datos para que coincidan con la tabla de SQLite
colnames(datos) <- columnas_db

# Insertar los datos en la tabla datos_colaboradores
dbWriteTable(conn, "datos_colaboradores", datos, append = TRUE, row.names = FALSE)

# Cerrar la conexión
dbDisconnect(conn)

print("Datos insertados en la base de datos correctamente.")
