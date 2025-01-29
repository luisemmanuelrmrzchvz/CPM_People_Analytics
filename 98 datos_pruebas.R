tengo el siguiente script en R, para alimentar una tabla en SQLite, aunque necesito ajustar el script de R, para antes de insertar los datos del archivo, valide si el registro de mi columna 1 de mi archivo, ya existe en la tabla de mi base de SQLite, si es el caso necesito que sustituya el campos de mi registro de la tabla, por los nuevos campos de mi registro en el arcchivo de xlsx, adicionalmente me imprima en la consola de R, los registros que identificó que ya existían, y por cuál registro los sustituyo, a modo de visualización del ajuste en la tabla; la tabla que tengo en SQLite la cree como: "CREATE TABLE incidencias (
    id_key INTEGER PRIMARY KEY AUTOINCREMENT,
    id_incidencia TEXT,
    creada_por INTEGER,
    fecha_creacion DATE,
    id_workflow_request INTEGER,
    categoria_ausentismo TEXT,
    tipo_ausencia TEXT,
    motivo_permiso TEXT,
    tramite_personal TEXT,
    dias_ausencia INTEGER,
    fecha_inicio DATE,
    fecha_fin DATE,
    id_colaborador INTEGER,
    folio_imss TEXT,
    ultima_modificacion_por INTEGER,
    fecha_ultima_modificacion DATE,
    status_incidencia TEXT
);"; y el script que necesito ajustar de R es: "# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)

# Ruta del archivo de entrada y base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
archivo_excel <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/Incidencias.xlsx"

# Leer el archivo XLSX ignorando la primera fila (títulos de columnas)
datos <- read_excel(archivo_excel, skip = 1, col_names = FALSE)

# Definir las columnas a formatear como fechas (3, 10, 11, 15)
columnas_fecha <- c(3, 10, 11, 15)

# Convertir las columnas seleccionadas a formato de fecha "YYYY-MM-DD"
for (col in columnas_fecha) {
  datos[[col]] <- format(as.Date(datos[[col]], origin = "1899-12-30"), "%Y-%m-%d")
}

# Conectar a la base de datos SQLite
conn <- dbConnect(SQLite(), db_path)

# Obtener los nombres de las columnas de la base de datos (sin incluir id_key)
columnas_db <- dbListFields(conn, "incidencias")
columnas_db <- columnas_db[columnas_db != "id_key"]

# Renombrar las columnas para coincidir con la base de datos
colnames(datos) <- columnas_db

# Insertar los datos en la tabla incidencias
dbWriteTable(conn, "incidencias", datos, append = TRUE, row.names = FALSE)

# Cerrar la conexión
dbDisconnect(conn)

print("Datos insertados en la base de datos correctamente.")
"; cabe hacer mención, que las columnas de DATE, necesito que las lea y guarde como "YYYY-MM-DD", dado que es una tabla de SQLite, y hago la conversión en R, dado que mi archivo de excel en las columnas 3 y 15 tienen formato "DD/MM/YYYY hh:mm:ss a.m." y las 10 y 11 tienen formato "DD/MM/YYYY", por eso hago las conversiones de fechas