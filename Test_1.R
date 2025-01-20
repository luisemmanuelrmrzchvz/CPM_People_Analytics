# Cargar las librerías necesarias
library(DBI)
library(RSQLite)
library(readr)

# Leer el archivo CSV
file_path <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/db_incidencias.csv"
df <- read_csv(file_path)

# Convertir las fechas en formato serial de Excel a formato Date en R
#df$start_date <- as.Date(df$start_date, origin = "1899-12-30")  # Convertir start_date
#df$end_date <- as.Date(df$end_date, origin = "1899-12-30")      # Convertir end_date

# Verificar las primeras filas de las fechas convertidas
#head(df$start_date)

# Conectarse a la base de datos SQLite (si no existe, se creará)
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(RSQLite::SQLite(), db_path)

# Crear la tabla en SQLite (si no existe)
# Ajusta los tipos de datos según los que hayas definido
dbExecute(conn, "
CREATE TABLE IF NOT EXISTS incidencias (
  id_colaborador INTEGER,
  start_date TEXT,
  end_date TEXT,
  time_type TEXT,
  quantity_days INTEGER,
  folio TEXT,
  last_modified_datetime TEXT
);
")

# Importar los datos convertidos a la base de datos
dbWriteTable(conn, "incidencias", df, append = TRUE, row.names = FALSE)

# Verificar si los datos se han cargado correctamente
query_result <- dbGetQuery(conn, "SELECT * FROM incidencias LIMIT 10;")
print(query_result)

# Cerrar la conexión
dbDisconnect(conn)