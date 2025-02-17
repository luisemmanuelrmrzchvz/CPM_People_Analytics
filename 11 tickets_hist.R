# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)

# Definir la ruta del archivo de Excel
ruta_archivo <- "C:/Users/racl26345/Documents/Reportes Automatizados/Inputs/CPM_10_Historico_Estados.xlsx"

# Leer el archivo de Excel, omitiendo las primeras 10 filas (la fila 10 contiene los títulos)
datos <- read_excel(ruta_archivo, range = cell_rows(11:100000), col_names = FALSE)

# Verificar la estructura de los datos leídos
print("Estructura de los datos leídos:")
print(head(datos))  # Mostrar las primeras filas de los datos
print(ncol(datos))  # Mostrar el número de columnas

# Asignar nuevos nombres a las columnas seleccionadas
nombres_columnas <- c("id_ticket", "fecha_creado", "estado_ticket", "agente_servicio", "fecha_interaccion", "hora_interaccion", "id_catalog")

# Seleccionar las columnas correctas (1, 4, 5, 6, 8, 9, 11) sin omitir la columna A
datos <- datos %>%
  select(c(1, 4, 5, 6, 8, 9, 11)) %>%  # Seleccionar las columnas correctas
  setNames(nombres_columnas)

# Verificar las columnas después de la selección
print("Columnas después de la selección y renombrado:")
print(head(datos))

# Eliminar saltos de línea en la columna id_catalog
datos <- datos %>%
  mutate(id_catalog = gsub("[\r\n]", "", id_catalog))  # Eliminar saltos de línea

# Extraer solo los números de la columna id_catalog
datos <- datos %>%
  mutate(id_catalog = gsub("[^0-9]", "", id_catalog))  # Eliminar todo excepto los números

# Filtrar los registros donde id_catalog sea un INTEGER de 10 dígitos
datos <- datos %>%
  filter(grepl("^\\d{10}$", id_catalog))  # Ajustado para 10 dígitos

# Verificar los datos después del filtrado
print("Datos después del filtrado:")
print(head(datos))

# Conectar a la base de datos SQLite
db_path <- "C:/Users/racl26345/Documents/DataBases/people_analytics.db"
conn <- dbConnect(SQLite(), db_path)

# Consultar los id_catalog existentes en la tabla catalog_tickets
id_catalog_validos <- dbGetQuery(conn, "SELECT id_catalog FROM catalog_tickets")$id_catalog

# Filtrar los datos para mantener solo los registros cuyo id_catalog esté en catalog_tickets
datos <- datos %>%
  filter(id_catalog %in% id_catalog_validos)

# Verificar los datos después del filtrado
print("Datos después de validar con catalog_tickets:")
print(head(datos))

# Eliminar saltos de línea en todas las columnas
datos <- datos %>%
  mutate(across(everything(), ~ gsub("[\r\n]", "", .)))

# Convertir las columnas de fecha al formato YYYY-MM-DD
columnas_fecha <- c("fecha_creado", "fecha_interaccion")  # Columnas a formatear como fechas
for (col in columnas_fecha) {
  datos[[col]] <- format(as.Date(datos[[col]], format = "%m/%d/%Y"), "%Y-%m-%d")
}

# Verificar los datos después de convertir las fechas
print("Datos después de convertir las fechas:")
print(head(datos))

# Consultar los id_ticket existentes en la tabla codigo_tickets
id_tickets_existentes <- dbGetQuery(conn, "SELECT id_ticket FROM codigo_tickets")$id_ticket

# Filtrar los datos para excluir registros con id_ticket ya existentes
datos_nuevos <- datos %>%
  filter(!id_ticket %in% id_tickets_existentes)

# Verificar los datos que se insertarán
print("Datos que se insertarán en la tabla codigo_tickets:")
print(head(datos_nuevos))

# Insertar solo los registros nuevos en la tabla codigo_tickets
if (nrow(datos_nuevos) > 0) {
  dbWriteTable(conn, "codigo_tickets", datos_nuevos, append = TRUE, row.names = FALSE)
  print(paste("Se insertaron", nrow(datos_nuevos), "registros nuevos en la tabla codigo_tickets."))
} else {
  print("No hay registros nuevos para insertar.")
}

# Cerrar la conexión a la base de datos
dbDisconnect(conn)

print("Proceso completado correctamente.")