library(reticulate)

# Importar las bibliotecas de Python
pd <- import("pandas")
sqlite3 <- import("sqlite3")
pytz <- import("pytz")

# ------------------------------------------------------------------------------
# Código Python compatible con R (sintaxis corregida)
# ------------------------------------------------------------------------------
py_run_string('
import sqlite3
import pandas as pd
import pytz
from datetime import datetime, timedelta

def calculate_effective_seconds(start_utc, end_utc, holidays_set):
    # Conversión UTC a CST (Ciudad de México)
    utc_zone = pytz.utc
    cst_zone = pytz.timezone("America/Mexico_City")
    
    start = start_utc.astimezone(cst_zone) if start_utc.tzinfo else utc_zone.localize(start_utc).astimezone(cst_zone)
    end = end_utc.astimezone(cst_zone) if end_utc.tzinfo else utc_zone.localize(end_utc).astimezone(cst_zone)

    if end == start:
        return 0
    if pd.isna(start) or pd.isna(end) or start >= end:
        return 0

    total_seconds = 0
    current_date = start.date()
    end_date = end.date()

    while current_date <= end_date:
        # Excluir festivos, domingos y días adicionales
        if current_date in holidays_set or current_date.weekday() == 6:
            current_date += timedelta(days=1)
            continue

        day_of_week = current_date.weekday()

        # Ventanas de servicio en CST
        if day_of_week < 5:  # Lunes-Viernes
            servicio_start = cst_zone.localize(datetime.combine(current_date, datetime.strptime("09:00:00", "%H:%M:%S").time()))
            servicio_end = cst_zone.localize(datetime.combine(current_date, datetime.strptime("18:00:00", "%H:%M:%S").time()))
        elif day_of_week == 5:  # Sábado
            servicio_start = cst_zone.localize(datetime.combine(current_date, datetime.strptime("09:00:00", "%H:%M:%S").time()))
            servicio_end = cst_zone.localize(datetime.combine(current_date, datetime.strptime("14:00:00", "%H:%M:%S").time()))
        else:
            current_date += timedelta(days=1)
            continue

        interval_start = max(start, servicio_start)
        interval_end = min(end, servicio_end)

        if interval_start < interval_end:
            total_seconds += (interval_end - interval_start).total_seconds()

        current_date += timedelta(days=1)

    return total_seconds

def calculate_row_seconds(row, holidays_set):
    try:
        if row["code_estado_ticket"] != "6":
            return calculate_effective_seconds(row["time_start_status"], row["time_end_status_parsed"], holidays_set)
        else:
            return 0
    except Exception as e:
        print(f"Error en fila {row.name}: {str(e)}")
        return 0

# ------------------------------------------------------------------------------
# Conexión y consultas (formato seguro para R)
# ------------------------------------------------------------------------------
conn = sqlite3.connect("C:/Users/racl26345/Documents/DataBases/people_analytics.db")

# Consulta de festivos y días adicionales con formato compatible
holidays_query = (
    "SELECT calendar.date " 
    "FROM calendar "
    "WHERE calendar.date >= \'2025-01-01\' "
    "    AND calendar.day_week <> 6 "
    "    AND calendar.laboral_no_odg <> calendar.laboral_no_odg_sin_festivos "
    "UNION "
    "SELECT calendar.date "
    "FROM calendar "
    "WHERE calendar.date BETWEEN \'2025-01-01\' AND DATE(\'now\') "
    "    AND calendar.laboral_no_odg_sin_festivos = 0 "
    "    AND calendar.day_week <> \'Domingo\';"
)

holidays_df = pd.read_sql_query(holidays_query, conn)
holidays_set = set(pd.to_datetime(holidays_df["date"]).dt.date)

# Consulta principal con formato seguro
query_principal = (
    "SELECT "
    "    h.id_key, "
    "    h.id_ticket, "
    "    h.fecha_creado, "
    "    h.agente_servicio, "
    "    h.prioridad, "
    "    h.time_start_status, "
    "    CASE "
    "        WHEN h.code_estado_ticket = 6 THEN h.time_start_status "
    "        ELSE h.time_end_status "
    "    END AS time_end_status, "
    "    CAST(h.code_estado_ticket AS TEXT) AS code_estado_ticket, "
    "    h.estado_ticket, "
    "    h.siguiente_accion_para, "
    "    h.seg_duracion "
    "FROM hist_status_tickets h "
    "LEFT JOIN hist_status_tickets_sw s "
    "    ON h.id_key = s.id_key "
    "WHERE "
    "    s.id_key IS NULL "
    "    OR ( "
    "        h.time_end_status != s.time_end_status "
    "        AND h.code_estado_ticket != \'6\' "
    "    );"
)

nuevos_registros = pd.read_sql_query(query_principal, conn)

# ------------------------------------------------------------------------------
# Procesamiento de datos
# ------------------------------------------------------------------------------
if not nuevos_registros.empty:
    nuevos_registros["original_time_end_status"] = nuevos_registros["time_end_status"]
    
    # Conversión de fechas
    nuevos_registros["time_start_status"] = pd.to_datetime(
        nuevos_registros["time_start_status"], 
        errors="coerce"
    ).dt.tz_localize("UTC")
    
    nuevos_registros["time_end_status_parsed"] = pd.to_datetime(
        nuevos_registros["time_end_status"], 
        errors="coerce"
    ).dt.tz_localize("UTC")
    
    # Determinar fin dinámico en CST
    cst = pytz.timezone("America/Mexico_City")
    now_cst = datetime.now(cst)
    current_date_cst = now_cst.date()
    
    def obtener_fin_dinamico(current_date):
        if current_date.weekday() < 5:
            end_time = datetime.strptime("18:00:00", "%H:%M:%S").time()
        elif current_date.weekday() == 5:
            end_time = datetime.strptime("14:00:00", "%H:%M:%S").time()
        else:
            current_date -= timedelta(days=1)
            end_time = datetime.strptime("14:00:00", "%H:%M:%S").time()
        
        naive = datetime.combine(current_date, end_time)
        localized = cst.localize(naive)
        return localized.astimezone(pytz.UTC)
    
    dynamic_end_utc = obtener_fin_dinamico(current_date_cst)
    
    # Manejar valores faltantes
    mask = (nuevos_registros["code_estado_ticket"] != "6") & nuevos_registros["time_end_status_parsed"].isna()
    nuevos_registros.loc[mask, "time_end_status_parsed"] = dynamic_end_utc
    
    # Calcular duración
    nuevos_registros["seg_duracion"] = nuevos_registros.apply(
        calculate_row_seconds, 
        axis=1, 
        args=(holidays_set,)
    )
    
    # Convertir a formato naive para SQLite
    nuevos_registros["time_start_status"] = nuevos_registros["time_start_status"].dt.tz_convert("America/Mexico_City").dt.tz_localize(None)
    nuevos_registros["time_end_status_parsed"] = nuevos_registros["time_end_status_parsed"].dt.tz_convert("America/Mexico_City").dt.tz_localize(None)
    
    # Restaurar valores originales
    nuevos_registros["time_end_status"] = nuevos_registros.apply(
        lambda row: row["original_time_end_status"] 
        if (row["code_estado_ticket"] != "6" and pd.isna(row["time_end_status_parsed"])) 
        else row["time_end_status_parsed"].strftime("%Y-%m-%d %H:%M:%S"),
        axis=1
    )
    
    nuevos_registros = nuevos_registros.drop(columns=["time_end_status_parsed", "original_time_end_status"])
    
    # UPSERT
    with conn:
        if len(nuevos_registros) > 0:
            ids = ",".join(map(str, nuevos_registros["id_key"].tolist()))
            conn.execute(f"DELETE FROM hist_status_tickets_sw WHERE id_key IN ({ids})")
            nuevos_registros.to_sql(
                "hist_status_tickets_sw", 
                conn, 
                if_exists="append", 
                index=False,
                dtype={"id_key": "INTEGER PRIMARY KEY"}
            )

conn.close()
')

# Mensaje de finalización
cat("¡Proceso completado exitosamente a las", format(Sys.time(), "%H:%M:%S"), "\n")