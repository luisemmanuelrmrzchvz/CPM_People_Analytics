library(reticulate)
library(readxl)
library(dplyr)

# Configurar Python (usando tu entorno de Anaconda)
use_condaenv("base")

# Importar bibliotecas de Python
pd <- import("pandas")
plt <- import("matplotlib.pyplot")
np <- import("numpy")
mpl <- import("matplotlib")
datetime <- import("datetime")
os <- import("os")

# Leer el archivo Excel en R
file_path <- "C:/Users/racl26345/Documents/Tablas para Automatizaciones/Tableros_Digitales.xlsx"

# Leer los datos desde Excel en R
cat("Leyendo archivo Excel desde R...\n")
data_r <- read_excel(file_path, sheet = "Finanzas")

# Limpiar nombres de columnas en R
colnames(data_r) <- c("Concepto", "Fecha", "Meta_Anual", "Meta_Mensual", "Real_Mensual", 
                      "Variacion_Mensual", "Alcance_Mensual", "Meta_Acumulada", 
                      "Real_Acumulado", "Variacion_Acumulado", "Alcance_Acumulado")

# Convertir columnas numéricas
convertir_numero <- function(x) {
  as.numeric(gsub(",", "", as.character(x)))
}

columnas_numericas <- c("Meta_Mensual", "Real_Mensual", "Meta_Acumulada", 
                        "Real_Acumulado", "Alcance_Acumulado")

for (col in columnas_numericas) {
  data_r[[col]] <- convertir_numero(data_r[[col]])
}

# Verificar que tenemos datos
cat("Datos leídos correctamente. Dimensiones:", dim(data_r), "\n")
cat("Métricas encontradas:", toString(unique(data_r$Concepto)), "\n")

# Pasar los datos a Python
py$data_r <- data_r

# Ejecutar código Python para crear el dashboard
py_run_string('
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import matplotlib as mpl
from datetime import datetime
import os
from matplotlib.colors import LinearSegmentedColormap
from scipy.interpolate import make_interp_spline
import locale

# Intentamos setear locale (no estrictamente necesario para el formateo que implementamos)
try:
    locale.setlocale(locale.LC_ALL, "es_MX.UTF-8")
except:
    try:
        locale.setlocale(locale.LC_ALL, "Spanish_Mexico.1252")
    except:
        try:
            locale.setlocale(locale.LC_ALL, "es_ES.UTF-8")
        except:
            pass

# Usar los datos pasados desde R
data = data_r

print("Datos recibidos desde R:")
print(f"Dimensiones: {data.shape}")
print(f"Métricas: {list(data['Concepto'].unique())}")

# Configuración minimalista para videowall - FONDO OSCURO
plt.style.use("default")
mpl.rcParams["figure.facecolor"] = "#1E1E1E"  # Gris muy oscuro
mpl.rcParams["axes.facecolor"] = "#2D2D2D"    # Gris oscuro para gráficos
mpl.rcParams["savefig.facecolor"] = "#1E1E1E"
mpl.rcParams["axes.edgecolor"] = "none"
mpl.rcParams["axes.labelcolor"] = "#E8E8E8"   # Texto claro
mpl.rcParams["xtick.color"] = "#B0B0B0"       # Marcadores claros
mpl.rcParams["ytick.color"] = "#B0B0B0"       # Marcadores claros
mpl.rcParams["text.color"] = "#E8E8E8"        # Texto principal claro
mpl.rcParams["font.size"] = 28
mpl.rcParams["font.family"] = "sans-serif"
mpl.rcParams["font.sans-serif"] = ["Segoe UI", "Arial", "DejaVu Sans"]
mpl.rcParams["axes.titlesize"] = 32
mpl.rcParams["axes.titleweight"] = "bold"
mpl.rcParams["axes.linewidth"] = 0
mpl.rcParams["lines.linewidth"] = 8
mpl.rcParams["lines.markersize"] = 20
mpl.rcParams["grid.alpha"] = 0.1

# Paleta de colores para fondo oscuro
colors = {
    "primary": "#4A7AFF",
    "secondary": "#00E5D4",
    "accent": "#FF5252",
    "success": "#4CD964",
    "warning": "#FFD740",
    "text": "#F5F5F5",
    "light_text": "#A0A0A0",
    "background": "#1E1E1E",
    "card": "#2D2D2D",
    "chyron_text": "#E8E8E8"
}

# Indicadores con semaforización invertida y cálculo especial
special_calculation_metrics = ["Cartera_Vencida", "EPRC_Gasto"]

# -------------------------
# Formateadores (formato México)
# -------------------------

def mx_format(value, decimals=0):
    """
    Formatea números usando el formato con coma como separador de miles
    y punto como separador decimal (ej: 1,234.56). Retorna string.
    """
    try:
        if pd.isna(value):
            return "0"
        # Cuando value no es float, convertir a float para formatear correctamente
        v = float(value)
        fmt = f"{{:,.{decimals}f}}"
        return fmt.format(v)
    except Exception:
        return str(value)


def mx_currency(value):
    return f"${mx_format(value, 0)}"


def mx_percentage(value):
    return f"{mx_format(value, 1)}%"

# -------------------------
# Reemplazo de format_number usando mx_format
# -------------------------

def format_number(value, is_currency=False, is_percentage=False, is_integer=False):
    try:
        if pd.isna(value):
            return "0"

        if is_percentage:
            return mx_percentage(value)

        if is_integer:
            formatted = mx_format(value, 0)
        else:
            # por defecto mostramos 1 decimal para números no enteros
            formatted = mx_format(value, 1) if (abs(float(value) - round(float(value))) >= 0.001) else mx_format(value, 0)

        if is_currency:
            return f"${formatted}"
        return formatted
    except Exception:
        return str(value)

# Mapeo de nombres para mostrar
name_mapping = {
    "Membresía_Socios": "Membresía (socios)",
    "Cartera_Total": "Cartera total",
    "Cartera_Vencida": "Cartera vencida",
    "EPRC_Gasto": "EPRC Gasto",
    "Captación_Tradicional": "Captación tradicional",
    "Recuperación_de_Cartera_Eliminada": "Recuperación de cartera eliminada",
    "Ingresos_Financieros": "Ingresos financieros",
    "Gastos_Financieros": "Gastos financieros",
    "Gtos_Admón_y_Prom": "Gtos. Admón. y Prom.",
    "Remanente": "Remanente"
}

# Determinar si un indicador usa formato de moneda
def is_currency_metric(metric_name):
    return metric_name != "Membresía_Socios"

# Grupos de indicadores según especificación
indicator_groups = {
    "CRECIMIENTO DE LA COOPERATIVA": ["Membresía_Socios", "Captación_Tradicional"],
    "FORTALEZA DE LA CARTERA": ["Cartera_Total", "Cartera_Vencida"],
    "RECUPERACIÓN Y EFICIENCIA": ["Recuperación_de_Cartera_Eliminada", "EPRC_Gasto"],
    "SALUD FINANCIERA": ["Ingresos_Financieros", "Gastos_Financieros"],
    "RENTABILIDAD Y SOSTENIBILIDAD": ["Gtos_Admón_y_Prom", "Remanente"]
}

# Función para calcular porcentaje especial para Cartera_Vencida y EPRC_Gasto
def calculate_special_percentage(sum_real_mensual, sum_meta_mensual):
    if sum_meta_mensual > 0:
        return (1 - ((sum_real_mensual - sum_meta_mensual) / sum_meta_mensual)) * 100
    else:
        return (sum_real_mensual / sum_meta_mensual) * 100 if sum_meta_mensual != 0 else 0

# Función para crear sección de indicador (4 cuadros: 2x2)
def create_indicator_section(fig, grid_spec, row, col, data, metric_name, subtitle):
    # Crear subgrid para este indicador (2 filas, 2 columnas) - REDUCIR TAMAÑO EN 20%
    subgrid = grid_spec[row:row+2, col*2:col*2+2].subgridspec(2, 2, hspace=0.25, wspace=0.20, 
                                                              height_ratios=[0.50, 0.50],
                                                              width_ratios=[0.48, 0.52])

    # Filtrar datos para la métrica
    metric_data = data[data["Concepto"] == metric_name].copy()

    # Convertir fechas y ordenar por mes
    metric_data["Fecha"] = pd.to_datetime(metric_data["Fecha"], format="%d/%m/%Y")
    metric_data = metric_data.sort_values("Fecha")

    # --- CUADRO 1: Velocímetro (superior izquierdo) ---
    ax_gauge = fig.add_subplot(subgrid[0, 0])
    ax_gauge.set_facecolor(colors["card"])

    # CALCULO CORREGIDO: % Alcance para velocímetro = Suma de "Real Mensual" / Suma de "Meta Mensual"
    if len(metric_data) > 0:
        sum_real_mensual = metric_data["Real_Mensual"].sum()
        sum_meta_mensual = metric_data["Meta_Mensual"].sum()

        if metric_name in special_calculation_metrics:
            percentage = calculate_special_percentage(sum_real_mensual, sum_meta_mensual)
        else:
            percentage = (sum_real_mensual / sum_meta_mensual) * 100 if sum_meta_mensual != 0 else 0
    else:
        percentage = 0

    # Crear velocímetro minimalista (con semaforización especial para Cartera_Vencida y EPRC_Gasto)
    is_special_metric = metric_name in special_calculation_metrics
    create_minimal_gauge(ax_gauge, percentage, metric_name, is_special_metric)

    # --- CUADRO 2: Datos acumulados (superior derecho) ---
    ax_data = fig.add_subplot(subgrid[0, 1])
    ax_data.set_facecolor(colors["card"])

    # CALCULO CORREGIDO: 
    if len(metric_data) > 0:
        sum_real_mensual = metric_data["Real_Mensual"].sum()
        sum_meta_mensual = metric_data["Meta_Mensual"].sum()
    else:
        sum_real_mensual = 0
        sum_meta_mensual = 0

    # Determinar si es moneda
    use_currency_format = is_currency_metric(metric_name)

    # Formatear números según el tipo de indicador
    display_real = format_number(sum_real_mensual, is_currency=use_currency_format, is_integer=True)
    display_meta = format_number(sum_meta_mensual, is_currency=use_currency_format, is_integer=True)

    ax_data.text(0.5, 0.7, "REAL ACUMULADO", ha="center", va="center", 
                fontsize=26, fontweight="bold", color=colors["text"], alpha=0.8)
    ax_data.text(0.5, 0.5, f"{display_real}", ha="center", va="center", 
                fontsize=38, fontweight="bold", color=colors["success"])

    ax_data.text(0.5, 0.3, "META ACUMULADA", ha="center", va="center", 
                fontsize=26, fontweight="bold", color=colors["text"], alpha=0.8)
    ax_data.text(0.5, 0.1, f"{display_meta}", ha="center", va="center", 
                fontsize=38, fontweight="bold", color=colors["primary"])

    ax_data.set_xlim(0, 1)
    ax_data.set_ylim(0, 1)
    ax_data.axis("off")

    # --- CUADROS 3-4: Gráfico de tendencia (inferior, ocupa 2 columnas) ---
    ax_trend = fig.add_subplot(subgrid[1, :])
    ax_trend.set_facecolor(colors["card"])
    create_trend_chart(ax_trend, metric_data, metric_name)

    return ax_gauge, ax_data, ax_trend

# Función para crear velocímetro minimalista con semaforización especial
def create_minimal_gauge(ax, value, title, is_special_metric=False):
    # Limitar valor para display entre -200% y 200%
    display_value = max(-200, min(value, 200))

    # Crear semicírculo simple (de 0 a pi)
    theta = np.linspace(0, np.pi, 100)
    r = 0.8 * np.ones(100)
    x = r * np.cos(theta)
    y = r * np.sin(theta)

    # Semaforización especial
    if is_special_metric:
        if value < 0:
            gauge_color = colors["accent"]
            range_text = "Rango: < 0% (Rojo)"
        else:
            gauge_color = colors["success"]
            range_text = "Rango: ≥ 0% (Verde)"
    else:
        if value >= 100:
            gauge_color = colors["success"]
            range_text = "Rango: ≥ 100% (Verde)"
        elif value >= 80:
            gauge_color = colors["warning"]
            range_text = "Rango: 80-99% (Amarillo)"
        else:
            gauge_color = colors["accent"]
            range_text = "Rango: < 80% (Rojo)"

    ax.fill_betweenx(y, 0, x, color=gauge_color, alpha=0.8)

    theta_needle = (display_value + 200) / 400 * np.pi
    x_needle = 0.7 * np.cos(theta_needle)
    y_needle = 0.7 * np.sin(theta_needle)
    ax.plot([0, x_needle], [0, y_needle], color=colors["text"], linewidth=6)
    ax.plot(0, 0, "o", markersize=12, color=colors["text"])

    # Valor porcentual: usar formateador MX
    ax.text(0, -0.2, mx_percentage(value), ha="center", va="center", 
            fontsize=42, fontweight="bold", color=colors["text"])

    # Nombre mapeado
    display_name = name_mapping.get(title, title)
    ax.text(0, -0.4, display_name, ha="center", va="center", 
            fontsize=28, fontweight="bold", color=colors["text"], alpha=0.8)

    ax.text(0, -0.65, range_text, ha="center", va="center", 
            fontsize=16, color=colors["light_text"], alpha=0.8)

    ax.set_xlim(-1, 1)
    ax.set_ylim(-0.7, 1)
    ax.set_aspect("equal")
    ax.axis("off")

# Función para crear gráfico de tendencia con doble eje vertical
def create_trend_chart(ax, metric_data, metric_name):
    # Los datos ya vienen filtrados y ordenados por fecha

    metric_data["Mes_Año"] = metric_data["Fecha"].dt.strftime("%b-%y")
    months = metric_data["Mes_Año"]
    real_values = metric_data["Real_Mensual"]
    meta_values = metric_data["Meta_Mensual"]

    x = np.arange(len(months))

    # Determinar escala apropiada y etiqueta del eje Y
    max_value = max(max(real_values) if len(real_values) > 0 else 0, 
                   max(meta_values) if len(meta_values) > 0 else 0)

    use_currency_format = is_currency_metric(metric_name)
    display_name = name_mapping.get(metric_name, metric_name)

    # Determinar escala (miles o millones) y factor de división
    if max_value >= 1000000:
        scale_factor = 1000000
        scale_label = "Millones"
        real_values_scaled = real_values / scale_factor
        meta_values_scaled = meta_values / scale_factor
    elif max_value >= 1000:
        scale_factor = 1000
        scale_label = "Miles"
        real_values_scaled = real_values / scale_factor
        meta_values_scaled = meta_values / scale_factor
    else:
        scale_factor = 1
        scale_label = ""
        real_values_scaled = real_values
        meta_values_scaled = meta_values

    # Crear etiqueta del eje Y
    if use_currency_format:
        ylabel = f"Monto de {display_name} (en {scale_label})" if scale_label else f"Monto de {display_name}"
    else:
        ylabel = f"{display_name} (en {scale_label})" if scale_label else f"{display_name}"

    # Barras más estrechas para evitar superposición
    width = 0.35
    bars_real = ax.bar(x - width/2, real_values_scaled, width, color=colors["success"], alpha=0.9, label="Real Mensual")
    bars_meta = ax.bar(x + width/2, meta_values_scaled, width, color=colors["primary"], alpha=0.7, label="Meta Mensual")

    # Configuración del eje izquierdo (volúmenes)
    ax.set_xticks(x)
    ax.set_xticklabels(months, fontsize=22, fontweight="bold")

    # Configurar límites del eje Y
    all_scaled_values = np.concatenate([real_values_scaled, meta_values_scaled])
    if len(all_scaled_values) > 0:
        y_min = min(all_scaled_values)
        y_max = max(all_scaled_values)
        margin = (y_max - y_min) * 0.15 if y_max != y_min else abs(y_max) * 0.15
        ax.set_ylim(max(0, y_min - margin) if y_min >= 0 else y_min - margin, y_max + margin)

    ax.tick_params(axis="y", labelsize=18, colors=colors["success"])

    # Formateador personalizado para usar formato MX
    def volume_formatter(x, p):
        try:
            # x puede ser float (por ejemplo 1.8 si se muestra en 'Miles')
            return mx_format(x, 1 if (abs(x - round(x)) >= 0.001) else 0)
        except:
            return str(x)

    ax.yaxis.set_major_formatter(mpl.ticker.FuncFormatter(volume_formatter))

    # Establecer etiqueta del eje Y
    ax.set_ylabel(ylabel, fontsize=24, color=colors["success"], fontweight="bold", labelpad=20)

    # Quitar bordes y grid
    for spine in ax.spines.values():
        spine.set_visible(False)

    ax.grid(True, alpha=0.1, axis="y")

# Función para crear faldón/chyron de leyendas en la parte inferior (sin superponer)
def create_legend_chyron(fig, grid_spec):
    ax_legend = fig.add_subplot(grid_spec[2, :])
    ax_legend.set_facecolor("#3D3D3D")
    ax_legend.set_position([0.1, 0.02, 0.8, 0.06])

    legend_elements = [
        {"color": colors["primary"], "label": "META MENSUAL"},
        {"color": colors["success"], "label": "REAL MENSUAL"}
    ]

    n_elements = len(legend_elements)
    element_width = 1.0 / n_elements

    for i, element in enumerate(legend_elements):
        x_pos = (i + 0.5) * element_width
        ax_legend.plot(x_pos - 0.05, 0.5, "o", markersize=20, color=element["color"])
        ax_legend.text(x_pos + 0.02, 0.5, element["label"], ha="left", va="center",
                      fontsize=24, fontweight="bold", color=colors["chyron_text"])n
    ax_legend.set_xlim(0, 1)
    ax_legend.set_ylim(0, 1)
    ax_legend.axis("off")

# Obtener fecha para nombre de archivo
current_date = datetime.now().strftime("%Y%m")

# Crear imágenes para cada grupo de indicadores
for group_name, metrics in indicator_groups.items():
    print(f"Creando imagen para: {group_name} - {metrics}")

    # Filtrar datos para las métricas de este grupo
    group_data = data[data["Concepto"].isin(metrics)].copy()

    # Crear figura 4K con gridspec para layout preciso
    fig = plt.figure(figsize=(38.4, 21.6), facecolor=colors["background"])

    # Definir grid: 3 filas con más espacio para contenido y leyenda separada
    gs = fig.add_gridspec(3, 4, height_ratios=[0.10, 0.75, 0.15], hspace=0.20, wspace=0.40)

    # --- FILA 0: SOLO TÍTULO (sin subtítulo) ---
    ax_title = fig.add_subplot(gs[0, :])
    ax_title.set_facecolor(colors["background"])n
    ax_title.text(0.5, 0.5, "INDICADORES DE ALINEACIÓN", ha="center", va="center",
                 fontsize=48, fontweight="bold", color=colors["text"])n
    ax_title.set_xlim(0, 1)
    ax_title.set_ylim(0, 1)
    ax_title.axis("off")

    # --- FILA 1: CONTENIDO (2 indicadores) ---
    # Crear secciones para cada indicador
    for i, metric in enumerate(metrics):
        create_indicator_section(fig, gs, 1, i, group_data, metric, group_name)

    # --- FILA 2: FALDÓN/CHYRON DE LEYENDAS (separado en parte inferior) ---
    create_legend_chyron(fig, gs)

    # Ajustar layout con márgenes para evitar superposición
    plt.tight_layout(rect=[0, 0.05, 1, 0.95])

    output_dir = r"C:\\Users\\racl26345\\Documents\\Reportes Automatizados\\Observatorio Digital"
    os.makedirs(output_dir, exist_ok=True)

    # Nombre de archivo basado en el grupo (mantener el nombre original con el subtítulo)
    group_slug = group_name.replace(" ", "_").replace("Ó", "O").replace("É", "E")
    output_path = os.path.join(output_dir, f"Videowall_{group_slug}_{current_date}_4K.png")

    plt.savefig(output_path, dpi=100, bbox_inches="tight", facecolor=colors["background"], 
                pad_inches=0.5)
    plt.close()

    print(f"Imagen guardada: {output_path}")

print("Proceso completado: Se crearon 5 imágenes para videowall")
')

cat("¡Proceso completado exitosamente a las", format(Sys.time(), "%H:%M:%S"), "\n")






























+ # Intentamos setear locale (no estrictamente necesario para el formateo que implementamos)
  + try:
  +     locale.setlocale(locale.LC_ALL, "es_MX.UTF-8")
+ except:
  +     try:
  +         locale.setlocale(locale.LC_ALL, "Spanish_Mexico.1252")
+     except:
  +         try:
  +             locale.setlocale(locale.LC_ALL, "es_ES.UTF-8")
+         except:
  +             pass
+ 
  + # Usar los datos pasados desde R
  + data = data_r
+ 
  + print("Datos recibidos desde R:")
+ print(f"Dimensiones: {data.shape}")
+ print(f"Métricas: {list(data['Concepto'].unique())}")
Error: unexpected symbol in:
  "print(f"Dimensiones: {data.shape}")
print(f"Métricas: {list(data['Concepto"