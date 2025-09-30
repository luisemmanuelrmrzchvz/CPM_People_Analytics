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

# Configurar locale para separadores de miles en español México
try:
    locale.setlocale(locale.LC_ALL, "es_MX.UTF-8")
except:
    try:
        locale.setlocale(locale.LC_ALL, "Spanish_Mexico.1252")
    except:
        try:
            locale.setlocale(locale.LC_ALL, "es_ES.UTF-8")
        except:
            print("Usando formato numérico por defecto")

# Usar los datos pasados desde R
data = data_r

print("Datos recibidos desde R:")
print(f"Dimensiones: {data.shape}")
print(f"Métricas: {list(data[\'Concepto\'].unique())}")

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
    "primary": "#4A7AFF",      # Azul más brillante para contraste
    "secondary": "#00E5D4",    # Verde azulado más vibrante
    "accent": "#FF5252",       # Rojo más brillante
    "success": "#4CD964",      # Verde más vibrante
    "warning": "#FFD740",      # Amarillo más brillante
    "text": "#F5F5F5",         # Texto casi blanco
    "light_text": "#A0A0A0",   # Texto secundario gris claro
    "background": "#1E1E1E",   # Fondo principal oscuro
    "card": "#2D2D2D",         # Fondo de gráficos gris oscuro
    "chyron_text": "#E8E8E8"   # Texto claro para chyron
}

# Indicadores con semaforización invertida y cálculo especial
special_calculation_metrics = ["Cartera_Vencida", "EPRC_Gasto"]

# Función para formatear números con comas para miles (formato México)
def format_number(value, is_integer=False, use_k=False):
    try:
        if pd.isna(value):
            return "0"
        
        if use_k and abs(value) >= 1000:
            # Convertir a miles con "k"
            value_k = value / 1000
            if abs(value_k) >= 1000:
                # Si es millones, usar M
                value_m = value_k / 1000
                return f"{value_m:,.1f}M".replace(",", "X").replace(".", ",").replace("X", ".")
            else:
                return f"{value_k:,.1f}k".replace(",", "X").replace(".", ",").replace("X", ".")
        else:
            # Formato normal con comas para miles
            if is_integer or abs(value - round(value)) < 0.001:
                return f"{value:,.0f}".replace(",", "X").replace(".", ",").replace("X", ".")
            else:
                return f"{value:,.1f}".replace(",", "X").replace(".", ",").replace("X", ".")
    except:
        return str(value)

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
    # Ajustar las proporciones para hacer los gráficos más pequeños
    subgrid = grid_spec[row:row+2, col*2:col*2+2].subgridspec(2, 2, hspace=0.25, wspace=0.20, 
                                                              height_ratios=[0.50, 0.50],  # Más equilibrio
                                                              width_ratios=[0.48, 0.52])   # Menos ancho para gráficos
    
    # Filtrar datos para la métrica
    metric_data = data[data["Concepto"] == metric_name].copy()
    
    # Convertir fechas y ordenar por mes
    metric_data["Fecha"] = pd.to_datetime(metric_data["Fecha"], format="%d/%m/%Y")
    metric_data = metric_data.sort_values("Fecha")
    
    # --- CUADRO 1: Velocímetro (superior izquierdo) ---
    ax_gauge = fig.add_subplot(subgrid[0, 0])
    ax_gauge.set_facecolor(colors["card"])
    
    # CALCULO CORREGIDO: % Alcance para velocímetro = Suma de "Real Mensual" / Suma de "Meta Mensual"
    # EXCEPCIÓN: Para "Cartera_Vencida" y "EPRC_Gasto" usar fórmula especial
    if len(metric_data) > 0:
        sum_real_mensual = metric_data["Real_Mensual"].sum()
        sum_meta_mensual = metric_data["Meta_Mensual"].sum()
        
        if metric_name in special_calculation_metrics:
            percentage = calculate_special_percentage(sum_real_mensual, sum_meta_mensual)
        else:
            percentage = (sum_real_mensual / sum_meta_mensual) * 100 if sum_meta_mensual != 0 else 0
    else:
        percentage = 0
    
    # Crear velocímetro minimalista (con semaforización invertida si corresponde)
    is_inverted = metric_name in special_calculation_metrics
    create_minimal_gauge(ax_gauge, percentage, metric_name, is_inverted)
    
    # --- CUADRO 2: Datos acumulados (superior derecho) ---
    ax_data = fig.add_subplot(subgrid[0, 1])
    ax_data.set_facecolor(colors["card"])
    
    # CALCULO CORREGIDO: 
    # Real Acumulado = Suma de "Real Mensual"
    # Meta Acumulada = Suma de "Meta Mensual"
    if len(metric_data) > 0:
        sum_real_mensual = metric_data["Real_Mensual"].sum()
        sum_meta_mensual = metric_data["Meta_Mensual"].sum()
    else:
        sum_real_mensual = 0
        sum_meta_mensual = 0
    
    # Determinar si usar formato "k" para miles (excepto Membresía)
    use_k_format = metric_name != "Membresía_Socios"
    
    display_real = sum_real_mensual / 1000 if use_k_format else sum_real_mensual
    display_meta = sum_meta_mensual / 1000 if use_k_format else sum_meta_mensual
    
    unit = "k" if use_k_format else ""
    
    ax_data.text(0.5, 0.7, "REAL ACUMULADO", ha="center", va="center", 
                fontsize=26, fontweight="bold", color=colors["text"], alpha=0.8)  # Fuente un poco más grande
    ax_data.text(0.5, 0.5, f"{format_number(display_real, use_k=False)}{unit}", ha="center", va="center", 
                fontsize=38, fontweight="bold", color=colors["success"])  # Fuente más grande
    
    ax_data.text(0.5, 0.3, "META ACUMULADA", ha="center", va="center", 
                fontsize=26, fontweight="bold", color=colors["text"], alpha=0.8)  # Fuente más grande
    ax_data.text(0.5, 0.1, f"{format_number(display_meta, use_k=False)}{unit}", ha="center", va="center", 
                fontsize=38, fontweight="bold", color=colors["primary"])  # Fuente más grande
    
    ax_data.set_xlim(0, 1)
    ax_data.set_ylim(0, 1)
    ax_data.axis("off")
    
    # --- CUADROS 3-4: Gráfico de tendencia (inferior, ocupa 2 columnas) ---
    ax_trend = fig.add_subplot(subgrid[1, :])
    ax_trend.set_facecolor(colors["card"])
    create_trend_chart(ax_trend, metric_data, metric_name)
    
    return ax_gauge, ax_data, ax_trend

# Función para crear velocímetro minimalista con semaforización opcional invertida
def create_minimal_gauge(ax, value, title, is_inverted=False):
    # Limitar valor para display entre -200% y 200%
    display_value = max(-200, min(value, 200))
    
    # Crear semicírculo simple (de 0 a pi)
    theta = np.linspace(0, np.pi, 100)
    r = 0.8 * np.ones(100)
    x = r * np.cos(theta)
    y = r * np.sin(theta)
    
    # Color basado en desempeño (invertido para ciertos indicadores)
    if is_inverted:
        # Semaforización invertida: verde debajo de 100%, rojo arriba de 100%
        if value <= 100:
            gauge_color = colors["success"]  # Bueno
        else:
            gauge_color = colors["accent"]   # Malo
    else:
        # Semaforización normal: verde arriba de 100%, rojo debajo de 80%
        if value >= 100:
            gauge_color = colors["success"]
        elif value >= 80:
            gauge_color = colors["warning"]
        else:
            gauge_color = colors["accent"]
    
    ax.fill_betweenx(y, 0, x, color=gauge_color, alpha=0.8)
    
    # Aguja simple: mapear value a ángulo entre 0 y pi
    # value en [-200, 200] -> angle en [0, pi]
    theta_needle = (display_value + 200) / 400 * np.pi
    x_needle = 0.7 * np.cos(theta_needle)
    y_needle = 0.7 * np.sin(theta_needle)
    ax.plot([0, x_needle], [0, y_needle], color=colors["text"], linewidth=6)
    ax.plot(0, 0, "o", markersize=12, color=colors["text"])
    
    # Valor porcentual
    ax.text(0, -0.2, f"{value:.0f}%", ha="center", va="center", 
            fontsize=42, fontweight="bold", color=colors["text"])
    
    # Título abreviado
    short_title = title.split(" ")[0] if len(title.split(" ")) > 0 else title
    ax.text(0, -0.4, short_title, ha="center", va="center", 
            fontsize=28, fontweight="bold", color=colors["text"], alpha=0.8)
    
    ax.set_xlim(-1, 1)
    ax.set_ylim(-0.5, 1)
    ax.set_aspect("equal")
    ax.axis("off")

# Función para crear gráfico de tendencia con doble eje vertical
def create_trend_chart(ax, metric_data, metric_name):
    # Los datos ya vienen filtrados y ordenados por fecha
    
    # Preparar datos - usar los valores mensuales directamente
    months = metric_data["Fecha"].dt.strftime("%b")
    real_values = metric_data["Real_Mensual"]
    meta_values = metric_data["Meta_Mensual"]
    
    # CORRECCIÓN: Usar Alcance Acumulado directamente (ya está en porcentaje, no dividir entre 100)
    alcance_values = metric_data["Alcance_Acumulado"]
    
    x = np.arange(len(months))
    
    # Crear eje secundario para porcentajes
    ax2 = ax.twinx()
    
    # Configurar límites del eje izquierdo (volúmenes) - adaptable con márgenes
    all_volume_values = np.concatenate([real_values, meta_values])
    
    # Permitir valores negativos y calcular límites adaptativos
    if len(all_volume_values) > 0:
        volume_min = min(all_volume_values)
        volume_max = max(all_volume_values)
        
        # Añadir margen del 15%
        margin = (volume_max - volume_min) * 0.15 if volume_max != volume_min else abs(volume_max) * 0.15
        
        # Si hay valores negativos, ajustar el mínimo; si no, empezar desde 0 o un poco negativo
        if volume_min >= 0:
            volume_min = max(0, volume_min - margin)
        else:
            volume_min = volume_min - margin
        
        volume_max = volume_max + margin
    else:
        volume_min = 0
        volume_max = 1
    
    # CORRECCIÓN: ELIMINAR RESTRICCIONES - ajustar eje derecho basado en los datos sin límites fijos
    if len(alcance_values) > 0:
        # Calcular límites basados en los datos con un margen del 20%
        alcance_min = min(alcance_values)
        alcance_max = max(alcance_values)
        
        # Añadir margen del 20% a ambos lados
        alcance_range = alcance_max - alcance_min
        if alcance_range > 0:
            margin = alcance_range * 0.2
            alcance_min = alcance_min - margin
            alcance_max = alcance_max + margin
        else:
            # Si no hay variación, crear un rango mínimo
            alcance_min = alcance_min - 10
            alcance_max = alcance_max + 10
    else:
        alcance_min = 0
        alcance_max = 100
    
    # Barras más estrechas para evitar superposición
    width = 0.25
    bars_real = ax.bar(x - width/2, real_values, width, color=colors["success"], alpha=0.9, label="Real Mensual")
    bars_meta = ax.bar(x + width/2, meta_values, width, color=colors["primary"], alpha=0.7, label="Meta Mensual")
    
    # Línea de tendencia acumulada en eje derecho (Alcance Acumulado)
    if len(x) > 2:
        x_smooth = np.linspace(x.min(), x.max(), 100)
        spl = make_interp_spline(x, alcance_values, k=2)
        y_smooth = spl(x_smooth)
        line = ax2.plot(x_smooth, y_smooth, color=colors["warning"], linewidth=6, label="Acumulado %")
    else:
        line = ax2.plot(x, alcance_values, color=colors["warning"], linewidth=6, marker="o", 
                       markersize=12, label="Acumulado %")
    
    # Configuración del eje izquierdo (volúmenes) - usar formato adaptativo
    ax.set_xticks(x)
    ax.set_xticklabels(months, fontsize=22, fontweight="bold")  # Fuente más grande
    ax.set_ylim(volume_min, volume_max)
    ax.tick_params(axis="y", labelsize=18, colors=colors["success"])  # Fuente más grande
    
    # Formateador personalizado para usar "k", "M" y comas mexicanas
    def volume_formatter(x, p):
        if x == 0:
            return "0"
        elif abs(x) >= 1000000:
            return f"{x/1000000:,.1f}M".replace(",", "X").replace(".", ",").replace("X", ".")
        elif abs(x) >= 1000:
            return f"{x/1000:,.0f}k".replace(",", "X").replace(".", ",").replace("X", ".")
        else:
            return f"{x:,.0f}".replace(",", "X").replace(".", ",").replace("X", ".")
    
    ax.yaxis.set_major_formatter(mpl.ticker.FuncFormatter(volume_formatter))
    
    # Configuración del eje derecho (porcentajes) - CORRECCIÓN: valores ya están en porcentaje
    ax2.set_ylim(alcance_min, alcance_max)
    ax2.tick_params(axis="y", labelsize=18, colors=colors["warning"])  # Fuente más grande
    # CORRECCIÓN: No multiplicar por 100, ya están en porcentaje
    ax2.yaxis.set_major_formatter(mpl.ticker.FuncFormatter(lambda x, p: f"{x:.0f}%"))
    
    # Ajustar posición de los ticks del eje derecho para separar la línea - MÁS SEPARACIÓN
    ax2.spines["right"].set_position(("outward", 25))  # Más separación para evitar empalmes
    
    # Quitar bordes y grid
    for spine in ax.spines.values():
        spine.set_visible(False)
    for spine in ax2.spines.values():
        spine.set_visible(False)
    
    ax.grid(True, alpha=0.1, axis="y")
    ax2.grid(False)

# Función para crear faldón/chyron de leyendas en la parte inferior (sin superponer)
def create_legend_chyron(fig, grid_spec):
    ax_legend = fig.add_subplot(grid_spec[2, :])
    ax_legend.set_facecolor("#3D3D3D")  # Fondo gris medio oscuro para el chyron
    ax_legend.set_position([0.1, 0.02, 0.8, 0.06])  # Posición fija en la parte inferior
    
    # Leyendas con texto oscuro para mejor contraste
    legend_elements = [
        {"color": colors["primary"], "label": "META MENSUAL"},
        {"color": colors["success"], "label": "REAL MENSUAL"}, 
        {"color": colors["warning"], "label": "ACUMULADO %"}
    ]
    
    # Calcular posiciones equitativas
    n_elements = len(legend_elements)
    element_width = 1.0 / n_elements
    
    for i, element in enumerate(legend_elements):
        x_pos = (i + 0.5) * element_width
        # Círculo de color
        ax_legend.plot(x_pos - 0.05, 0.5, "o", markersize=20, color=element["color"])
        # Texto de la leyenda - ahora en color oscuro
        ax_legend.text(x_pos + 0.02, 0.5, element["label"], ha="left", va="center",
                      fontsize=24, fontweight="bold", color=colors["chyron_text"])
    
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
    # Aumentar el espacio horizontal entre los dos indicadores
    gs = fig.add_gridspec(3, 4, height_ratios=[0.10, 0.75, 0.15], hspace=0.20, wspace=0.40)  # Más espacio entre gráficos
    
    # --- FILA 0: TÍTULO Y SUBTÍTULO ---
    ax_title = fig.add_subplot(gs[0, :])
    ax_title.set_facecolor(colors["background"])
    
    ax_title.text(0.5, 0.7, "INDICADORES DE ALINEACIÓN", ha="center", va="center",
                 fontsize=48, fontweight="bold", color=colors["text"])
    
    ax_title.text(0.5, 0.3, group_name, ha="center", va="center",
                 fontsize=36, fontweight="bold", color=colors["primary"])
    
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
    
    # Nombre de archivo basado en el grupo
    group_slug = group_name.replace(" ", "_").replace("Ó", "O").replace("É", "E")
    output_path = os.path.join(output_dir, f"Videowall_{group_slug}_{current_date}_4K.png")
    
    plt.savefig(output_path, dpi=100, bbox_inches="tight", facecolor=colors["background"], 
                pad_inches=0.5)
    plt.close()
    
    print(f"Imagen guardada: {output_path}")

print("Proceso completado: Se crearon 5 imágenes para videowall")
')

cat("¡Proceso completado exitosamente a las", format(Sys.time(), "%H:%M:%S"), "\n")