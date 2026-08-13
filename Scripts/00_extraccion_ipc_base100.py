import pandas as pd
import numpy as np
import os
import re
import unicodedata

# Directorios de trabajo
base_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(base_dir) if os.path.basename(base_dir).lower() == 'scripts' else base_dir

dir_archivos_ipc = os.path.join(project_root, 'Archivos_IPC')
dir_clima = os.path.join(project_root, 'Datos_clima')

os.makedirs(dir_clima, exist_ok=True)

# Selección de archivo local
excel_path = os.path.join(dir_archivos_ipc, 'ciudades_mensuales_master.xlsx')
if not os.path.exists(excel_path):
    excel_path = os.path.join(dir_archivos_ipc, 'ciudades_mensuales.xlsx')

df_raw = pd.read_excel(excel_path, header=None)

months_dict = {
    'enero': 1, 'febrero': 2, 'marzo': 3, 'abril': 4, 'mayo': 5, 'junio': 6,
    'julio': 7, 'agosto': 8, 'septiembre': 9, 'octubre': 10, 'noviembre': 11, 'diciembre': 12
}

def remove_accents(input_str):
    nfkd_form = unicodedata.normalize('NFKD', str(input_str))
    return u"".join([c for c in nfkd_form if not unicodedata.combining(c)]).lower().strip()

target_cities = {
    'medellin': 'Medellin',
    'barranquilla': 'Barranquilla',
    'bogota d.c.': 'Bogota',
    'bogota': 'Bogota',
    'cali': 'Cali',
    'bucaramanga': 'Bucaramanga',
    'pasto': 'Pasto',
    'villavicencio': 'Villavicencio'
}

# Localización de bloques por año
block_info = []
for idx in range(len(df_raw)):
    val = str(df_raw.iloc[idx, 0]).strip()
    if 'variaciones mensuales' in val.lower():
        match_yr = re.search(r'20\d{2}', val)
        if match_yr:
            yr = int(match_yr.group(0))
            if yr >= 2018:
                block_info.append((idx, yr))

records = []
for start_row, yr in block_info:
    header_row = df_raw.iloc[start_row + 1]
    col_months = {}
    for col_idx in range(1, len(header_row)):
        m_name = remove_accents(header_row.iloc[col_idx])
        if m_name in months_dict:
            col_months[col_idx] = months_dict[m_name]
            
    r_idx = start_row + 2
    while r_idx < len(df_raw):
        city_raw = df_raw.iloc[r_idx, 0]
        if pd.isna(city_raw) or any(k in str(city_raw).lower() for k in ['variaciones', 'fuente', 'nota', 'año']):
            break
            
        city_clean = remove_accents(city_raw)
        matched_city = None
        for k, v in target_cities.items():
            if k in city_clean:
                matched_city = v
                break
                
        if matched_city:
            for col_idx, m_num in col_months.items():
                val = df_raw.iloc[r_idx, col_idx]
                if pd.notna(val):
                    try:
                        records.append({
                            'anio': yr,
                            'mes': m_num,
                            'fecha': f"{yr}-{m_num:02d}-01",
                            'ciudad': matched_city,
                            'var_mensual': float(val)
                        })
                    except ValueError:
                        pass
        r_idx += 1

df_var = pd.DataFrame(records).drop_duplicates(subset=['fecha', 'ciudad'])
df_var['fecha'] = pd.to_datetime(df_var['fecha'])
df_var = df_var.sort_values(by=['ciudad', 'fecha']).reset_index(drop=True)

df_panel_var = df_var.pivot(index='fecha', columns='ciudad', values='var_mensual').loc['2018-01-01':'2026-05-01']

# Construcción de niveles IPC Base Diciembre 2018 = 100
df_panel_level = pd.DataFrame(index=df_panel_var.index, columns=df_panel_var.columns)
dec_2018 = pd.Timestamp('2018-12-01')

if dec_2018 in df_panel_level.index:
    df_panel_level.loc[dec_2018] = 100.00

    # Adelante (2019+)
    dates_forward = [d for d in df_panel_level.index if d > dec_2018]
    prev_date = dec_2018
    for d in dates_forward:
        for col in df_panel_level.columns:
            v = df_panel_var.loc[d, col]
            if pd.notna(v):
                df_panel_level.loc[d, col] = df_panel_level.loc[prev_date, col] * (1.0 + v / 100.0)
        prev_date = d

    # Atrás (2018)
    dates_backward = [d for d in df_panel_level.index if d < dec_2018]
    dates_backward.reverse()
    next_date = dec_2018
    for d in dates_backward:
        for col in df_panel_level.columns:
            v = df_panel_var.loc[next_date, col]
            if pd.notna(v):
                df_panel_level.loc[d, col] = df_panel_level.loc[next_date, col] / (1.0 + v / 100.0)
        next_date = d

df_panel_level = df_panel_level.astype(float).round(4)

# Guardar resultados
output_csv = os.path.join(dir_clima, 'IPC_Alimentos_Ciudades_Base100.csv')
df_panel_level.to_csv(output_csv)
df_panel_level.to_csv(os.path.join(dir_clima, 'IPC_Alimentos_Ciudades_V2.csv'))

print(f"IPC en niveles exportado a: {output_csv}")
