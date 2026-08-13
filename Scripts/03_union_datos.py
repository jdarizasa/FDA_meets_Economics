import pandas as pd
import os

base_dir = os.path.dirname(os.path.abspath(__file__))
project_root = os.path.dirname(base_dir) if os.path.basename(base_dir).lower() == 'scripts' else base_dir

dir_clima = os.path.join(project_root, 'Datos_clima')

precipitacion_file = os.path.join(dir_clima, 'CHIRPS_Diario_Deptos_2018_2026.csv')
temperatura_file = os.path.join(dir_clima, 'ERA5_Temperatura_Diaria_Deptos_2018_2026_v2.csv')

df_pre = pd.read_csv(precipitacion_file)
df_tem = pd.read_csv(temperatura_file)

df_union = pd.merge(df_pre, df_tem, on=['departamento', 'fecha'], how='inner')

deptos_clave = ['Cundinamarca', 'Boyaca', 'Huila', 'Tolima', 'Meta']
df_union = df_union[df_union['departamento'].isin(deptos_clave)].copy()

df_union = df_union.sort_values(by=['departamento', 'fecha']).reset_index(drop=True)

output_file = os.path.join(dir_clima, 'Panel_Climatico_Deptos_2018_2026.csv')
df_union.to_csv(output_file, index=False)

print(f"Panel climático unificado (2018-2026) guardado en: {output_file}")