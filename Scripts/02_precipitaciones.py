import ee

ee.Initialize(project="x-vector-503914-c0")

deptos_col = ee.FeatureCollection("FAO/GAUL/2015/level1") \
    .filter(ee.Filter.eq('ADM0_NAME', 'Colombia'))

chirps_daily = ee.ImageCollection("UCSB-CHG/CHIRPS/DAILY") \
    .filterDate('2018-01-01', '2026-06-01')

def extraer_precip(img):
    reduccion = img.reduceRegions(
        collection=deptos_col,
        reducer=ee.Reducer.mean(),
        scale=5566 
    )
    fecha = img.date().format('YYYY-MM-dd')
    
    def formatear_filas(feature):
        return feature.set({
            'fecha': fecha,
            'precip_mm': feature.get('mean'),
            'departamento': feature.get('ADM1_NAME')
        })
    return reduccion.map(formatear_filas)

panel_precip = chirps_daily.map(extraer_precip).flatten()

tarea_precip = ee.batch.Export.table.toDrive(
    collection=panel_precip,
    description='CHIRPS_Diario_Deptos_2018_2026', 
    folder='Datos_Climaticos', 
    fileFormat='CSV',
    selectors=['departamento', 'fecha', 'precip_mm'] 
)

tarea_precip.start()
print("Tarea de extracción de precipitación iniciada en Google Earth Engine.")