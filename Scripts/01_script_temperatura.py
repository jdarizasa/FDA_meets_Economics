import ee

ee.Initialize(project="x-vector-503914-c0")

deptos_col = ee.FeatureCollection("FAO/GAUL/2015/level1") \
    .filter(ee.Filter.eq('ADM0_NAME', 'Colombia'))

era5_daily = ee.ImageCollection("ECMWF/ERA5_LAND/DAILY_AGGR") \
    .filterDate('2018-01-01', '2026-06-01') \
    .select('temperature_2m')

def extraer_temp(img):
    img_celsius = img.subtract(273.15)
    reduccion = img_celsius.reduceRegions(
        collection=deptos_col,
        reducer=ee.Reducer.mean(),
        scale=9000 
    )
    fecha = img.date().format('YYYY-MM-dd')
    
    def formatear_filas(feature):
        return feature.set({
            'fecha': fecha,
            'temp_celsius': feature.get('mean'),
            'departamento': feature.get('ADM1_NAME')
        })
    return reduccion.map(formatear_filas)

panel_temp = era5_daily.map(extraer_temp).flatten()

tarea_temp = ee.batch.Export.table.toDrive(
    collection=panel_temp,
    description='ERA5_Temperatura_Diaria_Deptos_2018_2026_v2', 
    folder='Datos_Climaticos', 
    fileFormat='CSV',
    selectors=['departamento', 'fecha', 'temp_celsius'] 
)

tarea_temp.start()
print("Tarea de extracción de temperatura iniciada en Google Earth Engine.")