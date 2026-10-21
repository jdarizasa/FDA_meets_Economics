# Load libraries and data -------------------------------------------------

library(fda)
library(refund)
library(readr)
library(dplyr)
library(lubridate)
library(tidyverse)

DatosMeteo_raw <- read_csv("Data_Raw/Panel_Climatico_Deptos_2018_2026.csv", 
                  col_types = cols(fecha = col_datetime(format = "%Y-%m-%d")))
IPC_raw <- read_csv("Data_Raw/IPC_Alimentos_Ciudades_Base100.csv", 
                    col_types = cols(fecha = col_datetime(format = "%Y-%m-%d")))


# Cundinamarca  ------------------------------------------

# Data extraction and quality

CundiPrecip_df <- DatosMeteo_raw[DatosMeteo_raw$departamento=="Cundinamarca",c(2,3)]
CundiTemp_df <- DatosMeteo_raw[DatosMeteo_raw$departamento=="Cundinamarca",c(2,4)]

CundiPrecip_df <- CundiPrecip_df %>%
  mutate(
    anio = year(fecha),
    mes  = month(fecha),
    dia  = day(fecha),
    anio_mes = paste(anio, sprintf("%02d", mes), sep = "-")
  )%>%
  group_by(anio_mes) %>%
  ungroup()

CundiTemp_df <- CundiTemp_df %>%
  mutate(
    anio = year(fecha),
    mes  = month(fecha),
    dia  = day(fecha),
    anio_mes = paste(anio, sprintf("%02d", mes), sep = "-")
  )%>%
  group_by(anio_mes) %>%
  ungroup()

# Smoothing ---------------------------------------------------------------

# Precipitacion Cundinamarca

## Creacion de Matriz
CundiPrecip_df_ancho <- CundiPrecip_df %>%
  select(dia, anio_mes, precip_mm) %>%
  pivot_wider(names_from = anio_mes, values_from = precip_mm) %>%
  arrange(dia)

matriz_precip <- as.matrix(CundiPrecip_df_ancho[, -1])
set.seed(2026)
matriz_precip <- apply(matriz_precip, 2, function(x) {
  if (any(is.na(x))) {
    ifelse(!is.na(x),x,rnorm(length(x[is.na(x)]),0,1)^2)
  } else {
    x
  }
})

## Hiperparametros smoothing

dia_rango <- range(CundiPrecip_df_ancho$dia)
wbasis <- create.bspline.basis(
  rangeval = dia_rango,
  nbasis = 12
)

CundiPrecipSmooth <- smooth.basis(CundiPrecip_df_ancho$dia, matriz_precip, wbasis)

CundiPrecipfd <- CundiPrecipSmooth$fd
plot(CundiPrecipfd)

save(CundiPrecipfd, matriz_precip, file = "Data_processed/CundiPrecip_fd.RData")

# Temperatura Cundinamarca

## Creación de Matriz
CundiTemp_df_ancho <- CundiTemp_df %>%
  select(dia, anio_mes, temp_celsius) %>%
  pivot_wider(names_from = anio_mes, values_from = temp_celsius) %>%
  arrange(dia)

matriz_temp <- as.matrix(CundiTemp_df_ancho[, -1])
set.seed(2026)
matriz_temp <- apply(matriz_temp, 2, function(x) {
  if (any(is.na(x))) {
    ifelse(!is.na(x),x,rnorm(length(x[is.na(x)]),17.5,0.8))
  } else {
    x
  }
})

## Hiperparametros smoothing

dia_rango <- range(CundiTemp_df_ancho$dia)
wbasis <- create.bspline.basis(
  rangeval = dia_rango,
  nbasis = 12
)

CundiTempSmooth <- smooth.basis(CundiTemp_df_ancho$dia, matriz_temp, wbasis)

CundiTempfd <- CundiTempSmooth$fd
plot(CundiTempfd)

save(CundiTempfd, matriz_temp, file = "Data_processed/CundiTemp_fd.RData")

# Departamentos Vecinos ---------------------------------------------------


Vecinos <- c("Boyaca", "Huila", "Meta", "Tolima")
Distancias <- c(1/139, 1/315, 1/124, 1/202)

# Precipitacion
VecinosPrecip_df <- DatosMeteo_raw %>% 
  select(departamento, fecha, precip_mm) %>%
  filter(departamento %in% Vecinos)

VecinosPrecip_df <- VecinosPrecip_df %>%
  select(fecha, departamento, precip_mm) %>%
  pivot_wider(
    names_from = departamento,
    values_from = precip_mm
  ) %>%
  arrange(fecha)

VecinosPrecip_df$"Ponderada" <- (Distancias[1]*VecinosPrecip_df[,Vecinos[1]]+
                                        Distancias[2]*VecinosPrecip_df[,Vecinos[2]]+
                                        Distancias[3]*VecinosPrecip_df[,Vecinos[3]]+
                                        Distancias[4]*VecinosPrecip_df[,Vecinos[4]])/sum(Distancias)

VecinosPrecip_df <- VecinosPrecip_df %>%
  select(fecha, Ponderada) %>%
  mutate(
    anio = year(fecha),
    mes  = month(fecha),
    dia  = day(fecha),
    anio_mes = paste(anio, sprintf("%02d", mes), sep = "-")
  )

# Temperatura
VecinosTemp_df <- DatosMeteo_raw %>%
  select(departamento, fecha, temp_celsius) %>%
  filter(departamento %in% Vecinos)

VecinosTemp_df <- VecinosTemp_df %>%
  select(fecha, departamento, temp_celsius) %>%
  pivot_wider(
    names_from = departamento,
    values_from = temp_celsius
  ) %>%
  arrange(fecha)

VecinosTemp_df$"Ponderada" <- (Distancias[1]*VecinosTemp_df[,Vecinos[1]]+
                                   Distancias[2]*VecinosTemp_df[,Vecinos[2]]+
                                   Distancias[3]*VecinosTemp_df[,Vecinos[3]]+
                                   Distancias[4]*VecinosTemp_df[,Vecinos[4]])/sum(Distancias)

VecinosTemp_df <- VecinosTemp_df %>%
  select(fecha, Ponderada) %>%
  mutate(
    anio = year(fecha),
    mes  = month(fecha),
    dia  = day(fecha),
    anio_mes = paste(anio, sprintf("%02d", mes), sep = "-")
  )

# Smoothing ---------------------------------------------------------------

# Precipitacion Vecinos

## Creacion de Matriz
VecinosPrecip_df_ancho <- VecinosPrecip_df %>%
  select(dia, anio_mes, Ponderada) %>%
  pivot_wider(names_from = anio_mes, values_from = Ponderada) %>%
  arrange(dia)

matriz_Precip_v <- as.matrix(VecinosPrecip_df_ancho[, -1])
set.seed(2026)
matriz_Precip_v <- apply(matriz_Precip_v, 2, function(x) {
  if (any(is.na(x))) {
    ifelse(!is.na(x),x,rnorm(length(x[is.na(x)]),0,1)^2)
  } else {
    x
  }
})

## Hiperparametros smoothing

dia_rango <- range(VecinosPrecip_df_ancho$dia)
wbasis <- create.bspline.basis(
  rangeval = dia_rango,
  nbasis = 12
)

PrecipSmooth_v <- smooth.basis(VecinosPrecip_df_ancho$dia, matriz_Precip_v, wbasis)

VecinosPrecipfd <- PrecipSmooth_v$fd
plot(VecinosPrecipfd)

save(VecinosPrecipfd, matriz_Precip_v, file = "Data_processed/VecinosPrecip_fd.RData")

# Temperatura Vecinos
## Creacion de Matriz
VecinosTemp_df_ancho <- VecinosTemp_df %>%
  select(dia, anio_mes, Ponderada) %>%
  pivot_wider(names_from = anio_mes, values_from = Ponderada) %>%
  arrange(dia)

matriz_Temp_v <- as.matrix(VecinosTemp_df_ancho[, -1])
set.seed(2026)
matriz_Temp_v <- apply(matriz_Temp_v, 2, function(x) {
  if (any(is.na(x))) {
    ifelse(!is.na(x),x,rnorm(length(x[is.na(x)]),20,1))
  } else {
    x
  }
})

## Hiperparametros smoothing

dia_rango <- range(VecinosTemp_df_ancho$dia)
wbasis <- create.bspline.basis(
  rangeval = dia_rango,
  nbasis = 12
)

TempSmooth_v <- smooth.basis(VecinosTemp_df_ancho$dia, matriz_Temp_v, wbasis)

VecinosTempfd <- TempSmooth_v$fd
plot(VecinosTempfd)

save(VecinosTempfd, matriz_Temp_v, file = "Data_processed/VecinosTemp_fd.RData")


# Ciclo FOR ---------------------------------------------------------------

Vecinos <- c("Boyaca", "Huila", "Meta", "Tolima")

# Mapeo para el nombre completo del departamento (ajusta según tus datos)
nombres_departamento <- c(
  "Boyaca"  = "Boyaca",
  "Huila"   = "Huila",
  "Meta"    = "Meta",
  "Tolima"  = "Tolima"
)

for (dep in Vecinos) {
  
  # Nombre completo del departamento (para filtrar)
  dep_full <- nombres_departamento[[dep]]
  
  # ---------------------------------------------------------
  # 1. Extracción y preparación de datos
  # ---------------------------------------------------------
  Precip_df <- DatosMeteo_raw[DatosMeteo_raw$departamento == dep_full, c(2, 3)]
  Temp_df   <- DatosMeteo_raw[DatosMeteo_raw$departamento == dep_full, c(2, 4)]
  
  Precip_df <- Precip_df %>%
    mutate(
      anio = year(fecha),
      mes  = month(fecha),
      dia  = day(fecha),
      anio_mes = paste(anio, sprintf("%02d", mes), sep = "-")
    ) %>%
    group_by(anio_mes) %>%
    ungroup()
  
  Temp_df <- Temp_df %>%
    mutate(
      anio = year(fecha),
      mes  = month(fecha),
      dia  = day(fecha),
      anio_mes = paste(anio, sprintf("%02d", mes), sep = "-")
    ) %>%
    group_by(anio_mes) %>%
    ungroup()
  
  # ---------------------------------------------------------
  # 2. PRECIPITACIÓN
  # ---------------------------------------------------------
  Precip_df_ancho <- Precip_df %>%
    select(dia, anio_mes, precip_mm) %>%
    pivot_wider(names_from = anio_mes, values_from = precip_mm) %>%
    arrange(dia)
  
  matriz_precip <- as.matrix(Precip_df_ancho[, -1])
  set.seed(2026)
  matriz_precip <- apply(matriz_precip, 2, function(x) {
    if (any(is.na(x))) {
      ifelse(!is.na(x), x, rnorm(length(x[is.na(x)]), 0, 1)^2)
    } else {
      x
    }
  })
  
  # Hiperparámetros smoothing
  dia_rango <- range(Precip_df_ancho$dia)
  wbasis <- create.bspline.basis(rangeval = dia_rango, nbasis = 12)
  
  PrecipSmooth <- smooth.basis(Precip_df_ancho$dia, matriz_precip, wbasis)
  Precipfd <- PrecipSmooth$fd
  
  # Guardar con nombre dinámico
  assign(paste0(dep, "Precipfd"), Precipfd)
  assign(paste0(dep, "matriz_precip"), matriz_precip)
  save(
    list = c(paste0(dep, "Precipfd"), paste0(dep, "matriz_precip")),
    file = paste0("Data_processed/", dep, "Precip_fd.RData")
  )
  
  # ---------------------------------------------------------
  # 3. TEMPERATURA
  # ---------------------------------------------------------
  Temp_df_ancho <- Temp_df %>%
    select(dia, anio_mes, temp_celsius) %>%
    pivot_wider(names_from = anio_mes, values_from = temp_celsius) %>%
    arrange(dia)
  
  matriz_temp <- as.matrix(Temp_df_ancho[, -1])
  set.seed(2026)
  matriz_temp <- apply(matriz_temp, 2, function(x) {
    if (any(is.na(x))) {
      ifelse(!is.na(x), x, rnorm(length(x[is.na(x)]), 17.5, 0.8))
    } else {
      x
    }
  })
  
  # Hiperparámetros smoothing
  dia_rango <- range(Temp_df_ancho$dia)
  wbasis <- create.bspline.basis(rangeval = dia_rango, nbasis = 12)
  
  TempSmooth <- smooth.basis(Temp_df_ancho$dia, matriz_temp, wbasis)
  Tempfd <- TempSmooth$fd
  
  # Guardar con nombre dinámico
  assign(paste0(dep, "Tempfd"), Tempfd)
  assign(paste0(dep, "matriz_temp"), matriz_temp)
  save(
    list = c(paste0(dep, "Tempfd"), paste0(dep, "matriz_temp")),
    file = paste0("Data_processed/", dep, "Temp_fd.RData")
  )
  
  # Mensaje de progreso
  message("✅ Procesado: ", dep_full)
}

