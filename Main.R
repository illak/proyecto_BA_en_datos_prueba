library(tidyverse)
library(sf)

# Cargamos los datos
data_barrios <- st_read("data/barrios.csv", crs=4326)
data_ciclovias <- st_read("data/ciclovias_WGS84.csv", crs=4326)
data_murales <- st_read("data/murales.csv", crs=4326) # No tiene coords geográficas...
data_espacios <- st_read("data/espacios-culturales/espacios_culturales2021.shp")


# Transformamos a datos geográficos especificando LAT y LON
data_espacios$LATITUD <- as.numeric(data_espacios$LATITUD)
data_espacios$LONGITUD <- as.numeric(data_espacios$LONGITUD)

data_espacios_sin_na <- data_espacios %>% 
  filter(LONGITUD!="")

data_espacios <- st_as_sf(data_espacios_sin_na, coords = c("LATITUD","LONGITUD"))


ciclovias <- data_ciclovias %>% 
  filter(bicisenda %in% c("Ciclovías","Bicisendas"))


# Hacemos un buffer de 100mts
ciclobuffer <- st_buffer(data_ciclovias, 100)

dentro <- lengths(st_intersects(data_espacios, ciclobuffer)) > 0

data_espacios_filtrados <- data_espacios[dentro,] %>% 
  filter(FUNCION_PR %in% c("MONUMENTOS Y LUGARES HISTORICOS",
                           "CENTRO CULTURAL",
                           "BAR",
                           "ESPACIO FERIAL",
                           "BIBLIOTECA",
                           "LIBRERIA"))

ggplot() +
  geom_sf(data = data_barrios, color="grey", fill="#374649") +
  geom_sf(data = ciclovias, color="red") +
  geom_sf(data = data_espacios_filtrados, aes(color=FUNCION_PR)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill="#374649"),
    plot.margin = margin(0,0,0,0)
  )


