# Área de cargar paquetes lol
library(vegan)
library(tidyverse)
library(sf)
source('biodata/funciones.R')

# Paquetes para mapa de riqueza y abundancia.

#' ### Cargar paquetes
library(mapview)
library(RColorBrewer)

# Área de cargar datos 
# Mi familia de matas "Fabaceae-mimosoideae"

load('biodata/Fabaceae-mimosoideae.Rdata')
load('biodata/matriz_ambiental.Rdata') #Matriz ambiental, se carga como "bci_env_grid"


#' ### Imprimir datos en pantalla (impresiones parciales con head)
head(censo_fbc_m)
head(mc_fbc_m)
bci_env_grid # No necesita imprimirse parcialmente

#' ### También podemos usar
#' Requiere que se haya cargado ya la colección tidyverse
censo_fbc_m %>% tibble
mc_fbc_m %>% tibble

#' ### Lista de especies
sort(colnames(mc_fbc_m))

#' ### Número de sitios, tanto en matriz de comunidad como en ambiental
#' Verifica que coinciden
nrow(mc_fbc_m) #En la matriz de comunidad
nrow(bci_env_grid) #En la matriz ambiental

#' ### Riqueza numérica de especies (usando matriz de comunidad) por quadrat
#' Nota: cargar paquete vegan arriba, en el área de paquetes
specnumber(mc_fbc_m)
sort(specnumber(mc_fbc_m)) # Ordenados ascendentemente
summary(specnumber(mc_fbc_m)) # Resumen estadístico

#' ### Abundancia de especies por quadrat
sort(rowSums(mc_fbc_m))
summary(rowSums(mc_fbc_m)) # Resumen estadístico

#' ### Abundancia por especie
sort(colSums(mc_fbc_m))
summary(colSums(mc_fbc_m)) # Resumen estadístico

#' ### Riqueza numérica de toda la "comunidad"
specnumber(colSums(mc_fbc_m))

#' ### Abundancia de toda la comunidad
sum(colSums(mc_fbc_m))

#' ### Una tabla para el manuscrito, es necesario asignarle nombre
#' Para esto, usaré la colección "tidyverse"
abun_sp <- censo_fbc_m %>%
  group_by(Latin) %>% 
  count() %>% 
  arrange(desc(n))
abun_sp

#' ### Un gráfico para el manuscrito
#' Gráfico de mosaicos de la abundancia por especie por cuadros
abun_sp_q <- crear_grafico_mosaico_de_mc(mc_fbc_m, tam_rotulo = 12)
abun_sp_q
