###Análisis de agrupamiento (cluster analysis).
###Parte 3: Grupos (clústers), variables ambientales y mapas

knitr::opts_chunk$set(fig.width=12, fig.height=8)

##Paquetes

library(mapview)
library(tidyverse)
library(sf)
library(RColorBrewer)
source('biodata/funciones.R')

##Cargar Datos

load('biodata/Fabaceae-mimosoideae.Rdata')
load('biodata/matriz_ambiental.Rdata')
grupos_upgma_k2 <- readRDS('grupos_upgma_k2.RDS')
table(grupos_upgma_k2) #Importante, tener en cuenta los desiguales tamaños de los grupos

grupos_ward_k3 <- readRDS('grupos_ward_k3.RDS')
table(grupos_ward_k3)

##Paletas

rojo <- colorRampPalette(brewer.pal(8, "Reds"))
rojo_inv <- colorRampPalette(rev(brewer.pal(8, "Reds")))
colores_grupos <- brewer.pal(8, "Accent")

##EXPLORACIÓN DE EFECTOS
#Pruebas de igualdad de promedios de las variables entre 2 grupos.

##UPGMA DESCARTADO, PROBAR CON WARD

(m_amb_ward_k3 <- bci_env_grid %>%
    select_if(is.numeric) %>% select(-id) %>% 
    mutate(grupos_ward_k3) %>%
    st_drop_geometry() %>% 
    pivot_longer(-grupos_ward_k3, names_to = "variable", values_to = "valor"))

#A continuación, las pruebas (ANOVA (evalúa homogeneidad de medias; 
  #no se cumplen muchos de los supuestos requeridos para esta prueba) 
  #y Kruskal-Wallis (evalúa homogeneidad de medianas)):

m_amb_ward_k3 %>% 
  group_by(variable) %>% 
  summarise(
    p_valor_a = oneway.test(valor ~ grupos_ward_k3)$p.value,
    p_valor_k = kruskal.test(valor ~ grupos_ward_k3)$p.value) %>%
  arrange(p_valor_k) %>%
  print(n=Inf)

#Gráficos

m_amb_ward_k3 %>% 
  group_by(variable) %>% 
  ggplot() + aes(x = grupos_ward_k3, y = valor, fill = grupos_ward_k3) + 
  geom_boxplot() + 
  scale_fill_brewer(palette = 'Accent') +
  theme_bw() +
  theme(legend.position="none") +
  facet_wrap(~ variable, scales = 'free_y')

#Mapa

mapa_ward_k3 <- mapView(
  bci_env_grid %>% mutate(grupos_ward_k3),
  layer.name = 'Grupos (3) Ward',
  alpha.regions = 0.6,
  map.types = 'OpenTopoMap',
  legend = T,
  col.regions = colores_grupos[1:3],
  zcol = 'grupos_ward_k3') %>%
  addStaticLabels(label = bci_env_grid$id) %>% 
  leaflet::setView(
    lng = -79.85136,
    lat = 9.15097,
    zoom = 15)
mapa_ward_k3

mapa_ward_k3 %>% mapshot(
  file = 'mapa_ward_k3.png', 
  remove_controls = c("zoomControl", "layersControl", "homeButton")
)

##CASO DE JR, EJEMPLO
#Mapa de una de las variables donde se presentó 
  #efecto de su promedio (p<0.01), en este caso, Zinc (Zn)

#MAPA VARIABLE pH

mapa_ph <- mapView(
  bci_env_grid,
  layer.name = 'pH',
  alpha.regions = 0.6,
  map.types = 'OpenTopoMap',
  legend = T,
  col.regions = rojo_inv,
  zcol = 'pH') %>%
  addStaticLabels(label = bci_env_grid$id) %>% 
  leaflet::setView(
    lng = -79.85136,
    lat = 9.15097,
    zoom = 15)
mapa_ph

mapa_ph %>% mapshot(
  file = 'mapa_ph.png', 
  remove_controls = c("zoomControl", "layersControl", "homeButton")
)