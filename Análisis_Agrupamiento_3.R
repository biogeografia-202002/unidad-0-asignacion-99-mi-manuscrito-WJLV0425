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

(m_amb_upgma_k2 <- bci_env_grid %>%
    select_if(is.numeric) %>% select(-id) %>% 
    mutate(grupos_upgma_k2) %>%
    st_drop_geometry() %>% 
    pivot_longer(-grupos_upgma_k2, names_to = "variable", values_to = "valor"))