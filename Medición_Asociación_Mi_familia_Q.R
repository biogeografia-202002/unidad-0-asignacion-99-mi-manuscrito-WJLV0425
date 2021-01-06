## Modo Q

knitr::opts_chunk$set(fig.width=12, fig.height=8)

## Preámbulo, Paquetes a cargar.

library(vegan)
library(adespatial)
library(broom)
library(tidyverse)
library(sf)
library(cluster)
library(gclus)

source('biodata/funciones.R')

## Cargar datos

load('biodata/matriz_ambiental.Rdata')
load('biodata/Fabaceae-mimosoideae.Rdata')

## Modo Q, matrices de disimilaridad entre objetos

# Modo Q para datos cuantitativos de especies (abundancia)
# Aplicado a mi familia, en la forma de matriz de distancia euclídea, utilizando la transformación Hellinger

mi_fam_d_hel <- dist.ldc(mc_fbc_m, "hellinger", silent = T)
mi_fam_d_hel %>% tidy # Para evitar desbordar la consola

#Para interpretar esta matriz, es necesario representarla gráficamente.
#color fucsia (magenta, rosa) significa “corta distancia=muy similares”, y cian (celeste) significa “gran distancia=poco similares”.

coldiss(mi_fam_d_hel, diag = T)

#Puedes guardar el gráfico usando el botón Export de la pestaña Plots

#Una forma alterna de guardar el gráfico es mediante funciones de R. 
#A continuación uso una de ellas, la función png; la cual debe cerrar en dev.off.

png(
  filename = 'matriz_disimilaridad_hellinger.png',
  width = 2400, height = 1200, pointsize = 32
)
coldiss(mi_fam_d_hel, diag = T)
dev.off()

#Un error común consiste en referirse a los índices de Jaccard y de Sorensen “a secas”, sin especificar si se trata de disimilaridad (distancia) o de similaridad.
#Recalco: es imprescindible declarar qué tipo de métrica estás usando. 
#Correcto: “índice de disimilaridad de Jaccard”, “índice de similaridad de Sorensen”, o simplemente “similaridad de Jaccard”, “distancia de Jaccard”.
#Incorrecto: “índice de Jaccard”, “índice de Sorensen”.


## A continuación, muestro cómo calcular la distancia de Jaccard (DJ) en un único paso usando la función vegdist.

mi_fam_jac <- vegdist(mc_fbc_m, method = 'jac', binary = T)
mi_fam_jac %>% tidy # Mostrando sólo las primeras 10 combinaciones en modo data.frame

#El argumento binary=T en vegdist “ordena” que se realice primero decostand(mc_apcyn_melic_saptc, method = 'pa'), 
#lo cual convierte la matriz de comunidad en una de presencia/ausencia, 
#con la que posteriormente se calculará la matriz de distancia.

coldiss(mi_fam_jac, diag = T)

png(
  filename = 'matriz_distancia_Jaccard.png',
  width = 2400, height = 1200, pointsize = 32
)
coldiss(mi_fam_jac, diag = T)
dev.off()

#Como la distancia de Jaccard (DJ) es el complemento a 1 de la similaridad de Jaccard (SJ), es decir, 
#DJ=1-SJ, y dado que arriba calculamos la distancia, para obtener la similaridad, 
#sólo hay que restarle el valor de distancia a 1 (SJ=1-DJ).

(1 - mi_fam_jac) %>% tidy %>% rename(similaridad=distance) #Similaridad

#La fórmula de la similaridad de Jaccard es SJ=a/(a+b+c), donde a es el número de
#especies compartidas (presentes en ambos sitios comparados), b el número de especies
#exclusivas del sitio 2, y c el número de especies exclusivas del sitio 1.

#Para obtener las variables a, b y c, usaré La función betadiver del paquete vegan:

mi_fam_abc <- betadiver(mc_fbc_m) 
mi_fam_abc %>%
  map(tidy) %>%
  map(slice, 1) %>%
  map_df(I, .id = 'tipo') %>% 
  dplyr::select(tipo, n_especies=distance)

#Con betadiver también puedes calcular índices de similaridad. Por ejemplo, el Jaccard se calcula así:

betadiver(mc_fbc_m, method = 'j') %>% tidy

#Además de la distancia de Jaccard, otra distancia muy utilizada es la de Sorensen o Bray-Curtis. 
#Se calcula fácilmente con la función vegdist:

mi_fam_sor <- vegdist(mc_fbc_m, method = 'bray', binary = T)
mi_fam_sor %>% tidy

coldiss(mi_fam_sor, diag = T)

png(
  filename = 'matriz_distancia_Sorensen.png',
  width = 2400, height = 1200, pointsize = 32
)
coldiss(mi_fam_sor, diag = T)
dev.off()

## Modo Q para datos cuantitativos, NO de abundancia de especies (variables ambientales).
#En este ejemplo, usaré sólo variables de suelo, todas cuantitativas, puedes combinar
#con otras variables que hayas detectado como relevantes en el análisis de correlación.
#Nota que convertiré cada variable en puntuaciones z mediante la función scale. 
#Dado que cada variable tiene su propia escala de medición, 
#si se compararan sin transformación, se obtendrían resultados inconsistentes.

env_suelo_punt_z <- bci_env_grid %>%
  st_drop_geometry() %>% 
  dplyr::select(matches('^[A-T,Z]|^pH$', ignore.case = F)) %>% 
  scale()
env_suelo_punt_z_d <- dist(env_suelo_punt_z)
env_suelo_punt_z_d %>% tidy

coldiss(env_suelo_punt_z_d, diag = T)

png(
  filename = 'matriz_proximidad_variables_suelo.png',
  width = 2400, height = 1200, pointsize = 32
)
coldiss(env_suelo_punt_z_d, diag = T)
dev.off()

## Modo Q para datos cualitativos y cuantitativos (mixtos), NO de abundancia de especies (variables ambientales).

#Hetereogeneidad_ambiental. Índice cuantitativo calculado como la diversidad de Simpson a partir de frecuencias de tipos de micro-hábitats.
#Habitat. Tipo de hábitat. Asume los siguientes valores posibles: OldHigh, OldLow y OldSlope (bosque viejo en relieve alto, en vertientes y relieve bajo, respectivamente), Swamp (bosque en área encharcable) Young (bosque joven).
#Quebrada. Informa sobre si hay o no quebrada. Los valores posibles son Yes o No.

env_mix <- bci_env_grid %>%
  st_drop_geometry() %>%
  dplyr::select(heterogeneidad_ambiental, habitat, quebrada)
env_mix_d <- daisy(x = env_mix, metric = 'gower')
env_mix_d %>% as.dist %>% tidy

env_mix_d %>% coldiss(diag = T)

png(
  filename = 'matriz_proximidad_variables_mixtas.png',
  width = 2400, height = 1200, pointsize = 32
)
env_mix_d %>% coldiss(diag = T)
dev.off()
