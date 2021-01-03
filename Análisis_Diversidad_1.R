### ANÁLISIS DIVERSIDAD ALPHA

knitr::opts_chunk$set(fig.width=12, fig.height=8)

##Paquetes

library(vegan)
library(adespatial)
library(plyr)
library(RColorBrewer)
library(tidyverse)
library(sf)
library(SpadeR)
library(iNEXT)
source('biodata/funciones.R')

##Datos

load('biodata/Fabaceae-mimosoideae.Rdata')
load('biodata/matriz_ambiental.Rdata')
mi_fam <- mc_fbc_m
bci_env_grid %>% tibble

##TRABAJANDO SOLO CON WARD PORQUE MI UPGMA FUNDIÓ.

grupos_ward_k3 <- readRDS('grupos_ward_k3.RDS')
table(grupos_ward_k3)

##DIVERSIDAD ALPHA: ÍNDICES, ENTROPÍAS, EQUIDADES, RATIOS.

(indices <- alpha_div(mi_fam))

pairs(indices,
      lower.panel = panel.smooth,
      upper.panel = panel.cor,
      diag.panel = panel.hist,
      main = "Pearson Correlation Matrix")

indices_env <- bind_cols(
  indices,
  bci_env_grid %>%
    select_if(is.numeric) %>%
    st_drop_geometry %>%
    select(-id) %>% 
    select(-matches('^geom.*pct$')))
indices_env %>% tibble

ezCorM(indices_env, r_size_lims = c(3,5), label_size = 4)

##Modelos de abundancia de especies

mi_fam_mae <- radfit(mi_fam)
plot(mi_fam_mae)

##Rarefacción
#Riqueza por sitios

riqueza <- specnumber(mi_fam)
riqueza %>% sort

#Sitios con riqueza mínima y máxima.

riqueza[riqueza == min(riqueza)]
riqueza[riqueza == max(riqueza)]
range(riqueza)

#Abundancia por sitios

abundancia <- rowSums(mi_fam)
abundancia %>% sort

#Sitios con abundancias mínima y máxima

abundancia[abundancia == min(abundancia)]
abundancia[abundancia == max(abundancia)]
(rango_abun <- range(abundancia))

#Abundancia en el sitio más pobre

abundancia[riqueza == min(riqueza)]

#Abundancia en el sitio más rico

abundancia[riqueza == max(riqueza)]

#Riqueza en el sitio con menor abundancia

riqueza[abundancia == min(abundancia)]

#Riqueza en el sitio con mayor abundancia

riqueza[abundancia == max(abundancia)]

#Rarefacción a la abundancia más pequeña encontrada

riqueza_menor_abun <- rarefy(mi_fam, sample = rango_abun[1])
sort(riqueza)

sort(round(riqueza_menor_abun))

rarecurve(
  mi_fam,
  step = 1,
  sample = rango_abun[1],
  xlab = "Número de individuos (tamaño de muestra)",
  ylab = "Especies",
  label = TRUE,
  col = "blue"
)

##Riqueza de especies, estimación y comparación, “completitud de muestra”. (Chao y Chiu, 2016)

#Aproximación básica:
  
specpool(mi_fam)

specpool(mi_fam)[[1]]/specpool(mi_fam)*100

##Enfoques asintóticos y no asintóticos aplicados a la matriz de comunidad combinada
#Generar la matriz de comunidad combinada, en la que todos los sitios forman uno.

mi_fam_combinada <- colSums(mi_fam)
mi_fam_combinada %>% sort  

mi_fam_combinada_chao <- estimacion_riqueza_chao(
  mc = mi_fam_combinada,
  n_raras = 10)
mi_fam_combinada_chao$asintoticos_estimacion

mi_fam_combinada_chao$no_asintoticos_rarefaccion_extrapolacion
mi_fam_combinada_chao$no_asintoticos_rarefaccion_extrapolacion_grafico

##Enfoques asintóticos y no asintóticos aplicados a una matriz de comunidad agrupada

mi_fam_k3 <- mi_fam %>%
  mutate(g=grupos_ward_k3) %>%
  group_by(g) %>%
  summarise_all(sum) %>%
  select(-g) %>% 
  data.frame
mi_fam_k3 %>% rowSums %>% sort

mi_fam_k3_chao <- estimacion_riqueza_chao(
  mc = mi_fam_k3,
  n_raras = 10)

mi_fam_k3_chao$asintoticos_estimacion
mi_fam_k3_chao$no_asintoticos_rarefaccion_extrapolacion

#Gráfico

mi_fam_k3_chao$no_asintoticos_rarefaccion_extrapolacion_grafico


