### Medición de asociación. Modo R aplicado a mi familia Fabaceae Mimosoideae

knitr::opts_chunk$set(fig.width=12, fig.height=8)

##Paquetes

library(vegan)
library(adespatial)
library(broom)
library(tidyverse)
library(sf)
library(gclus)
source('biodata/funciones.R')

##Datos

load('biodata/matriz_ambiental.Rdata')
load('biodata/Fabaceae-mimosoideae.Rdata')

## Modo R: matrices de dependencia entre variables (índice de correlación)
##Modo R para datos cuantitativos de especies (abundancia)
#Compararé el grado de asociación entre especies, NO entre sitios.
#En modo R comparamos descriptores, no objetos. Especies, no cuadros.

#Primero, sustituyo el caracter de espacio por un en los nombres de las especies 
#(caracter \n), para facilitar la lectura de los nombres. Luego transpogo la Matriz.

mi_fam_t <- mc_fbc_m %>% 
  rename_all(gsub, pattern = ' ', replacement = '\n') %>% 
  t()
mi_fam_t %>% tibble

#Segundo, transformo la matriz transpuesta usando estandarización Chi.

mi_fam_t_chi <- decostand(mi_fam_t, "chi.square")
mi_fam_t_chi %>% tibble

#Tercero, calculo la distancia euclídea.

mi_fam_t_chi_d <- dist(mi_fam_t_chi)
mi_fam_t_chi_d %>% tidy

#Finalmente, creo el “mapa de calor”.

coldiss(mi_fam_t_chi_d, diag = TRUE)

png(
  filename = 'Modo_R_correlación_especies_abun_.png',
  width = 2400, height = 1200, pointsize = 32
)
coldiss(mi_fam_t_chi_d, diag = T)
dev.off()

#Modo R para datos binarios (presencia/ausencia)

mi_fam_t_jac <- vegdist(mi_fam_t, "jaccard", binary = TRUE)
mi_fam_t_jac %>% tidy

coldiss(mi_fam_t_jac, diag = TRUE)

png(
  filename = 'Modo_R_Presencia_Ausencia_Especies.png',
  width = 2400, height = 1200, pointsize = 32
)
coldiss(mi_fam_t_jac, diag = T)
dev.off()

#Modo R para datos cuantitativos, NO de abundancia de especies (variables ambientales)

env_num <- bci_env_grid %>%
  dplyr::select_if(is.numeric) %>%
  dplyr::select(-id, -matches('^U.*')) %>% 
  st_drop_geometry %>% 
  mutate(
    riqueza_mifam = specnumber(mc_fbc_m),
    abundancia_mifam = rowSums(mc_fbc_m)) %>% 
  rename_all(gsub, pattern = '_pct$', replacement = '') %>% 
  rename_all(gsub, pattern = '_| ', replacement = '\n')
env_num %>% tibble


p_cor_suelo_ar <- env_num %>%
  dplyr::select(matches('^[A-T,Z]|abundancia|riqueza|^pH$', ignore.case = F)) %>%
  ezCorM(r_size_lims = c(4,8), label_size = 3, method = 'pearson')

p_cor_suelo_ar


p_cor_suelo_ar_spearman <- env_num %>%
  dplyr::select(matches('^[A-T,Z]|abundancia|riqueza|^pH$', ignore.case = F)) %>%
  ezCorM(r_size_lims = c(4,8), label_size = 3, method = 'spearman')
p_cor_suelo_ar_spearman

png(
  filename = 'matriz_correlacion_suelo_abun_riq_spearman.png',
  width = 1920, height = 1080, res = 125
)
p_cor_suelo_ar_spearman
dev.off()

#PNG

p_cor_geomorf_ar <- env_num %>%
  dplyr::select(-matches('^[A-T,Z]|pH', ignore.case = F)) %>%
  ezCorM(r_size_lims = c(4,8), label_size = 3, method = 'pearson')
p_cor_geomorf_ar

p_cor_geomorf_ar_spearman <- env_num %>%
  dplyr::select(-matches('^[A-T,Z]|pH', ignore.case = F)) %>%
  ezCorM(r_size_lims = c(4,8), label_size = 3, method = 'spearman')
p_cor_geomorf_ar_spearman

png(
  filename = 'matriz_correlacion_geomorf_abun_riq_spearman.png',
  width = 1920, height = 1080, res = 110
)
p_cor_geomorf_ar_spearman
dev.off()

