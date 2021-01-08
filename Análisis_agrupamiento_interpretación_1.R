### Análisis de agrupamiento (cluster analysis). Interpretación y comparación de resultados.

knitr::opts_chunk$set(fig.width=12, fig.height=8)

##Paquetes

library(vegan)
library(tidyverse)
library(broom)
library(cluster)
library(gclus)
library(pvclust)
library(sf)
source('biodata/funciones.R')

#Cargar datos

load('biodata/Fabaceae-mimosoideae.Rdata')
mi_fam <- mc_fbc_m
load('biodata/matriz_ambiental.Rdata')
mi_fam %>% tibble

bci_env_grid %>% tibble


#Generar matriz de distancias de cuerdas

mi_fam_norm <- decostand(mi_fam, "normalize")
mi_fam_norm_d <- vegdist(mi_fam_norm, "euc")
mi_fam_norm_d %>% tidy

#Interpretación visual de dendrogramas

lista_cl <- list(
  Dendrograma_Enlace_Simple = hclust(mi_fam_norm_d, method = 'single'),
  cl_complete = hclust(mi_fam_norm_d, method = 'complete'),
  cl_upgma = hclust(mi_fam_norm_d, method = 'average'),
  Dendrograma_Enlace_Promedio_WARD = hclust(mi_fam_norm_d, method = 'ward.D2')
)

par(mfrow = c(2,2))
invisible(map(names(lista_cl), function(x) plot(lista_cl[[x]], main = x, hang = -1)))

par(mfrow = c(1,1))

#Solo WARD y SImple

lista_cl <- list(
  Dendrograma_Enlace_Simple = hclust(mi_fam_norm_d, method = 'single'),
  Dendrograma_Enlace_Promedio_WARD = hclust(mi_fam_norm_d, method = 'ward.D2')
)

par(mfrow = c(2,2))
invisible(map(names(lista_cl), function(x) plot(lista_cl[[x]], main = x, hang = -1)))

par(mfrow = c(1,1))

#Seleccionar método de agrupamiento por correlación cofenética.
#Usando la lista de objetos de clústers, calcularé la correlación cofenética dentro de un map

map_df(lista_cl, function(x) {
  coph_d <- cophenetic(x)
  corr <- cor(mi_fam_norm_d, coph_d)
  return(corr)
})

##Elegir número de clústers
##ParaUPGMA

anch_sil_upgma <- calcular_anchuras_siluetas(
  mc_orig = mi_fam, 
  distancias = mi_fam_norm_d, 
  cluster = lista_cl$cl_upgma)
anch_sil_upgma

#Gráfico

u_dend_reord <- reorder.hclust(lista_cl$cl_upgma, mi_fam_norm_d)
plot(u_dend_reord, hang = -1)
rect.hclust(
  tree = u_dend_reord,
  k = anch_sil_upgma$n_grupos_optimo)

#UPGMA
#compararé el dendrograma con el mapa de calor en un mismo gráfico, colocando los dendrogramas en los márgenes del gráfico. 
#Verificaré si el número de grupos hace sentido, recordando los grupos que inicialmente identifiqué.

heatmap(
  as.matrix(mi_fam_norm_d),
  Rowv = as.dendrogram(u_dend_reord),
  symm = TRUE,
  margin = c(3, 3),
  col = rev(cm.colors(4))
)

#Resultado para Ward

anch_sil_ward <- calcular_anchuras_siluetas(
  mc_orig = mi_fam, 
  distancias = mi_fam_norm_d, 
  cluster = lista_cl$cl_ward)
anch_sil_ward

#Gráfico

w_dend_reord <- reorder.hclust(lista_cl$cl_ward, mi_fam_norm_d)
plot(w_dend_reord, hang = -1)
rect.hclust(
  tree = w_dend_reord,
  k = anch_sil_ward$n_grupos_optimo)

#Para más particiones

plot(w_dend_reord, hang = -1)
rect.hclust(
  tree = w_dend_reord,
  k = anch_sil_ward$n_grupos_optimo + 1)

#Dendrograma conmapa de calor_ WARD

heatmap(
  as.matrix(mi_fam_norm_d),
  Rowv = as.dendrogram(w_dend_reord),
  symm = TRUE,
  margin = c(3, 3),
  col = rev(cm.colors(4))
)

##Evaluación mediante remuestreo por bootstrap multiescalar

#UPGMA

cl_pvclust_upgma <-
  pvclust(t(mi_fam_norm),
          method.hclust = "average",
          method.dist = "euc",
          iseed = 91, # Resultado reproducible
          parallel = TRUE)

# Añadir los valores de p
plot(cl_pvclust_upgma, hang = -1)
# Añadir rectángulos a los grupos significativos
lines(cl_pvclust_upgma)
pvrect(cl_pvclust_upgma, alpha = 0.91, border = 4)

##Bootstrap Multiescalar WARD

cl_pvclust_ward <-
  pvclust(t(mi_fam_norm),
          method.hclust = "ward.D2",
          method.dist = "euc",
          iseed = 191, # Resultado reproducible
          parallel = TRUE)

# Añadir los valores de p
plot(cl_pvclust_ward, hang = -1)
# Añadir rectángulos a los grupos significativos
lines(cl_pvclust_ward)
pvrect(cl_pvclust_ward, alpha = 0.91, border = 4)

##Recapitulando los grupos de sitios.

#Patrones comunes y dispares~
#¿Cómo declaro los grupos de sitios?

#UPGMA

(grupos_upgma_k2 <- as.factor(cutree(lista_cl$cl_upgma, k = 2)))

table(grupos_upgma_k2)

#WARD

(grupos_ward_k3 <- as.factor(cutree(lista_cl$cl_ward, k = 3)))
table(grupos_ward_k3)

#Guardaré estos vectores en archivos para reutilizarlos en scripts posteriores:

saveRDS(grupos_upgma_k2, 'grupos_upgma_k2.RDS')
saveRDS(grupos_ward_k3, 'grupos_ward_k3.RDS')
