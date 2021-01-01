###ANÁLISIS DE AGRUMAMIENTO

## Paquetes

library(vegan)
library(magrittr)
library(broom)
source('biodata/funciones.R')

##Datos

load('biodata/Fabaceae-mimosoideae.Rdata')
mi_fam <- mc_fbc_m

##Agrupamiento “aglomerativo” por enlace simple

mi_fam_norm <- decostand(mi_fam, "normalize")
mi_fam_norm_d <- vegdist(mi_fam_norm, "euc")
mi_fam_norm_d %>% tidy

attr(mi_fam_norm_d, "labels") <- rownames(mi_fam)
(cl_single <- hclust(mi_fam_norm_d, method = 'single'))

#Dendrograma

plot(cl_single, labels = rownames(mi_fam), hang = -1,
     main = "Sitios de BCI según composición de especies de Fabaceae Mimosoideae\n Enlace simple a partir de matriz de distancia de cuerdas",
     xlab = 'Sitios', ylab = 'Altura')

##Agrupamiento “aglomerativo” por enlace completo

(cl_complete <- hclust(mi_fam_norm_d, method = 'complete'))

plot(cl_complete, labels = rownames(mi_fam), hang = -1,
     main = "Sitios de BCI según composición de especies de Fabaceae Mimosoideae\nEnlace completo a partir de matriz de distancia de cuerdas",
     xlab = 'Sitios', ylab = 'Altura')

##Agrupamiento “aglomerativo” por enlace promedio

(cl_upgma <- hclust(mi_fam_norm_d, method = 'average'))

plot(cl_upgma, labels = rownames(mi_fam), hang = -1,
     main = "Sitios de BCI según composición de especies de Fabaceae Mimosoideae\nUPGMA a partir de matriz de distancia de cuerdas",
     xlab = 'Sitios', ylab = 'Altura')

##Agrupamiento por el método de Ward de varianza mínima

(cl_ward <- hclust(mi_fam_norm_d, method = 'ward.D2'))
plot(cl_ward, labels = rownames(mi_fam), hang = -1,
     main = "Sitios de BCI según composición de especies de Fabaceae Mimosoideae\nMétodo de Ward a partir de matriz de distancia de cuerdas",
     xlab = 'Sitios', ylab = 'Altura')