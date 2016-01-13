install.packages("fpc")
library("fpc")
library("cluster")

test=read.csv(file="/home/germaaan/proyectos/TID/test.csv", header=TRUE, sep=",", dec=",")

muestra=sample(1:dim(test)[1], 150)

test2=test[muestra, -c(1)]
test2$Sexo=ifelse(test2$Sexo == "Hombre", 1, 0)
test2$Soltero=ifelse(test2$EstadoCivil == "Soltero", 1, 0)
test2$Casado=ifelse(test2$EstadoCivil == "Casado", 1, 0)
test2$Otro=ifelse(test2$EstadoCivil == "Otro", 1, 0)
test2$Primarios=ifelse(test2$Estudios == "Primarios", 1, 0)
test2$Secundarios=ifelse(test2$Estudios == "Secundarios", 1, 0)
test2$Universitarios=ifelse(test2$Estudios == "Universitarios", 1, 0)
test2$EstadoCivil=NULL
test2$Estudios=NULL

# Distancia numéricos
numericos=data.frame(test2[, c(1, 3:5)])
for (j in 1:4) {x=numericos[,j] ; v=(x-mean(x))/sqrt(var(x)); numericos[,j]=v} # Normalizar valores
distancia_numericos=dist(numericos, method="euclidean")

# Distancia binarios television
binarios_tv=data.frame(test2[, -c(1, 3:5, 6)])
distancia_binarios_tv=dist(binarios_tv, method="binary")

# Distancia binarios redes sociales
binarios_rrss=data.frame(test2[, -c(1, 3:5, 7)])
distancia_binarios_rrss=dist(binarios_rrss, method="binary")

# Calculo distancia ponderada
distancia_ponderada_tv=(distancia_numericos+distancia_binarios_tv)/2
distancia_ponderada_rrss=(distancia_numericos+distancia_binarios_rrss)/2


### AGRUPACIÓN JERARQUICA POR EL MÉTODO DE WARD
## Uso de televisión
jerarquica_tv=hclust(distancia_ponderada_tv, method="ward.D2")
plot(jerarquica_tv, main="Dendograma agrupación jerarquica: Uso televisión", labels=ifelse(test[muestra, ]$UsoTV == 1, "Usa TV", "No usa TV"))
rect.hclust(jerarquica_tv, k=2)

# Variables para agrupamiento
agrupacion_jerarquica_tv=cutree(jerarquica_tv, k=2)

# Análisis de bondad
plotcluster(numericos, agrupacion_jerarquica_tv)
plotcluster(binarios_tv, agrupacion_jerarquica_tv)

# Coeficiente de silueta
silueta_jerarquica_tv=silhouette(agrupacion_jerarquica_tv, distancia_ponderada_tv)
plot(silueta_jerarquica_tv, col=1:2)


## Uso de redes sociales
jerarquica_rrss=hclust(distancia_ponderada_rrss, method="ward.D2")
plot(jerarquica_rrss, main="Dendograma agrupación jerarquica: Uso redes sociales", labels=ifelse(test[muestra, ]$UsoRRSS == 1, "Usa RRSS", "No usa RRSS"))
rect.hclust(jerarquica_rrss, k=2)

# Variables para agrupamiento
agrupacion_jerarquica_rrss=cutree(jerarquica_rrss, k=2)

# Análisis de bondad
plotcluster(numericos, agrupacion_jerarquica_rrss)
plotcluster(binarios_rrss, agrupacion_jerarquica_rrss)

# Coeficiente de silueta
silueta_jerarquica_rrss=silhouette(agrupacion_jerarquica_rrss, distancia_ponderada_rrss)
plot(silueta_jerarquica_rrss, col=1:2)




### AGRUPACIÓN K-MEDIAS
## Uso de televisión
kmedias_tv=kmeans(distancia_ponderada_tv, 2)
kmedias_tv$centers

# Variables para agrupamiento
agrupacion_kmedias_tv=kmedias_tv$cluster

# Análisis de bondad
plotcluster(numericos, agrupacion_kmedias_tv)
plotcluster(binarios_tv, agrupacion_kmedias_tv)

# Coeficiente de silueta
silueta_kmediastv=silhouette(agrupacion_kmedias_tv, distancia_ponderada_tv)
plot(silueta_kmediastv, col=1:2)


## Uso de redes sociales
kmedias_rrss=kmeans(distancia_ponderada_rrss, 2)
kmedias_rrss$centers

# Variables para agrupamiento
agrupacion_kmedias_rrss=kmedias_rrss$cluster

# Análisis de bondad
plotcluster(numericos, agrupacion_kmedias_rrss)
plotcluster(binarios_rrss, agrupacion_kmedias_rrss)

# Coeficiente de silueta
silueta_kmediasrrss=silhouette(agrupacion_kmedias_rrss, distancia_ponderada_rrss)
plot(silueta_kmediasrrss, col=1:2)




### AGRUPACIÓN K-MEOIDES
## Uso de televisión
kmedoides_tv=pam(distancia_ponderada_tv, 2)

# Variables para agrupamiento
agrupacion_kmedoides_tv=kmedoides_tv$clustering

# Análisis de bondad
plotcluster(numericos, agrupacion_kmedoides_tv)
plotcluster(binarios_tv, agrupacion_kmedoides_tv)

# Coeficiente de silueta
silueta_kmedoidestv=silhouette(agrupacion_kmedoides_tv, distancia_ponderada_tv)
plot(silueta_kmedoidestv, col=1:2)


## Uso de redes sociales
kmedoides_rrss=pam(distancia_ponderada_rrss, 2)

# Variables para agrupamiento
agrupacion_kmedoides_rrss=kmedoides_rrss$clustering

# Análisis de bondad
plotcluster(numericos, agrupacion_kmedoides_rrss)
plotcluster(binarios_rrss, agrupacion_kmedoides_rrss)

# Coeficiente de silueta
silueta_kmedoidesrrss=silhouette(agrupacion_kmedoides_rrss, distancia_ponderada_rrss)
plot(silueta_kmedoidesrrss, col=1:2)




cluster.stats(distancia_ponderada_tv, agrupacion_jerarquica_tv)
cluster.stats(distancia_ponderada_rrss, agrupacion_jerarquica_rrss)

cluster.stats(distancia_ponderada_tv, agrupacion_kmedias_tv)
cluster.stats(distancia_ponderada_rrss, agrupacion_kmedias_rrss)

cluster.stats(distancia_ponderada_tv, agrupacion_kmedoides_tv)
cluster.stats(distancia_ponderada_rrss, agrupacion_kmedoides_rrss)
