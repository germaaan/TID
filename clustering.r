install.packages("fpc")
library("fpc")
library("cluster")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",")

titanic2=titanic[, -c(1, 4, 7, 8)]
titanic2$Sexo = ifelse(titanic2$Sexo == "Hombre", 0, 1)
titanic2$PuertoEmbarque=as.integer(factor(titanic2$PuertoEmbarque))

# Distancias numéricas
numericos=data.frame(titanic2[, -c(1, 3)])
muestra=sample(1:dim(numericos)[1], 325) # Muestra para el análisis de bondad
distancia1=dist(numericos, method="euclidean")
distancia4=dist(numericos[muestra,], method="euclidean")

# Distancias binarias
binarios=data.frame(titanic2[, c(1, 3)])
distancia2=dist(binarios, method="binary")
distancia5=dist(binarios[muestra,], method="binary")

# Calculo distancias ponderadas
distancia3=(distancia1+distancia2)/2
distancia6=(distancia4+distancia5)/2


### AGRUPACIÓN JERARQUICA POR EL MÉTODO DE WARD
jerarquica=hclust(distancia3, method="ward.D2")
plot(jerarquica)
rect.hclust(jerarquica, k=2)

# Variables para agrupamiento
agrupacion_jerarquico=cutree(jerarquica, k=2)

# Análisis de bondad
plotcluster(numericos[muestra,], agrupacion_jerarquico[muestra])
plotcluster(binarios[muestra,], agrupacion_jerarquico[muestra])

# Coeficiente de silueta
shi=silhouette(agrupacion_jerarquico[muestra], distancia6)
plot(shi, col=1:2)

# Otras medidas de bondad del agrupamiento
cluster.stats(distancia3, agrupacion_jerarquico)


### AGRUPACIÓN MEDIANTE K-MEDIAS
kmeans.result=kmeans(distancia3, 2)
agrupacion_kmedias=kmeans.result$cluster

# Análisis de bondad
plotcluster(numericos[muestra,], agrupacion_kmedias[muestra])
plotcluster(binarios[muestra,], agrupacion_kmedias[muestra])

# Coeficiente de silueta
shi=silhouette(agrupacion_kmedias[muestra], distancia6)
plot(shi, col=1:2)

# Otras medidas de bondad del agrupamiento
cluster.stats(distancia3, agrupacion_kmedias)
