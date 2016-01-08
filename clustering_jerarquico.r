install.packages("fpc")
library("fpc")
library("cluster")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",")

titanic2=titanic[, -c(1, 4)]
titanic2$PuertoEmbarque=as.integer(factor(titanic2$PuertoEmbarque))
titanic2$Sexo = ifelse(titanic2$Sexo == "Hombre", 0, 1)

# Distancia numérica
numericos=data.frame(titanic2[, -c(1, 3)])
distancia1=dist(numericos)

# Distancia binaria
binarios=data.frame(titanic2$Sexo)
distancia2=dist(binarios, method="binary")

# Calculo distancia ponderada y cluster
distancia3=(distancia1+distancia2)/2
jerarquica=hclust(distancia3, method="ward.D2")
jerarquica
plot(jerarquica)
rect.hclust(jerarquica, k=2)
agrupacion=cutree(jerarquica, k=2)

# Análisis de bondad (restringido a 150 valores)
muestra=sample(1:dim(numericos)[1], 400)
plotcluster(numericos[muestra,],agrupacion[muestra])
plotcluster(binarios[muestra,],agrupacion[muestra])
distancia4=dist(numericos[muestra,])
distancia5=dist(binarios[muestra,])
distancia6=(distancia4+distancia5)/2
shi=silhouette(agrupacion[muestra], distancia6)
plot(shi, col=1:2)
cluster.stats(distancia3, agrupacion)
