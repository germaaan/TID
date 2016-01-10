install.packages("fpc")
library("fpc")
library("cluster")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",", dec=",")

muestra=sample(1:dim(titanic)[1], 150)

titanic2=titanic[muestra , c(5:8, 10)]
titanic2$Sexo=as.integer(ifelse(titanic2$Sexo == "Hombre", 0, 1))

# Distancia numéricos
numericos=data.frame(titanic2[, -c(1)])
for (j in 1:4) {x=numericos[,j] ; v=(x-mean(x))/sqrt(var(x)); numericos[,j]=v} # Normalizar valores
distancia_numericos=dist(numericos, method="euclidean")

# Distancia binarios
binarios=data.frame(titanic2$Sexo)
distancia_binarios=dist(binarios, method="binary")

# Calculo distancia ponderada
distancia_ponderada=(distancia_numericos+distancia_binarios)/2


### AGRUPACIÓN JERARQUICA POR EL MÉTODO DE WARD
## Supervivientes
jerarquica=hclust(distancia_ponderada, method="ward.D2")
plot(jerarquica, main="Dendograma agrupación jerarquica: Supervivientes", labels=ifelse(titanic[muestra, ]$Superviviente == 1, "Superviviente", "Fallecido"))
rect.hclust(jerarquica, k=2)

# Variables para agrupamiento
agrupacion_jerarquica=cutree(jerarquica, k=2)

# Análisis de bondad
plotcluster(numericos, agrupacion_jerarquica)

# Coeficiente de silueta
silueta=silhouette(agrupacion_jerarquica, distancia_ponderada)
plot(silueta, col=1:2)
