install.packages("fpc")
library("fpc")
library("cluster")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",", dec=",")

titanic2=titanic[ , c(5:8, 10)]
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
## Clases
jerarquica=hclust(distancia_ponderada, method="ward.D2")
plot(jerarquica, main="Dendograma agrupación jerarquica: Clases", labels=ifelse(titanic$Clase==1, "Primera", ifelse(titanic$Clase==2, "Segunda", "Tercera")))
rect.hclust(jerarquica, k=3)

# Variables para agrupamiento
agrupacion_jerarquica=cutree(jerarquica, k=3)

# Análisis de bondad
plotcluster(numericos, agrupacion_jerarquica)

# Coeficiente de silueta
muestra=sample(1:dim(numericos)[1], 325)
distancia_numericos_muestra=dist(numericos[muestra,])
distancia_binarios_muestra=dist(binarios[muestra,])
distancia_ponderada_muestra=(distancia_numericos_muestra+distancia_binarios_muestra)/2
silueta=silhouette(agrupacion_jerarquica[muestra], distancia_ponderada_muestra)
plot(silueta, col=1:3)
