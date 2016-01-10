install.packages("fpc")
library("fpc")
library("cluster")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",", dec=",")

titanic2=titanic[, c(2, 5:9)]
titanic2$Sexo=as.integer(ifelse(titanic2$Sexo == "Hombre", 0, 1))

# Distancia numéricos
numericos=data.frame(titanic2[, -c(1, 2)])
for (j in 1:4) {x=numericos[,j] ; v=(x-mean(x))/sqrt(var(x)); numericos[,j]=v} # Normalizar valores
distancia_numericos=dist(numericos, method="euclidean")

# Distancia binarios
binarios=data.frame(titanic2[, c(1, 2)])
distancia_binarios=dist(binarios, method="binary")

# Calculo distancia ponderada
distancia_ponderada=(distancia_numericos+distancia_binarios)/2


### AGRUPACIÓN POR K-MEDOIDES
kmedoides=pam(distancia_ponderada, 2)

# Variables para agrupamiento
agrupacion_kmedoides=kmedoides$clustering

# Análisis de bondad
muestra=sample(1:dim(numericos)[1], 325)
plotcluster(numericos[muestra,], agrupacion_kmedoides[muestra])
plotcluster(binarios[muestra,], agrupacion_kmedoides[muestra])

# Coeficiente de silueta
distancia_numericos_muestra=dist(numericos[muestra,])
distancia_binarios_muestra=dist(binarios[muestra,])
distancia_ponderada_muestra=(distancia_numericos_muestra+distancia_binarios_muestra)/2
silueta=silhouette(agrupacion_kmedoides[muestra], distancia_ponderada_muestra)
plot(silueta, col=1:2)

# Otras medidas de bondad del agrupamiento
cluster.stats(distancia_ponderada, agrupacion_kmedoides)
