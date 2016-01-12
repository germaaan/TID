install.packages("fpc")
library("fpc")
library("cluster")

test=read.csv(file="/home/germaaan/proyectos/TID/test.csv", header=TRUE, sep=",", dec=",")

muestra=sample(1:dim(test)[1], 150)

test2=test[muestra , -c(1, 9, 10)]
test2$Hombre=ifelse(test2$Sexo == "Hombre", 1, 0)
test2$Mujer=ifelse(test2$Sexo == "Mujer", 1, 0)
test2$Soltero=ifelse(test2$EstadoCivil == "Soltero", 1, 0)
test2$Casado=ifelse(test2$EstadoCivil == "Casado", 1, 0)
test2$Otro=ifelse(test2$EstadoCivil == "Otro", 1, 0)
test2$Primarios=ifelse(test2$Estudios == "Primarios", 1, 0)
test2$Secundarios=ifelse(test2$Estudios == "Secundarios", 1, 0)
test2$Universitarios=ifelse(test2$Estudios == "Universitarios", 1, 0)
test2$Sexo=NULL
test2$EstadoCivil=NULL
test2$Estudios=NULL

# Distancia numéricos
numericos=data.frame(test2[, c(1:4)])
for (j in 1:4) {x=numericos[,j] ; v=(x-mean(x))/sqrt(var(x)); numericos[,j]=v} # Normalizar valores
distancia_numericos=dist(numericos, method="euclidean")

# Distancia binarios
binarios=data.frame(test2[, -c(1:4)])
distancia_binarios=dist(binarios, method="binary")

# Calculo distancia ponderada
distancia_ponderada=(distancia_numericos+distancia_binarios)/2


### AGRUPACIÓN JERARQUICA POR EL MÉTODO DE WARD
## Uso de redes sociales
jerarquica=hclust(distancia_ponderada, method="ward.D2")
plot(jerarquica, main="Dendograma agrupación jerarquica: Uso redes sociales", labels=ifelse(test[muestra, ]$UsoRRSS == 1, "Usa RRSS", "No usa RRSS"))
rect.hclust(jerarquica, k=2)

# Variables para agrupamiento
agrupacion_jerarquica=cutree(jerarquica, k=2)

# Análisis de bondad
plotcluster(numericos, agrupacion_jerarquica)
plotcluster(binarios, agrupacion_jerarquica)

# Coeficiente de silueta
silueta=silhouette(agrupacion_jerarquica, distancia_ponderada)
plot(silueta, col=1:2)
