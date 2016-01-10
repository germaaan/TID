install.packages("fpc")
library("fpc")
library("cluster")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",", dec=",")
muestra=titanic[sample(nrow(titanic), 325), ]

titanic2=muestra[, c(6:9)]
for (j in 1:4) {x=titanic2[,j] ; v=(x-mean(x))/sqrt(var(x)); titanic2[,j]=v} # Normalizar valores


### AGRUPACIÓN POR CLUSTER DIFUSO
difuso=fanny(titanic2, 2)

table(difuso$clustering, muestra$Superviviente)
plot(difuso)

# Otras medidas de bondad del agrupamiento
cluster.stats(dist(titanic2), difuso$clustering, alt.clustering=as.integer(muestra$Superviviente))