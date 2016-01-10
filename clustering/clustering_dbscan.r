install.packages("fpc")
library("fpc")
library("cluster")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",", dec=",")
muestra=titanic[sample(nrow(titanic), 325), ]

titanic2=muestra[, c(6:9)]
for (j in 1:4) {x=titanic2[,j] ; v=(x-mean(x))/sqrt(var(x)); titanic2[,j]=v} # Normalizar valores

agrupamiento=dbscan(titanic2, eps=1.35, MinPts=5)
table(agrupamiento$cluster, muestra$Superviviente)
plot(agrupamiento, titanic2)
plot(agrupamiento, titanic2[c(1,2)])
plot(agrupamiento, titanic2[c(1,3)])
plot(agrupamiento, titanic2[c(1,4)])

x=table(agrupamiento$cluster, muestra$Superviviente)
nc=length(x[,1])
nc
silueta=silhouette(agrupamiento$cluster, dist(titanic2))
plot(silueta, col=1:nc)
