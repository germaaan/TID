install.packages("arules")
install.packages("arulesViz")
library("arules")
library("arulesViz")

test=read.csv(file="/home/germaaan/proyectos/TID/test.csv", header=TRUE, sep=",", dec=",")
test2=test[, -c(1)]

# Discretizar variables
for (j in 2:2){test2[, j-1]=ordered(cut(test[, j], breaks=quantile(test[, j])), labels=c("Joven-Adulto", "Adulto", "Adulto-Senior", "Senior"))}
test2$NumTV=factor(ifelse(test2$NumTV<=1, "Pocas-TVs", ifelse(test2$NumTV<=2, "Algunas-TVs", "Muchas-TVs")))
test2$PerCasa=factor(ifelse(test2$PerCasa<=2.5, "Pocas-Personas", ifelse(test2$PerCasa<=5, "Algunas-Personas", "Muchas-Personas")))
test2$TVPer=factor(ifelse(test2$TVPer<=1, "Baja", ifelse(test2$TVPer<=2, "Media", "Alta")))
test2$UsoTV=as.factor(ifelse(test2$UsoTV==1, "Usa TV", "No usa TV"))
test2$UsoRRSS=as.factor(ifelse(test2$UsoRRSS==1, "Usa RRSS", "No usa RRSS"))
test2

# Convertimos en una base de datos de transacciones
transacciones=as(test2, "transactions")
summary(transacciones)

itemFrequencyPlot(transacciones, support=0.1)

#Calculo apriori con los parámetros por defecto
sale1=apriori(transacciones, parameter = list(support=0.1, confidence=0.8, minlen=2))

# Salida del conjunto de reglas
summary(sale1)

sale3=sort(sale1, by="confidence")
salida1=inspect(sale3, ruleSep = "---->", itemSep = " + ", setStart = "", setEnd ="", linebreak=TRUE)

# Restriccion de reglas, solo las que predicen clases y con confianza mayor que 0.9
sale2=subset(sale3, subset=rhs %in% c("UsoTV=Usa TV", "UsoTV=No usa TV") & confidence>=0.9)
salida2=inspect(sale2, ruleSep = "---->", itemSep = " + ", setStart = "", setEnd ="", linebreak=TRUE)

# Visualizacion de reglas
plot(sale1, method="scatterplot", measure=c("support", "confidence"))
plot(sale1, shading="order", control=list(main = "Two-key plot", col=rainbow(5)))
plot(sale2, method="matrix", measure="confidence", control=list(main="Matriz", col=rainbow(8)))
plot(sale2, method="matrix3d", shading="order", control=list(main="Tridimensional"))
plot(sale1, method="grouped", measure="confidence", control=list(main="Cluster", k=10, col=rainbow(8)))
plot(sale2[1:10], method="graph", measure="confidence", control=list(main="Grafo"))


# Restriccion de reglas, solo las que predicen clases y con confianza mayor que 0.9
sale2=subset(sale3, subset=rhs %in% c("UsoRRSS=Usa RRSS", "UsoRRSS=No usa RRSS") & confidence>=0.9)
salida2=inspect(sale2, ruleSep = "---->", itemSep = " + ", setStart = "", setEnd ="", linebreak=TRUE)

# Visualizacion de reglas
plot(sale1, method="scatterplot", measure=c("support", "confidence"))
plot(sale1, shading="order", control=list(main = "Two-key plot", col=rainbow(5)))
plot(sale2, method="matrix", measure="confidence", control=list(main="Matriz", col=rainbow(8)))
plot(sale2, method="matrix3d", shading="order", control=list(main="Tridimensional"))
plot(sale1, method="grouped", measure="confidence", control=list(main="Cluster", k=10, col=rainbow(8)))
plot(sale2[1:10], method="graph", measure="confidence", control=list(main="Grafo"))
