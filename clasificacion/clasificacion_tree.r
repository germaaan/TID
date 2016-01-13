install.packages("tree")
library("tree")

test=read.csv(file="/home/germaaan/proyectos/TID/test.csv", header=TRUE, sep=",", dec=",")

test2=test[, -c(1)]
test2$UsoTV=as.factor(ifelse(test2$UsoTV==1, "Usa TV", "No usa TV"))
test2$UsoRRSS=as.factor(ifelse(test2$UsoRRSS==1, "Usa RRSS", "No usa RRSS"))

# Crear conjuntos aleatorios de entrenamiento y prueba (70% / 30%)
id=sample(2, nrow(test), replace=TRUE, prob=c(0.7, 0.3))
entrenamiento=test2[id==1, ]
prueba=test2[id==2, ]

# Definir modelo para la predicción
modelo=UsoTV~.
# Crear árbol
arbol=tree(modelo, data=entrenamiento)
plot(arbol) ; text(arbol, pretty=0)

# Test de los resultados
prediccion=predict(arbol, newdata=prueba, type="class")
test=table(prediccion, prueba$UsoTV)

diagonal=diag(test)
bien_clasificados_tree=(sum(diagonal)/nrow(prueba))*100
mal_clasificados_tree=100-bien_clasificados_tree
bien_clasificados_tree
mal_clasificados_tree

# Podamos árbol
validacionCruzada=cv.tree(arbol, FUN=prune.misclass)
plot(validacionCruzada)
validacionCruzada$dev # Obtenemos desviación
mejor=validacionCruzada$size[which(validacionCruzada$dev==min(validacionCruzada$dev))]
mejor
arbolPodado=prune.misclass(arbol, best=mejor)
plot(arbolPodado);text(arbolPodado, pretty=0)

# Test de los resultados árbol podado
prediccion=predict(arbolPodado, newdata=prueba, type="class")
test=table(prediccion, prueba$UsoTV)

diagonal=diag(test)
bien_clasificados_tree=(sum(diagonal)/nrow(prueba))*100
mal_clasificados_tree=100-bien_clasificados_tree
bien_clasificados_tree
mal_clasificados_tree


# Precisión, exhaustividad y valor-F
m=c(1:nrow(test))
n=m
precision=n
recall=m
fmeasure=m
fmeasure_total=0 
for(i in 1:nrow(test)){m[i]=sum(test[i,])}
for(i in 1:nrow(test)){n[i]=sum(test[,i])}
for(i in 1:nrow(test)){precision[i]=diagonal[i]/m[i]}
for(i in 1:nrow(test)){recall[i]=diagonal[i]/n[i]} 
for(i in 1:nrow(test)){fmeasure[i]=2*diagonal[i]/(m[i]+n[i])}
for(i in 1:nrow(test)){fmeasure_total=fmeasure_total+(fmeasure[i]/nrow(test))}

# precisionisión (precisionision)
# Número de resultados positivos correctos dividido por el número total de resultados
precision

# Exhaustividad (recall)
# Número de resultados positivos correctos dividido por el número total de resultados
# que deberían haber sido devueltos
recall

# Valor-F (F-measure)
# Media armónica de la precisionisión y la exhaustividad, mide la precisionisión del test
fmeasure
# Valor-F total
fmeasure_total
fmeasure_total_tree=fmeasure_total
