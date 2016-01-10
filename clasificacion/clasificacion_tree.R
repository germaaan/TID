install.packages("tree")
library("tree")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",", dec=",")

# Filtrar valores perdidos
#titanic2=filter(titanic, is.na(titanic$Superviviente)==FALSE)
titanic2=titanic
titanic2$Superviviente=as.factor(ifelse(titanic2$Superviviente==1, "Superviviente", "Fallecido"))

# Crear conjuntos aleatorios de entrenamiento y prueba (70% / 30%)
id=sample(2, nrow(titanic), replace=TRUE, prob=c(0.7, 0.3))
entrenamiento=titanic2[id==1, ]
prueba=titanic2[id==2, ]

# Definir modelo para la predicción
modelo=Superviviente~Clase+Edad+HermanosConyuges+PadresHijos+Tarifa
# Crear árbol
arbol=tree(modelo, data=entrenamiento)
table(predict(arbol, type="class"), entrenamiento$Superviviente)
summary(arbol)
plot(arbol) ; text(arbol)

# Test de los resultados
prediccion=predict(arbol, newdata=prueba, type="class")
test=table(prediccion, prueba$Superviviente)

diagonal=diag(test)
bien_clasificados=(sum(diagonal)/nrow(prueba))*100
mal_clasificados=100-bien_clasificados
bien_clasificados
mal_clasificados

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
#F-measure total
fmeasure_total
fmeasure_total_tree=fmeasure_total