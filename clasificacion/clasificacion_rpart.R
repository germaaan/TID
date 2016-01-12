install.packages("rpart")
install.packages("rattle")
install.packages("RColorBrewer")
library("rpart")
library("rattle")
library("RColorBrewer")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",", dec=",")

titanic2=titanic
titanic2$Superviviente=as.factor(ifelse(titanic2$Superviviente==1, "Superviviente", "Fallecido"))

# Crear conjuntos aleatorios de entrenamiento y prueba (70% / 30%)
id=sample(2, nrow(titanic), replace=TRUE, prob=c(0.7, 0.3))
entrenamiento=titanic2[id==1, ]
prueba=titanic2[id==2, ]

# Definir modelo para la predicción
modelo=Superviviente~Clase+Edad+HermanosConyuges+PadresHijos+Tarifa
# Crear árbol
arbol1=rpart(modelo, data=entrenamiento)
arbol2=rpart(modelo, data=entrenamiento, parms=list(split="information"))

arbol1
fancyRpartPlot(arbol1)
arbol2
fancyRpartPlot(arbol2)

# Test de los resultados
prediccion=predict(arbol1, newdata=prueba, type="class")
test=table(prediccion, prueba$Superviviente)

diagonal=diag(test)
bien_clasificados_rparty=(sum(diagonal)/nrow(prueba))*100
mal_clasificados_rparty=100-bien_clasificados_rparty
bien_clasificados_rparty
mal_clasificados_rparty

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
fmeasure_total_rparty=fmeasure_total
