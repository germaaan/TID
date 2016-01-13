install.packages("kknn")
install.packages("ROCR")
library("kknn")
library("ROCR")

titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",", dec=",")

titanic2=titanic
titanic2$Superviviente=as.factor(ifelse(titanic2$Superviviente==1, "Superviviente", "Fallecido"))

# Crear conjuntos aleatorios de entrenamiento y prueba (70% / 30%)
id=sample(2, nrow(titanic), replace=TRUE, prob=c(0.7, 0.3))
entrenamiento=titanic2[id==1, ]
prueba=titanic2[id==2, ]

# Definir modelo para la predicción
modelo=Superviviente~Clase+Edad+HermanosConyuges+PadresHijos+Tarifa
# Crear clasificacion
clasificacion=kknn(formula=modelo, entrenamiento, prueba, na.action=na.omit(), k=2)
summary(clasificacion)

# Test de los resultados
test=table(clasificacion$fit, prueba$Superviviente)
diagonal=diag(test)

bien_clasificados_knn=(sum(diagonal)/nrow(prueba))*100
mal_clasificados_knn=100-bien_clasificados_knn
bien_clasificados_knn
mal_clasificados_knn

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
fmeasure_total_knn=fmeasure_total

# Calculo de las curvas ROC
m=clasificacion$prob[, 2]
prediccion=prediction(m, prueba$Superviviente)
rendimiento=performance(prediccion, "tpr", "fpr")
plot(rendimiento, main="KNN")
