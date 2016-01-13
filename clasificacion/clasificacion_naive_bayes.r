install.packages("e1071")
install.packages("ROCR")
library("e1071")
library("ROCR")

test=read.csv(file="/home/germaaan/proyectos/TID/test.csv", header=TRUE, sep=",", dec=",")

test2=test
test2$UsoTV=as.factor(ifelse(test2$UsoTV==0, "No usa TV", "Usa TV"))

# Crear conjuntos aleatorios de entrenamiento y prueba (70% / 30%)
id=sample(2, nrow(test), replace=TRUE, prob=c(0.7, 0.3))
entrenamiento=test2[id==1, ]
prueba=test2[id==2, ]

# Definir modelo para la predicción
modelo=UsoTV~.
# Crear clasificación
clasificacion=naiveBayes(modelo, data=entrenamiento)
table(predict(clasificacion, entrenamiento, type="class"), entrenamiento$UsoTV)

# Test de los resultados
prediccion=predict(clasificacion, newdata=prueba, type="class")
test=table(prediccion, prueba$UsoTV)

diagonal=diag(test)
bien_clasificados_naive=(sum(diagonal)/nrow(prueba))*100
mal_clasificados_naive=100-bien_clasificados_naive
bien_clasificados_naive
mal_clasificados_naive

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
fmeasure_total_naive=fmeasure_total
