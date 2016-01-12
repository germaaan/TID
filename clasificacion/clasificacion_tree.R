install.packages("tree")
library("tree")

test=read.csv(file="/home/germaaan/proyectos/TID/test.csv", header=TRUE, sep=",", dec=",")

test2=test
#test2$UsoTV=as.factor(ifelse(test2$UsoTV==1, "Ve TV", "No ve TV"))
#test2$UsoRRSS=as.factor(ifelse(test2$UsoRRSS==1, "Usa RRSS", "No usa RRSS"))
test2$Sexo=ifelse(test2$Sexo == "Hombre", 1, 0)
test2$Soltero=ifelse(test2$EstadoCivil == "Soltero", 1, 0)
test2$Casado=ifelse(test2$EstadoCivil == "Casado", 1, 0)
test2$Otro=ifelse(test2$EstadoCivil == "Otro", 1, 0)
test2$Primarios=ifelse(test2$Estudios == "Primarios", 1, 0)
test2$Secundarios=ifelse(test2$Estudios == "Secundarios", 1, 0)
test2$Universitarios=ifelse(test2$Estudios == "Universitarios", 1, 0)
test2$EstadoCivil=NULL
test2$Estudios=NULL

# Crear conjuntos aleatorios de entrenamiento y prueba (70% / 30%)
id=sample(2, nrow(test), replace=TRUE, prob=c(0.7, 0.3))
entrenamiento=test2[id==1, ]
prueba=test2[id==2, ]

# Definir modelo para la predicción
modelo=UsoTV~.
summary(lm(modelo,data=entrenamiento))
# Crear árbol
modelo=UsoTV~Sexo+PerCasa+NumTV+TVPer+Secundarios
arbol=tree(modelo, data=entrenamiento)
table(predict(arbol, entrenamiento), entrenamiento$UsoTV)
summary(arbol)
plot(arbol) ; text(arbol)
mean(predict$class==entrenamiento$UsoTV)

# Test de los resultados
prediccion=predict(arbol, newdata=prueba, type="class")
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