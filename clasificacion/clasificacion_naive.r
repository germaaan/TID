install.packages("e1071")
install.packages("ROCR")
library("e1071")
library("ROCR")

test=read.csv(file="/home/germaaan/proyectos/TID/test.csv", header=TRUE, sep=",", dec=",")

test2=test[, -c(1)]
test2$UsoTV=as.factor(ifelse(test2$UsoTV==1, "Usa TV", "No usa TV"))
test2$UsoRRSS=as.factor(ifelse(test2$UsoRRSS==1, "Usa RRSS", "No usa RRSS"))

# Crear conjuntos aleatorios de entrenamiento y prueba (70% / 30%)
id=sample(2, nrow(test), replace=TRUE, prob=c(0.7, 0.3))
entrenamiento=test2[id==1, ]
prueba=test2[id==2, ]

# Definir modelo para la predicción
modelo_TV=UsoTV~.
# Crear clasificación
clasificacion_TV=naiveBayes(modelo_TV, data=entrenamiento)
table(predict(clasificacion_TV, entrenamiento, type="class"), entrenamiento$UsoTV)

# Test de los resultados
prediccion_TV=predict(clasificacion_TV, newdata=prueba, type="class")
test_TV=table(prediccion_TV, prueba$UsoTV)

diagonal_TV=diag(test_TV)
bien_clasificados_naive_TV=(sum(diagonal_TV)/nrow(prueba))*100
mal_clasificados_naive_TV=100-bien_clasificados_naive_TV
bien_clasificados_naive_TV
mal_clasificados_naive_TV

# Precisión, exhaustividad y valor-F
m_TV=c(1:nrow(test_TV))
n_TV=m_TV
precision_TV=n_TV
recall_TV=m_TV
fmeasure_TV=m_TV
fmeasure_total_TV=0 
for(i in 1:nrow(test_TV)){m_TV[i]=sum(test_TV[i,])}
for(i in 1:nrow(test_TV)){n_TV[i]=sum(test_TV[,i])}
for(i in 1:nrow(test_TV)){precision_TV[i]=diagonal_TV[i]/m_TV[i]}
for(i in 1:nrow(test_TV)){recall_TV[i]=diagonal_TV[i]/n_TV[i]} 
for(i in 1:nrow(test_TV)){fmeasure_TV[i]=2*diagonal_TV[i]/(m_TV[i]+n_TV[i])}
for(i in 1:nrow(test_TV)){fmeasure_total_TV=fmeasure_total_TV+(fmeasure_TV[i]/nrow(test_TV))}

# Precisión (precision)
# Número de resultados positivos correctos dividido por el número total de resultados
precision_TV

# Exhaustividad (recall)
# Número de resultados positivos correctos dividido por el número total de resultados
# que deberían haber sido devueltos
recall_TV

# Valor-F (F-measure)
# Media armónica de la Precisión y la exhaustividad, mide la Precisión del test
fmeasure_TV
# Valor-F total
fmeasure_total_TV
fmeasure_total_naive_TV=fmeasure_total_TV


# Definir modelo para la predicción
modelo_RRSS=UsoRRSS~.
# Crear clasificación
clasificacion_RRSS=naiveBayes(modelo_RRSS, data=entrenamiento)
table(predict(clasificacion_RRSS, entrenamiento, type="class"), entrenamiento$UsoRRSS)

# Test de los resultados
prediccion_RRSS=predict(clasificacion_RRSS, newdata=prueba, type="class")
test_RRSS=table(prediccion_RRSS, prueba$UsoRRSS)

diagonal_RRSS=diag(test_RRSS)
bien_clasificados_naive_RRSS=(sum(diagonal_RRSS)/nrow(prueba))*100
mal_clasificados_naive_RRSS=100-bien_clasificados_naive_RRSS
bien_clasificados_naive_RRSS
mal_clasificados_naive_RRSS

# Precisión, exhaustividad y valor-F
m_RRSS=c(1:nrow(test_RRSS))
n_RRSS=m_RRSS
precision_RRSS=n_RRSS
recall_RRSS=m_RRSS
fmeasure_RRSS=m_RRSS
fmeasure_total_RRSS=0 
for(i in 1:nrow(test_RRSS)){m_RRSS[i]=sum(test_RRSS[i,])}
for(i in 1:nrow(test_RRSS)){n_RRSS[i]=sum(test_RRSS[,i])}
for(i in 1:nrow(test_RRSS)){precision_RRSS[i]=diagonal_RRSS[i]/m_RRSS[i]}
for(i in 1:nrow(test_RRSS)){recall_RRSS[i]=diagonal_RRSS[i]/n_RRSS[i]} 
for(i in 1:nrow(test_RRSS)){fmeasure_RRSS[i]=2*diagonal_RRSS[i]/(m_RRSS[i]+n_RRSS[i])}
for(i in 1:nrow(test_RRSS)){fmeasure_total_RRSS=fmeasure_total_RRSS+(fmeasure_RRSS[i]/nrow(test_RRSS))}

# Precisión (precision)
# Número de resultados positivos correctos dividido por el número total de resultados
precision_RRSS

# Exhaustividad (recall)
# Número de resultados positivos correctos dividido por el número total de resultados
# que deberían haber sido devueltos
recall_RRSS

# Valor-F (F-measure)
# Media armónica de la Precisión y la exhaustividad, mide la Precisión del test
fmeasure_RRSS
# Valor-F total
fmeasure_total_RRSS
fmeasure_total_naive_RRSS=fmeasure_total_RRSS

