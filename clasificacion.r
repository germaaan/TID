install.packages("e1071")
install.packages("kknn")
install.packages("party")
install.packages("randomForest")
install.packages("rattle")
install.packages("RColorBrewer")
install.packages("rpart")
install.packages("tree")
library("e1071")
library("kknn")
library("party")
library("randomForest")
library("rattle")
library("RColorBrewer")
library("rpart")
library("tree")

test=read.csv(file="/home/germaaan/proyectos/TID/test.csv", header=TRUE, sep=",", dec=",")

test2=test[, -c(1)]
test2$UsoTV=as.factor(ifelse(test2$UsoTV==1, "Usa TV", "No usa TV"))
test2$UsoRRSS=as.factor(ifelse(test2$UsoRRSS==1, "Usa RRSS", "No usa RRSS"))

# Crear conjuntos aleatorios de entrenamiento y prueba (70% / 30%)
id=sample(2, nrow(test), replace=TRUE, prob=c(0.7, 0.3))
entrenamiento=test2[id==1, ]
prueba=test2[id==2, ]

# Definir modelo para la predicción de uso de TV
modelo_TV=UsoTV~.
# Crear árbol
arbol_TV=tree(modelo_TV, data=entrenamiento)
plot(arbol_TV) ; text(arbol_TV, pretty=0)

# Test de los resultados
prediccion_TV=predict(arbol_TV, newdata=prueba, type="class")
test_TV=table(prediccion_TV, prueba$UsoTV)

diagonal_TV=diag(test_TV)
bien_clasificados_tree_TV=(sum(diagonal_TV)/nrow(prueba))*100
mal_clasificados_tree_TV=100-bien_clasificados_tree_TV
bien_clasificados_tree_TV
mal_clasificados_tree_TV

# Podamos árbol
validacionCruzada_TV=cv.tree(arbol_TV, FUN=prune.misclass)
plot(validacionCruzada_TV)
validacionCruzada_TV$dev # Obtenemos desviación
mejor_TV=validacionCruzada_TV$size[which(validacionCruzada_TV$dev==min(validacionCruzada_TV$dev))]
mejor_TV
arbolPodado_TV=prune.misclass(arbol_TV, best=mejor_TV)
plot(arbolPodado_TV);text(arbolPodado_TV, pretty=0)

# Test de los resultados árbol podado
prediccion_TV=predict(arbolPodado_TV, newdata=prueba, type="class")
test_TV=table(prediccion_TV, prueba$UsoTV)

diagonal_TV=diag(test_TV)
bien_clasificados_tree_TV=(sum(diagonal_TV)/nrow(prueba))*100
mal_clasificados_tree_TV=100-bien_clasificados_tree_TV
bien_clasificados_tree_TV
mal_clasificados_tree_TV

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
fmeasure_total_tree_TV=fmeasure_total_TV


# Definir modelo para la predicción de uso de RRSS
modelo_RRSS=UsoRRSS~.
# Crear árbol
arbol_RRSS=tree(modelo_RRSS, data=entrenamiento)
plot(arbol_RRSS) ; text(arbol_RRSS, pretty=0)

# Test de los resultados
prediccion_RRSS=predict(arbol_RRSS, newdata=prueba, type="class")
test_RRSS=table(prediccion_RRSS, prueba$UsoRRSS)

diagonal_RRSS=diag(test_RRSS)
bien_clasificados_tree_RRSS=(sum(diagonal_RRSS)/nrow(prueba))*100
mal_clasificados_tree_RRSS=100-bien_clasificados_tree_RRSS
bien_clasificados_tree_RRSS
mal_clasificados_tree_RRSS

# Podamos árbol
validacionCruzada_RRSS=cv.tree(arbol_RRSS, FUN=prune.misclass)
plot(validacionCruzada_RRSS)
validacionCruzada_RRSS$dev # Obtenemos desviación
mejor_RRSS=validacionCruzada_RRSS$size[which(validacionCruzada_RRSS$dev==min(validacionCruzada_RRSS$dev))]
mejor_RRSS
arbolPodado_RRSS=prune.misclass(arbol_RRSS, best=mejor_RRSS)
plot(arbolPodado_RRSS);text(arbolPodado_RRSS, pretty=0)

# Test de los resultados árbol podado
prediccion_RRSS=predict(arbolPodado_RRSS, newdata=prueba, type="class")
test_RRSS=table(prediccion_RRSS, prueba$UsoRRSS)

diagonal_RRSS=diag(test_RRSS)
bien_clasificados_tree_RRSS=(sum(diagonal_RRSS)/nrow(prueba))*100
mal_clasificados_tree_RRSS=100-bien_clasificados_tree_RRSS
bien_clasificados_tree_RRSS
mal_clasificados_tree_RRSS

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
fmeasure_total_tree_RRSS=fmeasure_total_RRSS




# Crear árbol
arbol_TV=ctree(modelo_TV, data=entrenamiento)
plot(arbol_TV)

# Test de los resultados
prediccion_TV=predict(arbol_TV, newdata=prueba, type="response")
test_TV=table(prediccion_TV, prueba$UsoTV)

diagonal_TV=diag(test_TV)
bien_clasificados_ctree_TV=(sum(diagonal_TV)/nrow(prueba))*100
mal_clasificados_ctree_TV=100-bien_clasificados_ctree_TV
bien_clasificados_ctree_TV
mal_clasificados_ctree_TV

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
fmeasure_total_ctree_TV=fmeasure_total_TV


# Crear árbol
arbol_RRSS=ctree(modelo_RRSS, data=entrenamiento)
plot(arbol_RRSS)

# Test de los resultados
prediccion_RRSS=predict(arbol_RRSS, newdata=prueba, type="response")
test_RRSS=table(prediccion_RRSS, prueba$UsoRRSS)

diagonal_RRSS=diag(test_RRSS)
bien_clasificados_ctree_RRSS=(sum(diagonal_RRSS)/nrow(prueba))*100
mal_clasificados_ctree_RRSS=100-bien_clasificados_ctree_RRSS
bien_clasificados_ctree_RRSS
mal_clasificados_ctree_RRSS

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
fmeasure_total_ctree_RRSS=fmeasure_total_RRSS




# Crear árbol
arbol1_TV=rpart(modelo_TV, data=entrenamiento)
arbol2_TV=rpart(modelo_TV, data=entrenamiento, parms=list(split="information"))

arbol1_TV
fancyRpartPlot(arbol1_TV)
arbol2_TV
fancyRpartPlot(arbol2_TV)

# Test de los resultados árbol podado
prediccion_TV=predict(arbol2_TV, newdata=prueba, type="class")
test_TV=table(prediccion_TV, prueba$UsoTV)

diagonal_TV=diag(test_TV)
bien_clasificados_rpart_TV=(sum(diagonal_TV)/nrow(prueba))*100
mal_clasificados_rpart_TV=100-bien_clasificados_rpart_TV
bien_clasificados_rpart_TV
mal_clasificados_rpart_TV


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
fmeasure_total_rpart_TV=fmeasure_total_TV


# Crear árbol
arbol1_RRSS=rpart(modelo_RRSS, data=entrenamiento)
arbol2_RRSS=rpart(modelo_RRSS, data=entrenamiento, parms=list(split="information"))

arbol1_RRSS
fancyRpartPlot(arbol1_RRSS)
arbol2_RRSS
fancyRpartPlot(arbol2_RRSS)

# Test de los resultados árbol podado
prediccion_RRSS=predict(arbol2_RRSS, newdata=prueba, type="class")
test_RRSS=table(prediccion_RRSS, prueba$UsoRRSS)

diagonal_RRSS=diag(test_RRSS)
bien_clasificados_rpart_RRSS=(sum(diagonal_RRSS)/nrow(prueba))*100
mal_clasificados_rpart_RRSS=100-bien_clasificados_rpart_RRSS
bien_clasificados_rpart_RRSS
mal_clasificados_rpart_RRSS


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
fmeasure_total_rpart_RRSS=fmeasure_total_RRSS




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




# Crear clasificacion
clasificacion_TV=kknn(formula=modelo_TV, entrenamiento, prueba, na.action=na.omit(), k=2)
summary(clasificacion_TV)

# Test de los resultados
fit_TV=fitted(clasificacion_TV)
test_TV=table(fit_TV, prueba$UsoTV)
test_TV

diagonal_TV=diag(test_TV)
bien_clasificados_knn_TV=(sum(diagonal_TV)/nrow(prueba))*100
mal_clasificados_knn_TV=100-bien_clasificados_knn_TV
bien_clasificados_knn_TV
mal_clasificados_knn_TV

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
fmeasure_total_knn_TV=fmeasure_total_TV


# Crear clasificacion
clasificacion_RRSS=kknn(formula=modelo_RRSS, entrenamiento, prueba, na.action=na.omit(), k=2)
summary(clasificacion_RRSS)

# Test de los resultados
fit_RRSS=fitted(clasificacion_RRSS)
test_RRSS=table(fit_RRSS, prueba$UsoRRSS)
test_RRSS

diagonal_RRSS=diag(test_RRSS)
bien_clasificados_knn_RRSS=(sum(diagonal_RRSS)/nrow(prueba))*100
mal_clasificados_knn_RRSS=100-bien_clasificados_knn_RRSS
bien_clasificados_knn_RRSS
mal_clasificados_knn_RRSS

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
fmeasure_total_knn_RRSS=fmeasure_total_RRSS




# Crear árbol
arbol_TV=randomForest(modelo_TV, data=entrenamiento)
table(predict(arbol_TV), entrenamiento$UsoTV)
arbol_TV

# Test de los resultados
prediccion_TV=predict(arbol_TV, newdata=prueba, type="class")
test_TV=table(prediccion_TV, prueba$UsoTV)

diagonal_TV=diag(test_TV)
bien_clasificados_random_TV=(sum(diagonal_TV)/nrow(prueba))*100
mal_clasificados_random_TV=100-bien_clasificados_random_TV
bien_clasificados_random_TV
mal_clasificados_random_TV

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
fmeasure_total_random_TV=fmeasure_total_TV


# Crear árbol
arbol_RRSS=randomForest(modelo_RRSS, data=entrenamiento)
table(predict(arbol_RRSS), entrenamiento$UsoRRSS)
arbol_RRSS

# Test de los resultados
prediccion_RRSS=predict(arbol_RRSS, newdata=prueba, type="class")
test_RRSS=table(prediccion_RRSS, prueba$UsoRRSS)

diagonal_RRSS=diag(test_RRSS)
bien_clasificados_random_RRSS=(sum(diagonal_RRSS)/nrow(prueba))*100
mal_clasificados_random_RRSS=100-bien_clasificados_random_RRSS
bien_clasificados_random_RRSS
mal_clasificados_random_RRSS

# Precisión, exhaustividad y valor-F
m_RRSS=c(1:nrow(test_RRSS))
n_RRSS=m_RRSS
precision_RRSS=n_RRSS
recall_RRSS=m_RRSS
fmeasure_RRSS=m_RRSS
fmeasure_total_RRSS=0 
for(i in 1:nrow(test_RRSS)){m_RRSS[i]=sum(test_RRSS[i,])}
for(i in 1:nrow(test_RRSS)){n_RRSS[i]=sum(test_RRSS[i,])}
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
fmeasure_total_random_RRSS=fmeasure_total_RRSS




## RESULTADOS CLASIFICACIÓN USO TV
v1=c("Precision total", "Valor-F total")
v2=c(bien_clasificados_tree_TV, fmeasure_total_tree_TV)
v3=c(bien_clasificados_ctree_TV, fmeasure_total_ctree_TV)
v4=c(bien_clasificados_rpart_TV, fmeasure_total_rpart_TV)
v5=c(bien_clasificados_naive_TV, fmeasure_total_naive_TV)
v6=c(bien_clasificados_knn_TV, fmeasure_total_knn_TV)
v7=c(bien_clasificados_random_TV, fmeasure_total_random_TV)
td=data.frame(v1, v2, v3, v4, v5, v6, v7)
names(td)=c("Medida", "Arbol-Gini", "Arbol-Reg", "Arbol-Rpart", "Naive Bayes", "KNN", "Random Forest")
td 

## RESULTADOS CLASIFICACIÓN USO RRSS
v1=c("Precision total", "Valor-F total")
v2=c(bien_clasificados_tree_RRSS, fmeasure_total_tree_RRSS)
v3=c(bien_clasificados_ctree_RRSS, fmeasure_total_ctree_RRSS)
v4=c(bien_clasificados_rpart_RRSS, fmeasure_total_rpart_RRSS)
v5=c(bien_clasificados_naive_RRSS, fmeasure_total_naive_RRSS)
v6=c(bien_clasificados_knn_RRSS, fmeasure_total_knn_RRSS)
v7=c(bien_clasificados_random_RRSS, fmeasure_total_random_RRSS)
td=data.frame(v1, v2, v3, v4, v5, v6, v7)
names(td)=c("Medida", "Arbol-Gini", "Arbol-Reg", "Arbol-Rpart", "Naive Bayes", "KNN", "Random Forest")
td 
