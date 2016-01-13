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
