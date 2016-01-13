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
