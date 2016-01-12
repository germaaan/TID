test=read.csv(file="/home/germaaan/proyectos/TID/test.csv", header=TRUE, sep=",", dec=",")
summary(test)

test2=test
test2$UsoTV=ifelse(test2$UsoTV==1, "Ve TV", "No ve TV")
test2$UsoRRSS=ifelse(test2$UsoRRSS==1, "Usa RRSS", "No usa RRSS")

sexo=table(test2$Sexo)
barplot(sexo, ylim=c(0, 700), col=2, main="Personas según sexo", xlab="Sexo", 
        ylab="Nº de personas")

estado=table(test2$EstadoCivil)
barplot(estado, ylim=c(0, 700), col=3, main="Personas según estado civil", xlab="Estado civil", 
        ylab="Nº de personas")

estudios=table(test2$Estudios)
barplot(estudios, ylim=c(0, 700), col=4, main="Personas según nivel estudios", xlab="Nivel de estudios", 
        ylab="Nº de personas")

edad=factor(cut(test2$Edad, breaks=0+5*(0:10)))
barplot(table(edad), ylim=c(0, 500), col=5, main="Personas según intervalos de edad", 
        xlab="Intervalo de edades", ylab="Nº de personas")

plot(density(test2$Edad), main="Densidad de edades", ylab="Densidad")

tapply(test2$Edad, test2$Sexo, "summary")
boxplot(test2$Edad~test2$Sexo, col=3, main="Personas según sexo respecto a la edad",
        xlab="Sexo", ylab="Edad")
tapply(test2$Edad, test2$EstadoCivil, "summary")
boxplot(test2$Edad~test2$EstadoCivil, col=3, main="Personas según estado civil respecto a la edad",
        xlab="EstadoCivil", ylab="Edad")
tapply(test2$Edad, test2$Estudios, "summary")
boxplot(test2$Edad~test2$Estudios, col=4, main="Personas según nivel de estudios respecto a la edad",
        xlab="Estudios", ylab="Edad")

tvSexo=table(test2$UsoTV, test2$Sexo)
barplot(tvSexo, ylim=c(0, 900), main="Uso TV según sexo", xlab="Sexo", 
        ylab="Nº de personas", col=c(10, 11), legend=rownames(tvSexo), args.legend = 
          list(x="topright", bty="n", inset=c(-0.15, -0.15)))

tvEstado=table(test2$UsoTV, test2$EstadoCivil)
barplot(tvEstado, ylim=c(0, 900), main="Uso TV según estado civil", xlab="Estado civil", 
        ylab="Nº de personas", col=c(10, 11), legend=rownames(tvEstado), args.legend = 
          list(x="topright", bty="n", inset=c(-0.15, -0.15)))

tvEstudios=table(test2$UsoTV, test2$Estudios)
barplot(tvEstudios, ylim=c(0, 900), main="Uso TV según nivel de estudios", xlab="Nivel de estudios", 
        ylab="Nº de personas", col=c(10, 11), legend=rownames(tvEstudios), args.legend = 
          list(x="topright", bty="n", inset=c(-0.15, -0.15)))

rrssSexo=table(test2$UsoRRSS, test2$Sexo)
barplot(rrssSexo, ylim=c(0, 900), main="Uso redes sociales según sexo", xlab="Sexo", 
        ylab="Nº de personas", col=c(10, 11), legend=rownames(rrssSexo), args.legend = 
          list(x="topright", bty="n", inset=c(-0.15, -0.15)))

rrssEstado=table(test2$UsoRRSS, test2$EstadoCivil)
barplot(rrssEstado, ylim=c(0, 900), main="Uso redes sociales según estado civil", xlab="Estado civil", 
        ylab="Nº de personas", col=c(10, 11), legend=rownames(rrssEstado), args.legend = 
          list(x="topright", bty="n", inset=c(-0.15, -0.15)))

rrssEstudios=table(test2$UsoRRSS, test2$Estudios)
barplot(rrssEstudios, ylim=c(0, 900), main="Uso redes sociales según nivel de estudios", xlab="Nivel de estudios", 
        ylab="Nº de personas", col=c(10, 11), legend=rownames(rrssEstudios), args.legend = 
          list(x="topright", bty="n", inset=c(-0.15, -0.15)))

# Negro = Ve TV, Rojo = No ve TV
test3=test
test3$UsoTV=as.factor(ifelse(test2$UsoTV==1, "Ve TV", "No ve TV"))
test3$Sexo=as.integer(factor(test3$Sexo))
test3$EstadoCivil=as.integer(factor(test3$EstadoCivil))
test3$Estudios=as.integer(factor(test3$Estudios))
pairs(test3[, -c(1, 9)], col=1:2)

# Negro = Usa RRSS, Rojo = No usa RRSS
test4=test
test4$UsoRRSS=as.factor(ifelse(test2$UsoRRSS==1, "Usa RRSS", "No usa RRSS"))
test4$Sexo=as.integer(factor(test3$Sexo))
test4$EstadoCivil=as.integer(factor(test3$EstadoCivil))
test4$Estudios=as.integer(factor(test3$Estudios))
pairs(test3[, -c(1, 10)], col=1:2)

test5=test[, c(2, 6, 7, 8)]
factorial=factanal(test5, factors=1, scores="regression")
factorial

# Análisis factorial
y1=factorial$scores
y2=test$UsoTV
y3=test$UsoRRSS
h1=data.frame(y1, y2)
h2=data.frame(y1, y3)
str(h1)
boxplot(h1$Factor1 ~ h1$y2, main="Análisis factorial: uso de televisión", col=2)
str(h2)
boxplot(h2$Factor1 ~ h2$y3, main="Análisis factorial: uso de redes sociales",  col=3)

# Análisis de componentes principales
test6=test[, c(2, 6, 7, 8)]
componentes=prcomp(test6, retX=TRUE)
componentes
str(componentes)
componentes$x

componentes=prcomp(test6, retX=TRUE, tol=0.15)
componentes
str(componentes)
componentes$x
