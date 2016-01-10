titanic=read.csv(file="/home/germaaan/proyectos/TID/titanic.csv", header=TRUE, sep=",", dec=",")
summary(titanic)

titanic2=titanic
titanic2$Superviviente=ifelse(titanic2$Superviviente==1, "Superviviente", "Fallecido")
titanic2$Clase=ifelse(titanic2$Clase==1, "Primera", 
                      ifelse(titanic2$Clase==2, "Segunda", "Tercera"))

clase=table(titanic2$Clase)
barplot(clase, ylim=c(0, 1000), col=2, main="Pasajeros según clase", xlab="Clase", 
        ylab="Nº de pasajeros")

sexo=table(titanic2$Sexo)
barplot(sexo, ylim=c(0, 1000), col=3, main="Pasajeros según sexo", xlab="Sexo", 
        ylab="Nº de pasajeros")

edad=factor(cut(titanic2$Edad, breaks=0+10*(0:7)))
barplot(table(edad), ylim=c(0, 500), col=4, main="Pasajeros según intervalos de edad", 
        xlab="Intervalo de edades", ylab="Nº de pasajeros")

plot(density(titanic2$Edad), main="Densidad de edades", ylab="Densidad")

boxplot(titanic2$Edad~titanic2$Clase, col=2, main="Pasajeros según clase respecto a la edad",
        xlab="Clase", ylab="Edad")
boxplot(titanic2$Edad~titanic2$Sexo, col=3, main="Pasajeros según sexo respecto a la edad",
        xlab="Sexo", ylab="Edad")

supClase=table(titanic2$Superviviente, titanic2$Clase)
barplot(supClase, ylim=c(0, 1000), main="Supervivientes/Fallecidos según clase", xlab="Sexo", 
        ylab="Nº de pasajeros", col=c(10, 11), legend=rownames(supClase), args.legend = 
          list(x="topright", bty="n", inset=c(-0.15, -0.15)))

supSexo=table(titanic2$Superviviente, titanic2$Sexo)
barplot(supSexo, ylim=c(0, 1000), main="Supervivientes/Fallecidos según sexo", xlab="Sexo", 
        ylab="Nº de pasajeros", col=c(10, 11), legend=rownames(supSexo), args.legend = 
          list(x="topright", bty="n", inset=c(-0.15, -0.15)))

supEdad=table(titanic2$Superviviente, edad)
barplot(supEdad, ylim=c(0, 500), main="Supervivientes/Fallecidos según intervalo de edad", 
        xlab="Edad", ylab="Nº de pasajeros", col=c(10, 11), legend=rownames(supEdad), args.legend = 
          list(x="topright", bty="n", inset=c(-0.15, -0.15)))

# Negro = Superviviente, Rojo = Fallecido
titanic3=titanic[, -c(4)]
titanic3$Superviviente=as.factor(ifelse(titanic3$Superviviente==1, "Superviviente", "Fallecido"))
titanic3$PuertoEmbarque=as.integer(factor(titanic3$PuertoEmbarque))
for (j in 5:8) {x=titanic3[,j] ; v=(x-mean(x))/sqrt(var(x)); titanic3[,j]=v}
pairs(titanic3[, -c(2)], col=titanic3$Superviviente)
