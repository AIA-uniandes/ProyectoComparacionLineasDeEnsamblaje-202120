## TRABAJADOR 1

datos1 <- read.table('AjusteT1.txt', header=F)
library(fitdistrplus)
library(logspline)

#TAREA 1
plotdist(datos1[,1])
dist1 <- fitdist(datos[,1], "unif")
plot(dist1)
summary(dist1)
descdist(datos[,1], discrete = FALSE)

#TAREA 2
plotdist(datos1[,2])
dist1 <- fitdist(datos1[,2], "unif")
plot(dist1)
summary(dist1)
descdist(datos1[,2], discrete = FALSE)

#TAREA 3
plotdist(datos1[,3])
dist1 <- fitdist(datos1[,3], "unif")
plot(dist1)
summary(dist1)
descdist(datos1[,3], discrete = FALSE)

#TAREA 4
plotdist(datos1[,4])
dist1 <- fitdist(datos1[,4], "unif")
plot(dist1)
summary(dist1)
descdist(datos1[,4], discrete = FALSE)

#TAREA 5
plotdist(datos1[,5])
dist1 <- fitdist(datos1[,5], "unif")
plot(dist1)
summary(dist1)
descdist(datos1[,5], discrete = FALSE)

#TAREA 6
plotdist(datos1[,6])
dist1 <- fitdist(datos1[,6], "unif")
plot(dist1)
summary(dist1)
descdist(datos1[,6], discrete = FALSE)

#TAREA 7
plotdist(datos1[,7])
dist1 <- fitdist(datos1[,7], "unif")
plot(dist1)
summary(dist1)
descdist(datos1[,7], discrete = FALSE)

## TRABAJADOR 2

datos2 <- read.table('AjusteT2.txt', header=F)
library(fitdistrplus)
library(logspline)

#TAREA 1
plotdist(datos2[,1])
dist1 <- fitdist(datos2[,1], "unif")
plot(dist1)
summary(dist1)
descdist(datos2[,1], discrete = FALSE)

#TAREA 2
plotdist(datos2[,2])
dist1 <- fitdist(datos2[,2], "unif")
plot(dist1)
summary(dist1)
descdist(datos2[,2], discrete = FALSE)

#TAREA 3
plotdist(datos2[,3])
dist1 <- fitdist(datos2[,3], "unif")
plot(dist1)
summary(dist1)
descdist(datos2[,3], discrete = FALSE)

#TAREA 4
plotdist(datos2[,4])
dist1 <- fitdist(datos2[,4], "unif")
plot(dist1)
summary(dist1)
descdist(datos2[,4], discrete = FALSE)

#TAREA 5
plotdist(datos2[,5])
dist1 <- fitdist(datos2[,5], "unif")
plot(dist1)
summary(dist1)
descdist(datos2[,5], discrete = FALSE)

#TAREA 6
plotdist(datos2[,6])
dist1 <- fitdist(datos2[,6], "unif")
plot(dist1)
summary(dist1)
descdist(datos2[,6], discrete = FALSE)

#TAREA 7
plotdist(datos2[,7])
dist1 <- fitdist(datos2[,7], "unif")
plot(dist1)
summary(dist1)
descdist(datos2[,7], discrete = FALSE)

## GRAFICAR DATOS
datosT1 <- as.data.frame(datos1)
colnames(datosT1) <- c("Tarea 1","Tarea 2", "Tarea 3", "Tarea 4","Tarea 5", "Tarea 6", "Tarea 7" )
library(tidyr)
datosT1 <- gather(data=datosT1, key="Tarea",value="Tiempo",1:7)
trab1 <- rep("Trabajador 1",210)
datosT1$Trabajador <- trab1

datosT2 <- as.data.frame(datos2)
colnames(datosT2) <- c("Tarea 1","Tarea 2", "Tarea 3", "Tarea 4","Tarea 5", "Tarea 6", "Tarea 7" )
library(tidyr)
datosT2 <- gather(data=datosT2, key="Tarea",value="Tiempo",1:7)
trab2 <- rep("Trabajador 2",210)
datosT2$Trabajador <- trab2

datosT <- rbind(datosT1,datosT2)

library(ggplot2)

ggplot(datosT, aes(x=Tarea, y=Tiempo, fill=Trabajador )) + geom_boxplot() + ylab("Tiempo de procesamiento (min)")
