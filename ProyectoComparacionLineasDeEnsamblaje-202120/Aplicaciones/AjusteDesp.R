datos <- read.csv('TiempoDesp.csv', header=T)

library(fitdistrplus)

plotdist(datos[,1])

dist <- fitdist(datos[,1], "lnorm")
summary(dist)

plot(dist)

library(logspline)

descdist(datos[,1], discrete = FALSE)

#Línea de ensamble
datos2 <- read.csv('Tdesp_linea.csv', header=T)

library(fitdistrplus)

plotdist(datos2[,1])

dist <- fitdist(datos2[,1], "unif")
summary(dist)

plot(dist)

library(logspline)

descdist(datos2[,1], discrete = FALSE)

