### Suponemos que tenemos una variable tiempos y queremos estudiar un modelo de probabilidad que se 
### ajuste a ella
tiempos=read.table("tiempos.txt")
str(tiempos)
head(tiempos)
hist(tiempos$V1)
## Estimamos gráficamente la función de densidad
plot(density(tiempos$V1))
## Función de distribución empírica de los datos
plot(ecdf(tiempos$V1))
plot(ecdf(tiempos$V1), do.point=F)
summary(tiempos)
## Cargamos el paquete e1071 que tiene las funciones de asimetría y curtosis
skewness(tiempos$V1)
kurtosis(tiempos$V1)
### checking for outliers con el boxplot
boxplot(tiempos$V1)

## Estimación de parámetros. 
# Supongamos que visualmente hemos visto que podría ajustarse a una distribucion normal. 
## Para estimar sus parámetros, media y desviación típica, usamos la función fitdistr() 
## del paquete MASS:
fitdistr(tiempos$V1, "normal")
## Para, por ejemplo, una gamma
fitdistr(tiempos$V1, "gamma")
## Para, por ejemplo, una exponencial
fitdistr(tiempos$V1, "exponential")
## Para una Lognormal
fitdistr(tiempos$V1, "lognormal")
## Una Weibull
fitdistr(tiempos$V1, "weibull")

## Ver ajuste a ojo con el histograma, superponemos las funciones de densidad de una weibull, normal y lognormal
hist(tiempos$V1, freq=FALSE)
lines(seq(1,2,0.01),dweibull(seq(1,2,0.01),12.25,1.48))
# ? mejor
curve(dweibull(x,12.25,1.48),from=1,to=2, add=TRUE, col=2)
lines(seq(1,2,0.01),dnorm(seq(1,2,0.01),1.42,0.12),col=3)
lines(seq(1,2,0.01),dlnorm(seq(1,2,0.01),0.35,0.08),col=4)

## ver ajuste a ojo, con la función de distribución empírica
plot(ecdf(tiempos$V1),do.points=FALSE)
lines(seq(1,2,0.01),pweibull(seq(1,2,0.01),12.25,1.48))
lines(seq(1,2,0.01),pnorm(seq(1,2,0.01),1.42,0.12),col=2)
lines(seq(1,2,0.01),plnorm(seq(1,2,0.01),0.35,0.08),col=3)

### El QQplot es una herramienta que nos permite comparar visualmente los cuantiles de una 
## distribución teórica (posible modelo para los datos) con los cuantiles de la función de 
## distribución empírica de los datos y ver si se obtiene un buen ajuste.
library("car")
qqPlot(tiempos$V1, dist="norm")
qqPlot(tiempos$V1, dist="exp")
qqPlot(tiempos$V1, dist="gamma", shape=130, rate=91)
qqPlot(tiempos$V1, dist="lnorm", meanlog=0.35, sdlog=0.087)

## Paquete distr.
## muestrear de la distribución empírica de una muestra. 
## tiempos es el conjunto de datos que tienes

emp.cdf=DiscreteDistribution(tiempos$V1)

new.tiempos <- emp.cdf@r(250)
hist(tiempos$V1, freq=F)
hist(new.tiempos, freq=F)
plot(ecdf(tiempos$V1), col="blue")  
plot(ecdf(new.tiempos), col="red", pch=".", add=T)


## lo mismo pero usando la función de densidad: se usa un núcleo gaussiano (normal) para estimar la 
## densidad a partir de la muestra.
d=density(tiempos$V1)
new_tiempos <- sample(d$x, 1000,replace=TRUE, prob=d$y)

# plot original sample vs new sample
plot(density(tiempos$V1), col="blue")
lines(density(new_tiempos), col="red")
legend("topleft",c("empirical pdf","new sampling"), col=c("blue","red"), lwd=2) 


### Contrastes no paramétricos: Kolmogorov-Smirnov (para contrastar normalidad ver mas abajo)
## Contrastamos si la muestra proviene de una gamma con los parámetros estimados anteriormente. 
# EL test de K-S NO SIRVE cuando no tenemos de antemano los parámetros de la distribución sino que hay que estimarlos a partir de los datos. 
# Es una limitación muy importante. No obstante, os pongo las funciones que habría que usar. 
# Cuando el p-valor obtenido es pequeño, hace rechazar el test, se puede mejorar el resultado usando simulación con el test K-S 
# Ver libro de Ross, pag. 230.
ks.test(tiempos$V1, "pgamma",130, 91)
ks.test(tiempos$V1, "pexp", rate=0.7)
ks.test(tiempos$V1, "plnorm",0.35, 0.087)
ks.test(tiempos$V1, "pweibull",12, 1.5)

### Para una Weibull sale p-v=0.06. Por simulación, siguiendo a Ross.
n=55
D1=rep(0,2000)

for (headi in 1:2000){
  muestra=rweibull(55,12.25760728,1.48211145 )
  p1=fitdistr(muestra,"weibull")$estimate[1]
  q1=fitdistr(muestra,"weibull")$estimate[2]
  D1[i]=ks.test(muestra,"pweibull",p1,q1)$statistic
}

sum(D1>0.17824)/2000 #0.17824 es el valor observado en el k-s aplicado a la muestra.
#sale un p-valor de 0, rechazamos la Weibull

# Para una lognormal, por simulación, siguiendo a Ross.
n=55
D1=rep(0,2000)

for (i in 1:2000){
  muestra=rlnorm(55,0.35,0.087 )
  p1=fitdistr(muestra,"lognormal")$estimate[1]
  q1=fitdistr(muestra,"lognormal")$estimate[2]
  D1[i]=ks.test(muestra,"plnorm",p1,q1)$statistic
}

sum(D1>0.10142)/2000 #0.17824 es el valor observado en el k-s aplicado a la muestra.
#sale un p-valor de 0.18 (dependen de cada ejecución), no rechazamos la Lognormal.

## Contraste de la chi-2 de Pearson, a mano, para contrastar el ajuste a una distribución gamma, 
# tenemos 55 datos.
n.cl=8
puntos=min(tiempos)+(0:n.cl)*(max(tiempos)-min(tiempos))/n.cl
Tabla=table(cut(tiempos$V1,breaks=puntos))
fitdistr(tiempos$V1, "gamma")
E=vector() #esperados 
E[1]=55*pgamma(puntos[2],129.8,91.13)
E[8]=55*(1-pgamma(puntos[8],129.8,91.13))
for (i in 2:7){E[i]=(pgamma(puntos[i+1],129.8,91.13)-pgamma(puntos[i],129.8,91.13))*55}
Obs=vector()
for(i in 1:8){Obs[i]=Tabla[[i]]}
X2=sum((Obs-E)^2/E)
##p-valor
1-pchisq(X2,5)

### Contrastes de normalidad

## Jarque-Bera muy usado para contrastar normalidad en Econometría. Paquete tseries: 
library(tseries)
jarque.bera.test(tiempos$V1)

## Contraste de la Chi-2, SOLO para normalidad
library(nortest)
pearson.test(tiempos$V1)

## Paquete ggplot2, define una gramática de gráficos. Gráficos muy versátiles:
## Histograma
G=ggplot(tiempos, aes(x=V1))+geom_histogram(breaks=c(1.1,1.2,1.3,1.4,1.5,1.6,1.7),aes(x=V1,y=..density..), fill="grey", colour="white")
## Histograma + Weibull
GG=G+stat_function(fun=dweibull,color='orange',args=list(shape=12.25,scale=1.48),xlim=c(1.1,1.8),lwd=1)
## Weibull +Normal
GG+stat_function(fun=dnorm,color='red',args=list(mean=1.42,sd=0.12))





