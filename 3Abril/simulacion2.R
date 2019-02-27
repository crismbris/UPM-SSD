# Generamos de una distribución Beta(2,2) usando el método de rechazo:
u1=runif(1000)
u2=runif(1000)
x.beta22=u1[u2<=4*u1*(1-u1)] #Me quedo con los U1 que cumplen la condicion que hay entre corchetes
# comprobamos la proporción (de los 1000) de puntos aceptados
length(x.beta22)/1000
# La probabilidad teórica de aceptación de un valor es 0.666, lo que quiere decir que de 1000 valores se aceptarán solamente 666. Si
## queremos generar una muestra de tamaño n deberíamos generar aproximadamente n/p, para asegurarnos esa cantidad.
# Comprobamos con K-S el ajuste a esa distribución beta
ks.test(x.beta22, "pbeta", shape1=2,shape2=2)
# Dibujamos un histograma con la función de densidad superpuesta
hist(x.beta22, freq=F)
curve(dbeta(x, shape1=2, shape2=2), from=0, to=1, main="Beta(2,4)", add=TRUE,col=3)

# El mismo dibujo con el paquete ggplot2
x.beta22=data.frame(x.beta22)
G=ggplot(x.beta22, aes(x=x.beta22))+geom_histogram(binwidth=0.1,aes(x=x.beta22,y=..density..), fill="grey", colour="white")+stat_function(fun=dbeta,color='orange',args=list(shape1=2,shape2=2),lwd=1)
G

# Generamos una distribución N(0,1) por rechazo, usando como envolvente una Cauchy

x.cauchy=tan(pi*u1)
s=x.cauchy^2
x.norm1=x.cauchy[u2<=sqrt(exp(1))/2*(1+s)*(exp(-s/2))]
# paquete tseries
jarque.bera.test(x.norm1)
hist(x.norm1, freq=F)
curve(dnorm(x), from=-5, to=5, add=TRUE, col=3)


# Generamos de una distribución N(0,1), usando como envolvente la función de densidad logística 
# generamos de la logística por inversión
x.log=log(u1/(1-u1))
x.norm2=x.log[u2<=1/4*(1+exp(-x.log))^2*exp(x.log-x.log^2/2)]
jarque.bera.test(x.norm2)
hist(x.norm2, freq=F)
curve(dnorm(x), from=-5, to=5, add=TRUE, col=3)

# Generamos de una distribución Gamma(rate=1, p=shape=3/2) por rechazo, usando como envolvente una exponencial de igual media, es decir, una
# exponencial de media 3/2=1.5 y rate=2/3.
y=(-1.5)*log(runif(10000))#exponencial
ue2=runif(10000)
x.gamma1=y[ue2<=sqrt(2*exp(1)/3)*sqrt(y)*exp(-y/3)]
ks.test(x.gamma1, "pgamma", shape=1.5 ,rate=1)
hist(x.gamma1, freq=F, ylim=c(0,0.5))
curve(dgamma(x, shape=1.5, rate=1), from=0, to=10, add=TRUE,col=3)

# Generamos de una Gamma(rate=1,p=shape=2) condicionada a X>=5. Usamos como envolvente Y una exponencial de la misma media (2) condicionada a Y>=5.
# Generamos de la exponencial condicionada
y.2=5-2*log(runif(10000))
ue2=runif(10000)
x.gamma2=y.2[ue2<=(1/5)*exp(5/2)*y.2*exp(-y.2/2)]
hist(x.gamma2, freq=F)
curve(x*exp(-x)/(6*exp(-5)), from=5, to=10, add=TRUE,col=3)

# Generamos de la cola derecha de una N(0,1), truncada para A=5
x.aux=5-log(u1)/5
x.norm3=x.aux[u2<=exp(25/2)*exp(-x.aux^2/2)*exp(5*(x.aux-5))]
hist(x.norm3, freq=F)
curve(exp(-x^2/2)/(sqrt(2*pi)*pnorm(5,0,1,lower.tail=FALSE)), from=5, to=10, col=3, add=TRUE)

# paquete ars: adaptive rejection sampling
library("ars")
log.f <- function(x) log(3*x^2)
log.f.dx <- function(x) 2/x
vals<- ars(10000, log.f, log.f.dx, x=c(0.1,0.5,0.9),lb=TRUE, ub=TRUE, xlb=0, xub=1)
hist(vals,breaks=30,freq=FALSE)
curve(3*x^2,0,01,col="red",add=TRUE) 

# Ejercicio: usando la función ars(), generar 10000 valores de una
# beta(2,2)
log.f1 <- function(x) log(6*x*(1-x))
log.f1.dx <- function(x) 1/x-1/(1-x)
vals<- ars(10000, log.f1, log.f1.dx, x=c(0.1,0.5,0.9),lb=TRUE, ub=TRUE, xlb=0, xub=1)
hist(vals,breaks=30,freq=FALSE)
curve(6*x*(1-x),0,1,col="red",add=TRUE)

# Ejercicio: usando la función ars(), generar 10000 valores de una Gamma(rate=0.5, shape=2)
log.f2<-funciton(x)log((1/4)*x*exp(-x/2))

# Ejemplos de composición, ejemplo final de las transparencias para n=3
u1=runif(1000)
u2=runif(1000)
y.3=u1^{-1/3}
x.comp1=(-1/y.3)*log(u2)

# Simular de una Poisson(5) a partir de una Exponencial, simulo solamente un valor
u=runif(20)
which.max(cumprod(u)<exp(-5))-1

# Cauchy por ratio de uniformes
u1=runif(1000,-1,1)
u2=runif(1000,-1,1)
x.cauchy=u2[u1^2+u2^2<1]/u1[u1^2+u2^2<1]
length(x.cauchy)
ks.test(x.cauchy, "pcauchy")


