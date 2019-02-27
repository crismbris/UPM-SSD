# Distribución de la suma de dos uniformes (0,1) (hecho en clase analíticamente):
vals=runif(6000)+runif(6000)
hist(vals, breaks=50, freq=FALSE, main=expression("Sample vs true Density"))
curve(1*x, 0, 1, col="red", lwd=2, add=TRUE)
curve(2-x, 1, 2, col="red", lwd=2, add=TRUE)

### Simulación de la exponencial de media 500h., lambda=tasa=1/500=0.002, mediante inversión. Generamos 1000 valores.
miexpo=-500*log(1-runif(1000))
hist(miexpo, freq=FALSE)
curve(0.002*exp(-(0.002)*x), 0, 3000, col="red", lwd=2, add=TRUE)

# Usando el paquete ggplot2 para dibujar el histograma
miexpo=data.frame(miexpo)
summary(miexpo)
G=ggplot(miexpo, aes(x=miexpo))+geom_histogram(aes(x=miexpo,y=..density..), breaks=seq(1,4000,500),fill="grey", colour="white")
G+stat_function(fun=dexp,color='orange',args=list(rate=0.002),lwd=1)

## Estimación de la P(X<Y) cuando X, Y son exponenciales de tasa 3 y 8 respectivamente. Generamos muestras de 39000 valores.
## ¿Cuál es el resultado teórico de esta probabilidad?
e1=rexp(39000,rate=3)
# o bien 
e1=-(1/3)*log(1-runif(39000))
e2=rexp(39000,rate=8)
#o bien
e2=-(1/8)*log(1-runif(39000))
sum(e1<e2)/39000

########### Ejercicio 11
##### Creamos una matriz de dimensiones 5 filas y 10000 columnas, llena de 0's. La llamamos beta.
beta=matrix(0,nrow=5,ncol=10000)
### La lleno con valores generados de una exponencial de media 500 h, tasa=0.002 (o los generamos por inversi?n)
beta[1:5,]=rexp(10000,rate=0.002)
### Ordeno cada columna
betaord=apply(beta,2,sort)
### calculo la media de la fila segunda para estimar el tiempo medio de funcionamiento del sistema:
mean(betaord[2,])
##########

### El mismo ejercicio, usando el resultado de las betas
beta1=rbeta(10000,2,4)
beta2=-500*log(1-beta1)
mean(beta2)

## El mismo, usando 4 componentes, se rompe cuando se rompe la primera
beta1c=rbeta(10000,1,5)
beta2c=-500*log(1-beta1c)
mean(beta2c)
## también puede hacerse sabiendo la distribución del mínimo de 4 exponenciales
mean(rexp(10000,rate=0.008))

## El ejercico 12, 5 bloques, cada uno como el anterior de 5 subsistemas. Cada bloque falla cuando falla el segundo subsistema y el 
## sistema falla cuando lo hace el segundo bloque:
##### Creamos una matriz de dimensiones 5 filas y 10000 columnas, llena de 0's. La llamamos beta.
betaC=matrix(0,nrow=5,ncol=10000)
### La lleno con valores generados del X(2) de una exponencial de media 500 h, tasa=0.002 (o los generamos por inversión)
betaC[1:5,]=-500*log(1-rbeta(10000,2,4))
### Ordeno cada columna
betaCord=apply(betaC,2,sort)
### calculo la media de la fila segunda para estimar el tiempo medio de funcionamiento del sistema:
mean(betaCord[2,])

##########
#El ejercicio 12 directamente
##########
nuevo=array(0,dim=c(5,5,10000))
nuevo[,,]=rexp(250000, rate=0.002)
min2=matrix(nrow=5, ncol=10000)
for(i in 1:10000){nuevo[,,i]=apply(nuevo[,,i],2,sort)}
for(i in 1:10000){min2[1:5,i]=sort(nuevo[2,,i])}
mean(min2[2,])
##########

### Simulación de la distribución de la suma de los números que salen al lanzar cuatro dados:
resultado=sapply(1:10000, function(x){sum(sample(1:6,4,rep=T))})
barplot(table(resultado))

### Simulación de una distribución discreta, por ejemplo
x=c(1,2,3,4)
probs=c(0.1,0.2,0.3,0.4)
sample(x,50,replace=TRUE,prob=probs)

## Estudiar la distribución de log(u/1-u) si u es U(0,1)
miunif=runif(10000)
milog=log(miunif/(1-miunif))
hist(milog, freq=FALSE)
## Histograma, con la curva logística estándar superpuesta
hist(milog, freq=FALSE, ylim=c(0,0.25))
curve(dlogis(x, 0, 1), from=-10, to=10, main="Logística estándar", add=TRUE, col=2)

## Estudiar si es bueno el ajuste de la muestra generada a una distribución logística estándar.
fitdistr(milog,"logistic", start=list(location=0,scale=1))
ks.test(milog, "plogis",0, 1)

## Estudiar la distribución del máximo de tres v.a. uniformes en (0,1)
miunif2=matrix(runif(3000),nrow=3,ncol=1000)
hist(apply(miunif2,2,max), freq=FALSE)
curve(3*x^2, from=0, to=1, add=TRUE, col=2)

## Histograma, con la función de densidad de una v.a. Beta(3,1) superpuesta
hist(apply(miunif2,2,max), freq=FALSE)
curve(dbeta(x, shape1=3, shape2=1), from=0, to=1, main="Beta(3,1)", add=TRUE)



