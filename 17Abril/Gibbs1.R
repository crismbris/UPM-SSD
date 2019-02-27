#### # EJEMPLO 4
# Muestreador de Gibbs con variable aleatoria bidimensional discreta. Inicializaci贸n
BcondA=matrix(c(0.6,0.667,0.5,0.4,0.333,0.5), nrow=2, ncol=3, byrow=TRUE)
AcondB=matrix(c(0.5,0.5,0.334,0.25,0.166,0.25), nrow=3, ncol=2, byrow=TRUE)
a=1
b=1
margA=NULL
margB=NULL
samples=matrix(0, nrow=2, ncol=3)

#for loop
for(i in 1:1000){
b=sample(c(1,2),1,prob=BcondA[,a])
a=sample(c(1,2,3),1,prob=AcondB[,b])
margA=append(margA,a)
margB=append(margB,b)
samples[b,a]=samples[b,a]+1
}

### # EJEMPLO 5
## Gibbs de la Normal Bivariante de media (0.0), \sigma^2_x=\sigma^2_y=1 
#  y coeficiente de correlaci贸n \rho
gibbsBVN=function(x,y, n, rho){

#create a matrix to store values
m<-matrix(ncol=2,nrow=n)

#store initial values in matrix
m[1,]<-c(x,y)
#sampling iteration loop
for (i in 2:n){
#rnorm takes sd not variance
#update x conditional on y
x<-rnorm(1,rho*y,sqrt(1-rho^2))
#update y conditional on x from above
y<-rnorm(1,rho*x,sqrt(1-rho^2))

#store values in matrix
m[i,]<-c(x,y)
}
m
}

# gr谩ficos para ver los distintos puntos iniciales
NormalG1=gibbsBVN(0,0,1000,0.8)#Normal de media 0,0 de tama駉 100 y empieza en el pto 0.8
plot(NormalG1)
lines(NormalG1)
NormalG2=gibbsBVN(-2,3,1000,0.8)
plot(NormalG2, type='p')
lines(NormalG2)

## EJEMPLO 6
## Ejemplo del libro, de Gibbs
#valores iniciales
x1=vector()
x2=vector()
x2[1]=0.4
x1[1]=0.34
## for loop
for (i in 2:2000){
x1[i]=rexp(1,rate=1+x2[i-1]^2)
x2[i]=rnorm(1, mean=0, sd=sqrt(1/(2*x1[i])))
}	

hist(x1[1000:2000], freq=FALSE)
# superpongo la funci贸n de densidad marginal de x1 (la he obtenido resolviendo la integral)
curve(exp(-x)*(sqrt(1/(2*x)))*sqrt(2*pi)*(1/pi), from=0, to=8, add=TRUE, col=2)

#para la segunda componente, que resulta tener distribuci贸n de Cauchy (hecha la integral a mano)
hist(x2[1000:2000], xlim=c(-600,600),freq=FALSE)
curve((1/(1+x^2))*(1/pi), from=-600, to=600, add=TRUE, col=2)
ks.test(x2, "pcauchy")
ks.test(x2[1000:2000], "pcauchy")

## conjunta
plot(x1,x2, pch=20)

# EJEMPLO 6 hecho directamente, por la regla del producto:
# f(x1,x2)=f(x2)f(x1|x2)
# calculando la marginal para x2, que es Cauchy:
r1=rcauchy(1000)
rate1=(1+r1^2)
r2=rexp(1:1000,rate=rate1)
plot(r2,r1, pch=20)
hist(r2)

