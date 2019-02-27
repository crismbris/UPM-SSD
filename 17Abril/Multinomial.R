## Distribuciones Multivariantes

# EJEMPLO 1
# Multinomial, por ejemplo n=20, prob=c(0.3,0.2,0.5)
# Primero una Bin(20, 0.3)
m=40
muestra=matrix(nrow=3, ncol=m)
p=c(0.3,0.2,0.5)
for(i in 1:m){
x1=rbinom(1,20,p[1])
x2=rbinom(1,20-x1,p[2]/(1-p[1]))
x3=20-x1-x2
muestra[,i]=c(x1,x2,x3)
}

# EJEMPLO 2
# Obtener valores de una Normal bivariante Usando la función mvrnorm del paquete MASS
bivn=mvrnorm(10000, mu=c(5,5), Sigma=matrix(c(1,0.7,0.7,1),2))
hist(bivn[,1])
# dibujos
bivn.kde <- kde2d(bivn[,1], bivn[,2], n = 50)
image(bivn.kde)
contour(bivn.kde, add = T)

# # EJEMPLO 3
# de una Normal bivariante a partir de la marginal para x1, y la condicionada x2|x1
# usando que la condicionada también es normal
z1=rnorm(1000)
z2=rnorm(1000)
x1=5+z1
x2=5+0.7*z1+sqrt(1-0.7^2)*z2
hist(x2)
head(bivn)
