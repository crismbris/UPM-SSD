###############
# EJERCICIO 1 #
###############

#APARTADO A
u1=runif(100000,-1,0)
u2=runif(100000)
u3=runif(100000)
u4=runif(100000)
#calculamos la inversa de la funcion
res1 = u1[u1>=u2-1]
hist(res1,freq=F)
res2 = u3[u3<=1-u4]
hist(res2,freq=F)
res = c(res2,res1)
hist(res,main = "Apartado a",freq=F)
curve(dtriangle(x,a=-1,b=1,c=0), add = TRUE,col = 2)
ks.test(res, "ptriangle",a=-1,b=1,c=0)

#APARTADO B
a1=runif(100000,-1,0)
a2=runif(100000)
d1=runif(100000)
d2=runif(100000)
res1 = a1[a2<=a1+1]
hist(res1, freq=F)
res2 = d1[d2<=-d1+1]
hist(res2, freq=F)
res=c(res1,res2)
hist(res,main = "Apartado a",freq=F)
curve(dtriangle(x,a=-1,b=1,c=0), add = TRUE,col = 2)
ks.test(res, "ptriangle",a=-1,b=1,c=0)



###############
# EJERCICIO 2 #
###############


ejercicio2 <- function(){
  u1=runif(1000000)
  u2=runif(1000000)
  x.beta23=u1[u2<=(12*u1*(1-u1)^2)/3]
  ks.test(x.beta23, "pbeta", shape1=2 ,shape2=3)
  hist(x.beta23, freq=F)
  curve(dbeta(x, shape1=2, shape2=3), from=0, to=1, main="Beta(2,3)", add=TRUE,col=3)
}



###############
# EJERCICIO 3 #
###############


ejercicio3 <- function(){
  u=runif(100000)
  v=runif(100000,-sqrt(2/exp(1)),sqrt(2/exp(1)))
  result
  for (i in (1:100000)){
    if (v[i]^2 <= -4*u[i]^2*log(u[i])){
      result= c(result,(v[i]/u[i]))
    }
  }
  ks.test(result,pnorm, mean=0, sd=1)
  hist(result,freq=F)
  curve(dnorm(x), from=-5, to=5, add=TRUE, col=3)
  
}


