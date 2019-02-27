# La media del cociente de dos exponenciales independientes. Estudiamos la convergencia de la media
# variando el tamaño de la muestra de 1 a 10000. 
xtaxi=rexp(10000)
ytaxi=rexp(10000)
taxi=xtaxi/ytaxi
sumtaxi=cumsum(taxi)
mediastaxi=sumtaxi/(seq(1:10000))
mediastaxi=data.frame(mediastaxi)

ggplot(data=mediastaxi,aes(x=seq(1:10000), y=log(mediastaxi)))+
  geom_line(colour="blue")+ylim(0,5)
  

#### Qué pasa cuando sí se cumple el TCL, medias de exponenciales
xtaxi=data.frame(xtaxi)
ggplot(data=xtaxi,aes(x=seq(1:10000), y=cumsum(xtaxi)/seq(1:10000)))+
  geom_line(colour="blue")
####

# con intervalos de confianza
library("dplyr")
library("cumstats")
ndf=data.frame(Medias=cummean(taxi), sd=sqrt(cumvar(taxi)/seq(1:10000)))
ndf=mutate(ndf, cil=Medias-1.96*sd, ciu=Medias+1.96*sd, long=ciu-cil)

# longitud de los intervalos (línea negra) comparando con cuando se verifica el TCL (línea roja),
# en la que la longitud de los intervalos es proporcional a 1/sqrt(n).
ggplot(data=ndf,aes(x=seq(1:10000), y=log(long)))+
  geom_line() +
  geom_line(aes(x=seq(1:10000), y=1/sqrt(seq(1:10000)), color="red"))


# 10 veces, con 100000 datos cada una 
xtaxi=matrix(0,100000,10)
ytaxi=matrix(0,100000,10)
for (i in 1:10){xtaxi[,i]=rexp(100000)}
for (i in 1:10){ytaxi[,i]=rexp(100000)}
taxi=xtaxi/ytaxi
taxi=data.frame(taxi)
str(taxi)
sumtaxi=apply(taxi,2,cumsum)
for (i in 1:10){sumtaxi[,i]=sumtaxi[,i]/seq(1:100000)}

library("reshape2")
grandtaxi=melt(sumtaxi)

ggplot(data=grandtaxi,
       aes(x=Var1, y=log(value), colour=Var2)) +
  geom_line()



