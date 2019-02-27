llegadas = c(E3_llegadas)
ks.test(llegadas$llegadas,"pexp",4.615)
curve(dexp(x,rate=4.615))
hist(sol$llegadas,freq=F)
