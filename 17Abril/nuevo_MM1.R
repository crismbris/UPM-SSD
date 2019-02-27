# Simulación de un sistema M/M/1. Tasa de llegadas lambda=2 y tasa de servicios mu=3.
lambda = 2
mu = 3

# tiempo que dura la simulación
time = 10000
t = 0

# Q_hist[n] es el número de personas en el sistema antes de que se produzca el suceso n-ésimo
Q_hist = 0
# s es la suma acumulada para calcular la longitud media de la cola
s = 0

# El sistema está inicialmente vacío. Generamos la primera llegada:
T1 = rexp(1,rate=lambda)
Q = 1 # número de clientes en el sistema
event_times = T1
t = T1
num_event = 1 #número del suceso, éste es el primero

# El siguiente suceso puede ser una llegada o una salida. El tiempo hasta el próximo suceso es T1 
# y se distribuye como exp(lambda+mu). Para determinar si ese suceso es una llegada o una salida, 
# generamos una variable aleatoria U(0,1). Si u<lambda/(lambda+mu) será una llegada, si no, una
# salida.

while (t<time) {
  num_event = num_event+1
  if(Q>0) {
    # comprobamos que el sistema no estaba vacío
    T1 = rexp(1,rate=lambda+mu)
    p = runif(1,0,1)
    Q_hist[num_event] = Q
    Q = ifelse(p<lambda/(lambda+mu),Q+1,Q-1)
  } else {
    # Si el sistema estaba vacío, solo son posibles llegadas
    T1 = rexp(1,rate=lambda)
    Q_hist[num_event] = Q
    Q = 1
  }
  t = t+T1 # actualizamos el tiempo de la simulación
  event_times[num_event] = T1 # actualizamos el tiempo entre sucesos
  s = s+T1*Q_hist[num_event] # he pasado un tiempo T1 con una cola de Q_hist[num_event] clientes
}

# s/t es la longitud media de la cola.
plot(cumsum(event_times),Q_hist,type="s", xlab="Tiempo",ylab="Longitud de la cola",
     main="M/M/1 Simulation")
mtext(paste("longitud media de la cola=",s/t))
# También la podemos estimar como:
sum(Q_hist*event_times)/10000

# Proporción de tiempo en la simulación en la que la cola tiene longitud 0:
sum(event_times[Q_hist==0])/t
# El valor teórico es pi_0=1-rho, con rho=lambda/mu
1-lambda/mu

# Proporción de tiempo en la simulación en la que la cola tiene longitud 1:
sum(event_times[Q_hist==1])/t
# El valor teórico es pi_1=(1-rho)*rho, con rho=lambda/mu
(1-lambda/mu)*(lambda/mu)

# Paquete simmer: Discrete-Event simulation for R. Sistema M/M/1. Este paquete utiliza ggplot2 para
# los gráficos. 
lambda <- 2
mu <- 3
rho <- lambda/mu # = 2/3

mm1.trajectory <- trajectory() %>%
  seize("resource", amount=1) %>%
  timeout(function() rexp(1, mu)) %>%
  release("resource", amount=1)

mm1.env <- simmer() %>%
  add_resource("resource", capacity=1, queue_size=Inf) %>%
  add_generator("arrival", mm1.trajectory, function() rexp(1, lambda)) %>% 
  run(until=2000)

mm1.N <- rho/(1-rho) # número medio de clientes en el sistema

# 
resources <- get_mon_resources(mm1.env)
plot(resources, metric = "utilization")
# con o sin steps=TRUE
plot(resources, metric = "usage", items = c("queue", "server"))+geom_hline(yintercept=mm1.N)
plot(resources, metric = "usage", items = c("queue", "server"), steps=TRUE)+
  xlim(0, 20) + ylim(0, 4)

# A partir de la simulación, se puede calcular el tiempo medio que pasa un cliente en el sistema:
mm1.arrivals <- get_mon_arrivals(mm1.env)
mm1.t_system <- mm1.arrivals$end_time - mm1.arrivals$start_time

mm1.T <- mm1.N / lambda
mm1.T ; mean(mm1.t_system)

