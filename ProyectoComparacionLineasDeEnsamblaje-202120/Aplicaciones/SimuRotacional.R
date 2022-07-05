#Seru Rotacional con 7 tareas
library(simmer)
library(simmer.plot)
library(parallel)

#Constantes o parametros del modelo####
numTareas<- 7     # Numero de tareas en la linea
numSerus <- 2 #Número de trabajadores
Tiempo_Sim <- 240 # tiempo de la simulacion: 
replicas <- 100 #numero de replicas de la simulacion

#Estructura de la simulacion ####

#Tiempo de desplazamiento entre tareas
tDesp <- function() rlnorm(1, meanlog = -2.119, sdlog = 0.2626)

#Tiempo de proceso del trabajador 1
tarea1T1 <- function() runif(1, min = 0.35, max = 0.69)
tarea2T1 <- function() runif(1, min = 0.333, max = 0.567)
tarea3T1 <- function() runif(1, min = 0.617, max = 0.835)
tarea4T1 <- function() runif(1, min = 0.56, max = 0.833)
tarea5T1 <- function() runif(1, min = 0.95, max = 1.25)
tarea6T1 <- function() runif(1, min = 0.61, max = 0.7)
tarea7T1 <- function() runif(1, min = 0.667, max = 0.983)

#Tiempo de proceso del trabajador 2
tarea1T2 <- function() runif(1, min = 0.367, max = 0.55)
tarea2T2 <- function() runif(1, min = 0.317, max = 0.483)
tarea3T2 <- function() runif(1, min = 0.433, max = 0.633)
tarea4T2 <- function() runif(1, min = 0.45, max = 0.683)
tarea5T2 <- function() runif(1, min = 0.883, max = 1.25)
tarea6T2 <- function() runif(1, min = 0.5, max = 0.833)
tarea7T2 <- function() runif(1, min = 0.65, max = 0.917)

# TRAYECTORIA TAREAS

#Seru 1
tarea1_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 1 con el trabajador 1")%>% 
  seize("tarea_1",amount = 1) %>%
  timeout(tarea1T1)%>%
  release("tarea_1")

tarea2_T1 <- trajectory()%>%
  log_("Entra parte a la tarea 2 con el trabajador 1")%>% 
  seize("tarea_2", amount = 1) %>%
  timeout(tarea2T1)%>%
  release("tarea_2")

tarea3_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 3 con el trabajador 1")%>% 
  seize("tarea_3", amount = 1) %>%
  timeout(tarea3T1)%>%
  release("tarea_3")

tarea4_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 4 con el trabajador 1")%>% 
  seize("tarea_4", amount = 1) %>%
  timeout(tarea4T1)%>%
  release("tarea_4")

tarea5_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 5 con el trabajador 1")%>% 
  seize("tarea_5", amount = 1) %>%
  timeout(tarea5T1)%>%
  release("tarea_5")

tarea6_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 6 con el trabajador 1")%>% 
  seize("tarea_6", amount = 1) %>%
  timeout(tarea6T1)%>%
  release("tarea_6")

tarea7_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 7 con el trabajador 1")%>% 
  seize("tarea_7", amount = 1) %>%
  timeout(tarea7T1)%>%
  release("tarea_7")

#Seru 2
tarea1_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 1 con el trabajador 2")%>% 
  seize("tarea_1", amount = 1) %>%
  timeout(tarea1T1)%>%
  release("tarea_1")

tarea2_T2 <- trajectory()%>%
  log_("Entra parte a la tarea 2 con el trabajador 2")%>% 
  seize("tarea_2", amount = 1) %>%
  timeout(tarea2T1)%>%
  release("tarea_2")

tarea3_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 3 con el trabajador 2")%>% 
  seize("tarea_3", amount = 1) %>%
  timeout(tarea3T1)%>%
  release("tarea_3")

tarea4_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 4 con el trabajador 2")%>% 
  seize("tarea_4", amount = 1) %>%
  timeout(tarea4T1)%>%
  release("tarea_4")

tarea5_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 5 con el trabajador 2")%>% 
  seize("tarea_5", amount = 1) %>%
  timeout(tarea5T1)%>%
  release("tarea_5")

tarea6_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 6 con el trabajador 2")%>% 
  seize("tarea_6", amount = 1) %>%
  timeout(tarea6T1)%>%
  release("tarea_6")

tarea7_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 7 con el trabajador 2")%>% 
  seize("tarea_7", amount = 1) %>%
  timeout(tarea7T1)%>%
  release("tarea_7")

# TRAYECTORIA SERU

trabajador1 <- trajectory()%>% 
  #aviso en consola de llegada de parte al sistema.
  log_("Llega pieza a ser procesada por el trabajador 1")%>%  
  seize("Trabajador_1", amount = 1)%>%
  join(tarea1_T1)%>%
  timeout(tDesp)%>%
  join(tarea2_T1)%>%
  timeout(tDesp)%>%
  join(tarea3_T1)%>%
  timeout(tDesp)%>%
  join(tarea4_T1)%>%
  timeout(tDesp)%>%
  join(tarea5_T1)%>%
  timeout(tDesp)%>%
  join(tarea6_T1)%>%
  timeout(tDesp)%>%
  join(tarea7_T1)%>%
  timeout(tDesp) %>% 
  activate("Parte1-")%>% 
  log_("Sale la pieza del seru")%>% 
  release("Trabajador_1", amount = 1)

trabajador2 <- trajectory()%>% 
  #aviso en consola de llegada de parte al sistema.
  log_("Llega pieza a ser procesada por el trabajador 2")%>%  
  seize("Trabajador_2", amount = 1)%>%
  join(tarea1_T2)%>%
  timeout(tDesp)%>%
  join(tarea2_T2)%>%
  timeout(tDesp)%>%
  join(tarea3_T2)%>%
  timeout(tDesp)%>%
  join(tarea4_T2)%>%
  timeout(tDesp)%>%
  join(tarea5_T2)%>%
  timeout(tDesp)%>%
  join(tarea6_T2)%>%
  timeout(tDesp)%>%
  join(tarea7_T2)%>%
  timeout(tDesp) %>%
  activate("Parte2-")%>% 
  log_("Sale la pieza del seru")%>% 
  release("Trabajador_2",amount = 1)

#cuenta el tiempo de ejecucion de la simulacion
t <- proc.time()

#Réplicas de la simulacion
envs <- parallel::mclapply(1:replicas, function(w) {
  
  # Crear el modelo de simulacion
  env <- simmer("Rotacional")%>% 
    
    #RECURSOS
    
    #Trabajadores
    add_resource("Trabajador_1", 1, Inf)%>% 
    add_resource("Trabajador_2", 1, Inf)%>% 
    
    #Tareas para cada trabajador  
    add_resource("tarea_1", 1, Inf)%>% 
    add_resource("tarea_2", 1, Inf)%>%
    add_resource("tarea_3", 1, Inf)%>%
    add_resource("tarea_4", 1, Inf)%>%
    add_resource("tarea_5", 1, Inf)%>%
    add_resource("tarea_6", 1, Inf)%>%
    add_resource("tarea_7", 1, Inf)%>%

    #SOURCES 
    
    add_generator("Parte1-", trabajador1, at(0.1))%>%
    add_generator("Parte2-", trabajador2, at(0))%>%
    add_generator("Parte1-", trabajador1, when_activated())%>%
    add_generator("Parte2-", trabajador2, when_activated())%>%
    
    #Se corre el ambiente de simulacion dutante el tiempo de siulacion definido.
    run(until = Tiempo_Sim)%>%wrap()
  
})

proc.time()-t 

#Estadisticas de monitoreo de los recursos####
resources <- get_mon_resources(envs)
#Estadisticas de monitoreo de los arrivals####
arrivals <- get_mon_arrivals(envs)

#Cálculo del throughput TT, work in progress WP y flow time FT
library(data.table)
SimSummary <- function(env, k = 60) {
  x = data.table(get_mon_arrivals(envs))
  x = x[finished == TRUE]
  x = x[, nname := gsub("[0-9]","",name)]
  anames = unique(x$nname)
  
  id = 1
  datalist = c()
  for(i in anames) {
    for (j in 1:replicas){
      y = x[nname == i & replication == j]
      N = nrow(y)
      TH = k*N/max(y$end_time)
      FT = mean(y$end_time - y$start_time)
      PCE = sum(y$activity_time)/sum(y$end_time - y$start_time)
      dat = data.frame(i,j,N,round(TH,3),round(FT,3),round(TH*FT/60,2),round(PCE,3))
      datalist[[id]] <- dat
      id = id+1
    }
    
  }
  datasummary <- do.call(rbind,datalist)
  colnames(datasummary) <- c("Parte", "Réplica", "Cantidad", "TH", "FT", "WIP", "Eficiencia")
  
  return(datasummary)
  
}

# Resultados
SimSummary(envs)
summary = data.frame(SimSummary(envs))
t.test(summary$TH)
t.test(summary$FT)
t.test(summary$WIP)
t.test(summary$Cantidad)
plot(resources)
shapiro.test(summary$TH)
hist(summary$TH)

plot(resources)
plot(resources, metric ="utilization", c("Trabajador_1","Trabajador_2"))

plot(resources, metric ="usage", c("Trabajador_1","Trabajador_2"))
plot(resources, metric = "utilization", c("tarea_1", "tarea_2", "tarea_3", "tarea_4", "tarea_5", "tarea_6", "tarea_7"))

plot(arrivals, metric = "flow_time")
