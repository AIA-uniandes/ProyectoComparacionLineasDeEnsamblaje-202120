#Seru Divisional con 7 tareas
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
tDesp2 <- function() runif(1, min = 0.1333, max = 0.2)

#Tiempo de proceso del trabajador 1
tarea1T1 <- function() runif(1, min = 0.35, max = 0.69)
tarea2T1 <- function() runif(1, min = 0.333, max = 0.567)
tarea3T1 <- function() runif(1, min = 0.617, max = 0.835)
tarea4T1 <- function() runif(1, min = 0.56, max = 0.833)

#Tiempo de proceso del trabajador 2
tarea5T2 <- function() runif(1, min = 0.883, max = 1.25)
tarea6T2 <- function() runif(1, min = 0.5, max = 0.833)
tarea7T2 <- function() runif(1, min = 0.65, max = 0.917)

# TRAYECTORIA TAREAS

#Trabajador 1
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

#Trabajador 2
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

pieza <- trajectory()%>% 
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
  timeout(tDesp2)%>%
  release("Trabajador_1", amount = 1)%>% 
  activate("Parte")%>% 
  seize("Trabajador_2", amount = 1)%>% 
  join(tarea5_T2)%>%
  timeout(tDesp)%>%
  join(tarea6_T2)%>%
  timeout(tDesp)%>%
  join(tarea7_T2)%>%
  timeout(tDesp)%>%
  log_("Sale la pieza del seru")%>% 
  release("Trabajador_2", amount = 1)

#cuenta el tiempo de ejecucion de la simulacion
t <- proc.time()

#Réplicas de la simulacion
envs <- parallel::mclapply(1:replicas, function(w) {
  
  # Crear el modelo de simulacion
  env <- simmer("Divisional")%>% 
    
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
    
    add_generator("Parte", pieza, at(0))%>%
    add_generator("Parte", pieza, when_activated())%>%
    
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
t.test(summary$Eficiencia)
aplot(resources)

plot(resources)
plot(resources, metric ="usage", c("Trabajador_1","Trabajador_2"))
plot(resources, metric = "utilization", c("tarea_1", "tarea_2", "tarea_3", "tarea_4", "tarea_5", "tarea_6", "tarea_7"))
plot(arrivals, metric = "flow_time")

plot(pieza)
