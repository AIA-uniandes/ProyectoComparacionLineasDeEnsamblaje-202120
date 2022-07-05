#2 Seru YATAI con 7 tareas
#Se trabajan 8 horas al dia para un total de 2400 min

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
  log_("Entra parte a la tarea 1 del yatai 1")%>% 
  seize("tarea_1-seru_1",amount = 1) %>%
  timeout(tarea1T1)%>%
  release("tarea_1-seru_1")

tarea2_T1 <- trajectory()%>%
  log_("Entra parte a la tarea 2 del yatai 1")%>% 
  seize("tarea_2-seru_1", amount = 1) %>%
  timeout(tarea2T1)%>%
  release("tarea_2-seru_1")

tarea3_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 3 del yatai 1")%>% 
  seize("tarea_3-seru_1", amount = 1) %>%
  timeout(tarea3T1)%>%
  release("tarea_3-seru_1")

tarea4_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 4 del yatai 1")%>% 
  seize("tarea_4-seru_1", amount = 1) %>%
  timeout(tarea4T1)%>%
  release("tarea_4-seru_1")

tarea5_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 5 del yatai 1")%>% 
  seize("tarea_5-seru_1", amount = 1) %>%
  timeout(tarea5T1)%>%
  release("tarea_5-seru_1")

tarea6_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 6 del yatai 1")%>% 
  seize("tarea_6-seru_1", amount = 1) %>%
  timeout(tarea6T1)%>%
  release("tarea_6-seru_1")

tarea7_T1 <- trajectory()%>% 
  log_("Entra parte a la tarea 7 del yatai 1")%>% 
  seize("tarea_7-seru_1", amount = 1) %>%
  timeout(tarea7T1)%>%
  release("tarea_7-seru_1")
  
#Seru 2
tarea1_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 1 del yatai 2")%>% 
  seize("tarea_1-seru_2", amount = 1) %>%
  timeout(tarea1T1)%>%
  release("tarea_1-seru_2")
  
tarea2_T2 <- trajectory()%>%
  log_("Entra parte a la tarea 2 del yatai 2")%>% 
  seize("tarea_2-seru_2", amount = 1) %>%
  timeout(tarea2T1)%>%
  release("tarea_2-seru_2")
  
tarea3_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 3 del yatai 2")%>% 
  seize("tarea_3-seru_2", amount = 1) %>%
  timeout(tarea3T1)%>%
  release("tarea_3-seru_2")
  
tarea4_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 4 del yatai 2")%>% 
  seize("tarea_4-seru_2", amount = 1) %>%
  timeout(tarea4T1)%>%
  release("tarea_4-seru_2")
  
tarea5_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 5 del yatai 2")%>% 
  seize("tarea_5-seru_2", amount = 1) %>%
  timeout(tarea5T1)%>%
  release("tarea_5-seru_2")
  
tarea6_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 6 del yatai 2")%>% 
  seize("tarea_6-seru_2", amount = 1) %>%
  timeout(tarea6T1)%>%
  release("tarea_6-seru_2")
  
tarea7_T2 <- trajectory()%>% 
  log_("Entra parte a la tarea 7 del yatai 2")%>% 
  seize("tarea_7-seru_2", amount = 1) %>%
  timeout(tarea7T1)%>%
  release("tarea_7-seru_2")

# TRAYECTORIA SERU

yatai1 <- trajectory()%>% 
  #aviso en consola de llegada de parte al sistema.
  log_("Llega pieza a ser procesada en el Yatai1")%>%  
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
  log_("Sale la pieza del yatai1")%>% 
  release("Trabajador_1", amount = 1)

yatai2 <- trajectory()%>% 
  #aviso en consola de llegada de parte al sistema.
  log_("Llega pieza a ser procesada en el Yatai2")%>%  
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
  log_("Sale la pieza del yatai2")%>% 
release("Trabajador_2",amount = 1)
  
#cuenta el tiempo de ejecucion de la simulacion
t <- proc.time()

#Réplicas de la simulacion
envs <- parallel::mclapply(1:replicas, function(w) {
  
  # Crear el modelo de simulacion
  env <- simmer("Yatai")%>% 
  
  #RECURSOS
    
  #Trabajadores
  add_resource("Trabajador_1", 1, Inf)%>% 
  add_resource("Trabajador_2", 1, Inf)%>% 
    
  #Tareas para cada trabajador  
  add_resource("tarea_1-seru_1", 1, Inf)%>% 
  add_resource("tarea_2-seru_1", 1, Inf)%>%
  add_resource("tarea_3-seru_1", 1, Inf)%>%
  add_resource("tarea_4-seru_1", 1, Inf)%>%
  add_resource("tarea_5-seru_1", 1, Inf)%>%
  add_resource("tarea_6-seru_1", 1, Inf)%>%
  add_resource("tarea_7-seru_1", 1, Inf)%>%
    
  add_resource("tarea_1-seru_2", 1, Inf)%>% 
  add_resource("tarea_2-seru_2", 1, Inf)%>%
  add_resource("tarea_3-seru_2", 1, Inf)%>%
  add_resource("tarea_4-seru_2", 1, Inf)%>%
  add_resource("tarea_5-seru_2", 1, Inf)%>%
  add_resource("tarea_6-seru_2", 1, Inf)%>%
  add_resource("tarea_7-seru_2", 1, Inf)%>%
  
  
   #SOURCES 
    
  #Yatai 1
  add_generator("Parte1-", yatai1, at(0))%>%
  add_generator("Parte1-", yatai1, when_activated())%>%
  
  #Yatai 2
  add_generator("Parte2-", yatai2, at(0))%>%
  add_generator("Parte2-", yatai2, when_activated())%>%
  
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

plot(resources)
plot(resources, metric ="usage", c("Trabajador_1","Trabajador_2"))
plot(resources, metric="utilization", c("tarea_1-seru_1", "tarea_2-seru_1","tarea_3-seru_1","tarea_4-seru_1","tarea_5-seru_1","tarea_6-seru_1","tarea_7-seru_1"))
plot(resources, metric="utilization", c("tarea_1-seru_2", "tarea_2-seru_2","tarea_3-seru_2","tarea_4-seru_2","tarea_5-seru_2","tarea_6-seru_2","tarea_7-seru_2"))


#Gráficos
plot(resources, metric = "utilization")
plot(resources, metric = "usage")
plot(tarea1_T1)
plot(arrivals, metric = "flow_time")

#uso instantanea en unidades (capacidad de 1)
plot(resources, metric ="usage", "tarea_1-trabajador_1", items = "server", steps = T)

#Utilizacion por tarea/trabajador
plot(resources, metric="usage", c("tarea_1-trabajador_1", "tarea_2-trabajador_1","tarea_3-trabajador_1","tarea_4-trabajador_1","tarea_5-trabajador_1","tarea_6-trabajador_1","tarea_7-trabajador_1"))
plot(resources, metric="utilization", c("tarea_1-trabajador_2", "tarea_2-trabajador_2","tarea_3-trabajador_2","tarea_4-trabajador_2","tarea_5-trabajador_2","tarea_6-trabajador_2","tarea_7-trabajador_2"))

#####################################
#Tiempo promedio en el sistema####
resource_data <- get_mon_arrivals(envs) %>%
  dplyr::group_by(replication) %>%
  dplyr::summarise(mean = mean(end_time - start_time))
t.test(resource_data[["mean"]])
#intervalo de confianza 95%
sample_mean_t <- mean(t_system$mean)
se_t <- sd(t_system$mean) / sqrt(replicas)
variation_coef_t <- sd(t_system$mean)/abs(sample_mean_t)
lower_t <- sample_mean_t - 1.96 * se_t
upper_t <- sample_mean_t + 1.96 * se_t
c(lower_t, upper_t)
resultados <- dplyr::add_row(resultados,lowerMean=lower_t,Mean = sample_mean_t, upperMean=upper_t)

#Prueba:
Mean_pt <- mean(t_system$t_system[t_system$t_system>=0])
Mean_pt <- mean(t_system$t_system[t_system$t_system>0])

#####################################
#Tiempo de espera promedio en el sistema####
wt_system <- get_mon_arrivals(envs) %>%
dplyr::mutate(wt_system = end_time - activity_time - start_time) %>%
dplyr::group_by(replication) %>%
dplyr::summarise(mean = sum(wt_system)/(Num_Partes_Dia*numDias))

#intervalo de confianza 95%
#sample_mean_wt <- mean(wt_system$mean)
#se_wt <- sd(wt_system$mean) / sqrt(Repli)
#variation_coef_wt <- sd(wt_system$mean)/abs(sample_mean_wt)
#lower_wt <- sample_mean_wt - 1.96 * se_wt
#upper_wt <- sample_mean_wt + 1.96 * se_wt
#c(lower_wt, upper_wt)
#resultados <- dplyr::add_row(resultados, lowerMean=lower_wt,Mean = sample_mean_wt,upperMean=upper_wt)

#Prueba:
#Mean_pwt <- mean(wt_system$wt_system[wt_system$activity_time>=0])
#Mean_pwt <- mean(wt_system$wt_system[wt_system$activity_time>0])

#Estadisticas de monitoreo del sistema ####

#####################################
#Calculo WIP en el sistema (Time_persistant stats) ####
#WiP<-get_mon_attributes(envs)
#calc_wip_porfin <- function(tabla_wip) {
 # tabla_wip <- tabla_wip %>%
  #  dplyr::group_by( .data$replication) %>%
   # dplyr::mutate(dt = .data$time - dplyr::lag(.data$time)) %>%
    #dplyr::mutate(rectangulos = .data$dt * dplyr::lag( .data$value)) %>%
    #dplyr::summarise(meanWip = sum(.data$rectangulos, na.rm = TRUE) / sum(.data$dt, na.rm = TRUE))
#}

#wip_system <- calc_wip_porfin(WiP)

#intervalo de confianza 95%
#sample_mean_wip <- mean(wip_system$meanWip)
#se <- sd(wip_system$meanWip) / sqrt(Repli)
#variation_coef_wip <- sd(wip_system$meanWip)/abs(sample_mean_wip)
#lower_wip <- sample_mean_wip - 1.96 * se
#upper_wip <- sample_mean_wip + 1.96 * se
#c(lower_wip, upper_wip)
#resultados <- dplyr::add_row(resultados, lowerMean=lower_wip,Mean = sample_mean_wip,upperMean=upper_wip)

#####################################
#Calculo del throughput (ley de Little) wip=ts*th####
#Throughput <- as.matrix(wip_system[,2]/t_system[,2])
#Through_mean <- mean(Throughput)
#SE_Through <- sd(Throughput)/sqrt(Repli)
#variation_coef_Through <- (sd(Throughput)/Through_mean)
#lower_Through <-  Through_mean - 1.96 * SE_Through
#upper_Through <- Through_mean + 1.96 * SE_Through
#resultados <- dplyr::add_row(resultados, lowerMean=lower_Through,Mean=Through_mean,upperMean=upper_Through)

#####################################
#Resultados
#row.names(resultados)<-c("Uti","TIS","AQT","WIP","Th")
#var_coef <- matrix(c(variation_coef_util,variation_coef_t,variation_coef_wip,variation_coef_wt,variation_coef_Through),ncol = 1)
#var_coef <- data.frame(var_coef)
#row.names(var_coef) <- c("Uti","TIS","AQT","WIP","Th")
#resultados
#var_coef

