# -*- coding: utf-8 -*-
"""
Created on Wed Nov 17 06:51:46 2021

@author: melis
"""

from gurobipy import *
import math
import csv
import numpy as np

file = open('Divisional.csv','w')
archivo = csv.writer(file)


nEstaciones = 2
nTareas = 7
nTrabajadores = nEstaciones
nSerus = 1
nEscenarios = 50
nReplicas = 100

min_T1 = [0.35,0.33,0.617,0.56,0.95,0.61,0.667]
max_T1 = [0.69,0.567,0.835,0.833,1.25,0.7,0.983]

min_T2 = [0.367,0.317,0.433,0.45,0.883,0.5,0.65]
max_T2 = [0.55,0.483,0.633,0.683,1.25,0.833,0.917]

#define como un set las parejas de predecesores inmediatos
Predecesores = {
    (0,2),
    (1,2),
    (2,3),
    (3,4),
    (4,5),
    (5,6)
    }
                
#define una tuplelist de gurobi con el objeto Predecesores de tipo set
arcos = tuplelist(Predecesores)

header = ['Replica', 'FO', 'Sol_i', 'Sol_j', 'Sol_k']
archivo.writerow(header)

for p in range(nReplicas):
    
    tiempo=[[[0 for k in range(nTrabajadores)]for i in range(nTareas)]for r in range(nEscenarios)] 
    for k in range(nTrabajadores):
        if k == 0:
            for r in range(nEscenarios):
                for i in range(nTareas):
                    tiempo[r][i][k] = np.random.uniform(min_T1[i], max_T1[i]) 
        else:
            for r in range(nEscenarios):
                for i in range(nTareas):
                    tiempo[r][i][k] = np.random.uniform(min_T2[i], max_T2[i])
    # Model
    m = Model("Divisional")
        
    # Tiempo de procesamiento al final de la última pieza
    T={}
    T = m.addVar(lb = 0, obj = 1, vtype = GRB.CONTINUOUS, name = "TIEMPO TOTAL")
                                                                                                                 
    # Variable de decision de asignacion: X[i, j, k] == 1 si tarea i va sobre la estacion j con el trabajador k
    X={} 
    for i in range(nTareas): 
        for j in range(nEstaciones):
            for k in range(nTrabajadores):
                X[i,j,k]=m.addVar(lb = 0, ub = 1,vtype = GRB.BINARY, name= "X%d.%d.%d" % (i,j,k))           
    
       
                   
    # y[k,j] si el trabajador k es asignado a la estación j
    y = {}
    for k in range(nTrabajadores):
        for j in range(nEstaciones):
            y[k,j] = m.addVar(lb = 0, ub = 1, vtype = GRB.BINARY, name= "y%d.%d" % (k,j))
        
    #El objetivo es minimizar el tiempo total de procesamiento
    m.ModelSense = 1
    
    # Update model to integrate new variables
    m.update()
        
    m.addConstr((quicksum(quicksum(quicksum(quicksum(tiempo[r][i][k]*X[i,j,k] for k in range(nTrabajadores)) for j in range(nEstaciones)) for i in range(nTareas)) for r in range(nEscenarios)))/nEscenarios  <= T , "TIEMPO TOTAL")
        
    #Cada tarea asignada a una estación con trabajador
    for i in range(nTareas):
        m.addConstr(quicksum(quicksum(X[i,j,k] for k in range(nTrabajadores)) for j in range(nEstaciones)) == nSerus, "ASIGNA%d" % i)
        
        # Cada estación con trabajador tienen una tarea asignada
    for j in range(nEstaciones):
        for k in range(nTrabajadores):
            m.addConstr(quicksum(X[i,j,k] for i in range(nTareas)) >= y[k,j], "TAREA%d.%d" % (j,k))
        
        # Máximo número de tareas asignadas a una estación y a un trabajador
    for j in range(nEstaciones):
        for k in range(nTrabajadores):
            m.addConstr(quicksum(X[i,j,k] for i in range(nTareas))<= y[k,j]*nTareas, "TAREA2%d.%d" % (j,k))
        
    for k in range(nTrabajadores):
        m.addConstr(quicksum(y[k,j] for j in range(nEstaciones)) == 1, "TRABAJADOR%d" % k)
            
    for j in range(nEstaciones):
        m.addConstr(quicksum(y[k,j] for k in range(nTrabajadores)) == 1, "ESTACION%d" % j)
               
    for r in (range(nEscenarios)):
        for k in (range(nTrabajadores-1)):
            m.addConstr(quicksum(quicksum(tiempo[r][i][k]*X[i,j,k] for i in range(nTareas)) for j in range(nEstaciones)) - quicksum(quicksum(tiempo[r][i][k+1]*X[i,j,k+1] for i in range(nTareas)) for j in range(nEstaciones)) <= 0.2*quicksum(quicksum(tiempo[r][i][k+1]*X[i,j,k+1] for j in range(nEstaciones)) for i in range(nTareas)) , "BALANCEO1%d.%d" %(r,k))
            m.addConstr(-(quicksum(quicksum(tiempo[r][i][k]*X[i,j,k] for i in range(nTareas)) for j in range(nEstaciones)) - quicksum(quicksum(tiempo[r][i][k+1]*X[i,j,k+1] for i in range(nTareas)) for j in range(nEstaciones))) <= 0.2*quicksum(quicksum(tiempo[r][i][k+1]*X[i,j,k+1] for j in range(nEstaciones)) for i in range(nTareas)) , "BALANCEO2%d.%d" %(r,k))
        
    for u,v in arcos:
        m.addConstr(quicksum(quicksum(X[u,j,k]*j for j in range(nEstaciones)) for k in range(nTrabajadores)) <= quicksum(quicksum(X[v,j,k]*j for j in range(nEstaciones)) for k in range(nTrabajadores)), "PRED %d.%d" % (u,v))
         
    
    m.update()
    
    # Solve
    m.optimize()
    
    # Print solution
    if m.status == GRB.Status.OPTIMAL:
        for j in range(nEstaciones):
            for i in range(nTareas): 
                for k in range(nTrabajadores):
                    if X[i,j,k].x>0.0:
                        data = [p,m.ObjVal,i,j,k]
                        archivo.writerow (data)  
                        
    else:
        data = [p, 'Infactible']
        archivo.writerow(data)
     
    #archivo.write ( '\n ')
    
file.close()