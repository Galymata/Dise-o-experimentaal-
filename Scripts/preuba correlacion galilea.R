resp <- data.frame(
  Tiempo = c(12, 15, 17, 18, 20, 21, 22, 26),
  Edad = c(13, 25, 20, 35, 45, 30, 60, 95)
  )
resp

plot(resp$Tiempo, resp$Edad,
     col = "pink",
     pch = 19, 
     xlab = "Tiempo respuesta",
     ylab = "Edad")

#Crear nuevas columnas con los rangos (1 a 8)
resp$Rango_Tiempo <- rank(resp$Tiempo, ties.method = "first")
resp$Rango_Edad <- rank(resp$Edad, ties.method = "first")

#ver resultado
resp

plot(resp$Rango_Tiempo, resp$Rango_Edad,
     col = "blue",
     pch = 19, #cambia el simbolo
     cex = 1.2, #Aumenta el tamaño del simbolo
     xlab = "Rango variable tiempo", #eje X
     ylab = "Rango edad" #eje Y
     )
cor.test(resp$Rango_Tiempo, resp$Rango_Edad, method = "spearman")

tau <- data.frame(
  A = c(1,2,3,4,5,6),
  B = c(3,1,4,2,6,5)
)
cor.test(tau$A, tau$B, method = "kendall") 

plot(tau$A, tau$B,
     col = "red", #color de datos
     pch = 19, #cambia la forma del simbolo
     cex = 1.4, #modifica el tamaño del simbolo
     xlab = "Evaulacion med. A", #Eje x
     ylab = "Evaulacion med. B", #Eje Y
     )
#La evaulacion de la saludud de los pacientes por ambos medicos 
#de forma indivudual no es coinicidente (r= 0.47)
# y no presenta significancia estadisitica (p-value= 0.2722)

