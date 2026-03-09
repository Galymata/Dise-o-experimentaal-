#Prueba de T 

calidad <- read.csv("ejemplo_2.csv", header = T)
calidad$Tratamiento <- as.factor(calidad$Tratamiento)

hist(subset(calidad$IE, calidad$Tratamiento == "Ctrl"))
hist(subset(calidad$IE, calidad$Tratamiento == "Fert"))

boxplot(calidad$IE ~ calidad$Tratamiento,
        xlab = "Nivel de factore",
        ylab = "Frecuencia",
        col= "pink")

HO= No hay diferencia estadistica significativa en las plantas fertilizadas y no fertilizadas 
HA= Si hay diferencia estadistica significativa en las plantas fertilizadas y no fertilizadas 
Se toma la hipotesis alterna 

#separar los niveles del facotr

Ctrl <- (subset(calidad$IE, calidad$Tratamiento == "Ctrl"))
Fert <- (subset(calidad$IE, calidad$Tratamiento == "Fert"))


shapiro.test(Ctrl)
shapiro.test(Fert)

var(Fert)
var(Ctrl)

bartlett.test(calidad$IE ~ calidad$Tratamiento)


t.test(calidad$IE ~ calidad$Tratamiento, var.equal = T)
