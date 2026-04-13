# ALMA GALILEA MATA ALAMEDA 
#13/04/2026

#ANOVA
localidad <- read.csv("Datos_Rascon_Anova.csv", header = T )
View(localidad)
localidad$Paraje <- (localidad$Paraje)

boxplot(localidad$DA ~ localidad$Paraje,
        xlab = "Paraje",
        ylab = "DBH (cm)",
        col = "pink")
#Normalidad de datos DBH O DAP
shapiro.test(localidad$DAP)

#revisas a profundiad los datos de DAP

hist(localidad$DAP)

mean(localidad$EDAD)

localidad$logDAP <- log10(localidad$DAP)
localidad$sqrDAP <- sqrt(localidad$DAP)+1
hist(localidad$ogDAP
hist(localidad$sqrDAP)
shapiro.test(localidad$logDAP)
shapiro.test(localidad$sqrDAP)

bartlett.test(localidad$sqrDAP ~ localidad$Paraje)

#Se cumplen la normalidad y homogenidad de varianzas de la 
#variable transformada por raiz cuarada (sqrt)

#procesos a realizar en ANOVA con sqrDAP
#funcion aov

paraje.aov <- aov(localidad$sqrDAP ~ localidad$Paraje)

paraje.aov

summary(paraje.aov)

TukeyHSD(paraje.aov)
plot(TukeyHSD(paraje.aov))
