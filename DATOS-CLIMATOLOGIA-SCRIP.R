# MAGT
# 14/04/2026
# Datos climatológicos
# Cumbres NL, Villagran Tamps, Carranza Coahuila

library(lubridate)
library(hms)
clima <- read.csv("Climatologia.csv", header =T)
clima$Localidad <- as.factor(clima$Localidad)
clima$Fecha <- as.Date(clima$Fecha, format = "%d/%m/%Y")
clima$Hora <- hms::as_hms(clima$Hora)


boxplot(clima$TEMP~ clima$Localidad,
        col= "indianred",
        ylab = "Temp (°C)",
        xlab = "Localidades",
        ylim =c(10,35),
        outcol = "indianred", #marcar outliers en rojo
        outpch = 19) # cambiar la forma a circulo relleno

tapply(clima$TEMP, clima$Localidad, mean)

# Temperatura en el curso del semana

cumbres <- subset(clima,Localidad =="Cumbres")
cumbres <- cumbres[order(cumbres$fechaHora),]
cumbres$Hora <- as.character(cumbres$Hora)
clima$FechaHora <- as.POSIXct(paste(clima$Fecha, clima$Hora),
                              format = "%Y-%m-%d %H:%M:%S")

plot(cumbres$FechaHora , cumbres$TEMP,
     type = "l",
     col = "indianred",
     lwd =2,
     xaxt = "n",
     xlab = "Localidad EMAs",
     ylab  = "Temperatura (°C)")

# Normalidad de adtos
tapply(clima$TEMP, clima$Localidad, mean)
shapiro.test(clima$TEMP)



# ANOVA

clim.aov <- aov(clima$TEMP ~ clima$Localidad)
summary(clim.aov)

# Aceptamos H1, por lo tanto realizamos una prueba de Tukey
TukeyHSD(clim.aov)

# Homogeneidad de varianzas
tapply(clima$TEMP, clima$Localidad, var)
bartlett.test(clima$TEMP ~ clima $Localidad)

