# Disño en Bloques con arreglo
# Factorial 2x2
# Ejemplo de Bolques balanceados
#####################################
#  jarueda@unal.edu.co              #
#####################################
y = scan()
8.53 17.53 31.4 32
20.53 21.07 26.2 23.8
12.53 20.8 31.33 28.87
14 17.33 45.8 25.06
10.8 20.07 40.2 29.33


A = factor(c(rep(c('a1','a1','a2','a2'),5)))
B = factor(c(rep(c('b1','b2'),10)))
bloq <- factor(c(rep('I',4),rep('II',4),rep('III',4),rep('IV',4),rep('V',4)))

datos =data.frame(A,B,bloq,y)

interaction.plot(A,B,y)
interaction.plot(B,A,y)

pairwise.t.test(y,A)
pairwise.t.test(y,B)

tapply(y,list(A,B),mean)
tapply(y,list(A,B),sum)

#ANÁLISIS DE VARIANZA:
analisis.de.var<-aov(y~A*B+bloq,datos)
analisis.de.var2<-aov(y~A*B,datos)


plot.design(y~A*B+bloq)
boxplot(y~B)
boxplot(y~A)
boxplot(y~A*B)
analisis.de.var
summary(analisis.de.var)
summary(analisis.de.var2)

#OBTENIENDO MEDIAS POR NIVEL DEL FACTOR
model.tables(analisis.de.var,"means")
# TABLA ANOVA
anova(analisis.de.var)
model.tables(analisis.de.var,type = "effects")

#INVOCANDO LA PRUEBA DE BARTLETT:
bartlett.test(y~A*B+bloq)

#INVOCANDO PRUEBA DE NORMALIDAD:
shapiro.test(residuals(analisis.de.var))

#INTERVALOS DE TUKEY
TukeyHSD(analisis.de.var)
#O BIEN ASÍ:
TukeyHSD(analisis.de.var,ordered=T,conf.level = 0.95)

nf<-layout(rbind(c(0,1,1,0),c(0,2,2,0)))
#OBTEBIENDO GRÁFICOS DE INTERVALOS DE TUKEY:
plot(TukeyHSD(analisis.de.var,ordered=T,conf.level = 0.95))
plot(TukeyHSD(analisis.de.var,conf.level = 0.95))