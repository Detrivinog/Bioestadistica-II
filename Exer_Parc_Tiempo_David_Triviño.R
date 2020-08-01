#Parcelas divididas en el tiempo
#Diseño de 5 trtamientos A
# 3 Bloques
#3 Cortes B
#b1 Primer corte
require(graphics)
y1=c(23.8,20.5,20.3,28.5,34.3,38.4,28,41.2,32.2,
    18.5,30.6,20.9,28.5,38,18.2,18.8,35.1,27)
bloques<-factor(rep(c("I","II","III"),6))
A<-factor(rep(c("a0","a1","a2","a3","a4","a5"), c(3,3,3,3,3,3)))
tabla1<-tapply(y1,list(A,bloques),sum)
tabla1
data1<-data.frame(tabla1)
m1.lm<-lm(y1~A+bloques)
m1.anova<-anova(m1.lm)
m1.anova
#
#b2 Segundo corte
y2=c(26.8,15.6,26,31.7,33.3,32,31,38,27,26.5,28,
     22.6,30,32,20.6,21.6,29.8,25.4)
tabla2<-tapply(y2,list(A,bloques),sum)
data2<-data.frame(tabla2)
m2.lm<-lm(y2~A+bloques)
m2.anova<-anova(m2.lm)
m2.anova
#
#b3 tercer corte
y3=c(24.3,19.8,20.8,27.7,28.4,28.6,28.4,32.9,29,22.4,
     24.5,22.8,35.4,29.2,17.6,17.6,31,25.8)
tabla3<-tapply(y3,list(A,bloques),sum)
data3<-data.frame(tabla3)
m3.lm<-lm(y3~A+bloques)
m3.anova<-anova(m3.lm)
m3.anova
#
#Ninguno de los tratamientos presenta un cambio significativo en cada 
#uno de los cortes realizados en distinto tiempo
#con un nivel de significancia del 5%   
#Total
y=c(y1,y2,y3)
Bloques<<-factor(rep(c("I","II","III"),18))
TratA<-factor(rep(c("a0","a0","a0","a1","a1",
                     "a1","a2","a2","a2","a3","a3","a3",
                     "a4","a4","a4","a5","a5","a5"),c(3)))
B<-factor(rep(c("b1","b2","b3"),c(18,18,18)))
#Se abrupan los datos los factores y los bloques en un data.frame data 
data<-data.frame(y,Bloques,TratA,B) 
#
#Gráficas
#
interaction.plot(TratA,B,y,main="Interacción AxB",trace.label="Cortes" ,
                 xlab="Tratamientos de A",ylab="Media de datos",type="l", col=c(1,2,4))
#
interaction.plot(B,TratA,y,main="Interacción BxA",trace.label="Trat de A" ,
                 xlab="Tratamientos de B",ylab="Media de datos",type="l", col=c(1,2,4,5,3,6))
#
#En la gráfica interacción AxB los cortes se comportan de manera similar al pasar por los niveles
#de los tratamientos de A. También, el corte b1, linea punteada negra, alcanza valores
#máximo en los tratamientos a1 y a2. Lo que puede generar una significancia.
#
#La gráfica interacción BxA los tratamintos de A se comportan de manera similar, sin 
#mostrar una interacción significativa. Los tratamientos se comportan muy lineal al 
#pasar por los diferentes cortes.
#
#Numero de niveles de los factores y bloques
l.a<-length(levels(TratA))
l.bl<-length(levels(Bloques))
l.b<-length(levels(B))
#Grados de libertad
gl.a=l.a-1 #GL A
gl.bl=l.bl-1 #GL Bloques
gl.b=l.b-1  #GL B
#
#Término de corrección y Suma de Cuadrados
TC=((sum(y)^2)/length(y))
SCTot=sum(y^2)-TC
#
#Tabla del factor A y Bloques
tabla_ABloq<-tapply(y,list(TratA,Bloques),sum)
tabla_ABloq
dataABl<-data.frame(tabla_ABloq)
#Suma de cuadrados
SCPG=(sum(tabla_ABloq^2)/(l.b))-TC #SC Parcela Grande  
SCA<-((sum((rowSums(tabla_ABloq))^2))/(l.bl*l.b))-TC #SC factor A
SCBloq<-((sum((colSums(tabla_ABloq))^2))/(l.a*l.b))-TC #SC bloques
SCEa=SCPG-SCA-SCBloq #Suma de cuadrados del error de A
#
#Tabla de la combinación AxB
tabla_combAB<-tapply(y,list(TratA,B),sum)
tabla_combAB
#Suma de Cuadrados
SCcomAB=(sum(tabla_combAB^2)/l.bl)-TC #SC combinación AxB
SCB<-((sum((colSums(tabla_combAB))^2))/(l.a*l.bl))-TC #SC del factor B
SCAB=SCcomAB-SCA-SCB #SC de la Interacción AB
#
#Tabla de la combinación BloquesxB
tabla_BBloq<-tapply(y,list(B,Bloques),sum)
tabla_BBloq
#Suma de Cuadrados
SCcomBBl=(sum(tabla_BBloq^2)/(l.a))-TC #SC combinación BxBloques
SCBBloq=SCcomBBl-SCBloq-SCB #SC interacción BxBloques 
SCEb=SCTot-SCBBloq-SCB-SCPG-SCAB #SC del error de B
#
(SCEb+SCBBloq+SCB+SCPG+SCAB)==SCTot 
#
#¿Como generar la tabla del Analisis de varianza total? 
#Idea: generar un data.frame a partir de los vectores:
#Fuentes de Vaiación, Df, Sum Sq, Mean Sq, F value, Pr(>F)
#tomando como fuente de idea el comando str(modelo.anova)
#donde se define la estructura de tabla de resumen para los ANOVA
#
fuentes<-c("Bloques","Tratamiento(A)","Error(A)","Cortes(B)",
           "Int AB","Int BloqB","Error B","Total")
#
Df<-c((gl.bl),(gl.a),(gl.bl*gl.a),(gl.b),(gl.a*gl.b),
      (gl.bl*gl.b),(gl.bl*gl.b*gl.a),((length(y))-1))
#
Sum_sq<-c(SCBBloq,SCA,SCEa,SCB,SCAB,SCBBloq,SCEb,SCTot)
#
Mean_sq<-(Sum_sq/Df)
#
F_value<-c(NA,(Mean_sq[2]/Mean_sq[3]),NA,(Mean_sq[4]/Mean_sq[7]),
           (Mean_sq[5]/Mean_sq[7]),(Mean_sq[6]/Mean_sq[7]),NA,NA)
#
F_tab<-c(NA,qf(.95,Df[2],Df[3]),NA,qf(.95,Df[4],Df[7]),qf(.95,Df[5],Df[7]),
         qf(.95,Df[6],Df[7]),NA,NA)
#
Valor_P<-c(NA,1-pf(F_value[2],Df[2],Df[3]),NA,1-pf(F_value[4],Df[4],Df[7]),
      1-pf(F_value[5],Df[5],Df[7]),1-pf(F_value[6],Df[6],Df[7]),NA,NA)
#
#qf() es una función que genera valores tabulares
#pf() es una función que genera un valor de probabilidad, es decir, un valor P.
#
anova<-data.frame(fuentes,Df,Sum_sq,Mean_sq,F_value,F_tab,Valor_P)
anova #Impreme la tabla de resumen del Analisis de Varianza
#
#Apartir de la tabla, y teniendo en cuenta el valor P, los cortes(B) son 
#Significativos con un nivel de significancia del 5%. Esto quire decir que 
#que hay diferencias sobre las variables respuesta entre los cortes. 
#
#Por otro lado, la interacción BloquesxCortes es significativa al 5%,
#Sin embargo esta información reafirma la naturaleza de los bloques, la cual 
#es que sean diferentes entre si, y por ende generan un cambio de los 
#tratamientos al pasar por los distintos niveles de los bloques. Cabe 
#agregar que esta interacción se hace relevante al disminuir error experimental.
#
#Distancias Criticas
meanAB<-tapply(y,list(TratA,B),mean)
meanAB
#
#Distancia Critica para los niveles de A
DCA=(sqrt(Mean_sq[3]/(l.b*l.bl)))*qt(.975,Df[3])
DCA
#Distancia Critica AB
#
#Medias de A en cada nivel de B
DCAb1=(sqrt(m1.anova$Mean[3]/l.bl))*qt(.975,m1.anova$Df[3])
DCAb1
DCAb2=(sqrt(m2.anova$Mean[3]/l.bl))*qt(.975,m2.anova$Df[3])
DCAb2
DCAb3=(sqrt(m3.anova$Mean[3]/l.bl))*qt(.975,m3.anova$Df[3])
DCAb3
#Enre cortes b1 y b2
DCAb1_b2=(sqrt((m1.anova$Mean[3]+m2.anova$Mean[3])/2*l.bl))qt(.975,Df[3])
DCAb1_b2
#Dos Medias generales de B
Sx=sqrt(Mean_sq[7]/(l.a*l.bl))
t=((gl.b*Mean_sq[7]*qt(.975,Df[7]))+(Mean_sq[3]*qt(.975,Df[3])))/((gl.b*Mean_sq[7])+Mean_sq[3])
Sx
t
DCB=Sx*t
DCB
MeanB<-colMeans(meanAB)
#Comparaciones Logicas con la distancia crítica
(MeanB[1]-MeanB[3])>DCB
(MeanB[2]-MeanB[3])>DCB
(MeanB[1]-MeanB[2])>DCB
#
#Como la interacción de los Cortes es significativa, entonces se realiza la comparación
#entre las medias de los cortes descritas en el vector MeanB, para el cual el corte b1
# y b2 difieren respecto al corte b3, sin embargo entre los cortes b1 y b2 no hay un cambio
#significativo.
#
#
#Conclusiones Finales del trabajo en R
#
#Para el desarrollo del ejercicio: 
#Al introducir los datos, la mejor manera es relizarlo de forma general. 
#Obteniendo así un data.frame como data así el manejo de los datos y la 
#presentación de las distintas tablas es mas fácil con la función tapply()
#
#El desarrollor el procedimiento que expone Hernan Gomez hace el ejercicio
#Un trabajo dispendioso al manejar numerosas variables y tablas. Sin embargo
#conocer y manejar los modelos de efectos mixtos facilita el desarrollo del ejercicio. 
#
#En el momento de armar la tabla de resumen anova, con los distintos vectores, sería 
#mucho mas fácil desarrollar una función que realice este trabajo de manera automatica
#especial para parcelas divididas. 
#
#El manejo de nuevas funciones, facilito el ejercicio como son las funciones taply(), 
#colSums(), Sin embargo este trabajo da pie para indagar sobre la construcción de nuevas
#funciones propias, el manejo de matrices y graficas que mejoren la representación de los datos.






