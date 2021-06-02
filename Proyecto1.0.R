library(readr)
shops <- read_csv("C:/Users/HP/Desktop/shop_data.csv")
View(shops)

shops <- na.omit(shops)



y=shops$shopids 
x1=shops$follower
x2=shops$response_rate
x3=shops$response_time
x4=shops$rating_bad
x5=shops$rating_normal
x6=shops$rating_good
x7=shops$rating_star

###
plot(x1,y)
plot(x2,y)
plot(x3,y)
plot(x4,y)
plot(x5,y)
plot(x6,y)
plot(x7,y)

###
matriz = cbind(y,x1,x2,x3,x4,x5,x6,x7)
mm = cor(matriz)
##

reg=lm(y~x1+x2+x3+x4+x5+x6+x7)
summary(reg)
yest=reg$coefficients[1]+reg$coefficients[2]*x1+reg$coefficients[3]*x2+reg$coefficients[4]*x3+reg$coefficients[5]*x4+reg$coefficients[6]*x5+reg$coefficients[7]*x6+reg$coefficients[8]*x7


############################# PRUEBA T ###################################

sig =0.05  
0.00685 <sig   
#Por valor critico de t:
gl= length(x1)-4-1
tabla= qt(sig/2,gl,lower.tail = F) 
t=summary(reg) [["coefficients"]][,"t value"]
tAbs = abs(t[2])

tabla < tAbs  #True

#si hay relacion


#######
#Por p-value:
0.12639<sig  

#Por valor cr�tico de t:
t1Abs = abs(t[3])
tabla < t1Abs  

#No hay relacion 

#b3
#Por p-value:
0.03850 <sig  #TRUE
#Por valor cr�tico de t:
t2Abs = abs(t[4])
tabla < t2Abs   #TRUE

#Existe una relaci�n 

######
#Por p-value:
0.25470 <sig 
#Por valor cr�tico de t:
t3Abs = abs(t[5])
tabla < t3Abs 

#no existe
####
0.51380 <sig 
#Por valor cr�tico de t:
t3Abs = abs(t[6])
tabla < t3Abs  
#no hay relacion

####
0.53802 <sig 
#Por valor cr�tico de t:
t3Abs = abs(t[7])
tabla < t3Abs  
#no hay relacion
######
0.01739 <sig 
#Por valor cr�tico de t:
t3Abs = abs(t[8])
tabla < t3Abs  
#si hay relacion

#en conclusion se puede observar que hay relacion entre x1,x3,x7

#variables que no aportan al modelo
regno = lm (y~x2+x4+x5+x6)
plot(regno)

summary(regno)

#variables que aportan al modelo
regsi = lm (y~x1+x3+x7)
summary(regsi)
plot(regsi)


#Comprobacion de las suposiciones del modelo.
rs= rstandard(reg)

plot(x1,rs)
abline(0,0)

#La grafica presenta heterocedasticidad,por lo cual se descarta la homocedasticidad

plot(x2,rs)
abline(0,0)
#La grafica presenta heterocedasticidad, por lo cual se descarta la homocedasticidad

plot(x3,rs)
abline(0,0)
#La grafica presenta heterocedasticidad,por lo cual se descarta la homocedasticidad

plot(x4,rs)
abline(0,0)
#La grafica presenta heterocedasticidad,por lo cual se descarta la homocedasticidad
plot(x5,rs)
abline(0,0)

plot(x6,rs)
abline(0,0)

plot(x7,rs)
abline(0,0)

#Distribucion Normal
plot(reg)
#---------------------------//////Primera regresion\\\\\\\\---------------------------
#comprobaciones de las supocicion del modelo
#-----------------////Grafica Q-Q\\\\\\----------------------
#podemos ver en la grafica Q-Q  que no cumple con la condiciond de normalidad por que los valores extremos
#no se ajustan a la distribuion normal dada por la linea 

#----------------/////Residuals vs Fitted\\\\\------------------------

#nos permite ver que no existe linealidad,pues la linea de color rojo no tiene una tendencia lineal con respecto al eje horizontal 
#

#----------------------/////Scale-location\\\\\------------------------
#presenta heterocedasticidad pues como se ve en la grafica la linea roja de tendencia no es horizontal  

#---------------////Residual vs Leverage\\\\-----------------------
#se puede observar que hay presencia de valores influyentes como el valor que se encuentra en la posicion 27, supera el lumbrar
#por que tiene una distancia de cook mayor que 1

#-----------------------/////Segunda regresion\\\\\\\\\\\-----------------------
plot(regno)
#comprobaciones de la segunda supocicion del modelo
#-----------------////Grafica Q-Q\\\\\\----------------------
#podemos ver que de manera parecida la grafica tiene una similitud con la primera grafica de Q-Q y de igual manera no cumple con la condiciond de normalidad por que los valores extremos
#no se ajustan a la distribuion normal dada por la linea 

#----------------/////Residuals vs Fitted\\\\\------------------------

#nos permite ver que no existe linealidad,pues la linea de color rojo no tiene una tendencia lineal con respecto al eje horizontal 

#----------------------/////Scale-location\\\\\------------------------
#presenta heterocedasticidad pues como se ve en la grafica la linea roja de tendencia no es horizontal 

#---------------////Residual vs Leverage\\\\-----------------------
#se puede observar que hay presencia de valores influyentes como el valor que se encuentra en la posicion 27, supera el lumbrar
#por que tiene una distancia de cook mayor que 1

#------------------////////Tercera regresion\\\\\---------------------
plot(regsi)
#comprobaciones dela tercera supocicion del modelo
#-----------------////Grafica Q-Q\\\\\\----------------------
#podemos ver que de manera parecida la grafica tiene una similitud con la primera y segunda grafica de Q-Q y de igual manera no cumple con la condiciond de normalidad por que los valores extremos
#no se ajustan a la distribuion normal dada por la linea 

#----------------/////Residuals vs Fitted\\\\\------------------------

#nos permite ver que no existe linealidad,pues la linea de color rojo no tiene una tendencia lineal con respecto al eje horizontal 

#

#----------------------/////Scale-location\\\\\------------------------
#presenta heterocedasticidad pues como se ve en la grafica la linea roja de tendencia no es horizontal 

#---------------////Residual vs Leverage\\\\-----------------------
#se puede observar que hay presencia de valores influyentes como el valor que se encuentra en la posicion 27, supera el lumbrar
#por que tiene una distancia de cook mayor que 1

#--------/////primera regresion\\\\----------
#outlier y  valores influyentes 
rs = rstandard(reg)
# residual estandarizado [+2,-2]
#posicion 1, 20, 30, 75, 138, 192, 193
hi = influence(reg)$hat
#verificamos si existe un valor que supere el valor de 0.9
#existe un valor influyente 27
cook = cooks.distance(reg)
cook1= which(cook>1)
rs1=which(rs >2,-2)
#-------////Segunda regresion\\\\\------
#outlier y  valores influyentes 
rs2 = rstandard(regno)
# residual estandarizado [+2,-2]
#posicion  1  20  21  47  72  75 138 193 
hi2 = influence(regno)$hat
#verificamos si existe un valor que supere el valor de 0.9
#no existen numeros que superen 
cook2 = cooks.distance(regno)
cook3= which(cook2>1)
rs3=which(rs2>2,-2)
#--------////Tercera Regresion\\\\------
#outlier y  valores influyentes 
rs4 = rstandard(regsi)
# residual estandarizado [+2,-2]
#posicion 1  20  21  47  75 136 193  
hi4 = influence(regsi)$hat
#verificamos si existe un valor que supere el valor de 0.9
#no existen que numeros que superen
cook5 = cooks.distance(regsi)
cook4= which(cook5>1)
rs4=which(rs4 >2,-2)
plot(cook2)