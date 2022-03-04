#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.INTEGRANTES.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#Alvarado Pérez César Porfirio
#Barrancos Cruz Luis Fernando
#Cuéllar Chávez Eduardo de Jesús
#Ramírez Maciel José Antonio


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-PREPARATIVOS.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#comenzamos cargando las librerías. Usaremos esta función para asegurarnos
#de tener descargadas las librerías necesarias:
install_or_load_pack <- function(pack){
    
    create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
    
    if (length(create.pkg))
        
        install.packages(create.pkg, dependencies = TRUE)
    
    sapply(pack, require, character.only = TRUE)
    
}
#Ahora cargamos las que usaremos
install_or_load_pack(c("readr","hms","moments","ggplot2","xts","tidyverse",
                       "tidyquant","dplyr",'timeSeries','QRM'))
#vamos a hacer una variable con el nombre del activo. Trabajaremos con JP-Morgan
#inspirándonos en el documental de 'inside job'
activo='JPM'
#Descargamos los datos desde Yahoo finance con esta función del paquete QRM, usando
#las fechas desde lo más atrás que podamos, Según la página de Yahoo Finance
#esta fecha es el 17 de marzo de 1980'
dirty_data <- tq_get(activo, from = "1980-03-17",to = "2021-07-18", get = "stock.prices")
#Nos quedamos solo con date y close, que son las fechas y el precio de cierre
datos<-dirty_data[c('date','close')]
#Le ponemos nombres en mayúscula al inicio, esto solo con fines estéticos
colnames(datos)<-(c('Date','Close'))
#Pasamos a tipo fecha la columna de Date. Viendo nuestros datos vemos que 
#están en formato YYYY-mm-dd
datos$Date<-as.Date(datos$Date,format='%Y-%m-%d')
#Pasamos los datos de cierre a numéricos
datos$Close<-as.numeric(datos$Close)
#Quitamos los na 
datos<-na.omit(datos)
#reseteamos índices
row.names(datos)<-NULL
#Esta etiqueta servirá para los gráficos
etiqueta <- c(activo,"Precio en dólares","Porcentaje","Rendimientos logarítmicos")
#Visualizamos los datos históricos. Estos son los precios aún
par(cex=0.7,mfrow=c(1,1)) 
par(cex.axis = 0.6, cex.lab = .8, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
plot(x=datos$Date,y=datos$Close,col="red",type="l",ylab=etiqueta[2],xlab= "",
     lwd = 1, font = 1, font.lab = 1,main = etiqueta[1])

#.-.-.-.-.-.-.-.-.-.-.-.-.-CÁLCULOS DIARIOS.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Calculamos los rendimientos logarítmicos DIARIOS.
#así que lo guardamos en la siguiente variable
R_Diarios<-diff(log(datos$Close),lag = 1)*100
#Hacemos un dataframe para los rendimientos logarítmicos. Nótese que se toma a 
#partir de la segunda fecha disponible ya que, para la primera fecha no tenemos
#manera de calcular su rendimiento lagarítmico al ser nuestro punto de inicio
df_Diario<-data.frame(x=datos$Date[2:nrow(datos)],y=R_Diarios)
#Graficamos los rendimientos logarítmicos
par(cex.axis = 0.6, cex.lab = .8, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
plot(x=datos$Date[2:nrow(datos)],y=R_Diarios,col="blue",type="l",
     ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,
     main = paste(etiqueta[4],'Diarios',sep=' '),cex.main=1)
#Creamo un vector con los nombres de las medidas
medidas<-c('Media','Desviación estándar', 'Skewness', 'Kurtosis')
#Creamos una matriz para almacenar la información de las medidas:
Rendimientos_Diarios<-matrix(data = NA,ncol =4,nrow = 1 )
#Media
Rendimientos_Diarios[1,1]<-round(mean(R_Diarios),5)
#Desviación estándar
Rendimientos_Diarios[1,2]<-round(sd(R_Diarios),5)
#Skewness
Rendimientos_Diarios[1,3]<-round(skewness(R_Diarios),5)
#Kurtosis
Rendimientos_Diarios[1,4]<-round(kurtosis(R_Diarios)+3,5)
#Visualizamos
Rendimientos_Diarios<-as.data.frame(Rendimientos_Diarios)
#Le damos el nombre
colnames(Rendimientos_Diarios)<-medidas
#Graficamos el kernel e histograma de los rendimientos logarítmicos, así como
#una normal con parámetros la media y desviación estándar antes calculados en 
#el dataframe Rendimientos_Diarios
par(mfrow=c(1,1)) 
Ker<-density(R_Diarios)
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(R_Diarios, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2, xlab ="Rendimientos logarítmicos Diarios",ylab ="Densidad", main = activo)
y<-curve(dnorm(x,mean=mean(R_Diarios),sd=sd(R_Diarios)), from=-100, to=100, add=TRUE, 
         col="black") 
lines(Ker,col="red")
legend("topright",legend=c("Histograma","Distribución Normal","Kernel"),lty=1, 
       col=c('deepskyblue3', 'black', 'firebrick3'), bty='n')
#Ahora haremos el QQ-plot de los rendimientos logarítmicos diarios
par(mfrow=c(1,1))
qqnorm(R_Diarios,ylim = c(-7,7),main='QQ-Plot de rendimientos diarios')
qqline(R_Diarios,col = "firebrick",lwd=2,lty=2)
#Hacemos ahora el autocorrelagrama de los rendimientos logarítmicos diarios y 
#comparamos con el de los rendimientos logarítmicos absolutos diario
par(mfrow=c(2,1))
acf(R_Diarios, lwd=8, lend=1, col="coral2",main="Rendimientos logarítmicos Diarios")
acf(abs(R_Diarios), lwd=8, lend=1, col="coral2",
    main="Valor absoluto de los rendimientos logarítmicos Diarios")


#.-.-.-.-.-.-.-.-.-.-.-.-.-CÁLCULOS SEMANALES.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Calculamos los rendimientos semanales.
R_semanales<-diff(log(datos$Close),lag = 5)*100
#calculamos límites
#Hacemos un dataframe para los rendimientos logarítmicos. Nótese que se toma a 
#partir de la sexta fecha disponible ya que estamos considerando 5 días como una
#semana (Así lo tomaba el profesor). Entonces, hasta el sexto día podemos comparar.
df_semanales<-data.frame(x=datos$Date[6:nrow(datos)],y=R_semanales)
#Graficamos los rendimientos logarítmicos
par(mfrow=c(1,1)) 
par(cex.axis = 0.6, cex.lab = .8, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
plot(x=datos$Date[6:nrow(datos)],y=R_semanales,col="blue",type="l",
     ylab=etiqueta[3],xlab=" ",lwd = 0.5, font = 1, font.lab =1,
     main = paste(etiqueta[4],'Semanales',sep=' '),cex.main=1)
#Creamos una matriz para almacenar la información de las medidas:
Rendimientos_Semanales<-matrix(data = NA,ncol =4,nrow = 1 )
#Media
Rendimientos_Semanales[1,1]<-round(mean(R_semanales),5)
#Desviación estándar
Rendimientos_Semanales[1,2]<-round(sd(R_semanales),5)
#Skewness
Rendimientos_Semanales[1,3]<-round(skewness(R_semanales),5)
#Kurtosis
Rendimientos_Semanales[1,4]<-round(kurtosis(R_semanales)+3,5)
#Visualizamos
Rendimientos_Semanales<-as.data.frame(Rendimientos_Semanales)
#Le damos el nombre
colnames(Rendimientos_Semanales)<-medidas
#Graficamos el kernel e histograma de los rendimientos logarítmicos, así como
#una normal con parámetros la media y desviación estándar antes calculados en 
#el dataframe Rendimientos_Semanales
par(mfrow=c(1,1)) 
Ker<-density(R_semanales)
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(R_semanales, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2,xlab ="Rendimientos logarítmicos Semanales",
     ylab ="Densidad", main = activo)
y<-curve(dnorm(x,mean=mean(R_semanales),sd=sd(R_semanales)), from=-100, to=100, add=TRUE, 
         col="black") 
lines(Ker,col="red")
legend("topright",legend=c("Histograma","Distribución Normal","Kernel"),lty=1, 
       col=c('deepskyblue3', 'black', 'firebrick3'), bty='n')
#Ahora haremos el QQ-plot de los rendimientos logarítmicos semanales
par(mfrow=c(1,1))
qqnorm(R_semanales,ylim = c(-7,7),main='QQ-Plot de rendimientos semanales')
qqline(R_semanales,col = "firebrick",lwd=2,lty=2)
#Hacemos ahora el autocorrelagrama de los rendimientos logarítmicos semananales y 
#comparamos con el de los rendimientos logarítmicos absolutos semanales
par(mfrow=c(2,1))
acf(R_semanales, lwd=8, lend=1, col="coral2",main="Rendimientos logarítmicos Semanales")
acf(abs(R_semanales), lwd=8, lend=1, col="coral2",
    main="Valor absoluto de los rendimientos logarítmicos Semanales")





#.-.-.-.-.-.-.-.-.-.-.-.-.-CÁLCULOS MENSUALES-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Calculamos los rendimientos mensuales.
R_mensaules<-diff(log(datos$Close),lag = 21)*100
#Hacemos un dataframe para los rendimientos logarítmicos. Nótese que se toma a 
#partir de la sexta fecha disponible ya que estamos considerando 21 días como una
#semana (Así lo tomaba el profesor). Entonces, hasta el 22vo día podemos comparar.
df_mensuales<-data.frame(x=datos$Date[22:nrow(datos)],y=R_mensaules)
#Graficamos los rendimientos logarítmicos
par(mfrow=c(1,1))
plot(x=datos$Date[22:nrow(datos)],y=R_mensaules,col="cadetblue4",type="l",ylab=etiqueta[3],xlab=" ",
     lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos logarítmicos mensuales",
     cex.main=1)
#Creamos una matriz para almacenar la información de las medidas:
Rendimientos_Mensuales<-matrix(data = NA,ncol =4,nrow = 1 )
#Media
Rendimientos_Mensuales[1,1]<-round(mean(R_mensaules),5)
#Desviación estándar
Rendimientos_Mensuales[1,2]<-round(sd(R_mensaules),5)
#Skewness
Rendimientos_Mensuales[1,3]<-round(skewness(R_mensaules),5)
#Kurtosis
Rendimientos_Mensuales[1,4]<-round(kurtosis(R_mensaules)+3,5)
#Visualizamos
Rendimientos_Mensuales<-as.data.frame(Rendimientos_Mensuales)
#Le damos el nombre
colnames(Rendimientos_Mensuales)<-medidas
#Graficamos el kernel e histograma de los rendimientos logarítmicos, así como
#una normal con parámetros la media y desviación estándar antes calculados en 
#el dataframe Rendimientos_Mensuales
par(mfrow=c(1,1)) 
Ker<-density(R_mensaules)
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(R_mensaules, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2,xlab ="Rendimientos logarítmicos Mensuales",
     ylab ="Densidad", main = activo)
y<-curve(dnorm(x,mean=mean(R_mensaules),sd=sd(R_mensaules)), from=-100, to=100, add=TRUE, 
         col="black") 
lines(Ker,col="red")
legend("topright",legend=c("Histograma","Distribución Normal","Kernel"),lty=1, 
       col=c('deepskyblue3', 'black', 'firebrick3'), bty='n')
#Ahora haremos el QQ-plot de los rendimientos logarítmicos mensuales
par(mfrow=c(1,1))
qqnorm(R_mensaules,ylim = c(-7,7),main='QQ-Plot de rendimientos mensuales')
qqline(R_mensaules,col = "firebrick",lwd=2,lty=2)
#Hacemos ahora el autocorrelagrama de los rendimientos logarítmicos mensuales y 
#comparamos con el de los rendimientos logarítmicos absolutos mensuales
par(mfrow=c(2,1))
acf(R_mensaules, lwd=8, lend=1, col="coral2",main="Rendimientos logarítmicos Mensuales")
acf(abs(R_mensaules), lwd=8, lend=1, col="coral2",
    main="Valor absoluto de los rendimientos logarítmicos Mensuales")






#.-.-.-.-.-.-.-.-.-.-.-.-.-CÁLCULOS ANUALES.--.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#Calculamos los rendimientos mensuales.
R_anules<-diff(log(datos$Close),lag = 360)*100
#Hacemos un dataframe para los rendimientos logarítmicos. Nótese que se toma a 
#partir de la sexta fecha disponible ya que estamos considerando 360 días como una
#semana (Así lo tomaba el profesor). Entonces, hasta el 361º día podemos comparar
df_anuales<-data.frame(x=datos$Date[361:nrow(datos)],y=R_anules)
#Graficamos los rendimientos logarítmicos
par(mfrow=c(1,1)) 
plot(x=datos$Date[361:nrow(datos)],y=R_anules,col="cadetblue4",type="l",ylab=etiqueta[3],
     xlab=" ",lwd = 0.5, font = 1, font.lab =1,main = "Rendimientos logarítmicos anuales",
     cex.main=1)
#Creamos una matriz para almacenar la información de las medidas:
Rendimientos_Anuales<-matrix(data = NA,ncol =4,nrow = 1 )
#Media
Rendimientos_Anuales[1,1]<-round(mean(R_anules),5)
#Desviación estándar
Rendimientos_Anuales[1,2]<-round(sd(R_anules),5)
#Skewness
Rendimientos_Anuales[1,3]<-round(skewness(R_anules),5)
#Kurtosis
Rendimientos_Anuales[1,4]<-round(kurtosis(R_anules)+3,5)
#Visualizamos
Rendimientos_Anuales<-as.data.frame(Rendimientos_Anuales)
#Le damos el nombre
colnames(Rendimientos_Anuales)<-medidas
#Graficamos el kernel e histograma de los rendimientos logarítmicos, así como
#una normal con parámetros la media y desviación estándar antes calculados en 
#el dataframe Rendimientos_Anuales
par(mfrow=c(1,1)) 
Ker<-density(R_anules)
par(cex.axis = 0.6, cex.lab = 1, cex.main = 1, cex.sub = 1, mgp = c(1.5,0.5, 0))
hist(R_anules, breaks=200,freq=FALSE,col = "deepskyblue3",border = "deepskyblue4",
     lwd=2,xlab ="Rendimientos logarítmicos Anuales",
     ylab ="Densidad", main = activo)
y<-curve(dnorm(x,mean=mean(R_anules),sd=sd(R_anules)), from=-100, to=100, add=TRUE, 
         col="black") 
lines(Ker,col="red")
legend("topright",legend=c("Histograma","Distribución Normal","Kernel"),lty=1, 
       col=c('deepskyblue3', 'black', 'firebrick3'), bty='n')
#Ahora haremos el QQ-plot de los rendimientos logarítmicos anuales
par(mfrow=c(1,1))
qqnorm(R_anules,ylim = c(-7,7),main='QQ-Plot de rendimientos anuales')
qqline(R_anules,col = "firebrick",lwd=2,lty=2)
#Hacemos ahora el autocorrelagrama de los rendimientos logarítmicos anuales y 
#comparamos con el de los rendimientos logarítmicos absolutos anuales
par(mfrow=c(2,1))
acf(R_anules, lwd=8, lend=1, col="coral2",main="Rendimientos logarítmicos Anuales")
acf(abs(R_anules), lwd=8, lend=1, col="coral2",
    main="Valor absoluto de los rendimientos logarítmicos Anuales")







#.-.-.-.-.-.-.-.-.-.-.-.-.-.-Medidas de Riesgo.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#Estimaremos los parámetros con la función fit.st
#Recordemos que tenemos que trabajar con la distribución de PÉRDIDAS
#por lo que multiplicamos los rendimientos diarios por un -1 para obtener
#las pérdidas logarítmicas diarias
vect_losses<-(-1)*unlist(df_Diario$y)


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-PARAMÉTRICO T-STUDENT.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#.-.-.-.-.-.-.-.-AJUSTANDO LOS PARÁMETROS

#Comenzamos ajustando los datos a una t-student usando la función fit.st() de QRM
#pero esta función requiere de una matriz, sino nos manda un error, así que 
#crearemos una 
matrix_close<-matrix(data=vect_losses,nrow=length(vect_losses),ncol=1,
                     byrow = TRUE)
t.fit <- fit.st(matrix_close)
#Además de los grados de libertad, esta función nos devuelve el parámetro de 
#desplazamiento/posición mu, uno de escala sigma. Visualizamos
t.fit
#Grados de libertad
nu <- as.double(t.fit$par.ests[1])
#Parámetro de posición/desplazamiento
mu <- as.double(t.fit$par.ests[2])
#Parámetro de escala
sigma <- as.double(t.fit$par.ests[3])
#visualizamos
par(cex=0.7,mfrow=c(1,1)) 
curve(mu+sigma*dt(x,df=nu), from=-10, to=10, 
      col="black",ylab = '',main='t-student ajustada a la distribución de 
      pérdidas diarias')
#.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-ALPHA 95%
#Esta función nos ayuda a calcular el expected shortfall. Viene en el paquete
#QRM, que es el que usa el profesor en su código
ES.st95 <- ESst(0.95,mu=mu,sd=sigma,df=nu)
#Con la función ya integrada en R que nos devuelve el cuantil pedido a una 
#t-student con mu=0 y sigma=1, solo multiplicamos dicho cuantil por el parámetro
#de escala y sumamos el parámentro de desplazamiento/posición 
VaR.st95 <- mu+sigma*qt(0.95,df=nu)
#.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-ALPHA 99%
#Esta función nos ayuda a calcular el expected shortfall. Viene en el paquete
#QRM, que es el que usa el profesor en su código
ES.st99 <- ESst(0.99,mu=mu,sd=sigma,df=nu)
#Con la función ya integrada en R que nos devuelve el cuantil pedido a una 
#t-student con mu=0 y sigma=1, solo multiplicamos dicho cuantil por el parámetro
#de escala y sumamos el parámentro de desplazamiento/posición 
VaR.st99 <- mu+sigma*qt(0.99,df=nu)


#.-.-.-.-.-.-.-.-.-.-.-.-.-.-PARAMÉTRICO NORMAL.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-

#Estimaremos los parámetros mu y sigma usando los estimadores insesgados S^2 y
# 'X barra'
par(cex=0.7,mfrow=c(1,1)) 
curve(dnorm(x,mean=mean(vect_losses),sd=sd(vect_losses)), from=-10, to=10, 
      col="black",ylab = '',main='Normal ajustada a la distribución de 
      pérdidas diarias')
#.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-ALPHA 95%

#Recordemos que en clase vimos la expresión para el expected shortfall de una
#Normal (mu,sigma). Haremos uso de dicha expresión directamente:
ES.norm95<-mean(vect_losses)+sd(vect_losses)*((dnorm(qnorm(0.95)))/(1-0.95))
#Ahora el VaR, que es simplemente el cuantil
VaR.norm95<-qnorm(0.95,mean=mean(vect_losses),sd=sd(vect_losses))
#.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-ALPHA 99%

#Recordemos que en clase vimos la expresión para el expected shortfall de una
#Normal (mu,sigma). Haremos uso de dicha expresión directamente:
ES.norm99<-mean(vect_losses)+sd(vect_losses)*((dnorm(qnorm(0.99)))/(1-0.99))
#Ahora el VaR, que es simplemente el cuantil
VaR.norm99<-qnorm(0.99,mean=mean(vect_losses),sd=sd(vect_losses))



#.-.-.-.-.-.-.-.-.-.-.-.--.-.NO PARAMÉTRICO.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.

#Aquí, en lugar de asumir una distribución, trabajaremos con la distribución de
#datos empírica que ya tenemos.
#visualizamos, haciendo un ajuste para el binwidth primero. 
breaks <- pretty(range(vect_losses), n = nclass.FD(vect_losses), min.n = 1)
bwidth <- breaks[2]-breaks[1]
ggplot(df_Diario, aes(x=y)) + 
    geom_histogram(color="black", fill="blue",binwidth=bwidth)+
    labs(title = 'Distribución empírica de pérdidas diarias',
         x='Pérdidas logarítmicos')
#.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-ALPHA 95%

#Calculamos el cuantil 0.95 de nuestros datos empíricos obtenidos, con la función 
#quantile. Names está en False para que solo nos dé el número
VaR.hist95<-quantile(vect_losses,prob=0.95,names=FALSE)

#Ahora, para el Expected Shortfall, tomamos el promedio sobre todos los valores
#de nuestra distrinbución de datos empírica que sean mayores al cuantil 0.95
ES.hist95<- mean(vect_losses[vect_losses>=VaR.hist95])

#.-.-.-.-.-.-.-.-.-.-.--.-.-.-.-.-ALPHA 99%

#Calculamos el cuantil 0.99 de nuestros datos empíricos obtenidos, con la función 
#quantile. Names está en False para que solo nos dé el número
VaR.hist99<-quantile(vect_losses,prob=0.99,names=FALSE)

#Ahora, para el Expected Shortfall, tomamos el promedio sobre todos los valores
#de nuestra distrinbución de datos empírica que sean mayores al cuantil 0.99
ES.hist99<- mean(vect_losses[vect_losses>=VaR.hist99])
#Para el Expected shortfall tomamos el promedio sobre todos los  valores
#de nuestra distrinbución de datos empírica que sean mayores al cuantil 0.95
VaR.hist95<-quantile(vect_losses,prob=0.95,names=FALSE)

#Cabe destacar que aquí estamos midiendo el value at risk con las pérdidas 
#logarítmicas diarias.