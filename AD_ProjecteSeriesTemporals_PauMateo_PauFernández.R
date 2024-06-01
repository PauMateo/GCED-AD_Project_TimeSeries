library(TSstudio)
library(ggplot2)
library(zoo)
library(scales)
library(tidyverse)
library(dplyr)

serie.dat <- read.table("enerusa.dat")


serie=ts(read.table("enerusa.dat"),start=1990,freq=12)
plot(serie,main="Energy USA",ylim=c(6500,10000), col="orange2")
abline(v=1990:2019,col="blue",lty=3)

ts_plot(serie,
        title = "Energia primària comsumida globalment als Estats Units",
        Xtitle = "Temps",
        Ytitle = "Milers de milions de Btu",
        width = 2,
        Ygrid = T,
        Xgrid=T)

date = c();

for(i in c(1990:2018))
  for(j in c(1:12)){
    x = i + (j-1)/12
    datenew = c(date, x)
    date = datenew
    print(x)
  }


date
dades = data.frame(serie.dat, date)

ggplot(data=dades, aes(
  x = date,
  y = V1)) +
  geom_point() +
  geom_line() + 
  geom_smooth()



plot(decompose(serie))



########## variància ??
length(serie);
boxplot(matrix(serie[1:348], nrow=12), col="azure3")

mitjana <- apply( matrix(serie[1:276], nrow=12), 2, mean)  #mitjana
desviació_estàndard <- apply( matrix(serie[1:276], nrow=12), 2, sd)    #desviacions estàndard
plot(desviació_estàndard~mitjana)
abline(lm(desviació_estàndard~mitjana), col=2, lwd=2)


mitjana <- apply( matrix(lnserie[1:276], nrow=12), 2, mean)  #mitjana
desviació_estàndard <- apply( matrix(lnserie[1:276], nrow=12), 2, sd)    #desviacions estàndard
plot(desviació_estàndard~mitjana)
abline(lm(desviació_estàndard~mitjana), col=2, lwd=2)
boxplot(matrix(lnserie[1:348], nrow=12), col="azure3")

plot(serie)
lnserie = log(serie)




monthplot(lnserie, col="black")
d12lnserie = diff(lnserie, 12)

monthplot(d12lnserie)
d1d12lnserie = diff(d12lnserie)

var(lnserie)
var(d12lnserie)
var(d1d12lnserie)

plot(d1d12lnserie)
monthplot(d1d12lnserie)

var(diff(d1d12lnserie))


par(mfrow=c(1,2))
acf(d1d12lnserie,ylim=c(-1,1),lag.max=76,lwd=2,col=c(2,rep(1,11)))
pacf(d1d12lnserie,ylim=c(-1,1),lag.max=76,lwd=2,col=c(rep(1,11),2))
par(mfrow=c(1,1))

 #ABANS D'AJUSTAR ELS COEFICIENTS
 (m1<-arima(d1d12lnserie,order=c(2,0,0),seasonal=list(order=c(3,0,0),period=12)))
 
 (m2<-arima(d1d12lnserie,order=c(0,0,3),seasonal=list(order=c(3,0,0),period=12)))
 
 (m3<-arima(d1d12lnserie,order=c(2,0,0),seasonal=list(order=c(0,0,2),period=12)))
 
 (m4<-arima(d1d12lnserie,order=c(0,0,3),seasonal=list(order=c(0,0,2),period=12)))

 (m5<-arima(d1d12lnserie,order=c(2,0,0),seasonal=list(order=c(1,0,1),period=12)))
 
 (m6<-arima(d1d12lnserie,order=c(0,0,3),seasonal=list(order=c(1,0,1),period=12)))
  
 (m7<-arima(d1d12lnserie,order=c(1,0,1),seasonal=list(order=c(1,0,1),period=12)))
 
 (m8<-arima(d1d12lnserie,order=c(1,0,1),seasonal=list(order=c(3,0,0),period=12)))
 
 (m9<-arima(d1d12lnserie,order=c(1,0,1),seasonal=list(order=c(0,0,2),period=12)))




(m1<-arima(lnserie,order=c(2,1,0),seasonal=list(order=c(3,1,0),period=12)))

(m2<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(3,1,0),period=12)))

(m3<-arima(lnserie,order=c(2,1,0),seasonal=list(order=c(0,1,2),period=12)))

(m4<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12)))
#(m4_<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12)))


(m5<-arima(lnserie,order=c(2,1,0),seasonal=list(order=c(1,1,1),period=12)))

(m8<-arima(lnserie,order=c(1,1,1),seasonal=list(order=c(3,1,0),period=12)))
 
(m9<-arima(lnserie,order=c(1,1,1),seasonal=list(order=c(0,1,2),period=12)))
#(m9_<-arima(lnserie,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12)))


#ens quedem amb el m4 i el m9

############################################################################################
#  VALIDATION
############################################################################################

source("validation.R")

library(forecast)
plot(m4)
plot(m9)

validation(m4, d1d12lnserie)
validation(m9, d1d12lnserie)

##############################################
################# capacitat de predicció
#############################################

lnserie2=window(lnserie, end=c(2017,12))
(m4a<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12)))
(m4b<-arima(lnserie2,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12)))

rbind(coef(m4a), coef(m4b))
#Això ens diu que el model és estable ja que m1a és bastant semblant a m1b

pr=predict(m4b,n.ahead=12)
ll=exp(pr$pred-1.96*pr$se)
ul=exp(pr$pred+1.96*pr$se)
pred=exp(pr$pred)

ts.intersect(ll,pred,ul,serie)

ts.plot(serie,pred,ll,ul, ylim=c(7200, 10000), xlim=c(2016,2019), col=c(1,2,4,4), lty=c(1,1,3,3), type="o")

#Mètriques d'exactitud i precisió
obs=window(serie,start=2018)

(rmse=sqrt(mean((obs-pred)^2)))
(mae=mean(abs(obs-pred)))

(rmspe4=sqrt(mean(((obs-pred)/obs)^2))*100)
(mape4=mean(abs((obs-pred)/obs))*100)
(mean4 = mean(ul-ll))

#model9
lnserie2=window(lnserie, end=c(2017,12))
(m9a<-arima(lnserie,order=c(1,1,1),seasonal=list(order=c(0,1,2),period=12)))
(m9b<-arima(lnserie2,order=c(1,1,1),seasonal=list(order=c(0,1,2),period=12)))

rbind(coef(m9a),coef(m9b))
#Això ens diu que el model és estable ja que m1a és bastant semblant a m1b

pr=predict(m9b,n.ahead=12)
ll=exp(pr$pred-1.96*pr$se)
ul=exp(pr$pred+1.96*pr$se)
pred=exp(pr$pred)

ts.intersect(ll,pred,ul,serie)

ts.plot(serie,pred,ll,ul, xlim=c(2016,2019), ylim=c(7200, 10000), col=c(1,2,4,4), lty=c(1,1,3,3), type="o")

#Mètriques d'exactitud i precisió
obs=window(serie,start=2018)

(rmse=sqrt(mean((obs-pred)^2)))
(mae=mean(abs(obs-pred)))

(rmspe9=sqrt(mean(((obs-pred)/obs)^2))*100)
(mape9=mean(abs((obs-pred)/obs))*100)

(mean9 = mean(ul-ll))






#treim el 4 ja que té una millor indepència de residus (lleguerament)



ultim=c(2017, 12)

##### Previsions a llarg termini amb el model complet ######
pred=predict(m4,n.ahead=12)
pr<-ts(c(tail(lnserie,1),pred$pred),start=ultim+c(1,0),freq=12)
se<-ts(c(0,pred$se),start=ultim+c(1,0),freq=12)

#Intervals
tl1<-ts((pr-1.96*se),start=ultim+c(1,0),freq=12)
tu1<-ts((pr+1.96*se),start=ultim+c(1,0),freq=12)
pr1<-ts((pr),start=ultim+c(1,0),freq=12)

ts.plot(serie,exp(tl1),exp(tu1),exp(pr1),lty=c(1,2,2,1),col=c(1,4,4,2),xlim=c(ultim[1]-1,ultim[1]+3),type="o")
abline(v=(ultim[1]-1):(ultim[1]+3),lty=3,col=4)




############################################################################################
#  CALENDAR EFFECTS
############################################################################################



source("CalendarEffects.r")
##### Previsions a llarg termini amb el model complet ######
data2=c(ultim[1]+2, 1, 12)

wTradDays2=Wtrad(data2)
cpunt2=ts(rep(1,12),start=c(ultim[1]+2),freq=12)

pred=predict(modEC,n.ahead=12,newxreg=data.frame(wTradDays2,cpunt2))
predic=pred$pr
pr<-ts(c(serie[length(serie)],predic),start=ultim+c(1,0),freq=12)
se<-ts(c(0,pred$se),start=ultim+c(1,0),freq=12)

#Intervals
tl2<-ts(pr-1.96*se,start=ultim+c(1,0),freq=12)
tu2<-ts(pr+1.96*se,start=ultim+c(1,0),freq=12)
pr2<-ts(pr,start=ultim+c(1,0),freq=12)

ts.plot(serie,tl2,tu2,pr2,lty=c(1,2,2,1),col=c(1,4,4,2),xlim=ultim[1]+c(-2,3),type="o",main="Model ARIMA(0,1,4)(0,1,1)12")
abline(v=(ultim[1]-2):(ultim[1]+3),lty=3,col=4)data=c(start(serie)[1],start(serie)[2], length(serie)) #starting year, month, series size

(wTradDays=Wtrad(data)) #creates auxiliary variable for trading days configurations (5/2 the ideal proportion)

(wEast=Weaster(data)) #creates auxiliary variable for Easter configurations (half easter on March and other half on April: ideal distribution)



(m4TD<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12), xreg=wTradDays))
(m4TD<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays))
(m4EA<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12), xreg=wEast))
(m4Both<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12), xreg=data.frame(wTradDays, wEast)))




# Estimate the CEs and get the corrected series, serieEC.



EfecTD=coef(m4TD)["wTradDays"]*wTradDays
lnserieTD = lnserie - EfecTD
serieTD = exp(lnserieTD)

# mirem quines transformacions són necessàries per fer estacionària la sèrie:
plot(serieTD - serie)
boxplot(matrix(serieTD[1:348], nrow=12), col="azure3")
var(diff(lnserieTD, 12))
var(diff(diff(lnserieTD, 12)))
var(diff(diff(diff(lnserieTD, 12))))

d1d12lnserieTD=diff(diff(lnserieTD,12))

monthplot(d1d12lnserieTD)
plot(d1d12lnserieTD); abline(h=0)

par(mfrow=c(1,2))
acf(d1d12lnserieTD,ylim=c(-1,1),lag.max=72,col=c(2,rep(1,11)),lwd=2)
pacf(d1d12lnserieTD,ylim=c(-1,1),lag.max=72,col=c(rep(1,11),2),lwd=2)
par(mfrow=c(1,1))


#Ajustem MODELS:

(m1TD<-arima(lnserieTD,order=c(2,1,0),seasonal=list(order=c(3,1,0),period=12)))
(m1TD<-arima(lnserieTD,order=c(2,1,0),seasonal=list(order=c(2,1,0),period=12)))

(m2TD<-arima(lnserieTD,order=c(0,1,3),seasonal=list(order=c(3,1,0),period=12)))

(m3TD<-arima(lnserieTD,order=c(2,1,0),seasonal=list(order=c(0,1,2),period=12)))

(m4TD<-arima(lnserieTD,order=c(0,1,3),seasonal=list(order=c(0,1,2),period=12)))
(m4TD<-arima(lnserieTD,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12)))
(m4TD<-arima(lnserieTD,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12)))

(m5TD<-arima(lnserieTD,order=c(2,1,0),seasonal=list(order=c(1,1,1),period=12)))

(m6TD<-arima(lnserieTD,order=c(0,1,3),seasonal=list(order=c(1,1,1),period=12)))

(m7TD<-arima(lnserieTD,order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12)))

(m8TD<-arima(lnserieTD,order=c(1,1,1),seasonal=list(order=c(3,1,0),period=12)))

(m9TD<-arima(lnserieTD,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12)))

#### els models 4 i 9 tenen AIC extremadament similar! mirem com validen:

validation(m4TD, d1d12lnserieTD)
validation(m9TD, d1d12lnserieTD)


###############################
# ajustem el model identificat a la sèrie orignial
################################


(m4TD<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays))
(m9TD<-arima(lnserie,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays))

validation(m4TD, d1d12lnserieTD)
validation(m9TD, d1d12lnserieTD)

#ens quedem amb el 4 ja que el acf i pacf encaixen lleugerament més amb els teòrics.





lnserie2=window(lnserie, end=c(2017,12))
data2=c(start(lnserie2)[1],start(lnserie2)[2], length(lnserie2)) #starting year, month, series size

(wTradDays2=Wtrad(data2))


(m4TDa<-arima(lnserie,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays))
(m4TDb<-arima(lnserie2,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays2))

rbind(coef(m4TDa), coef(m4TDb))
#Això ens diu que el model és estable ja que m1a és bastant semblant a m1b

pr4=predict(m4TDb,n.ahead=12, newxreg=window(wTradDays,start=c(2018,1)))
ll4=exp(pr4$pred-1.96*pr4$se)
ul4=exp(pr4$pred+1.96*pr4$se)
pred4=exp(pr4$pred)

ts.intersect(ll4,pred4,ul4,serie)

ts.plot(serie,pred4,ll4,ul4, ylim=c(7200, 10000), xlim=c(2016,2019), col=c(1,2,4,4), lty=c(1,1,3,3), type="o")

#Mètriques d'exactitud i precisió
obs=window(serie,start=2018)

(rmse=sqrt(mean((obs-pred4)^2)))
(mae=mean(abs(obs-pred4)))

(rmspe4=sqrt(mean(((obs-pred4)/obs)^2))*100)
(mape4=mean(abs((obs-pred4)/obs))*100)
(mean4 = mean(ul4-ll4))

#model9
lnserie2=window(lnserie, end=c(2017,12))
(m9TDa<-arima(lnserie,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays))
(m9TDb<-arima(lnserie2,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays2))

rbind(coef(m9a),coef(m9b))
#Això ens diu que el model és estable ja que m1a és bastant semblant a m1b

pr9=predict(m9TDb,n.ahead=12, newxreg=window(wTradDays,start=c(2018,1)))
ll9=exp(pr9$pred-1.96*pr9$se)
ul9=exp(pr9$pred+1.96*pr9$se)
pred9=exp(pr$pred)

ts.intersect(ll,pred,ul,serie)

ts.plot(serie,pred,ll,ul, xlim=c(2016,2019), ylim=c(7200, 10000), col=c(1,2,4,4), lty=c(1,1,3,3), type="o")

#Mètriques d'exactitud i precisió
obs=window(serie,start=2018)

(rmse=sqrt(mean((obs-pred)^2)))
(mae=mean(abs(obs-pred)))

(rmspe9=sqrt(mean(((obs-pred)/obs)^2))*100)
(mape9=mean(abs((obs-pred)/obs))*100)

(mean9 = mean(ul-ll))


















###############################################################################################
# ATÍPICS MODEL 4
###############################################################################################

source("atipics2.r")
m4TD.atip=outdetec(m4TD,dif=c(1,12),crit=3.0,LS=F)
atipics=m4TD.atip$atip[order(m4TD.atip$atip[,1]),]
meses=c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
m4TD.atip$sigma2
data.frame(atipics,Fecha=paste(meses[(atipics[,1]-1)%%12+1],start(serie)[1]+((atipics[,1]-1)%/%12)),perc.Obs=atipics[,3])


lnserie.lin = lineal(lnserie, m4TD.atip$atip)
d1d12lnserie.lin=diff(diff(lnserie.lin,12))


plot(lnserie - lnserie.lin)


par(mfrow=c(1,2))
acf(d1d12lnserie.lin,ylim=c(-1,1),lag.max=84,col=c(2,rep(1,11)),lwd=2)
pacf(d1d12lnserie.lin,ylim=c(-1,1),lag.max=84,col=c(rep(1,11),2),lwd=2)
par(mfrow=c(1,1))

#considerem models:

(m1TD.lin<-arima(lnserie.lin,order=c(2,1,0),seasonal=list(order=c(3,1,0),period=12), xreg=wTradDays))
(m1TD.lin<-arima(lnserie.lin,order=c(2,1,0),seasonal=list(order=c(2,1,0),period=12), xreg=wTradDays))

(m2TD.lin<-arima(lnserie.lin,order=c(0,1,3),seasonal=list(order=c(3,1,0),period=12), xreg=wTradDays))

(m3TD.lin<-arima(lnserie.lin,order=c(2,1,0),seasonal=list(order=c(0,1,2),period=12), xreg=wTradDays))

(m4TD.lin<-arima(lnserie.lin,order=c(0,1,3),seasonal=list(order=c(0,1,2),period=12), xreg=wTradDays))

(m5TD.lin<-arima(lnserie.lin,order=c(2,1,0),seasonal=list(order=c(1,1,1),period=12), xreg=wTradDays))

(m6TD.lin<-arima(lnserie.lin,order=c(0,1,3),seasonal=list(order=c(1,1,1),period=12), xreg=wTradDays))

(m7TD.lin<-arima(lnserie.lin,order=c(1,1,1),seasonal=list(order=c(1,1,1),period=12), xreg=wTradDays))

(m8TD.lin<-arima(lnserie.lin,order=c(1,1,1),seasonal=list(order=c(3,1,0),period=12), xreg=wTradDays))

(m9TD.lin<-arima(lnserie.lin,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays))

(m4TD.lin<-arima(lnserie.lin,order=c(0,1,2),seasonal=list(order=c(0,1,2),period=12), xreg=wTradDays))
(m4TD.lin<-arima(lnserie.lin,order=c(0,1,2),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays))

#ara el 9 té millor AIC !!

(m9TD.lin<-arima(lnserie.lin,order=c(1,1,1),seasonal=list(order=c(0,1,2),period=12), xreg=wTradDays))
(m9TD.lin<-arima(lnserie.lin,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays))

#ens quedem amb el 9 ?

validation(m9TD, d1d12lnserie.lin)



EfecTD=coef(m9TD.lin)["wTradDays"]*wTradDays
lnserieTD.lin=lnserie.lin-EfecTD
plot(lnserie-lnserieTD.lin)




plot(lnserie)
lines(lnserieTD.lin,col=2)



######################################
# capacitat predictiva i prediccions
######################################

ultim=c(2017,12)


lnserie.lin.2=window(lnserie.lin,end=ultim)
wTradDays.2=window(wTradDays,end=ultim)


(m9TD.lin)
(m9TD.lin.2<-arima(lnserie.lin.2,order=c(1,1,1),seasonal=list(order=c(0,1,1),period=12), xreg=wTradDays.2))



# ESTABLE !!!!!!
#prediccions::



ultim=c(2017,12)

pred=predict(m9TD.lin.2, n.ahead=12, newxreg=window(wTradDays, start = c(ultim[1]+1, 1)))
predic=pred$pr
wLS=sum(atipics[atipics$type_detected=="LS" & atipics$Obs <= length(serie)-12, 3])
pr<-ts(c(tail(lnserie.lin.2,1),predic+wLS),start=ultim,freq=12)


se<-ts(c(0,pred$se),start=ultim,freq=12)

#Intervals
tl<-ts(pr-1.96*se,start=ultim,freq=12)
tu<-ts(pr+1.96*se,start=ultim,freq=12)
pr<-ts(pr,start=ultim,freq=12)

ts.plot(serie,exp(tl),exp(tu),exp(pr),lty=c(1,2,2,1),col=c(1,4,4,2),xlim=ultim[1]+c(-3,2),type="o")
abline(v=(ultim[1]-3):(ultim[1]+2),lty=3,col=4)



################
#  MESURES
################
obs=window(serie,start=2018)

(rmspe = sqrt(mean((obs-exp(pred$pred))/obs)^2)*100 )
(mape = mean(abs((obs-exp(pred$pred))/obs))*100 )


(mean(exp(tu-tl)))
AIC(m9TD.lin)







data3=c(ultim[1]+2, 1, 12)

wTradDays3=Wtrad(data3)
wEast3=Weaster(data3)
pred=predict(m9TD.lin,n.ahead=12,newxreg=wTradDays3)
predic=pred$pr
wLS=sum(m4TD.atip$atip[m4TD.atip$atip$type_detected=="TC",3])
pr<-ts(c(lnserie[length(serie)],predic+wLS),start=ultim+c(1,0),freq=12)
se<-ts(c(0,pred$se),start=ultim+c(1,0),freq=12)

#Intervals
tl3<-ts(pr-1.96*se,start=ultim+c(1,0),freq=12)
tu3<-ts(pr+1.96*se,start=ultim+c(1,0),freq=12)
pr3<-ts(pr,start=ultim+c(1,0),freq=12)

ts.plot(serie,exp(tl3),exp(tu3),exp(pr3),lty=c(1,2,2,1),col=c(1,4,4,2),xlim=ultim[1]+c(-1,3),type="o",main="Model ARIMA(0,1,1)(0,1,1)12+CE+IA+Outliers")
abline(v=(ultim[1]-1):(ultim[1]+3),lty=3,col=4)














