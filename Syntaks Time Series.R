#Required Packages
library(forecast)
library(tseries)
library(FinTS)
library(rugarch)
library(lmtest)
library(xlsx)
library(zoo)

#Load Datasets
data=read.xlsx(file.choose(),sheetName="Sheet1")

#Summary Data
summary(data)
attach(data)
data=ts(data,start=c(1990,1),frequency = 12)
win.graph()

#Time series plot
plot.ts(data)
abline(reg = lm(data~time(data)))

#Lambda
lambda=BoxCox.lambda(data,method="guerrero")
lambda
data1=BoxCox(data,lambda)
lambda=BoxCox.lambda(data1)
lambda

#Uji stasioneritas
adf.test(data,k=10)
pp.test(data)
acf(data,lag=50)

#Jumlah diff
ndiffs(data)

#Transformasi log return
dataLR=diff(log(data))

#Differencing
dataDiff=diff(data)

#Plot
plot.ts(dataDiff)

#Uji Stasioneritas
adf.test(dataDiff)
pp.test(dataDiff)
acf(dataDiff)

#Plot ACF dan PACF
acf(dataDiff,main="Plot ACF Cadangan Devisa")
acf(dataDiff,plot=FALSE)
pacf(dataDiff,main="Plot PACF Cadangan Devisa")
pacf(dataDiff,plot=FALSE)

#Model Auto Arima
model1=auto.arima(data,max.p=10,max.q=10,stationary = TRUE,seasonal = TRUE,trace=TRUE,method="CSS",lambda = )
model2=auto.arima(data,max.p=10,max.q=10,stationary = FALSE,seasonal = TRUE,trace=TRUE,method="ML",lambda = )
summary(model1)
summary(model2)
coeftest(model1)
coeftest(model2)
ts.plot(data,model1$fitted,col=c("red","blue"))
tsdiag(model1)

#RESIDU MODEL TERBAIK
resi1=resid(model1)
win.graph()
checkresiduals(model1)

#UJI ASUMSI
ks.test(resi1,"pnorm",mean=mean(resi1),sd=sd(resi1))
jarque.bera.test(resi1)
Box.test(model1$residuals,type="Ljung-Box",lag=18)
ArchTest(resi1,demean=FALSE)

#FORECAST
RAMALAN=forecast(model1,h=10)
PLOT(RAMALAN)

#ARIMA MODELLING
#non-seasonal ARIMA model
modelARIMA=arima(data, order = c(3,1,0),
                 include.mean=TRUE)
summary(modelARIMA)                                   #ARIMA (3,1,0)
coeftest(modelARIMA)                                 #significance test for parameter

#seasonal ARIMA model
modelSARIMA=arima(data, order = c(0,1,1),     
                 seasonal = list(order = c(0,0,2),period =12),
                 include.mean=TRUE)
summary(modelSARIMA)                                   #ARIMA (0,1,1)(0,0,2)12
coeftest(modelSARIMA) 