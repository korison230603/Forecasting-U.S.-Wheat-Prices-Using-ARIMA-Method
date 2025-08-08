library(mvnormtest)
library(lmtest)
library(IMTest)

#Memanggil Data
data=read.delim("clipboard")
data
summary(data)


#Mengubah data ke bentuk time series
data1=ts(data, start=c(2019),freq=12)
data1

#Eksplorasi Data
summary(data1)
var(data1)
sd(data1)

#Membuat Plot
Plot.ts(data1)
adf.test(data1)

#uji stasioneritas data dalam ragam
lambda=BoxCox.lambda(data1)
lambda

#Transformasi Box-Cox
transformasi=log(data1,0.5)
lambda2=BoxCox.lambda(transformasi)
lambda2


#Diff
Data2=diff(data1)
Data2

#Uji Stasioneritas Data Kembali
adf.test(Data2)

#Diff
Data3=diff(Data2)
Data3

#Uji Stasioneritas Data Kembali
adf.test(Data3)

#identifikasi model sementara
#Plot
acf(Data3)
pacf(Data3)

#Model terbaik
fit=arima(data1,c(2,2,2))
fit
fit1=arima(data1,c(2,2,1))
fit1
fit2=arima(data1,c(2,2,0))
fit2
fit3=arima(data1,c(1,2,2))
fit3
fit4=arima(data1,c(1,2,1))
fit4
fit5=arima(data1,c(1,2,0))
fit5
fit6=arima(data1,c(0,2,2))
fit6
fit7=arima(data1,c(0,2,1))
fit7
fit8=arima(data1,c(0,2,0))
fit8

AIC(fit,fit1,fit2,fit3,fit4,fit5,fit6,fit7,fit8)

#Uji Signifikansi parameter (t)
coef(fit1)
coeftest(fit1)

#Uji Normalitas Residual
myresid=fit1$residuals
shapiro.test(myresid)
qqnorm(myresid)
qqline(myresid)

#Uji independensi residual (white noise)
boxresult<-LjungBoxTest(myresid)
Plot(boxresult[,3],main="Ljung-Box_Test",xlab="Lag",ylab="p-value")
Box.test(myresid,type="Ljung-Box")

#Prediksi
Fitted=ts(data1+myresid)
Fitted1=ts(Fitted,start=c(2019,1), freq=12)
Fitted1
write.csv(Fitted1, "Fitted1.csv")

#Peramalan
Peramalan=predict(fit,n.ahead=12)
Peramalan=Peramalan$pred
Permalan=ts(Peramalan, start=c(2024,1),freq=12)
Peramalan

#Akurasi
mape=mean(abs(myresid)/data1, na.rm = TRUE)*100
mape

#Plot Hasil
Plot(data1, main="Plot Peramalan Harga Gandum AS ", lwd=2, col="black", ylim=c(100,1000), xlim=c(2019,2024), type="o", pch=15)
limitDate=end(data1)[1]+(end(data1)[2]-1)/frequency(data1)
abline(v=limitDate, lty=7)
lines(Fitted1, lwd=2, col="darkturquoise", type="o", pch=12) 
lines(Peramalan,col="red", type="o", pch=10) 
legend("topleft", legend=c("Data Aktual", "Fitted Value", "Peramalan"), col=c("black", "darkturquoise", "red"), lty=1, pch=c(15,12,10), cex=0.8, inset=0.05)
