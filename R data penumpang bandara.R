#Library
#Untuk membaca data dengan format Excel
library(readxl)
# Untuk mengubah data menjadi Time Series, Acf, Pacf, Model Arima dan ADF Test
library(tseries)
# Untuk melihat signifikansi dari Koefesien
library(lmtest) 
# Untuk memprediksi data nantinya
library(forecast)
library(ggplot2)
library(TSA)

#INPUT DATA
setwd("C:/Users/ASUS/Documents/Kuliah/2. Jurusan/Semester 4/Analisis Deret Waktu 01/Tubes ADW")
Data <- read_excel("data penumpang bandara.xlsx",sheet = "Sheet1")
DataPolonia <- ts(Data$Polonia, frequency = 12)
DataHatta <- ts(Data$`Soekarno Hatta`, frequency = 12)
DataJuanda <- ts(Data$Juanda, frequency = 12)
DataNgurah <- ts(Data$`Ngurah Rai`, frequency = 12)
DataHasa <- ts(Data$Hasanudin, frequency = 12)
DataJumlah <- ts(Data$Jumlah, frequency = 12)

#Membuat grafik untuk data Time Series
#Data Polonia
plot(DataPolonia)
plot(DataPolonia, main = 'Polonia', ylab = 'Data', col = 'red', type='o')
#Membuat garis rataan untuk grafik
abline(h=mean(DataPolonia), col = 'blue')

#Data Hatta
plot(DataHatta)
plot(DataHatta, main = 'Hatta', ylab = 'Data', col = 'red', type='o')
#Membuat garis rataan untuk grafik
abline(h=mean(DataHatta), col = 'blue')

#Data Juanda
plot(DataJuanda)
plot(DataJuanda, main = 'Juanda', ylab = 'Data', col = 'red', type='o')
#Membuat garis rataan untuk grafik
abline(h=mean(DataJuanda), col = 'blue')

#Data Ngurah
plot(DataNgurah)
plot(DataNgurah, main = 'Ngurah', ylab = 'Data', col = 'red', type='o')
#Membuat garis rataan untuk grafik
abline(h=mean(DataNgurah), col = 'blue')

#Data Hasa
plot(DataHasa)
plot(DataHasa, main = 'Hasa', ylab = 'Data', col = 'red', type='o')
#Membuat garis rataan untuk grafik
abline(h=mean(DataHasa), col = 'blue')

#Data Jumlah
plot(DataJumlah)
plot(DataJumlah, main = 'Jumlah', ylab = 'Data', col = 'red', type='o')
#Membuat garis rataan untuk grafik
abline(h=mean(DataJumlah), col = 'blue')

#Plot data secara seasonal
ggseasonplot(DataJumlah)

#Plot per subseries, misalnya perbulan
ggsubseriesplot(DataJumlah)
#plot ACF data
acf(DataJumlah, lag.max = 60, main='Plot ACF tsData')

#DIFERENSI
#Diferensi original 
dif_tren = diff(DataJumlah)
plot(dif_tren, main='Plot hasil diferensiasi original',col='brown')

#Diferensi musiman untuk menghilangkan pola musiman
dif_musiman =diff(DataJumlah,lag = 12)
plot(dif_musiman, main='Plot hasil diferensiasi musiman',col='purple')

#Diferensi gabungan
dif_gabungan=(diff(diff(DataJumlah), lag = 12))
plot(dif_gabungan, main='Plot hasil diferensiasi original dan musiman',col='purple')

#Diferensi gabungan 2
dif_gabungan2=(diff(diff(dif_gabungan), lag = 12))
plot(dif_gabungan2, main='Plot hasil diferensiasi original dan musiman',col='purple')

#CEK STASIONERITAS
#dengan plot ACF
acf(dif_gabungan, lag.max = 60, main='Plot ACF dif_gabungan')
#dengan uji ADF(Augmented Dickey-Fuller)
adf.test(dif_gabungan)

#dengan plot ACF
acf(dif_gabungan2, lag.max = 60, main='Plot ACF dif_gabungan')
#dengan uji ADF(Augmented Dickey-Fuller)
adf.test(dif_gabungan2)

#IDENTIFIKASI ORDE
#Plot ACF PACF data
acf(DataJumlah, lag.max = 60, main='Plot ACF Data Jumlah')			
pacf(DataJumlah, lag.max = 60, main='Plot PACF Data Jumlah')
#Plot ACF PACF data diferensi gabungan
acf(dif_gabungan, lag.max = 60, main='Plot ACF dif_gabungan')
pacf(dif_gabungan, lag.max = 60, main='Plot PACF dif_gabungan') 
#Plot ACF PACF data diferensi gabungan 2
acf(dif_gabungan2, lag.max = 60, main='Plot ACF dif_gabungan2')
pacf(dif_gabungan2, lag.max = 60, main='Plot PACF dif_gabungan2') 

#ESTIMASI PARAMETER
#Pilih nilai AIC terkecil
#Estimasi Parameter Model SARIMA (1,1,1)(0,1,0)12
(model_1 = arima(DataJumlah,order=c(1,1,1),seasonal=list(order=c(0,1,0),period=12)))
#Estimasi Parameter Model SARIMA (1,1,2)(0,1,0)12
(model_2 = arima(DataJumlah,order=c(1,1,2),seasonal=list(order=c(0,1,0),period=12)))
#Estimasi Parameter Model SARIMA (1,1,1)(0,1,3)12
(model_3 = arima(DataJumlah,order=c(1,1,1),seasonal=list(order=c(0,1,3),period=12)))
#Estimasi Parameter Model SARIMA (1,1,2)(0,1,3)12
(model_4 = arima(DataJumlah,order=c(1,1,2),seasonal=list(order=c(0,1,3),period=12)))
#Estimasi Parameter Model SARIMA (1,1,1)(3,1,0)12
(model_5 = arima(DataJumlah,order=c(1,1,1),seasonal=list(order=c(3,1,0),period=12)))
#Estimasi Parameter Model SARIMA (1,1,2)(3,1,0)12
(model_6 = arima(DataJumlah,order=c(1,1,2),seasonal=list(order=c(3,1,0),period=12)))
#Estimasi Parameter Model SARIMA (1,1,1)(3,1,3)12
(model_7 = arima(DataJumlah,order=c(1,1,1),seasonal=list(order=c(3,1,3),period=12)))
#Estimasi Parameter Model SARIMA (1,1,2)(3,1,3)12
(model_8 = arima(DataJumlah,order=c(1,1,2),seasonal=list(order=c(3,1,3),period=12)))
#Estimasi Parameter Model SARIMA (1,2,1)(0,2,0)12
(model_9 = arima(DataJumlah,order=c(1,2,1),seasonal=list(order=c(0,2,0),period=12)))
#Estimasi Parameter Model SARIMA (1,2,2)(0,2,0)12
(model_10 = arima(DataJumlah,order=c(1,2,2),seasonal=list(order=c(0,2,0),period=12)))
#Estimasi Parameter Model SARIMA (1,2,1)(0,2,3)12
(model_11 = arima(DataJumlah,order=c(1,2,1),seasonal=list(order=c(0,2,3),period=12)))
#Estimasi Parameter Model SARIMA (1,2,2)(0,2,3)12
(model_12 = arima(DataJumlah,order=c(1,2,2),seasonal=list(order=c(0,2,3),period=12)))
#Estimasi Parameter Model SARIMA (1,2,1)(3,2,0)12
(model_13 = arima(DataJumlah,order=c(1,2,1),seasonal=list(order=c(3,2,0),period=12)))
#Estimasi Parameter Model SARIMA (1,2,2)(3,2,0)12
(model_14 = arima(DataJumlah,order=c(1,2,2),seasonal=list(order=c(3,2,0),period=12)))
#Estimasi Parameter Model SARIMA (1,2,1)(3,2,3)12
(model_15 = arima(DataJumlah,order=c(1,2,1),seasonal=list(order=c(3,2,3),period=12)))
#Estimasi Parameter Model SARIMA (1,2,2)(3,2,3)12
(model_16 = arima(DataJumlah,order=c(1,2,2),seasonal=list(order=c(3,2,3),period=12)))

#Estimasi nilai koefisien dari model terbaik
#p-value < alpha, artinya koefisien signifikan
coeftest(model_3)
coeftest(model_5)
coeftest(model_9)
coeftest(model_11)
coeftest(model_12)

#UJI DIAGNOSTIK
par(mfrow=c(2,2))
#plot residual: rataan konstan nol
plot(rstandard(model_9),ylab="Standardized Residuals",type='o', main='Plot Residual Model_1',col='red')
abline(h=0)
#plot QQ: distribusi normal
qqnorm(rstandard(model_9))
qqline(rstandard(model_9))
#histogram: distribusi normal
hist(rstandard(model_9), main='Histogram residual model_9')
#ACF: kalau tidak melewati batas, tidak ada kolerasi
acf(rstandard(model_9),lag=12,main='Plot residual model model_9')
#Ljung box test untuk melihat korelasi residuals
#p-value < alpha, H0 ditolak (H0: tidak ada korelasi)
Box.test(residuals(model_9),lag = 12, type="Ljung-Box")
#Shapiro wilk test untuk melihat kenormalan residuals
#p-value < alpha, H0 ditolak (H0: data berdistribusi normal)
shapiro.test(rnorm(model_9))

#FORECAST
#untuk memprakirakan CO2 untuk bulan mendatang
(fc=forecast(DataJumlah,model=model_9, h=12))
#Plot data dengan hasil forecast
autoplot(fc)
