library(fpp)
library(fpp2)
library(TTR)
op <- read.csv("~/Downloads/Oil_price_1.csv")
attributes(op) #Show type
summary(op$PRICE) #mean, min etc
op_ts = ts(op$PRICE, start = 2000, end = c(2017,12), frequency = 12)
op_ts_2 = window(op_ts, start = c(2016,2), end = c(2017,12), frequency = 12)

boxplot(op_ts)
boxplot(op_ts~cycle(op_ts), xlab = "Months", ylab = "OIL PRICE")

#FULL TS
op_ts
plot(op_ts)
Acf(op_ts) #Corelellogram 
#for testing 
df = tail(op, 12)
op_2018_true = df[,2]

#decomp
decomp=stl(op_ts,s.window = "periodic")
plot(decomp)
plot(decompose(op_ts))
cat("The time series is:",decompose(op_ts)$type)

#seasonal adjustment
SA_ts = seasadj(decomp)
plot(SA_ts)


#mean_f
mean_forecast = meanf(op_ts, h=4, level = c(20,40)) #mean forecast, level gives confidence interval
plot(op_ts)
lines(mean_forecast$fitted, col="red") #plotting the graph over the original graph, we used $fitted to get the graph
forecast_mean = forecast(mean_forecast, h=4)
plot(forecast_mean)
lines(mean_forecast$fitted, col="red")
accuracy(forecast_mean)
residual = mean_forecast$residuals
fitted = mean_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(mean_forecast$fitted,mean_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(mean_forecast$x,mean_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)


#naive 
#h = as.integer(readline(prompt = "How many month of 2018 would you like to predict:"))
h = 12 #number of predictions
naive_forecast = naive(op_ts, h=h)
lines(naive_forecast$fitted, col="green")
#accuracy(naive_forecast)
#residual
residual = naive_forecast$residuals
fitted = naive_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(naive_forecast$fitted,naive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(naive_forecast$x,naive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_naive= forecast(naive_forecast, h=h)
plot(forecast_naive)
lines(naive_forecast$fitted, col="green")
true = op_2018_true[h]
accuracy(forecast_naive, x=true)

#snaive
h = 12
snaive_forecast = snaive(op_ts, 12)
lines(snaive_forecast$fitted, col="blue")
accuracy(snaive_forecast)
#residuals
residual = snaive_forecast$residuals
fitted = snaive_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(snaive_forecast$fitted,snaive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(snaive_forecast$x,snaive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_snaive= forecast(snaive_forecast, h=h)
plot(forecast_snaive)
lines(snaive_forecast$fitted, col="blue")
true = op_2018_true[h]
accuracy(forecast_snaive, x=true)

#random walk
h = 12
rwf_forecast = rwf(op_ts, 12, drift = TRUE)
lines(rwf_forecast$fitted, col="blue")
#residuals
residual = rwf_forecast$residuals
fitted = rwf_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(rwf_forecast$fitted,rwf_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(rwf_forecast$x,rwf_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_rwf= forecast(rwf_forecast, h=h)
plot(forecast_rwf)
lines(rwf_forecast$fitted, col="blue")
true = op_2018_true[h]
accuracy(forecast_rwf, x=true)


#Moving Avg
h=12
ma4_forecast = ma(op_ts,order=4)
ma12_forecast = ma(op_ts,order=12)
ma24_forecast = ma(op_ts,order=24) 
plot(op_ts)
lines(ma4_forecast, col="blue")
lines(ma12_forecast, col="green")
lines(ma24_forecast, col="yellow")
#residual
checkresiduals(ma12_forecast)
#forecast
forecast_ma12 = forecast(ma12_forecast, h=h, allow.multiplicative.trend = TRUE)
plot(op_ts)
plot(forecast_ma12, col="green")
lines(op_ts, col="black")
lines(forecast_ma12, col="red")
true = op_2018_true[h]
accuracy(forecast_ma12, x=true)

#24month moving avg
forecast_ma24 = forecast(ma24_forecast, h=h, allow.multiplicative.trend = TRUE)
plot(forecast_ma24, col="red")
lines(op_ts, col="black")
true = op_2018_true[h]
accuracy(forecast_ma24, x=true)


#ets
ets_forecast = ets(op_ts)
forecast_ets = forecast.ets(ets_forecast,h=12)
plot(forecast_ets)
lines(ets_forecast$fitted, col='pink')
#residuals
residual = ets_forecast$residuals
fitted = ets_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(ets_forecast$fitted,ets_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(ets_forecast$x,ets_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_ets= forecast(ets_forecast, h=h)
plot(forecast_ets)
true = op_2018_true[h]
accuracy(forecast_ets, x=true)


#holt_winters
h = 12
hw_forecast= HoltWinters(op_ts)
plot(hw_forecast)
forecast_hw = forecast(hw_forecast,h=12)
plot(forecast_hw)
#residuals
residual = hw_forecast$residuals
fitted = hw_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(hw_forecast$fitted,hw_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(hw_forecast$x,hw_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_hw= forecast(hw_forecast, h=h)
plot(forecast_hw)
lines(hw_forecast$fitted, col="green")
true = op_2018_true[h]
accuracy(forecast_hw, x=true)


#Arima
auto.arima(op_ts)
arima_forecast = auto.arima(op_ts, ic="bic",stepwise = FALSE) 

#residuals
residual = arima_forecast$residuals
fitted = arima_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(arima_forecast$fitted,arima_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(arima_forecast$x,arima_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_arima= forecast(arima_forecast, h=h)
plot(forecast_arima)
lines(arima_forecast$fitted, col = "blue")
true = op_2018_true[h]
accuracy(forecast_arima, x=true)

#all accuracy measures
accuracy(forecast_mean, x=true)
accuracy(forecast_naive, x=true)
accuracy(forecast_snaive, x=true)
accuracy(forecast_rwf, x=true)
accuracy(forecast_ma12, x=true)
accuracy(forecast_ma24, x=true)
accuracy(forecast_ets, x=true)
accuracy(forecast_hw, x=true)
accuracy(forecast_arima, x=true)


#-------------------------------------------------------#
#Window 1 Starting May 2011
op_ts_1 = window(op_ts, start = c(2011,5), end = c(2017,12), frequency = 12)
op_ts_1
plot(op_ts_1)
Acf(op_ts_1) #Corelellogram 
#for testing 
df = tail(op, 12)
op_2018_true = df[,2]

#decomp
decomp=stl(op_ts_1,s.window = "periodic")
plot(decomp)
plot(decompose(op_ts_1))
cat("The time series is:",decompose(op_ts_1)$type)


#mean_f
mean_forecast = meanf(op_ts_1, h=4, level = c(20,40)) #mean forecast, level gives confidence interval
plot(op_ts_1)
lines(mean_forecast$fitted, col="red") #plotting the graph over the original graph, we used $fitted to get the graph
forecast_mean = forecast(mean_forecast, h=12)
plot(forecast_mean)
accuracy(forecast_mean)
residual = mean_forecast$residuals
fitted = mean_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(mean_forecast$fitted,mean_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(mean_forecast$x,mean_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
forecast_mean= forecast(mean_forecast, h=12)
plot(forecast_mean)
true = op_2018_true[h]
accuracy(forecast_mean, x=true)


#naive 
#h = as.integer(readline(prompt = "How many month of 2018 would you like to predict:"))
h = 12 #number of predictions
naive_forecast = naive(op_ts_1, h=h)
plot(op_ts_1)
lines(naive_forecast$fitted, col="green")
#accuracy(naive_forecast)
#residual
residual = naive_forecast$residuals
fitted = naive_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(naive_forecast$fitted,naive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(naive_forecast$x,naive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_naive= forecast(naive_forecast, h=h)
plot(forecast_naive)
true = op_2018_true[h]
accuracy(forecast_naive, x=true)

#snaive
h = 12
snaive_forecast = snaive(op_ts_1, 12)
lines(snaive_forecast$fitted, col="blue")
accuracy(snaive_forecast)
#residuals
residual = snaive_forecast$residuals
fitted = snaive_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(snaive_forecast$fitted,snaive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(snaive_forecast$x,snaive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_snaive= forecast(snaive_forecast, h=h)
plot(forecast_snaive)
true = op_2018_true[h]
accuracy(forecast_snaive, x=true)

#random walk
h = 12
rwf_forecast = rwf(op_ts_1, 12, drift = TRUE)
plot(op_ts_1)
lines(rwf_forecast$fitted, col="blue")
#residuals
residual = rwf_forecast$residuals
fitted = rwf_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(rwf_forecast$fitted,rwf_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(rwf_forecast$x,rwf_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_rwf= forecast(rwf_forecast, h=h)
plot(forecast_rwf)
true = op_2018_true[h]
accuracy(forecast_rwf, x=true)


#Moving Avg
h=12
ma4_forecast = ma(op_ts_1,order=4)
ma12_forecast = ma(op_ts_1,order=12)
ma24_forecast = ma(op_ts_1,order=24) 
plot(op_ts_1)
lines(ma4_forecast, col="blue")
lines(ma12_forecast, col="green")
lines(ma24_forecast, col="yellow")
#residual
checkresiduals(ma12_forecast)
#forecast
forecast_ma12 = forecast(ma12_forecast, h=h, allow.multiplicative.trend = TRUE)
plot(forecast_ma12, col="green")
lines(op_ts_1, col="black")
true = op_2018_true[h]
accuracy(forecast_ma12, x=true)

#24month moving avg
forecast_ma24 = forecast(ma24_forecast, h=h, allow.multiplicative.trend = TRUE)
plot(forecast_ma24)
true = op_2018_true[h]
accuracy(forecast_ma24, x=true)


#ets
ets_forecast = ets(op_ts_1)
forecast_ets = forecast.ets(ets_forecast,h=12)
plot(forecast_ets)
lines(ets_forecast$fitted, col='pink')
#residuals
residual = ets_forecast$residuals
fitted = ets_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(ets_forecast$fitted,ets_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(ets_forecast$x,ets_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_ets= forecast(ets_forecast, h=h)
plot(forecast_ets)
true = op_2018_true[h]
accuracy(forecast_ets, x=true)


#holt_winters
h = 12
hw_forecast= HoltWinters(op_ts_1)
plot(hw_forecast)
forecast_hw = forecast(hw_forecast,h=12)
plot(forecast_hw)
#residuals
checkresiduals(forecast_hw)
residual = hw_forecast$residuals
fitted = hw_forecast$fitted
plot(residual)
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(hw_forecast$fitted,hw_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(hw_forecast$x,hw_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_hw= forecast(hw_forecast, h=h)
plot(forecast_hw)
true = op_2018_true[h]
accuracy(forecast_hw, x=true)


#Arima
auto.arima(op_ts_1)
arima_forecast = auto.arima(op_ts_1, ic="bic",stepwise = FALSE) 

#residuals
residual = arima_forecast$residuals
fitted = arima_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(arima_forecast$fitted,arima_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(arima_forecast$x,arima_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_arima= forecast(arima_forecast, h=h)
plot(forecast_arima)
lines(arima_forecast$fitted, col = "blue")
true = op_2018_true[h]
accuracy(forecast_arima, x=true)

#all accuracy scores
accuracy(forecast_mean, x=true)
accuracy(forecast_naive, x=true)
accuracy(forecast_snaive, x=true)
accuracy(forecast_rwf, x=true)
accuracy(forecast_ma12, x=true)
accuracy(forecast_ma24, x=true)
accuracy(forecast_ets, x=true)
accuracy(forecast_hw, x=true)
accuracy(forecast_arima, x=true)


#--------------------------------------------------#
#Window 2 Starting Feb 2016
op_ts_2
plot(op_ts_2)
Acf(op_ts_2) #Corelellogram 
#for testing 
df = tail(op, 12)
op_2018_true = df[,2]

#decomp
decomp=stl(op_ts_2,s.window = "periodic")
plot(decomp)
plot(decompose(op_ts_2))
cat("The time series is:",decompose(op_ts_2)$type)

#seasonal adjustment
SA_ts = seasadj(decomp)
plot(SA_ts)


#mean_f
mean_forecast = meanf(op_ts_2, h=4, level = c(20,40)) #mean forecast, level gives confidence interval
plot(op_ts_2)
lines(mean_forecast$fitted, col="red") #plotting the graph over the original graph, we used $fitted to get the graph
forecast_mean = forecast(mean_forecast, h=4)
plot(forecast_mean)
lines(mean_forecast$fitted, col="red")
accuracy(forecast_mean)
residual = mean_forecast$residuals
fitted = mean_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(mean_forecast$fitted,mean_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(mean_forecast$x,mean_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)


#naive 
#h = as.integer(readline(prompt = "How many month of 2018 would you like to predict:"))
h = 12 #number of predictions
naive_forecast = naive(op_ts_2, h=h)
lines(naive_forecast$fitted, col="green")
#accuracy(naive_forecast)
#residual
residual = naive_forecast$residuals
fitted = naive_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(naive_forecast$fitted,naive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(naive_forecast$x,naive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_naive= forecast(naive_forecast, h=h)
plot(forecast_naive)
lines(naive_forecast$fitted, col="green")
true = op_2018_true[h]
accuracy(forecast_naive, x=true)

#snaive
h = 12
snaive_forecast = snaive(op_ts_2, 12)
lines(snaive_forecast$fitted, col="blue")
accuracy(snaive_forecast)
#residuals
residual = snaive_forecast$residuals
fitted = snaive_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(snaive_forecast$fitted,snaive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(snaive_forecast$x,snaive_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_snaive= forecast(snaive_forecast, h=h)
plot(forecast_snaive)
lines(snaive_forecast$fitted, col="blue")
true = op_2018_true[h]
accuracy(forecast_snaive, x=true)

#random walk
h = 12
rwf_forecast = rwf(op_ts_2, 12)
lines(rwf_forecast$fitted, col="blue")
#residuals
residual = rwf_forecast$residuals
fitted = rwf_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(rwf_forecast$fitted,rwf_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(rwf_forecast$x,rwf_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_rwf= forecast(rwf_forecast, h=h)
plot(forecast_rwf)
lines(rwf_forecast$fitted, col="blue")
true = op_2018_true[h]
accuracy(forecast_rwf, x=true)


#Moving Avg
h=12
ma4_forecast = ma(op_ts_2,order=2)
ma12_forecast = ma(op_ts_2,order=4)
ma24_forecast = ma(op_ts_2,order=8) 
plot(op_ts_2)
lines(ma4_forecast, col="blue")
lines(ma12_forecast, col="green")
lines(ma24_forecast, col="yellow")
#residual
checkresiduals(ma4_forecast)
#forecast
forecast_ma12 = forecast(ma12_forecast, h=4)
plot(forecast_ma12)
true = op_2018_true[h]
accuracy(forecast_ma24, x=true)

#24month moving avg
forecast_ma24 = forecast(ma24_forecast, h=h, allow.multiplicative.trend = TRUE)
plot(forecast_ma24)
true = op_2018_true[h]
accuracy(forecast_ma24, x=true)


#ets
ets_forecast = ets(op_ts_2)
forecast_ets = forecast.ets(ets_forecast,h=12)
plot(forecast_ets)
lines(ets_forecast$fitted, col='pink')
#residuals
residual = ets_forecast$residuals
fitted = ets_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(ets_forecast$fitted,ets_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(ets_forecast$x,ets_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_ets= forecast(ets_forecast, h=h)
plot(forecast_ets)
true = op_2018_true[h]
accuracy(forecast_ets, x=true)


#holt_winters
h = 12
hw_forecast= HoltWinters(op_ts_2)
plot(hw_forecast)
forecast_hw = forecast(hw_forecast,h=12)
plot(forecast_hw)
#residuals
residual = hw_forecast$residuals
fitted = hw_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(hw_forecast$fitted,hw_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(hw_forecast$x,hw_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_hw= forecast(hw_forecast, h=h)
plot(forecast_hw)
lines(hw_forecast$fitted, )
true = op_2018_true[h]
accuracy(forecast_hw, x=true)


#Arima
auto.arima(op_ts_2)
arima_forecast = auto.arima(op_ts_2, ic="bic",stepwise = FALSE) 

#residuals
residual = arima_forecast$residuals
fitted = arima_forecast$fitted
plot(residual, ylab="Residual")
hist(residual, xlab="Residual")
Acf(residual)
plot.ts(arima_forecast$fitted,arima_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
plot.ts(arima_forecast$x,arima_forecast$residuals,xy.labels = FALSE,xy.lines = FALSE)
#forecast
forecast_arima= forecast(arima_forecast, h=h)
plot(forecast_arima)
lines(arima_forecast$fitted, col = "blue")
true = op_2018_true[h]
accuracy(forecast_arima, x=true)

#All Accuracy measures
accuracy(forecast_mean, x=true)
accuracy(forecast_naive, x=true)
accuracy(forecast_snaive, x=true)
accuracy(forecast_rwf, x=true)
accuracy(forecast_ma12, x=true)
accuracy(forecast_ma24, x=true)
accuracy(forecast_ets, x=true)
accuracy(forecast_hw, x=true)
accuracy(forecast_arima, x=true)














