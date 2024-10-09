library(tseries)
library(forecast)

dataset <- read.csv("D:/ntu/time_series/wwwusage.txt",header = FALSE)
x <- dataset$V1
x
x<- as.numeric(x)
x
clean_x <- subset(x,!is.na(x))
clean_x
length(clean_x)
ts_clean_x <- ts(clean_x)
ts_clean_x
ts.plot(ts_clean_x, ylab="x")
abline(a=max(ts_clean_x),b=0,col='red',)
abline(a=min(ts_clean_x),b = 0,col='green')
abline(a=mean(ts_clean_x),b = 0,col='blue')
legend("topleft",title="",c("MAX=228","Mean=137.8","MIN=83"),fill=c("red","blue","green"),horiz = TRUE)

acf(ts_clean_x,100)
pacf(ts_clean_x,100)


adf.test(ts_clean_x,c("stationary"),k=1)
adf.test(diff(ts_clean_x,differences = 1),c("stationary"),k=1)
adf.test(diff(ts_clean_x,differences = 2),c("stationary"),k=1)

kpss.test(ts_clean_x,null="Level",lshort=TRUE)
kpss.test(diff(ts_clean_x,differences=1),null="Level",lshort=TRUE)
kpss.test(diff(ts_clean_x,differences=2),null="Level",lshort=TRUE)

diff_x <- diff(ts_clean_x,differences =1)
diff_x
ts.plot(diff_x, ylab ="x (First-Order Differenced)" )


mean(diff_x)
max(diff_x)
min(diff_x)
acf(diff_x,100)
pacf(diff_x,100)

ts.yw <- ar.yw(diff_x, order.max=5)
ts.yw

\\
model <- arima(ts_clean_x, order=c(3,1,0))
model
tsdiag(model)
model$residuals

Box.test(model$residuals, type="Ljung-Box")

ts.plot(model$residuals, ylab="Residuals")
hist(model$residuals, freq = TRUE, xlab = "Residuals", ylab = "Frequency")
mean_data = mean(model$residuals)
sd_data = sd(model$residuals)
curve(dnorm(model$residuals, mean = mean_data, sd = sd_data) * length(model$residuals) * diff(hist(model$residuals, plot = FALSE)$breaks)[1], add = TRUE, col = "red", lwd = 2)

modeltrain <- arima(ts_clean_x[0:91], order=c(3,1,0))
modeltrain
forecast <- predict(modeltrain,n.ahead=10)
forecast

ts_clean_x[91:100]
plot(ts_clean_x[0:90], type="l", xlab="Time", ylab="x",xlim=c(0,120),ylim=c(80,230))
lines(seq(90,100),ts_clean_x[90:100], col='red')
lines(seq(90,100),c(ts_clean_x[90],forecast$pred), col='blue')
legend("topleft", legend=c("Training", "Actual", "Forecasted"), col=c("black", "red", "blue"), lty=1)

\\
model2 <- arima(ts_clean_x, order=c(1,1,1))
model2
tsdiag(model2)
model2$residuals

Box.test(model2$residuals, type="Ljung-Box")

ts.plot(model2$residuals, ylab="Residuals")
hist(model2$residuals, freq = TRUE, xlab = "Residuals", ylab = "Frequency")
mean_data2 = mean(model2$residuals)
sd_data2 = sd(model2$residuals)
curve(dnorm(x, mean = mean_data2, sd = sd_data2) * length(model2$residuals) * diff(hist(model2$residuals, plot = FALSE)$breaks)[1], add = TRUE, col = "red", lwd = 2)

modeltrain2 <- arima(ts_clean_x[0:91], order=c(1,1,1))
modeltrain2
forecast2 <- predict(modeltrain2,n.ahead=10)
forecast2

ts_clean_x[91:100]
plot(ts_clean_x[0:90], type="l", xlab="Time", ylab="x",xlim=c(0,120),ylim=c(80,230))
lines(seq(90,100),ts_clean_x[90:100], col='red')
lines(seq(90,100),c(ts_clean_x[90],forecast2$pred), col='blue')
legend("topleft", legend=c("Training", "Actual", "Forecasted"), col=c("black", "red", "blue"), lty=1)


\\\\
diff2_x <- diff(ts_clean_x,differences =2)
diff2_x
ts.plot(diff2_x, ylab ="x (Second-Order Differenced)" )

mean(diff2_x)
max(diff2_x)
min(diff2_x)

acf(diff2_x,100)
pacf(diff2_x,100)

ts.yw <- ar.yw(diff_x, order.max=5)
ts.yw
\\
model3 <- arima(ts_clean_x, order=c(2,2,0))
model3
tsdiag(model3)
model3$residuals

Box.test(model3$residuals, type="Ljung-Box")

ts.plot(model3$residuals, ylab="Residuals")
hist(model3$residuals, freq = TRUE, xlab = "Residuals", ylab = "Frequency")
mean_data3 = mean(model3$residuals)
sd_data3 = sd(model3$residuals)
curve(dnorm(x, mean = mean_data3, sd = sd_data3) * length(model3$residuals) * diff(hist(model3$residuals, plot = FALSE)$breaks)[1], add = TRUE, col = "red", lwd = 2)

modeltrain3 <- arima(ts_clean_x[0:91], order=c(2,2,0))
modeltrain3
forecast3 <- predict(modeltrain3,n.ahead=10)
forecast3

ts_clean_x[91:100]
plot(ts_clean_x[0:90], type="l", xlab="Time", ylab="x",xlim=c(0,120),ylim=c(80,280))
lines(seq(90,100),ts_clean_x[90:100], col='red')
lines(seq(90,100),c(ts_clean_x[90],forecast3$pred), col='blue')
legend("topleft", legend=c("Training", "Actual", "Forecasted"), col=c("black", "red", "blue"), lty=1)


mape_model1 <- mean(abs((ts_clean_x[91:100]-forecast$pred)/ts_clean_x[91:100]))*100
mape_model1
mape_model2 <- mean(abs((ts_clean_x[91:100]-forecast2$pred)/ts_clean_x[91:100]))*100
mape_model2
mape_model3 <- mean(abs((ts_clean_x[91:100]-forecast3$pred)/ts_clean_x[91:100]))*100
mape_model3

rmse_model1 <- sqrt(mean(ts_clean_x[91:100]-forecast$pred)^2)
rmse_model1
rmse_model2 <- sqrt(mean(ts_clean_x[91:100]-forecast2$pred)^2)
rmse_model2
rmse_model3 <- sqrt(mean(ts_clean_x[91:100]-forecast3$pred)^2)
rmse_model3

train_model1_e <- sqrt(modeltrain$sigma2)
train_model1_e
train_model2_e <- sqrt(modeltrain2$sigma2)
train_model2_e
train_model3_e <- sqrt(modeltrain3$sigma2)
train_model3_e

