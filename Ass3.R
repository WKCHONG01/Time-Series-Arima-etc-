


library(TSA)
library(astsa)
library(zoo)
library(xts)
library(quantmod)
library(fBasics)
library(forecast)
library(ggplot2)
library(fGarch)
library(rugarch)
library(tseries)
library(zoo)

myData = read.csv("D:/ntu/time_series/AAPL.csv", header = TRUE, sep=",")
myData

myData$Date <- as.Date(myData$Date)
myData$Open <- as.double(myData$Open)
myData$High <- as.double(myData$High)
myData$Low <- as.double(myData$Low)
myData$Close <- as.double(myData$Close)
myData$Adj.Close <- as.double(myData$Adj.Close)
myData$Volume <- as.double(myData$Volume)
rownames(myData) <- myData$Date
myData$Date <- NULL
myData
summary(myData)


global.xlab <- 'Date'
global.ylab <- 'Adjusted Closing Price (USD)'
global.stockname <- 'AAPL'
AdjClose <- myData[,'Adj.Close']
Date <- as.Date(row.names(myData))
Date
ggplot(myData, aes(x = Date, y = AdjClose)) +
  geom_line() +  # Draw the line
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Yearly breaks
  labs(title = "Adjusted Closing Price of AAPL (2002-2017)",
       x = "Date",
       y = "Adjusted Closing Price (USD)") +
  theme_minimal()


monthly_data <- to.monthly(myData, OHLC=FALSE)
adjusted_close <- monthly_data[,'Adj.Close']
AdjClose_ts <- ts(adjusted_close, frequency=12)
stl_result <- stl(AdjClose_ts, s.window = "period")
plot(stl_result)

data_AC <- myData[,'Adj.Close']
data_AC
acf(data_AC,48)
pacf(data_AC,48)
data_AC

adf.test(data_AC,c("stationary"),k=1)
log_data_AC <- log(data_AC)
log_data_AC
diff1 <- diff(log_data_AC,differences = 1)*100
plot(diff1, type="l")


acf(diff1,48)
pacf(diff1,48)
adf.test(diff1)
acf(abs(diff1),48)
pacf(abs(diff1),48)
plot(abs(diff1),type="l")
acf(diff1^2,48)
pacf(diff1^2,48)
plot((diff1)^2,type="l")

qqnorm(diff1)
qqline(diff1, col = 2)
skewness(diff1) 
kurtosis(diff1) #

eacf(diff1) # 0,2 1,2 2,2 3,3 
eacf(abs(diff1)) # 1,1 1,2 2,2
eacf(diff1^2) # 1,1 2,2 3,3 
# 0,2 1,1 1,2 2,2 3,3  


G02 = garch(diff1, order=c(0,2))
summary(G02)


G11 = garch(diff1, order=c(1,1))
summary(G11)


G12 = garch(diff1, order=c(1,2))
summary(G12)


G22 = garch(diff1, order=c(2,2))
summary(G22)


G33 = garch(diff1, order=c(3,3))
summary(G33)

AIC(G02)
AIC(G11)
AIC(G12)
AIC(G22)
AIC(G33)

checkresiduals(G11)
qqnorm(residuals(G11)); qqline(residuals(G11), col = 2)
ggtsdisplay(abs(residuals(G11)))
ggtsdisplay(residuals(G11)^2)
gBox(G11,method='squared')

train_num <- (length(data_AC) - 30)
data_train <- head(data_AC, train_num)
data_test <- tail(data_AC, round(length(data_AC) - train_num)) 

garchspec <- ugarchspec(mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
                        variance.model=list(model = "sGARCH", garchOrder=c(1,1)),
                        distribution.model = "norm")
fit <- ugarchfit(garchspec, diff1, out.sample=length(data_test)-1)


garchspec2 <- ugarchspec(mean.model=list(armaOrder=c(0,1), include.mean=TRUE),
                        variance.model=list(model = "sGARCH", garchOrder=c(1,1)),
                        distribution.model = "norm")
fit2 <- ugarchfit(garchspec2, diff1, out.sample=length(data_test)-1)

garchspec3 <- ugarchspec(mean.model=list(armaOrder=c(1,0), include.mean=TRUE),
                         variance.model=list(model = "sGARCH", garchOrder=c(1,1)),
                         distribution.model = "norm")
fit3 <- ugarchfit(garchspec3, diff1, out.sample=length(data_test)-1)

garchspec4 <- ugarchspec(mean.model=list(armaOrder=c(1,1), include.mean=TRUE),
                         variance.model=list(model = "sGARCH", garchOrder=c(1,1)),
                         distribution.model = "norm")
fit4<- ugarchfit(garchspec4, diff1, out.sample=length(data_test)-1)

garchspec5 <- ugarchspec(mean.model=list(armaOrder=c(1,2), include.mean=TRUE),
                         variance.model=list(model = "sGARCH", garchOrder=c(1,1)),
                         distribution.model = "norm")
fit5 <- ugarchfit(garchspec5, diff1, out.sample=length(data_test)-1)

garchspec6 <- ugarchspec(mean.model=list(armaOrder=c(2,1), include.mean=TRUE),
                         variance.model=list(model = "sGARCH", garchOrder=c(1,1)),
                         distribution.model = "norm")
fit6 <- ugarchfit(garchspec6, diff1, out.sample=length(data_test)-1)

garchspec7 <- ugarchspec(mean.model=list(armaOrder=c(2,2), include.mean=TRUE),
                         variance.model=list(model = "sGARCH", garchOrder=c(1,1)),
                         distribution.model = "norm")
fit7 <- ugarchfit(garchspec7, diff1, out.sample=length(data_test)-1)


# Extracting AIC 
aic_value1 <- infocriteria(fit)[1]
aic_value2 <- infocriteria(fit2)[1]
aic_value3 <- infocriteria(fit3)[1]
aic_value4 <- infocriteria(fit4)[1]
aic_value5 <- infocriteria(fit5)[1]
aic_value6 <- infocriteria(fit6)[1]
aic_value7 <- infocriteria(fit7)[1]

# Printing the AIC 
print(paste("AIC:", aic_value1))
print(paste("AIC:", aic_value2))
print(paste("AIC:", aic_value3))
print(paste("AIC:", aic_value4))
print(paste("AIC:", aic_value5))
print(paste("AIC:", aic_value6))
print(paste("AIC:", aic_value7))


forecast = ugarchforecast(fit7, n.ahead=length(data_test)-1, n.roll=length(data_test)-1)
plot(forecast, which="all")
