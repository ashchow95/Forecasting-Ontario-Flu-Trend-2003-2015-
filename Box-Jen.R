## STAT 443 Forecasting Final Project Regression Section ##

flu.data <- read.table("C:/Users/Lam/Documents/data.txt", header=TRUE, sep=",", skip = 11)
ont.data <- flu.data[9]
flu.tsx <- ts(flu.data[9], start = 2003 + 39/52,end = 2015 + 35/52, frequency = 52)
plot.ts(flu.tsx)

train <- ts(ont.data[1:310, ],start = 2003 + 39/52,end = 2009 + 35/52,frequency=52) # 50% training set
test <- ts(ont.data[311:465,], start = 2009 + 36/52 , end = 2012 + 34/52, frequency=52) # 25% testing set
valid <- ts(ont.data[466:620,], frequency=52) # 25% valid set

logtrain <- log(train)
par(mfrow=c(1,1))

difftrain <- diff(train,lag = 52)
difflogtrain <- diff(logtrain,lag = 52)
acf(train, lag.max = 310)
acf(train, lag.max = 310, type ="partial")
acf(difftrain, lag.max = 310)
acf(difftrain, lag.max = 310, type ="partial")
acf(logtrain, lag.max = 310)
acf(difflogtrain, lag.max = 310)
acf(difflogtrain, lag.max = 310, type = "partial")


logtrain <- log(train)
plot.ts(train)
plot.ts(logtrain)
plot.ts(diff(train,lag = 52))
plot.ts(diff(diff(train,lag = 52)))
plot.ts(diff(logtrain,lag = 52))
plot.ts(diff(diff(logtrain,lag = 52)))
M1 <- arima(train,order=c(2,1,2), seasonal=list(order=c(0,1,1), period=52))
M2 <- arima(train,order=c(2,1,1), seasonal=list(order=c(0,1,1), period=52))
M3 <- arima(train,order=c(1,1,1), seasonal=list(order=c(0,1,1), period=52))
M4 <- arima(train,order=c(1,1,2), seasonal=list(order=c(0,1,1), period=52))
pred_M1 = predict(M1,n.ahead = 310, interval = "pred")
Press_M1 = sum((test-pred_M1$pred[1:155])^2)
plot.ts(test, ylim= c(-500, 15000))
points(pred_M1$pred , type = "l", col = "red")
points(pred_M1$pred + 1.96*pred_M1$se, type= "l", col = "blue")
points(pred_M1$pred - 1.96*pred_M1$se, type= "l", col = "blue")
qqplot(test,pred_M1$pred)

pred_M2 = predict(M2,n.ahead = 310, interval = "pred")
Press_M2 = sum((test-pred_M2$pred[1:155])^2)
plot.ts(test, ylim= c(-500, 15000))
points(pred_M2$pred , type = "l", col = "red")
points(pred_M2$pred + 1.96*pred_M2$se, type= "l", col = "blue")
points(pred_M2$pred - 1.96*pred_M2$se, type= "l", col = "blue")
qqplot(test,pred_M2$pred)

pred_M3 = predict(M3,n.ahead = 310, interval = "pred")
Press_M3 = sum((test-pred_M3$pred[1:155])^2)
plot.ts(test, ylim= c(-500, 15000))
points(pred_M3$pred , type = "l", col = "red")
points(pred_M3$pred + 1.96*pred_M3$se, type= "l", col = "blue")
points(pred_M3$pred - 1.96*pred_M3$se, type= "l", col = "blue")
qqplot(test,pred_M3$pred)

pred_M4 = predict(M4,n.ahead = 310, interval = "pred")
Press_M4 = sum((test-pred_M4$pred[1:155])^2)
plot.ts(test, ylim= c(-500, 15000))
points(pred_M4$pred , type = "l", col = "red")
points(pred_M4$pred + 1.96*pred_M4$se, type= "l", col = "blue")
points(pred_M4$pred - 1.96*pred_M4$se, type= "l", col = "blue")
qqplot(test,pred_M4$pred)



logM1 <- arima(logtrain,order=c(2,1,2), seasonal=list(order=c(0,1,1), period=52))
logM2 <- arima(logtrain,order=c(2,1,1), seasonal=list(order=c(0,1,1), period=52))
logM3 <- arima(logtrain,order=c(1,1,1), seasonal=list(order=c(0,1,1), period=52))
logM4 <- arima(logtrain,order=c(1,1,2), seasonal=list(order=c(0,1,1), period=52))
pred_logM1 = predict(logM1,n.ahead = 310, interval = "pred")
Press_logM1 = sum((test-exp(pred_logM1$pred[1:155]))^2)
plot.ts(test, ylim= c(-500, 15000))
points(exp(pred_logM1$pred) , type = "l", col = "red")
points(exp(pred_logM1$pred + 1.96*pred_logM1$se), type= "l", col = "blue")
points(exp(pred_logM1$pred - 1.96*pred_logM1$se), type= "l", col = "blue")
qqplot(test,exp(pred_logM1$pred))
abline(1,1)
acf(logM1$residuals)
acf(logM1$residuals, type = "partial")

pred_logM2 = predict(logM2,n.ahead = 310, interval = "pred")
Press_logM2 = sum((test-exp(pred_logM2$pred[1:155]))^2)
plot.ts(test, ylim= c(-500, 15000))
points(exp(pred_logM2$pred) , type = "l", col = "red")
points(exp(pred_logM2$pred + 1.96*pred_logM2$se), type= "l", col = "blue")
points(exp(pred_logM2$pred - 1.96*pred_logM2$se), type= "l", col = "blue")
qqplot(test,exp(pred_logM2$pred))
abline(1,1)
acf(logM2$residuals)
acf(logM2$residuals, type = "partial")


pred_logM3 = predict(logM3,n.ahead = 310, interval = "pred")
Press_logM3 = sum((test-exp(pred_logM3$pred[1:155]))^2)
plot.ts(test, ylim= c(-500, 15000))
points(exp(pred_logM3$pred) , type = "l", col = "red")
points(exp(pred_logM3$pred + 1.96*pred_logM3$se), type= "l", col = "blue")
points(exp(pred_logM3$pred - 1.96*pred_logM3$se), type= "l", col = "blue")
qqplot(test,exp(pred_logM3$pred))
abline(1,1)
acf(logM3$residuals)
acf(logM3$residuals, type = "partial")


pred_logM4 = predict(logM4,n.ahead = 310, interval = "pred")
Press_logM4 = sum((test-exp(pred_logM4$pred[1:155]))^2)
plot.ts(test, ylim= c(-500, 15000))
points(exp(pred_logM4$pred) , type = "l", col = "red")
points(exp(pred_logM4$pred + 1.96*pred_logM4$se), type= "l", col = "blue")
points(exp(pred_logM4$pred - 1.96*pred_logM4$se), type= "l", col = "blue")
qqplot(test,exp(pred_logM4$pred))
abline(1,1)
acf(logM4$residuals)
acf(logM4$residuals, type = "partial")

AIC(M1)
AIC(M2)
AIC(M3)
AIC(M4)
Press_M1
Press_M2
Press_M3
Press_M4
min(AIC(M1),AIC(M2),AIC(M3),AIC(M4))
min(Press_M1,Press_M2,Press_M3,Press_M4)

AIC(logM1)
AIC(logM2)
AIC(logM3)
AIC(logM4)
Press_logM1
Press_logM2
Press_logM3
Press_logM4
min(AIC(logM1),AIC(logM2),AIC(logM3),AIC(logM4))
min(Press_logM1,Press_logM2,Press_logM3,Press_logM4)
shapiro.test(logM1$residuals)

