## STAT 443 Forecasting Final Project Regression Section ##

flu.data <- read.table("C:/Users/Alaric/Desktop/WINTER/STAT 443/Project/data.txt", header=TRUE, sep=",", skip = 11)
ont.data <- flu.data[,c("Ontario")]
flu.ts <- ts(ont.data, start = c(2003,39), frequency = 52)

train <- ts(ont.data[1:310], start = c(2003, 39), frequency = 52)
test <- ts(ont.data[311:465], start = c(2009, 37), frequency = 52)
valid <- ts(ont.data[466:620], start = c(2012, 36), frequency = 52)

# Log 
log.train <- log(train)
log.test <- log(test)
log.valid <- log(valid)

# Training Set
time <- time(log.train)
week <- as.factor(cycle(log.train)) 

# Testing set
time.test<-time(log.test)
week.test<-as.factor(cycle(log.test))
fit.test<-lm(log.test~ time.test+ week.test)
X2 <-model.matrix(fit.test)

# Useful Function
library(forecast)

PRESS <- function(prediction) {
  #' calculate the PRESS
  PRESS <- sum(prediction$residual^2)
  return(PRESS)
}

analyze <- function(value) # Residual Analysis
{
  par(mfcol=c(2,2)) # Compact the aforementioned plots
  ts.plot(value, gpars=list(xlab="year", ylab="residauls")) # 1st plot: TS of residuals
  title("Time Series Plot of Residual") 
  abline(h=mean(value)) # Mean 0 Line
  
  qqnorm(value) # 2nd plot: QQplot
  qqline(value)
  
  ACF <- acf(value, main ="ACF") # 3rd plot: ACF
  PACF <- acf(value, main ="PACF", type="partial") # 4th plot: PACF
}

## Model Selection
# Model 1: Addictive Model
model1_fit <- lm(log.train ~ time + week)
plot(log.train, main = "Fitted Linear Regression")
points(time, model1_fit$fitted, type='l', col="red")
residual <- residuals(model1_fit)
analyze(residual)
summary(model1_fit)

# Model 2: Interaction Model
model2_fit <- lm(log.train ~ (time + week)^2)
plot(log.train, main = "Fitted Linear Regression")
points(time, model2_fit$fitted, type='l', col="red")
residual <- residuals(model2_fit)
analyze(residual)

# Analysis yields that ARMA model is better predictor
## Start with ARMA (1,0)

# Model 3: ARMA(1,0) of addictive model
X <- model.matrix(model1_fit)
model3_arma <-arima(log.train, order = c(1,0,0), xreg = X[,2:53]) # 2:53 because weeks
plot(log.train, main = "Fitted Linear Regression")
points(time, log.train - model3_arma$res, type='l', col="red")
analyze(residuals(model3_arma))
# Model 4: ARMA(1,1) of addictive model (To handle unexplained spike at lag 1 and 2)
X <- model.matrix(model1_fit)
model4_arma <-arima(log.train, order = c(1,0,1), xreg = X[,2:53])
plot(log.train, main = "Fitted Linear Regression")
points(time, log.train - model4_arma$res, type='l', col="red")
analyze(residuals(model4_arma))
# Model 5: ARMA(2,1) of addictive model (one spiked reduced, one spike remains)
X <- model.matrix(model1_fit)
model5_arma <-arima(log.train, order = c(2,0,1), xreg = X[,2:53])
plot(log.train, main = "Fitted Linear Regression")
points(time, log.train - model5_arma$res, type='l', col="red")
analyze(residuals(model5_arma))
# Model 6: ARMA(2,2) of addictive model
X <- model.matrix(model1_fit)
model6_arma <-arima(log.train, order = c(2,0,2), xreg = X[,2:53])
plot(log.train, main = "Fitted Linear Regression")
points(time, log.train - model6_arma$res, type='l', col="red")
analyze(residuals(model6_arma))



# Prediction Model 4 (Best Model with ARMA with p = 1)
model4_pred <- forecast(model4_arma, n.ahead = 155, xreg = X2[,2:53]) # prediction for 155 weeks
plot(model4_pred)
lines(log.test)


# Prediction Model 5 (Best Model with ARMA with p = 2)
model6_pred <- forecast(model6_arma, n.ahead = 155, xreg = X2[,2:53]) # prediction for 155 weeks
plot(model6_pred)
lines(log.test)

# Analysis
model3_pred <- forecast(model3_arma, n.ahead = 155, xreg = X2[,2:53])
model5_pred <- forecast(model5_arma, n.ahead = 155, xreg = X2[,2:53])

AIC <- cbind(Model3 = model3_arma$aic, Model4 = model4_arma$aic, Model5 = model5_arma$aic, Model6 = model6_arma$aic)
rownames(AIC) <- "AIC"
press <- cbind(Model3 = PRESS(model3_pred), Model4 = PRESS(model4_pred), Model5 =PRESS(model5_pred), Model6 =PRESS(model6_pred))
rownames(press) <- "PRESS"





