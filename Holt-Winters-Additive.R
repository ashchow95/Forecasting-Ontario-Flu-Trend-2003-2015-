# Read in Flu Ontario data
FluData = read.csv(file.choose())

# Convert data in to time series data
FluDataTS = ts(FluData$Ontario, frequency = 52)
# Plot the time series
plot(FluDataTS, ylab = "Flu", main = "Time series data of the Flu") # Seasonal effect is there but the variance is too large
plot(log(FluDataTS), ylab = "Log of Flu Data", main = "Time Series of Log Flu Data")# Variance is reduced
LogFluData = log(FluDataTS)

# Defining data for Holt Winters 
FluDataHW = HoltWinters(LogFluData, seasonal="add")
plot(FluDataHW, predict(FluDataHW, n.ahead = 52))#Plotting Holt Winters additive model with a 52 week predition

PI = predict(FluDataHW, 52, prediction.interval = TRUE) # prediction interval for the Holt-Winter transformation of the data
plot(PI[,1], ylim = c(3,10), ylab = "Predicted Values",main = "Plot of predicted values")# plotting the prediction values for the next 52 weeks
lines(PI[,2], col="orange") #Plotting upper bound for the prediction interval
lines(PI[,3], col="blue") #Plotting lower bound for the prediction interval