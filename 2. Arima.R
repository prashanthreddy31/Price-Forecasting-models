## ARIMA ##

library(tseries)
library(forecast)
library(ggplot2)


#importing the dataset
data <- read_excel("Dataset.xlsx")
View(data)
attach(data)

#converting the Dataframe into time series data
ts_data <- ts(data$Price ,start = c(2010,1), end =c(2023,6), frequency = 12)

#test for normality of data
#Shapiro Wilk test for checking the normality of the data
shapiro.test(data$Price)

#Jarque bera test for checking the normality of the data
jarque.bera.test(data$Price)

#Box-pierce test for checking the auto correlation
Box.test(Price)

#tests for stationarity
# Augmented Dickey fuller test
adf.test(Price)
# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(Price)
#  Phillips–Perron test
pp.test(Price)


#tests for checking the seasonality of the time series data
library(seastests)
library(uroot)
fried(ts_data)
hegy.test(ts_data)

#ACF & PACF plots
acf(Price,lag.max = 50, main= "ACF")
pacf(Price,lag.max = 50, main = "PACF")

plot.ts(ts_data)

# Splitting the data into train & test set at the ratio 9:1
train_data <- Price[1:146]
test_data <- Price[147:162]

# Fitting the model
ar_model <- auto.arima(train_data,stepwise = FALSE,approximation = FALSE, 
                       ic = "aic", trace = TRUE,allowdrift = FALSE)
lmtest :: coeftest(ar_model)%>% round (.,3)

# Testing the accuracy of the model
Pred <- forecast(ar_model , h=16)
accuracy(Pred, test_data)

# training set accuarcy
Fitted <- fitted(ar_model)
accuarcy(Fitted, train_data)

#Diagnostic checking
et <- residuals(model1)
# Testing for normality of residuals
shapiro.test(et)
#testing the auto-correlation of residuals
Box.test(et, lag = 10, type = "Ljung-Box")

#checking for randomness of residuals
library(randtests)
runs.test(et)

#test for non-linearity
bds.test(et)

# Residual Plots#
checkresiduals(et,test ="LB", theme=theme_classic())

plot.ts(et,ylab="residuals", main ="Residual Plot")
acf(et,lag.max = 50, main ="Residual ACF")

#Normal Density Curve
library(rcompanion)
plotNormalHistogram( et, prob = FALSE, col="grey", border="black",density =10,
                     breaks = 10,length = 10000, linecol="red", lwd = 3 )
boxplot(et)
