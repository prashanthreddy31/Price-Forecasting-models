## RECURSIVE FORECAST MODEL ##


library(randomForest)
library(forecast)
library(magrittr)

#converting into time series data
ts_data <-ts(thesis_data$Price ,start = c(2010,1), end =c(2023,6), frequency = 12)


# the desired number of lags (six months)
lag_order <- 6
# the forecast horizon (six months)
horizon <-12

# embedding magic!
ts_data_mbd <- embed(ts_data, lag_order + 1) 

y_train <- ts_data_mbd[, 1] # the target
X_train <- ts_data_mbd[, -1] # everything but the target

y_test <- window(ts_data, start = c(2022, 7), end = c(2023, 6))
# the year 2023
X_test <- ts_data_mbd[nrow(ts_data_mbd), c(1:lag_order)] # the test set consisting
# of the six most recent values (we have six lags) of the training set. It's the
# same for all models.

forecasts_rf <- numeric(horizon)

for (i in 1:horizon){
  # set seed
  set.seed(1234)
  
  # fit the model
  fit_rf <- randomForest(X_train, y_train, ntree = 1000)
  
  # predict using the test set
  forecasts_rf[i] <- predict(fit_rf, X_test)
  
  # here is where we repeatedly reshape the training data to reflect the time distance
  # corresponding to the current forecast horizon.
  y_train <- y_train[-1] 
  X_train <- X_train[-nrow(X_train), ] 
}

# convert to ts format
y_pred <- ts(forecasts_rf,
             start = c(2022, 7),
             frequency = 12)

#Checking the accuracy of the model
library(Metrics)
MAE = mae(y_test, y_pred)
RMSE = rmse(y_test, y_pred)
MAPE = mape(y_test, y_pred)