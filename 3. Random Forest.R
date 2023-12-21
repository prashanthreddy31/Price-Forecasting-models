library(randomForest)
library(forecast)
library(ggplot2)

#importing the dataset
data <- read_excel("Dataset.xlsx")
View(data)
attach(data)

ts_data <-ts(data$Price ,start = c(2010,1), end =c(2023,6), frequency = 12)

#lag values
rf_data <- data.frame(cbind(
                          ts_data ,
                          lag01= lag(ts_data),
                          lag02 =lag(ts_data ,2),
                          lag03=lag(ts_data ,3),
                          lag04=lag(ts_data ,4),
                          lag05=lag(ts_data ,5),
                          y_lag06=lag(ts_data ,6)))

#Removing the missed values from the data
rf_data <- rf_data[complete.cases(rf_data) ,]

#Splitting the data set into training & test sets
rf_train <- rf_data [1:125 ,]
rf_test <- rf_data [126:length(rf_data$ts_data) ,]
X_test <- rf_test[,-7]
y_test <- rf_test[,7]

#Random forest model
rf_fit <- randomForest(y_lag06 ~ .,data=rf_train, ntree=1000, mtry=4)
rf_fit
rf_pred <-predict(rf_fit ,X_test)
rf_pred

#Checking the accuracy of the model
library(Metrics)
MAE = mae(y_test, rf_pred)
RMSE = rmse(y_test, rf_pred)
MAPE = mape(y_test, rf_pred)


#Tuning parameters
t <- tuneRF(rf_data[,1:6],
            rf_data$lag06,    # exclude the response variable
            stepFactor = 0.5, # per each interactions number of variables tried at each split (mtry) in inflated by 0.5
            plot = TRUE,      # plot OOB
            ntreeTry = 1000,  # tuning number of trees
            trace = TRUE,
            improve = 0.05)

Res<-(rf_train$lag06)-(rf_fit$predicted)

#Normal Density Curve
checkresiduals(Res,theme=theme_classic())
library(rcompanion)
plotNormalHistogram( Res, prob = FALSE, col="grey", border="black",density =10,
                     breaks = 10,length = 10000, linecol="red", lwd = 3 )
