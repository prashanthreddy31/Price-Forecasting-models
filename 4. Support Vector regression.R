library(e1071)
library(forecast)
library(tseries)

#importing the dataset
data <- read_excel("Dataset.xlsx")
View(data)
attach(data)

#converting into time series data
ts_data <- ts(data$Price ,start = c(2010,1), end =c(2023,6), frequency = 12)

#creating lag values
svm_data <- data.frame(cbind(
  ts_data ,
  lag01= lag(ts_data),
  lag02 =lag(ts_data ,2),
  lag03=lag(ts_data ,3),
  lag04=lag(ts_data ,4),
  lag05=lag(ts_data ,5),
  y_lag06=lag(ts_data ,6)))

#Removing the missed values from the data
svm_data <- svm_data[complete.cases(svm_data) ,]

#Splitting the data set into training & test sets
svm_train <- svm_data [1:125 ,]
svm_test <- svm_data [126:length(svm_data$ts_data) ,]

X_test <- svm_test[,-7]
y_test <- svm_test[,7]

#Fitting the svm model
svm_fit <- svm(lag06 ~ .,data=svm_train, type="eps-regression",kernel="radial")
svm_fit

#Prediction over the test set
svm_pred <-predict(svm_fit ,X_test)

#Checking the accuracy of the model
library(Metrics)
MAE = mae(y_test, svm_pred)
RMSE = rmse(y_test, svm_pred)
MAPE = mape(y_test, svm_pred)


#Tuning the hyper parameters using Grid search algorithm
set.seed (1271)
SVMOPT=tune.svm(lag06~.,data = svm_train ,
                cost = c(10^( -5:0)),
                gamma = c(0.001 ,.005 ,0.01 ,0.1) ,
                epsilon = 10^( -4:0))

print(SVMOPT)


SVM_Bst_Model=SVMOPT$best.model
fitted_svm<-SVMBstModel$fitted

SVMBst_pred <- predict(SVMBstModel ,svm_test [,-7])
SVMBst_pred

#test set accuracy
accuracy(SVMBst_pred , svm_test[,7])

#training set accuracy
accuracy(fitted_svm,svm_train$lag06)

res <-SVMBstModel$residuals

#Residuals plot
checkresiduals(Res,theme=theme_classic())

#Normal Density Curve
library(rcompanion)
plotNormalHistogram( res, prob = FALSE, col="grey", border="black",density =10,
                     breaks = 10,length = 10000, linecol="red", lwd = 3 )