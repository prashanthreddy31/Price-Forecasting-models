library(readxl)

#Import the Data set
data <- read_excel("Dataset.xlsx")
View(data)
attach(data)

# prepare sample data in the form of data frame with cols of time steps and values  
year<- 1:30
df <- data.frame(year,data)
colnames(df)<-c("year","Yield")

#Trend analysis using Simple Linear regression
model <- lm(Yield ~ year, data = df)
summary(model)

#Trend analysis using Mann-kendall trend test & sen`s slope estimator`
library(trend)
attach(df)

#Mann-Kendall test
mk.test(Yield)

#Sen`s slope estimator
sens.slope(Yield)
