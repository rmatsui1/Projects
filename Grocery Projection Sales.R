library(tidyverse)
summersales23 <- read_excel("C:/Users/Ricky/Desktop/Data Analyst/Portfolio/Sakura-Ya/Sales By Item Sakura-ya 6.26-6.28 Edited.xlsx")
Sales <- read.csv("Monthly Sales Sakuraya.csv")
view(Sales)

ggplot(data = Sales, mapping = aes(x = Month, y = Total.sales, group = 1)) +
  geom_line () +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 13, by = 1)) +
  labs(title = "Monthly sales", x = "Month", y = "Sales")

sales_actual<-Sales$Total.sales


#Using the Naive method to forecast the next month
naive14 <- c(NA, sales_actual)
naive14


#Create functions for the accuracy measures with vector of actual values 
#and vector of predicted values as inputs
mae<-function(actual,pred){
  mae <- mean(abs(actual-pred), na.rm=TRUE)
  return (mae)
}

mse<-function(actual,pred){
  mse <- mean((actual-pred)^2, na.rm=TRUE)
  return (mse)
}

rmse<-function(actual,pred){
  rmse <- sqrt(mean((actual-pred)^2, na.rm=TRUE))
  return (rmse)
}  

mape<-function(actual,pred){
  mape <- mean(abs((actual - pred)/actual), na.rm=TRUE)*100
  return (mape)
}

#Adjust the vector of predicted values to align with the sales_actuals vector
Naive_preds <- naive14[-length(naive14)]

mae(sales_actual, Naive_preds)
mse(sales_actual, Naive_preds)
rmse(sales_actual, Naive_preds)
mape(sales_actual, Naive_preds)


#use the simple moving average method to forecast the 14th month of sales

sma14<-SMA (sales_actual, n=3)
sma14

#The last value in the vector is the forecast for sales for the 13th week

#Adjust the vector of predicted values to align with the sales_actuals vector
sales_ma_pred<-c(NA, sma14[-length(sma14)]) 
sales_ma_pred

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(sales_actual, sales_ma_pred)
mse(sales_actual, sales_ma_pred)
rmse(sales_actual, sales_ma_pred)
mape(sales_actual, sales_ma_pred)



#use the exponential smoothing method with alpha = 0.2 to forecast the 
#13th week of milk sales
exp14 <- EMA (sales_actual, n=1, ratio = .2)
exp14

#The last value in the vector is the forecast for sales for the 13th week

#Adjust the vector of predicted values to align with the sales_actuals vector
exp_pred <- c(NA, exp14[-length(exp14)])

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mape(sales_actual, exp_pred)
mae(sales_actual, exp_pred)
mse(sales_actual, exp_pred)
rmse(sales_actual, exp_pred)


#use the exponential smoothing method with alpha = 0.4 to forecast the 
#13th week of milk sales
exp13_4 <- EMA (sales_actual, n=1, ratio = .4)
exp13_4

#The last value in the vector is the forecast for sales for the 13th week

#Adjust the vector of predicted values to align with the sales_actuals vector
exp_pred_4 <- c(NA, exp13_4[-length(exp13_4)])

#Calculate accuracy measures with vector of actual values and vector
#of predicted values as inputs
mae(sales_actual, exp_pred_4)
mse(sales_actual, exp_pred_4)
rmse(sales_actual, exp_pred_4)
mape(sales_actual, exp_pred_4)


#The lowest MAE method is the exponential smoothing mehtod with alpha = .2, resulting
#in it being the best method. We predict the 14th month will be sale of $334,289.4




#Creating visualizations 
    
view(summersales23)
head(summersales23)
str(summersales23)

summary(summersales23$Department)

#Checking for missing values

sum(is.na(summersales23$Qty))
which(is.na(summersales23$Qty))

#NA values is not missing values but an error in the data. We can take them out.

dt <- summersales23 %>%
  drop_na(Qty)

#Removing NA and none columns 
select(summersales23, )

#Creating a scatterplot to compare departments to sales

ggplot(dt, aes(
  x = Category,
  y = Qty
)) +
  geom_point(aes(
    color = 
  ))

#FacetWraps

ggplot(summersales23, aes(
  x = Category,
  y = Qty
)) +
  geom_point(aes(
    color = ,
    shape = 
  )) +
  facet_wrap(~Category)


facet