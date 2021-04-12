
#To demonstrate forecasting we'll specifically use the PHLX semiconductor stock index (SOX). We'll be observing the close price of this index as a time series, and forecasting one month out.
#So that we can test our forecast, the training and testing will be done on prices from August 1, 2019 through July 31, 2020, and we will try to determine the price as of the end of August, 2020.
#Note that the dataset does not contain weekends.

library(readxl)
PHLX <- read_excel("~/MBA Classes/Summer Term/Corporate Strategic Management/Final Stuff/Group Case Study/PHLX.xlsx")

head(PHLX)

#defining the number of days we want to look ahead (21 business days), and how many days we want to compose the testing set (85 business days ~ 1/3 of the data)
h <- 21
g <- 85

#Attempting to use the ets(), or "Error, Trend, Seasonality" model throws an error with the parameter <model = "AAA"> because the data is non-seasonal.  
vector<-PHLX$Close[is.na(PHLX$Close) == FALSE]
Train<-vector[1:(length(vector)-g)]
Test<-vector[(length(vector)-g+1):length(vector)] #define these first to get the accuracy of the trained forecast against the test set
Initial_Model<-ets(Train, model = "AAA", h=g)
Initial_Model_Forecast<-forecast(Initial_Model)


#We could use a different argument, such as <model = "AAN">, but for data with error and trend, we will instead use a Holt model.  We can fit a rudimentary model in R simply using the holt() function.

vector<-PHLX$Close[is.na(PHLX$Close) == FALSE]

#this is the non-optimized model
No_Opt <- holt(vector, h = h)
forecast(No_Opt)


#Note that the forecasted index price for the end of August is $2190.491.  Checking the Yahoo Finance data for the PHLX index shows that the actual index price as of the end of August 2020 was $2260.43.  This gives us an error of about 3.10%.

error <- abs(2190.49 - 2260.43)/2260.43
error

#This result is not bad, but it turns our we can optimize the trend smoothing parameter "beta" in our model to be even more accurate.  
#We are using the 253 data points prior to August 1, 2020, which we break down into 168 for training and 85 (variable "g" specified above) for testing. First we break our data into a training set and a testing split.


vector<-PHLX$Close[is.na(PHLX$Close) == FALSE]
Train<-vector[1:(length(vector)-g)]
Test<-vector[(length(vector)-g+1):length(vector)] #define these first to get the accuracy of the trained forecast against the test set


#Then we test value of beta between 0 and 1 to see what yields the optimal forecast against our test set.

param<-seq(0.01, 0.99, by=0.01)
RMSE<-NA
for (i in seq_along(param))
{
  fit<-holt(Train, beta = param[i], h=g, damped = FALSE, exponential = FALSE) #typically you are optimizing the "latest" parameter available
  future<-forecast(fit)
  RMSE[i] = accuracy(future, Test)[2,2]
}
error<-tibble(param, RMSE)
minimum<-filter(error, RMSE==min(RMSE))
ggplot(error, aes(param, RMSE))+
  geom_line()+
  geom_point(data = minimum, color = "blue", size = 2)+
  xlab("Beta")+
  ylab("RMSE")+
  ggtitle("Optimizing Parameter Beta")
print(minimum)

#Here we see that our RMSE is minimized when we use beta = 0.11, indicating that we do not rely entirely on the most recent values, the older values still hold some weight.  

Final_Model<-holt(vector, h=h, beta = minimum[[1]])
forecast(Final_Model)

#Here we see that after optimizing our model, the predicted index value as of the end of August becomes $22381.33, giving us an error of about 2.56%.

error_2 <- abs(2318.33 - 2260.43)/2260.43
error_2


#We can see that we have a fairly accurate prediction in the graphic below.

df <- as.data.frame(forecast(Final_Model))

colnames(df)[1] <- "Forecast"
colnames(df)[2] <- "Low_80"
colnames(df)[3] <- "High_80"
colnames(df)[4] <- "Low_95"
colnames(df)[5] <- "High_95"

Forecast_Dates <- PHLX$Forecast_Dates[is.na(PHLX$Forecast_Dates) == FALSE]
df <- cbind(df, Forecast_Dates)
ggplot()+
  geom_line(aes(x = PHLX$Date, y = PHLX$Close), group = 1)+
  geom_line(aes(x = df$Forecast_Dates, y = df$Forecast), group = 1, size = 1.5, color = "blue")+
  geom_point(aes(x = as.POSIXct("2020-08-31"), y = 2260), color = "red")+
  #xlim(c(as.POSIXct("2020-01-01"), as.POSIXct("2020-08-31")))+
  geom_ribbon(aes(x = df$Forecast_Dates, ymin = df$Low_95, ymax = df$High_95), alpha = 0.15)+
  geom_ribbon(aes(x = df$Forecast_Dates, ymin = df$Low_80, ymax = df$High_80), alpha = 0.3)+
  scale_y_continuous(labels = scales::dollar, limits = c(0,3000))+
  xlab("Date")+
  ylab("PHLX Index Price")+
  ggtitle("Forecasting PHLX Index Price")


#Note that even with parameter optimization, exponential forecasts are still best applied to short term forecasting scenarios.
