#####Loading Libraries and Introducing Data#####

if (!require("forecast")) install.packages("forecast")
if (!require("rvest")) install.packages("rvest")
if (!require("rapportools")) install.packages("rapportools")


library(rvest)
library(forecast)
library(rapportools)

#To demonstrate forecasting we'll specifically use the PHLX semiconductor stock index (SOX).
#We'll be observing the close price of this index as a time series, and forecasting one month out.
#So that we can test our forecast, the training and testing will be done on prices from August 1, 2019
#through July 31, 2020, and we will try to determine the price as of the end of August, 2020.
#Note that the data set does not contain weekends.

#The Data can be loaded by accessing the .csv file I've loaded into github;  the intent is to make this R file 
#self-contained so the user can simply run the script and observe the output.  However, if there is any trouble
#reading the data in from github, please feel free to directly download the .xlsx or .csv file from my github
#repository.

#Accesing the data via web scraping.

url <-"https://github.com/jai-ranchod/demo_2/blob/dd072b722fc735a87d03a88ee8d63784137f764e/PHLX.csv"

h <- read_html(url)
Nodes <- h %>% html_nodes("table")
table <- html_table(Nodes[[1]])
colnames(table) <- table[1,]
table <- table[-1,]
table <- table[,-1]
PHLX <- table
PHLX$Close <- as.numeric(PHLX$Close)

head(PHLX)

#####Creating Initial Model#####

#Here we define the number of days we want to look ahead (21 business days), and how many days we want to compose the testing set (85 business days ~ 1/3 of the data)
h <- 21
g <- 85

#Attempting to use the ets(), or "Error, Trend, Seasonality", model throws an error with the parameter <model = "AAA"> because the data is non-seasonal.  
vector<-PHLX$Close[is.na(PHLX$Close) == FALSE]
Train<-vector[1:(length(vector)-g)]
Test<-vector[(length(vector)-g+1):length(vector)]
Initial_Model<-ets(Train, model = "AAA", h=g)
Initial_Model_Forecast<-forecast(Initial_Model)


#We could use a different argument, such as <model = "AAN">, but for data with error and trend, we will instead use a Holt model.
#We can fit a rudimentary model in R simply using the holt() function.

options(digits = 6)
vector<-PHLX$Close[is.na(PHLX$Close) == FALSE]

#this is the non-optimized model
No_Opt <- holt(vector, h = h)
forecast(No_Opt)


#Note that the forecasted index price for the end of August is $2190.49.  Checking the Yahoo Finance data for the PHLX index shows that the actual index price as of the end of August 2020 was $2260.43.
#This gives us an error of about 3.10%.

error <- abs(2190.49 - 2260.43)/2260.43
error
#####Optimizing 'Beta' Parameter#####

#This result is not bad, but it turns our we can optimize the trend smoothing parameter "beta" in our model to be even more accurate.  
#We are using the 253 data points prior to August 1, 2020, which we will break down into 168 for training and 85 (variable "g" specified above) for testing.

vector<-PHLX$Close[is.na(PHLX$Close) == FALSE]
Train<-vector[1:(length(vector)-g)]
Test<-vector[(length(vector)-g+1):length(vector)]

#Then we test values of beta between 0 and 1 to see what yields the optimal forecast against our test set.

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

#Here we see that our RMSE is minimized when we use beta = 0.11, indicating that we do not rely entirely on the most recent values; the older values still hold some influence.  

#####Applying Final Forecast#####

Final_Model<-holt(vector, h=h, beta = minimum[[1]])
forecast(Final_Model)


error_2 <- abs(forecast(Final_Model)[[2]][h] - 2260.43)/2260.43
error_2
#Here, we see that after optimizing our model, the predicted index value as of the end of August becomes $2318.33, giving us an error of about 2.56%.

#We can see the accuracy of this prediction in the graphic below.  The actual value is the red dot, the
#forecast is in blue, and 80% and 95% confidence intervals are shaded.

df <- as.data.frame(forecast(Final_Model))

colnames(df)[1] <- "Forecast"
colnames(df)[2] <- "Low_80"
colnames(df)[3] <- "High_80"
colnames(df)[4] <- "Low_95"
colnames(df)[5] <- "High_95"

Forecast_Dates <- PHLX$Forecast_Dates[(is.na(PHLX$Forecast_Dates) == FALSE & is.empty(PHLX$Forecast_Dates) == FALSE)]
df <- cbind(df, Forecast_Dates)

PHLX$Date <- as.Date(PHLX$Date, format = "%m/%d/%y")
df$Forecast_Dates <- as.Date(df$Forecast_Dates, format = "%m/%d/%y")
ggplot()+
  geom_line(aes(x = PHLX$Date, y = PHLX$Close))+
  geom_line(aes(x = df$Forecast_Dates, y = df$Forecast), group = 1, size = 1.5, color = "blue")+
  geom_point(aes(x = as.Date("2020-08-31"), y = 2260), color = "red")+
  geom_ribbon(aes(x = df$Forecast_Dates, ymin = df$Low_95, ymax = df$High_95), alpha = 0.15)+
  geom_ribbon(aes(x = df$Forecast_Dates, ymin = df$Low_80, ymax = df$High_80), alpha = 0.3)+
  scale_y_continuous(labels = scales::dollar, limits = c(0,3000))+
  xlab("Date")+
  ylab("PHLX Index Price")+
  ggtitle("Forecasting PHLX Index Price")


#Note that even with parameter optimization, exponential forecasts are still best applied to short term forecasting scenarios.
