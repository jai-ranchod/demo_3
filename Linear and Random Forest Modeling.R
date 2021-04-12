#####Loading Required Libraries#####

if (!require("pedometrics")) install.packages("pedometrics")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")
if (!require("readxl")) install.packages("readxl")
if (!require("corrplot")) install.packages("corrplot")
library(pedometrics)
library(dplyr)
library(caret)
library(randomForest)
library(readxl)
library(corrplot)


#####Data Set Introduction and Objective#####
d<-"https://docs.google.com/spreadsheets/d/0BxQfpNgXuWoIWUdZV1ZTc2ZscnM/export?format=csv"
Lung_Capacity<-read.csv(d)

Lung_Capacity$Smoke <- as.numeric(Lung_Capacity$Smoke == "yes")
Lung_Capacity$Gender <- as.numeric(Lung_Capacity$Gender == "male")
Lung_Capacity$Caesarean <- as.numeric(Lung_Capacity$Caesarean == "yes")

colnames(Lung_Capacity)[4] <- "Smoker_YN"
colnames(Lung_Capacity)[5] <- "Male_YN"
colnames(Lung_Capacity)[6] <- "Caesarean_YN"
View(Lung_Capacity)
#This data set evaluates lung capacity for individuals with differing medical and demographic characteristics

#LungCap - Lung capacity
#Age - Age of subject
#Height - Height of subject
#Smoker_YN - 1 if subject is smoker, 0 otherwise
#Male_YN - 1 if the subject is male, 0 otherwise
#Caesarean_YN - 1 if the subject was a caesarean birth,  otherwise

max(Lung_Capacity$Age)
min(Lung_Capacity$Age)
#Notice that the age range for this data set is 3-19
#The objective is to build a machine learning model to predict lung capacity based on available predictors.
#We start with a linear model, then move on to a random forest model.

#####Linear Modeling:  Checking Assumptions of Linear Modeling#####
Capacity <- as.data.frame(Lung_Capacity)
Capacity$Smoker_YN <- as.factor(Capacity$Smoker_YN)
Capacity$Male_YN <- as.factor(Capacity$Male_YN)
Capacity$Caesarean_YN <- as.factor(Capacity$Caesarean_YN)

Model <- lm(LungCap ~ Age + Height + Smoker_YN + Male_YN + Caesarean_YN, data = Capacity)
stepVIF(Model)

Model <- stepVIF(Model)

#All of our predictors meet our threshold and can stay in the model for now.  Next we see what the model looks like.

summary(Model)

#Here we see that we are describing approximately 85% of the variation in lung capacity with our predictors.  We further see that we have a highly statistically significant F-statistic.
#Finally, we note that all of our predictors have a p-value less than 0.05, indicating they are all statistically significant.  If this were not the case, we would begin the "backward selection" process by removing the predictor with the highest p-value and re-running the model until all predictors were below the significance threshold.
#Unsurprisingly we note that being a smoker reduces lung capacity.  We note as well that age increases lung capacity which may seem counterintuitive, until we see that the maximum age in this dataset is 19.

min(Lung_Capacity$Age)
max(Lung_Capacity$Age)

#We finally must make sure we are meeting the assumptions of linear modeling.  We do not know anything about the data collection process, so we cannot be sure that the samples are genuinely independent.
#However, we will assume for now that they are.  Next, we can generate diagnostic plots with the simple plot() function.

par(mfrow = c(2,2))
plot(Model)
par(mfrow = c(1,1))

#The residuals plot in the upper left-hand corner shows us that we are meeting our linearity and homoscedasticity assumptions.
#The qq plot in the upper right hand corner shows us that we are meeting our normality assumption as well.
#In general, we can feel quite good that we have a useful linear model for predicting the lung capacity of individuals between the ages of 3 and 19.

#####Linear Modeling: Splitting into test set and train set#####
#Now we split the data into a training set and a testing set.
set.seed(1)
y <- Capacity$LungCap
testIndex <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train <- Capacity[-testIndex,]
test <- Capacity[testIndex,]

train_x <- train[,-1]
train_y <- train$LungCap

test_x <- test[,-1]
test_y <- test$LungCap
#####Linear Modeling: Training a linear model#####
set.seed(3)
control <- trainControl(method="cv", number = 5)
LinearModel <- train(train_x, train_y, method = "lm", trControl = control)

summary(LinearModel)
#Notice that age seems to increase predicted lung capacity.  This may seem counterintuitive until we
#recall that the ages of the participants span from 3 to 19.
#From the summary of our linear model we can see that we explain 86% of the variance in lung capacity with 
#the features in our model.  This is a good start, now let's see how we do against our test set.
#####Linear Modeling: Evaluating the linear model#####

lmPredictions <- predict(LinearModel, test_x)

lmRMSE <- sqrt(sum((lmPredictions-test_y)^2)/length(lmPredictions))
lmRMSE

max(Capacity$LungCap)
min(Capacity$LungCap)

#Given that we have a range of over 14, an RMSE of slightly uner 1 is a strong performance for our linear model.

#####Random Forest: Training the model#####
#We will use the same train-test split established in the context of the linear model above.
#We will attempt to optimize both node size and number of trees.  Given that we have only 5 predictors
#we can try 1-10 node size  and 1-5 randomly selected
#predictors
set.seed(2)

fit <- randomForest(LungCap~., data = Capacity)
plot(fit)

#The fit of the plot indicates we will need about 250 trees to stabilize prediction accuracy
set.seed(4)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1,2,3,4,5))
nodeSize <- seq(1,30,1)
rfRMSE <- sapply(nodeSize, function(ns){
  rf <- train(train_x, train_y,
              method = "rf",
              ntree = 250,
              nodesize = ns,
              tuneGrid = grid,
              trControl = control)$results["RMSE"]
})

results <- as.data.frame(rbind(rfRMSE[[1]], rfRMSE[[2]], rfRMSE[[3]], rfRMSE[[4]], rfRMSE[[5]], rfRMSE[[6]], rfRMSE[[7]], rfRMSE[[8]], rfRMSE[[9]], rfRMSE[[10]], rfRMSE[[11]], rfRMSE[[12]], rfRMSE[[13]], rfRMSE[[14]], rfRMSE[[15]], rfRMSE[[16]], rfRMSE[[17]], rfRMSE[[18]], rfRMSE[[19]], rfRMSE[[20]], rfRMSE[[21]], rfRMSE[[22]], rfRMSE[[23]], rfRMSE[[24]], rfRMSE[[25]], rfRMSE[[26]], rfRMSE[[27]], rfRMSE[[28]], rfRMSE[[29]], rfRMSE[[30]]))
colnames(results)[1] <- "One"
colnames(results)[2] <- "Two"
colnames(results)[3] <- "Three"
colnames(results)[4] <- "Four"
colnames(results)[5] <- "Five"
View(results)

which(min(results) == results)
#####Random Forest: Testing and evaluating the model#####
#In the "results" table we have the columns identified by the number of randomly selected predictors
#(mtry) and the rows identified by node size.  The lowest RMSE we see in this table is 1.067,
#which is found at mtry = 3, node size = 23.  Therefore, these are the parameters
#we will use in the model on the test set
testRF <- train(train_x,
                train_y,
                nodesize = 23,
                tuneGrid = data.frame(mtry = 3),
                ntree = 250)

rfPredictions <- predict(testRF, test_x)

rfRMSE <- sqrt(sum((rfPredictions-test_y)^2)/length(rfPredictions))
rfRMSE
#We see that the random forest RMSE of 0.9897 is comparable to the linear model RMSE but slightly lower.

varImp(testRF)
#Notice that the feature importance almost exactly lines up with the predictor p-values in the linear model.
#You can see this by running:
summary(linearModel)
#again.
#####Ensemble Prediction#####

#These are good predictions, and give us confidence in our ability to predict lung capacity on an 
#out-of-sample set, but we might be able to do better with an ensemble technique.  We do this by averaging
#the predicted lung capacities between the linear model and the random forest.  This average then serves as 
#our prediction.

ensemblePredictions <- (rfPredictions + lmPredictions)/2
ensembleRMSE <- sqrt(sum((ensemblePredictions-test_y)^2)/length(ensemblePredictions))
ensembleRMSE

(rfRMSE - ensembleRMSE)/(rfRMSE)

(lmRMSE - ensembleRMSE)/(lmRMSE)

#With an ensemble prediction, we decrease RMSE by about 0.5% over the random forest model and about 1.5% over
#the linear model.
#In the end, we can be confident that if the patterns in the data continue to hold, we can accurately predict
#the lung capacity of individuals aged 3-19 years old.

