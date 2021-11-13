#####Loading Libraries and Introducing Data#####
#First we're simply going to load in the necessary libraries for our linear modeling and random forest
#analysis
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

#First we'll load in the dataset, then we'll introduce it.
h <- "https://docs.google.com/spreadsheets/d/0BxQfpNgXuWoIWUdZV1ZTc2ZscnM/edit?resourcekey=0-gqXT7Re2eUS2JGt_w1y4vA#gid=1055321634"
t <- read_html(h)
Nodes <- t %>% html_nodes("table")
table <- html_table(Nodes[[1]])
colnames(table) <- table[1,]
table <- table[-1,]
table <- table %>% select(LungCap, Age, Height, Smoke, Gender, Caesarean)
Lung_Capacity <- table

Lung_Capacity$LungCap <- as.numeric(Lung_Capacity$Lung)
Lung_Capacity$Age <- as.numeric(Lung_Capacity$Age)
Lung_Capacity$Height <- as.numeric(Lung_Capacity$Height)
Lung_Capacity$Smoke <- as.numeric(Lung_Capacity$Smoke == "yes")
Lung_Capacity$Gender <- as.numeric(Lung_Capacity$Gender == "male")
Lung_Capacity$Caesarean <- as.numeric(Lung_Capacity$Caesarean == "yes")

colnames(Lung_Capacity)[4] <- "Smoker_YN"
colnames(Lung_Capacity)[5] <- "Male_YN"
colnames(Lung_Capacity)[6] <- "Caesarean_YN"
View(Lung_Capacity)
#This data set evaluates lung capacity for individuals with differing medical and demographic characteristics
#It is pulled from the "Marin Stats" website, found here:
#https://www.statslectures.com/

#LungCap - Lung capacity
#Age - Age of subject
#Height - Height of subject
#Smoker_YN - 1 if subject is smoker, 0 otherwise
#Male_YN - 1 if the subject is male, 0 otherwise
#Caesarean_YN - 1 if the subject was a caesarean birth,  otherwise

min(Lung_Capacity$Age)
max(Lung_Capacity$Age)

#Notice that the age range for this data set is 3-19
#The objective is to build a machine learning model to predict lung capacity based on available predictors.
#We start with a linear model, then move on to a random forest model.

#####Linear Modeling:  Checking Assumptions of Linear Modeling#####
#The first thing we need to do is check for collinearity.  We will do this by first 
#converting the binary predictors to factors, then checking the
#variance inflation factors. The stepVIF() function uses a step-wise approach to feature selection based on 
#variance inflation factors.  The default threshold is 10, which we leave in place here.  For reference
#the variance inflation factor of a predictor i is defined as:
#
#VIF_i = 1/(1-(R_i)^2)
#where (R_i)^2 is the coefficient of determination of the regression of all other predictors onto the ith predictor.

Capacity <- as.data.frame(Lung_Capacity)
Capacity$Smoker_YN <- as.factor(Capacity$Smoker_YN)
Capacity$Male_YN <- as.factor(Capacity$Male_YN)
Capacity$Caesarean_YN <- as.factor(Capacity$Caesarean_YN)

Model <- lm(LungCap ~ Age + Height + Smoker_YN + Male_YN + Caesarean_YN, data = Capacity)
stepVIF(Model)

#All of our predictors meet our threshold and can stay in the model for now. 


#We do not know anything about the data collection process, so we cannot be sure that the samples are genuinely independent.
#However, we will assume for now that they are.  Next, we can generate diagnostic plots with the simple plot() function.

par(mfrow = c(2,2))
plot(Model)
par(mfrow = c(1,1))

#The residuals plot in the upper left-hand corner shows us that we are meeting our linearity and homoscedasticity assumptions.
#The qq plot in the upper right hand corner shows us that we are meeting our normality assumption as well.
#In general, we can feel quite good that we have a useful linear model for predicting the lung capacity of individuals between the ages of 3 and 19.

#####Linear Modeling: Splitting into test set and train set#####
#Now we split the data into a training set and a testing set.
#Before performing cross-validation to determine optimal model parameters, we need to split
#the data into a training set and a test set.  Cross validation will be performed entirely within
#the training set, then the final model will be tested against the test set.
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
#Here we train the model with the train() function that cross-validates the model by partitioning
#the training set into one smaller training set and one validation set, in this case 5 times over
#meaning we are using "5-fold" cross-validation.

set.seed(3)
control <- trainControl(method="cv", number = 5)
LinearModel <- caret::train(LungCap ~., data = train, method = "lm", trControl = control)

summary(LinearModel)


#From the summary of our linear model we can see that we explain 85% of the variance in lung capacity with 
#the features in our model, as we saw above.
#This is a good start, now let's see how we do against our test set.
#####Linear Modeling: Evaluating the linear model#####
#Here we run the model produced by cross-validation against the test set that we held out at
#the very beginning.

lmPredictions <- predict(LinearModel, newdata = test, type = "raw")

lmRMSE <- sqrt(sum((lmPredictions-test_y)^2)/length(lmPredictions))
lmRMSE

max(Capacity$LungCap)
min(Capacity$LungCap)

#Given that we have a range of over 12, an RMSE of about 1 is a strong performance for our linear model.

#####Random Forest: Training the model#####
#Here we try to predict the same outcome on the same data set using a random forest model.
#We will use the same train-test split established in the context of the linear model above.
#We will attempt to optimize both node size and number of trees.  Given that we have only 5 predictors
#we can try 1-10 node size  and 1-5 randomly selected predictors.
#First we need to see how many trees we will need in order to stabilize prediction accuracy.
set.seed(2)

fit <- randomForest(LungCap~., data = Capacity)
plot(fit)

#The fit of the plot indicates we will need about 300 trees to stabilize prediction accuracy
set.seed(4)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1,2,3,4,5))
nodeSize <- seq(1,30,1)
rfRMSE <- sapply(nodeSize, function(ns){
  rf <- train(train_x, train_y,
              method = "rf",
              ntree = 300,
              nodesize = ns,
              tuneGrid = grid,
              trControl = control)$results["RMSE"]
})

df <- c(0,0,0,0,0)
for(i in 1:30)
{
  df <- as.data.frame(rbind(df, rfRMSE[[i]]))
  
}
df <- df[-1,]

for(i in 1:30)
{
  rownames(df)[i] <- i
}

results <- df
colnames(results)[1] <- "One"
colnames(results)[2] <- "Two"
colnames(results)[3] <- "Three"
colnames(results)[4] <- "Four"
colnames(results)[5] <- "Five"
View(results)

which(min(results) == results)

#Here we see that the minimum RMSE is the 78th entry in the table, or row 18, column 3; minimum RMSE ~ 1.107

#####Random Forest: Testing and evaluating the model#####
#In the "results" table we have the columns identified by the number of randomly selected predictors
#(mtry) and the rows identified by node size.  The lowest RMSE we see in this table is 1.077,
#which is found at mtry = 3, node size = 12.  Therefore, these are the parameters
#we will use in the model on the test set
testRF <- train(train_x,
                train_y,
                nodesize = 12,
                tuneGrid = data.frame(mtry = 3),
                ntree = 300)

rfPredictions <- predict(testRF, test_x)

rfRMSE <- sqrt(sum((rfPredictions-test_y)^2)/length(rfPredictions))
rfRMSE

#We see that the random forest RMSE of 1.15 is not as good as the linear model RMSE.

