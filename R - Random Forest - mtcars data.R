#####Introducing Data#####
if (!require("car")) install.packages("car")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
if (!require("randomForest")) install.packages("randomForest")
library(caret)
library(dplyr)
library(car)
library(randomForest)

#Here we try to predict the gas mileage of various cars using the mtcars data set that is built into R

data("mtcars")
str(mtcars)

#Clear definition of the attributes in the mtcars data set can be found here:
#https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html
#But we will also show a brief introduction here:
#mpg	Miles/(US) gallon
#cyl	Number of cylinders
#disp	Displacement (cu.in.)
#hp	Gross horsepower
#drat	Rear axle ratio
#wt	Weight (1000 lbs)
#qsec	1/4 mile time
#vs	Engine (0 = V-shaped, 1 = straight)
#am	Transmission (0 = automatic, 1 = manual)
#gear	Number of forward gears
#carb	Number of carburetors

#Reviewing the data, we see that all but two of our predictors should have the numeric data type.
#"vs" and "am" would be more appropriate as factors.

mtcars$vs <- factor(mtcars$vs)
mtcars$am <- factor(mtcars$am)
#####Training the Model#####
#Here we try to predict the mpg outcome  using a random forest model.
#We will use the same train-test split established in the context of the linear model.
#We will attempt to optimize both node size and number of trees.  Given that we have 10 predictors
#we can try 1-30 node size  and 1-10 randomly selected predictors.
#First we need to see how many trees we will need in order to stabilize prediction accuracy.
set.seed(1)

fit <- randomForest(mpg~., data = mtcars)
plot(fit, main = "Trees Required to Stabilize \n  Prediction Accuracy")

#It looks like we'll need around 200 trees to stabilize prediction accuracy.
set.seed(2)
y <- mtcars$mpg
testIndex <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train <- mtcars[-testIndex,]
test <- mtcars[testIndex,]

train_x <- train[,-1]
train_y <- train$mpg

test_x <- test[,-1]
test_y <- test$mpg



set.seed(3)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1,2,3,4,5,6,7,8,9,10))
nodeSize <- seq(1,30,1)
rfRMSE <- sapply(nodeSize, function(ns){
  rf <- train(train_x, train_y,
              method = "rf",
              ntree = 200,
              nodesize = ns,
              tuneGrid = grid,
              trControl = control)$results["RMSE"]
})

df <- c(0,0,0,0,0,0,0,0,0,0)
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
colnames(results)[6] <- "Six"
colnames(results)[7] <- "Seven"
colnames(results)[8] <- "Eight"
colnames(results)[9] <- "Nine"
colnames(results)[10] <- "Ten"
View(results)

which(min(results) == results)

#We've found that the lowest RMSE is found at the 31st entry of the data table,
#or (since the columns are 30 entries long) the first entry of the second second
#column, where we find RMSE ~ 2.197
#####Testing the model#####

#In the "results" table we have the columns identified by the number of randomly selected predictors
#(mtry) and the rows identified by node size.  The lowest RMSE we see in this table is ~2.197,
#which is found at mtry = 2, node size = 1.  Therefore, these are the parameters
#we will use in the model on the test set
testRF <- train(train_x,
                train_y,
                nodesize = 1,
                tuneGrid = data.frame(mtry = 2),
                ntree = 200)

rfPredictions <- predict(testRF, test_x)

rfRMSE <- sqrt(sum((rfPredictions-test_y)^2)/length(rfPredictions))
rfRMSE
#Here we find the test RMSE to be ~2.24.








