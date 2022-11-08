
#####Abalone Data#####
if (!require("AppliedPredictiveModeling")) install.packages("AppliedPredictiveModeling")
if (!require("randomForest")) install.packages("randomForest")
if (!require("dplyr")) install.packages("dplyr")
if (!require("caret")) install.packages("caret")
library(caret)
library(randomForest)
library(dplyr)
library(AppliedPredictiveModeling)
data(abalone)
str(abalone)
#Abalone is a type of mollusk found throughout the world.  In this data set, we use
#physical attributes to predict age. From the r-project.org site:

#"The age of abalone is determined by cutting the shell through the cone, staining it, and counting
#the number of rings through a microscope – a boring and time-consuming task. Other measurements, which are
#easier to obtain, are used to predict the age. Further information, such as weather patterns and location
#(hence food availability) may be required to solve the problem.From the original data examples with missing
#values were removed (the majority having the predicted value missing), and the ranges of the continuous values
#have been scaled for use with an ANN (by dividing by 200)."

#Therefore, our objective is to predict rings, and thus age.

head(abalone)
#Here we see that the first thing we need to do is create a set of three dummy variables
#for the "type" predictor which we will convert to factors 

abalone <- abalone %>% mutate(Infant = 0, Male = 0, Female = 0)
abalone$Infant[abalone$Type=="I"] <- 1
abalone$Male[abalone$Type == "M"] <- 1
abalone$Female[abalone$Type == "F"] <- 1
abalone$Male <- factor(abalone$Male)
abalone$Female <- factor(abalone$Female)
abalone$Infant <- factor(abalone$Infant)

a <- abalone %>% dplyr::select(Rings, LongestShell, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight, Infant, Male, Female)
#####Training the Model#####
set.seed(1)
fit <- randomForest(Rings~., data = a)
plot(fit, main = "Trees Required to Stabilize \n Prediction Accuracy")
nTree <- 200
#It looks like we'll need around 200 trees to stabilize prediction accuracy.
#As you run this, please recall that random forest model training is computationally intense.  With our
#mtcars data set , this was not an issue given the size of the data set.  Here, however, with a much
#larger data set, the computation may take a while.  Feel free to use the results described below and
#skip running this code if desired.

set.seed(2)
y <- a$Rings
testIndex <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train <- a[-testIndex,]
test <- a[testIndex,]

train_x <- train[,-1]
train_y <- train$Rings

test_x <- test[,-1]
test_y <- test$Rings

#According to Boehmke and Greenwell
#(https://bradleyboehmke.github.io/HOML/random-forest.html)
#A typical mtry default value for regression is p/3.  We have p=10, which would set our default at 3.33
#just to be sure, we can 'scan' values 1:10
#Similarly, R documentation
#(https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest)
#sets the default nodeSize value for regression at 5, so we 'scan' 1:10 just to be sure
#Now that we've got our nodeSiz range, mtry range, and ntree parameters all set up, we can begin training
#As an aside, even though we've justified reasonable parameter usage here, random forests do take some
#time to train.  Results will be commented below; feel free to read through the rest of this file and run
#the code later to confirm results.

set.seed(3)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1,2,3,4,5,6,7,8,9,10))
nodeSize <- seq(1,10,1)
nodeSizeLength <- (length(nodeSize))
x <- 0
rfRMSE <- sapply(nodeSize, function(ns){
  rf <- train(train_x, train_y,
              method = "rf",
              ntree = nTree,
              nodesize = ns,
              tuneGrid = grid,
              trControl = control)$results["RMSE"]
  
})

df <- c(0,0,0,0,0,0,0,0,0,0)
for(i in 1:nodeSizeLength)
{
  df <- as.data.frame(rbind(df, rfRMSE[[i]]))
  
}
df <- df[-1,]

for(i in 1:nodeSizeLength)
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

k <- which(min(results) == results)

optmtry <- ceiling(k/nodeSizeLength)
optNodeSize <- nodeSizeLength-(optmtry*nodeSizeLength-k)

optmtry
optNodeSize
results[optNodeSize, optmtry]
#Here we see that the lowest RMSE is ~2.143 and is found at mtry = 3 and nodeSize = 8.
#Now we can see how our model does against the held out test set.

testRF <- train(train_x,
                train_y,
                nodesize = optNodeSize,
                tuneGrid = data.frame(mtry = optmtry),
                ntree = nTree)

rfPredictions <- predict(testRF, test_x)

rfRMSE <- sqrt(sum((rfPredictions-test_y)^2)/length(rfPredictions))
rfRMSE

max(abalone$Rings)
min(abalone$Rings)
#We see that we have a test RMSE of ~2.134 for an outcome that ranges from
#1 to 29 with a density plot shown here:
a %>% ggplot(aes(x = Rings))+
  geom_density(fill = "blue", alpha = 0.2)+
  theme_bw()+
  ggtitle("Density Plot of Abalone Rings")
#This test RMSE indicates that we've found a strong random forest model for 
#predicting abalone ring size (and thus age)

