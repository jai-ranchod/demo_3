#####KNN#####
#We also notice that the ratio of predictors p to observations n is quite high indicating a k-nearest neighbors 
#model may work well.  The p/n ratio is important for this model; in k-nearest neighbors we need to have
#confidence that there will be a sufficient density of observations in p-dimensional space.  Otherwise we
#compare our test data to observations that are not actually very similar.


#####Loading Libraries and Introducing Data#####
#Here we use a logistic regression model as a means of binary classification to predict who will survive and who will not survive the infamous titanic wreck
#of 1912.  We use the built in "titanic_train" data set to train a logistic regression model which we then test against
#a held out test subset of the "titanic_train" data set.

#We plainly have a binary outcome, we can reasonably assume there is independence among the observations, and a brief examination
#of the data will clearly show that we have enough data points to puruse a logistic regression model.

if (!require("titanic")) install.packages("titanic")
if (!require("splines")) install.packages("splines")
if (!require("broom")) install.packages("broom")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("pedometrics")) install.packages("pedometrics")

library(titanic)
library(splines)
library(broom)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pedometrics)
#First we process the titanic_train data set to make it a little more logistic regression friendly
titanic <-  titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare)

#We'll remove NA rows as there aren't very many
titanic <- na.omit(titanic)
#making the sex column binary with male = 1
titanic$sex_binary <- as.integer(titanic$Sex == "male")


#####Normalizing data#####
#It is important in KNN to normalize data so that the range of an attribute does not disproportionately
#impact the distance between observations.

#Let's look into the "fare" attribute first
hist(titanic$Fare, main = "Histogram of Fare", xlab = "Fare", ylab = "Count of Passengers")
#We notice that there appear to be some outliers around the 500 range. And that the distribution is definitely
#not normal.

#Now let's look at the Age predictor
hist(titanic$Age, main = "Histogram of Age", xlab = "Age", ylab = "Count of Passengers")
#This distribution is much easier to handle.  

#However, given the outliers in the "Fare" attribute, we should
#try out the "robust scaler" noramlization method.
#####Robust Normalization#####

#We use the following formula:
# x_new = x-median/(75_quartile-25_quartile)
#This transformation method is robust against outliers, and thus will be helpful given the "Fare" attribute

Age_q25 <- summary(titanic$Age)[[2]]
Age_q50 <- summary(titanic$Age)[[3]]
Age_q75 <- summary(titanic$Age)[[5]]

titanic$AgeNorm <- (titanic$Age - Age_q50)/(Age_q75-Age_q25)
range(titanic$Age)
range(titanic$AgeNorm)
#Here we see that we have significantly reduced the range of the "age" attribute, making it much more conducive
#to KNN.  However, note that we still have a range of ~4.5 in the normalized column.  That means we are still
#exposing ourselves to potentially applying much more weight to the "Age" attribute than to a binary predictor
#like "sex".  To correct that problem, i.e. make sure all attributes are on the same scale, we would do a
#max/min normalization.  We can try both techniques separately, then try both at the same time, and see how it goes.
#We'll complete modeling with the robust scaling mechanism first.
Fare_q25 <- summary(titanic$Fare)[[2]]
Fare_q50 <- summary(titanic$Fare)[[3]]
Fare_q75 <- summary(titanic$Fare)[[5]]

titanic$FareNorm <- (titanic$Fare - Fare_q50)/(Fare_q75 - Fare_q25)
range(titanic$Fare)
range(titanic$FareNorm)

#We see a similar phenomena here.  We've substantially reduced the range of the predictor, but we are still leaving ourselves exposed to 
#applying more weight to some predictors than others in a way that we don't want to.  Let's continue for now, we can return to this issue later.

Pclass_q25 <- summary(titanic$Pclass)[[2]]
Pclass_q50 <- summary(titanic$Pclass)[[3]]
Pclass_q75 <- summary(titanic$Pclass)[[5]]

titanic$ClassNorm <- (titanic$Pclass - Pclass_q50)/(Pclass_q75 - Pclass_q25)

Sib_q25 <- summary(titanic$SibSp)[[2]]
Sib_q50 <- summary(titanic$SibSp)[[3]]
Sib_q75 <- summary(titanic$SibSp)[[5]]

titanic$SibNorm <- (titanic$SibSp - Sib_q50)/(Sib_q75 - Sib_q25)

ParentChild_q25 <- summary(titanic$Parch)[[2]]
ParentChild_q50 <- summary(titanic$Parch)[[3]]
ParentChild_q75 <- summary(titanic$Parch)[[5]]

titanic$ParentChildNorm <- (titanic$Parch - ParentChild_q50)/(ParentChild_q75 - ParentChild_q25)

Sex_q25 <- summary(titanic$sex_binary)[[2]]
Sex_q50 <- summary(titanic$sex_binary)[[3]]
Sex_q75 <- summary(titanic$sex_binary)[[5]]

titanic$SexNorm <- (titanic$sex_binary - Sex_q50)/(Sex_q75 - Sex_q25)

titanicNorm <- titanic %>% select(Survived, ClassNorm, sex_binary, AgeNorm, SibNorm, ParentChildNorm, FareNorm)
for(i in 1:nrow(titanicNorm))
{
  if(titanicNorm$Survived[i] == 1)
  {
    titanicNorm$Survived[i] <- "Survived"
  }
  if(titanicNorm$Survived[i] == 0)
  {
    titanicNorm$Survived[i] <- "Died"
  }
}
titanicNorm$Survived <- factor(titanicNorm$Survived)
#Now that we have a normalized data set, we can split our data into a training set and a hold out test set.
#We will use the training set for cross-validation to determine the optimal parameter k, then see how we do on
#the held out test set.

set.seed(1)
vec <- sample(c(1:nrow(titanicNorm)), (nrow(titanicNorm)/5),replace = FALSE)
testNorm <- titanicNorm[vec,]
trainNorm <- titanicNorm[-vec,]


#We us 10-fold cross validation to find the optimal k value in terms of accuracy
set.seed(2)
control <- trainControl(method = "cv", number = 10, p = .9, classProbs = TRUE)
train_knn <- train(Survived ~ ClassNorm + sex_binary + AgeNorm + SibNorm + ParentChildNorm + FareNorm, method = "knn",
                   data = trainNorm,
                   tuneGrid = data.frame(k = seq(1, 71, 2)),
                   trControl = control,
                   metric = "Kappa") 

ggplot(train_knn, highlight = TRUE)+
  xlab("k (Neighbors)")+
  ylab("Accuracy")+
  ggtitle("Accuracy via cross-training")

train_knn$bestTune
#Here we see that the optimal number of neighbors k is 33 based on 10-fold cross validation.
#Now we can see how our model does on the hold out test set.

knn_robustnorm_fit <- knn3(Survived ~ ., data = trainNorm, k = train_knn$bestTune$k)
y_hat_knn <- predict(knn_robustnorm_fit, newdata = testNorm, type = "class")
y_hat_knn

RobustAccuracy <- confusionMatrix(y_hat_knn, testNorm$Survived)$overall[["Accuracy"]]
RobustKappa <- confusionMatrix(y_hat_knn, testNorm$Survived)$overall[["Kappa"]]
RobustAccuracy
RobustKappa
#####Max/Min Normalization#####
#Now we create a model using a max/min normalization method that will scale each predictor down to some value between 0 and 1
#Using this normalization method we need to be mindful of outliers.
#Let's first re-set our data so as to avoid confusion.
titanic <-  titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare)
titanic <- na.omit(titanic)
titanic$sex_binary <- as.integer(titanic$Sex == "male")

#First, let's look at the "fare" predictor again.
hist(titanic$Fare, main = "Histogram of Fare", xlab = "Fare", ylab = "Count of Passengers")
#Notice the outliers to which this normalization method is sensitive.  One way to correct this is to artificially cap
#the values before normalization.

t <- titanic[which(titanic$Fare > 250),]
t[order(t$Fare),]

#Notice that there are no values between 263 and 512.  If we simply coerce the values of 512 down to 263 prior to normalization
#we will have much less of an issue with outliers.  
for(i in 1:nrow(titanic))
{
  if(titanic$Fare[i] > 263)
  {
    titanic$Fare[i] <- 263
  }else{
    titanic$Fare[i] <- titanic$Fare[i]
  }
}
#Given the smaller original ranges of the other predictors and the distribution of the age predictor we saw earlier, we 
#can move forward with direct transformation of the rest of the data set.
titanic$ClassNorm <- (titanic$Pclass - mean(titanic$Pclass))/(max(titanic$Pclass) - min(titanic$Pclass))
titanic$AgeNorm <- (titanic$Age - mean(titanic$Age))/(max(titanic$Age) - min(titanic$Age))
titanic$SibNorm <- (titanic$SibSp - mean(titanic$SibSp))/(max(titanic$SibSp) - min(titanic$SibSp))
titanic$ParentChildNorm <- (titanic$Parch - mean(titanic$Parch))/(max(titanic$Parch) - min(titanic$SibSp))
titanic$FareNorm <- (titanic$Fare - mean(titanic$Fare))/(max(titanic$Fare) - min(titanic$Fare))
titanic$SexNorm <- (titanic$sex_binary - mean(titanic$Fare))/(max(titanic$sex_binary) - min(titanic$sex_binary))


titanicNorm <- titanic %>% select(Survived, ClassNorm, sex_binary, AgeNorm, SibNorm, ParentChildNorm, FareNorm)
for(i in 1:nrow(titanicNorm))
{
  if(titanicNorm$Survived[i] == 1)
  {
    titanicNorm$Survived[i] <- "Survived"
  }
  if(titanicNorm$Survived[i] == 0)
  {
    titanicNorm$Survived[i] <- "Died"
  }
}
titanicNorm$Survived <- factor(titanicNorm$Survived)
#Againt, we split our data into a training set and a hold out test set and use the training set for 
#cross-validation to determine the optimal parameter k, then see how we do on the held out test set.


set.seed(3)
vec <- sample(c(1:nrow(titanicNorm)), (nrow(titanicNorm)/5),replace = FALSE)
testNorm <- titanicNorm[vec,]
trainNorm <- titanicNorm[-vec,]


#We us 10-fold cross validation to find the optimal k value in terms of accuracy
set.seed(4)
control <- trainControl(method = "cv", number = 10, p = .9, classProbs = TRUE)
train_knn <- train(Survived ~ ClassNorm + sex_binary + AgeNorm + SibNorm + ParentChildNorm + FareNorm, method = "knn",
                   data = trainNorm,
                   tuneGrid = data.frame(k = seq(1, 71, 2)),
                   trControl = control,
                   metric = "Kappa") 

ggplot(train_knn, highlight = TRUE)+
  xlab("k (Neighbors)")+
  ylab("Accuracy")+
  ggtitle("Accuracy via cross-training")

train_knn$bestTune
#Interestingly we get a significantly different optimal k-value using max/min normalization.
#With robust normalization we saw k=33, here we see k=3.

knn_maxmin_fit <- knn3(Survived ~ ., data = trainNorm, k = train_knn$bestTune$k)
y_hat_knn <- predict(knn_maxmin_fit, newdata = testNorm, type = "class")
y_hat_knn

MaxMinAccuracy <- confusionMatrix(y_hat_knn, testNorm$Survived)$overall[["Accuracy"]]
MaxMinKappa <- confusionMatrix(y_hat_knn, testNorm$Survived)$overall[["Kappa"]]
MaxMinAccuracy
MaxMinKappa

#We see a significant increase in both accuracy and kappa using the max/min normalization method.  This is not
#unexpected since the max/min method coerces all predictors to the same scale, whereas the robust normalization method
#does not.  We do notice some outliers in the "fare" predictor, but evidently these outliers are not enough to make
#the robust normalization method more effective than the max/min method.
#Now let's try both methods simultaneously.  That is, we'll transform via the robust methodology first, then from there
#coerce everything to the same scale via the max/min method.
#####Ensemble Normalization#####
titanic <-  titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare)
titanic <- na.omit(titanic)
titanic$sex_binary <- as.integer(titanic$Sex == "male")

Age_q25 <- summary(titanic$Age)[[2]]
Age_q50 <- summary(titanic$Age)[[3]]
Age_q75 <- summary(titanic$Age)[[5]]

titanic$AgeNorm <- (titanic$Age - Age_q50)/(Age_q75-Age_q25)
range(titanic$Age)
range(titanic$AgeNorm)
#Here we see that we have significantly reduced the range of the "age" attribute, making it much more conducive
#to KNN.  However, note that we still have a range of ~4.5 in the normalized column.  That means we are still
#exposing ourselves to potentially applying much more weight to the "Age" attribute than to a binary predictor
#like "sex".  To correct that problem, i.e. make sure all attributes are on the same scale, we would do a
#max/min normalization.  We can try both techniques separately, then try both at the same time, and see how it goes.
#We'll complete modeling with the robust scaling mechanism first.
Fare_q25 <- summary(titanic$Fare)[[2]]
Fare_q50 <- summary(titanic$Fare)[[3]]
Fare_q75 <- summary(titanic$Fare)[[5]]

titanic$FareNorm <- (titanic$Fare - Fare_q50)/(Fare_q75 - Fare_q25)
range(titanic$Fare)
range(titanic$FareNorm)

#We see a similar phenomena here.  We've substantially reduced the range of the predictor, but we are still leaving ourselves exposed to 
#applying more weight to some predictors than others in a way that we don't want to.  Let's continue for now, we can return to this issue later.

Pclass_q25 <- summary(titanic$Pclass)[[2]]
Pclass_q50 <- summary(titanic$Pclass)[[3]]
Pclass_q75 <- summary(titanic$Pclass)[[5]]

titanic$ClassNorm <- (titanic$Pclass - Pclass_q50)/(Pclass_q75 - Pclass_q25)

Sib_q25 <- summary(titanic$SibSp)[[2]]
Sib_q50 <- summary(titanic$SibSp)[[3]]
Sib_q75 <- summary(titanic$SibSp)[[5]]

titanic$SibNorm <- (titanic$SibSp - Sib_q50)/(Sib_q75 - Sib_q25)

ParentChild_q25 <- summary(titanic$Parch)[[2]]
ParentChild_q50 <- summary(titanic$Parch)[[3]]
ParentChild_q75 <- summary(titanic$Parch)[[5]]

titanic$ParentChildNorm <- (titanic$Parch - ParentChild_q50)/(ParentChild_q75 - ParentChild_q25)

Sex_q25 <- summary(titanic$sex_binary)[[2]]
Sex_q50 <- summary(titanic$sex_binary)[[3]]
Sex_q75 <- summary(titanic$sex_binary)[[5]]

titanic$SexNorm <- (titanic$sex_binary - Sex_q50)/(Sex_q75 - Sex_q25)

titanicNorm1 <- titanic %>% select(Survived, ClassNorm, sex_binary, AgeNorm, SibNorm, ParentChildNorm, FareNorm)
for(i in 1:nrow(titanicNorm1))
{
  if(titanicNorm1$Survived[i] == 1)
  {
    titanicNorm1$Survived[i] <- "Survived"
  }
  if(titanicNorm1$Survived[i] == 0)
  {
    titanicNorm1$Survived[i] <- "Died"
  }
}
titanicNorm1$Survived <- factor(titanicNorm1$Survived)

#For the sake of staying true to our intent of combining the previously used techniques, we will repeat our treatment
#of outliers in the "Fare" predictor

hist(titanicNorm1$Fare, main = "Histogram of Fare", xlab = "Fare", ylab = "Count of Passengers")
t <- titanicNorm1[which(titanicNorm1$Fare > 8),]
t[order(t$Fare),]

#Notice the gap this time appears to be between 9.763 abd 19.6
for(i in 1:nrow(titanicNorm1))
{
  if(titanicNorm1$FareNorm[i] > 9.763408)
  {
    titanicNorm1$FareNorm[i] <- 9.763408
  }else{
    titanicNorm1$FareNorm[i] <- titanicNorm$FareNorm[i]
  }
}

titanicNorm1$ClassNorm <- (titanicNorm1$ClassNorm - mean(titanicNorm1$ClassNorm))/(max(titanicNorm1$ClassNorm) - min(titanicNorm1$ClassNorm))
titanicNorm1$AgeNorm <- (titanicNorm1$AgeNorm - mean(titanicNorm1$AgeNorm))/(max(titanicNorm1$AgeNorm) - min(titanicNorm1$AgeNorm))
titanicNorm1$SibNorm <- (titanicNorm1$SibNorm - mean(titanicNorm1$SibNorm))/(max(titanicNorm1$SibNorm) - min(titanicNorm1$SibNorm))
titanicNorm1$ParentChildNorm <- (titanicNorm1$ParentChildNorm - mean(titanicNorm1$ParentChildNorm))/(max(titanicNorm1$ParentChildNorm) - min(titanicNorm1$ParentChildNorm))
titanicNorm1$FareNorm <- (titanicNorm1$FareNorm - mean(titanicNorm1$FareNorm))/(max(titanicNorm1$FareNorm) - min(titanicNorm1$FareNorm))
titanicNorm1$SexNorm <- (titanicNorm1$sex_binary - mean(titanicNorm1$sex_binary))/(max(titanicNorm1$sex_binary) - min(titanicNorm1$sex_binary))

titanicNorm1 <- titanicNorm1 %>% select(Survived, ClassNorm, AgeNorm, SibNorm, ParentChildNorm, FareNorm, SexNorm)


set.seed(5)
vec <- sample(c(1:nrow(titanicNorm1)), (nrow(titanicNorm1)/5),replace = FALSE)
testNorm <- titanicNorm1[vec,]
trainNorm <- titanicNorm1[-vec,]


#We us 10-fold cross validation to find the optimal k value in terms of accuracy
set.seed(6)
control <- trainControl(method = "cv", number = 10, p = .9, classProbs = TRUE)
train_knn <- train(Survived ~ ClassNorm + SexNorm + AgeNorm + SibNorm + ParentChildNorm + FareNorm, method = "knn",
                   data = trainNorm,
                   tuneGrid = data.frame(k = seq(1, 71, 2)),
                   trControl = control,
                   metric = "Kappa") 

ggplot(train_knn, highlight = TRUE)+
  xlab("k (Neighbors)")+
  ylab("Accuracy")+
  ggtitle("Accuracy via cross-training")

train_knn$bestTune
#Interestingly we get a significantly different optimal k-value using max/min normalization.
#With robust normalization we saw k=33, here we see k=3.

knn_ensemble_fit <- knn3(Survived ~ ., data = trainNorm, k = train_knn$bestTune$k)
y_hat_knn <- predict(knn_ensemble_fit, newdata = testNorm, type = "class")
y_hat_knn

EnsembleAccuracy <- confusionMatrix(y_hat_knn, testNorm$Survived)$overall[["Accuracy"]]
EnsembleKappa <- confusionMatrix(y_hat_knn, testNorm$Survived)$overall[["Kappa"]]
EnsembleAccuracy
EnsembleKappa

#Notice that kapp is and accuracy are lower than with the max/min method.  This indicates that attempting to normalize
#the data in multiple ways represents too much pre-processing of the data to the point where the Euclidean distance
#between the observations is warped.  Honestly, I've never seen anyone try this before and it's not surprsing that this
#happened, but I had some extra time on my hands and thought it might be fun to try.
