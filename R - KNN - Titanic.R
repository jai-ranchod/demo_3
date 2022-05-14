#####KNN#####
#Wehn observing the Titanic data set, we notice that the ratio of predictors p to observations n is quite high indicating a k-nearest neighbors 
#model may work well.  The p/n ratio is important for this model; in k-nearest neighbors we need to have
#confidence that there will be a sufficient density of observations in p-dimensional space.  Otherwise we
#compare our test data to observations that are not actually very similar.


#####Loading Libraries and Introducing Data#####
#Here we are interested in solving a classification problem to predict who will survive and who will not survive the infamous titanic wreck
#of 1912.  We use the built in "titanic_train" data set to train a k-nearest neighbors model which we then test against
#a held out test subset of the "titanic_train" data set.

if (!require("titanic")) install.packages("titanic")
if (!require("splines")) install.packages("splines")
if (!require("broom")) install.packages("broom")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("pedometrics")) install.packages("pedometrics")
if (!require("corrplot")) install.packages("corrplot")
if (!require("caret")) install.packages("caret")


library(titanic)
library(splines)
library(broom)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pedometrics)
library(corrplot)
library(caret)
#First we process the titanic_train data set
titanic <-  titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare)
#We keep the rows that we have reason to believe may inform the survival status of the passenger.  This means excluding
#cabin, port of embarkment, ticket number, and name.  Cabin and port of embarkment could feasibly carry some information about 
#the socio-economic standing of the passenger, but certainly not more than is held in the fare or class predictors.
sum(is.na(titanic$Survived))
sum(is.na(titanic$Pclass))
sum(is.na(titanic$Sex))
sum(is.na(titanic$Age))
sum(is.na(titanic$SibSp))
sum(is.na(titanic$Parch))
sum(is.na(titanic$Fare))

#Examination of the data reveals that we have NA values, and that all of our NA fields are in the age column.


#In this particular case, it makes sense to focus on the data where we have complete observations, as explained by this
#excerpt from mastersindatascience.org:

#"The imputation method develops reasonable guesses for missing data. It’s most useful when the percentage of missing data is low.
#If the portion of missing data is too high, the results lack natural variation that could result in an effective model.
#The other option is to remove data. When dealing with data that is missing at random, related data can be deleted to reduce bias.
#Removing data may not be the best option if there are not enough observations to result in a reliable analysis."

#https://www.mastersindatascience.org/learning/how-to-deal-with-missing-data/

#We still have plenty of data with which to perform useful analysis.  We have NA values in about 20% of the data set, 
#which is far greater than the 5% recommended threshold for imputation.

titanic <- na.omit(titanic)
#making the sex column binary with male = 1
titanic$sex_binary <- as.integer(titanic$Sex == "male")

#We also want to make sure the we have a set of predictors that are independent of each other.  The first thing we can do is create a correlation 
#matrix to analyze how closely aligned some of these predictors may be.
titanic_corr <- titanic[,c(1,2,4,5,6,7,8)]
Matrix <- cor(titanic_corr)
corrplot(Matrix)

#We notice two reasonably strong relationships here.  The first is between sex and survival.  This is not surprising, nor will it 
#present a problem for our model.  The second is between Fare and Class.  This also makes sense given that higher class tickets cost more.
#Let's focus on this and see exactly how much of a correlation is present.

cor(titanic$Fare, titanic$Pclass)

#The correlation has a magnitude of about 0.55 which is actually not as much of an issue as one may have thought initially.  It's difficult to tell
#what to do here.  Clearly a higher class ticket will generally cost more money, indicating we should remove the Fare predictor since class is ultimately
#what would be more indicative of survival.  However, it is possible that within classes there would be more or less desirable cabins that would have different
#prices and also influence survival status, indicating we should keep the Fare predictor in the model.  Let's keep the Fare predictor active for now, and we can experiment
#with it later.

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
t <- t %>% select(Survived, Pclass, sex_binary, Age, SibSp, Parch, Fare)
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
titanic$ClassNorm <- (titanic$Pclass - min(titanic$Pclass))/(max(titanic$Pclass) - min(titanic$Pclass))
titanic$AgeNorm <- (titanic$Age - min(titanic$Age))/(max(titanic$Age) - min(titanic$Age))
titanic$SibNorm <- (titanic$SibSp - min(titanic$SibSp))/(max(titanic$SibSp) - min(titanic$SibSp))
titanic$ParentChildNorm <- (titanic$Parch - min(titanic$Parch))/(max(titanic$Parch) - min(titanic$Parch))
titanic$FareNorm <- (titanic$Fare - min(titanic$Fare))/(max(titanic$Fare) - min(titanic$Fare))
titanic$SexNorm <- (titanic$sex_binary - min(titanic$sex_binary))/(max(titanic$sex_binary) - min(titanic$sex_binary))


titanicNorm <- titanic %>% select(Survived, ClassNorm, SexNorm, AgeNorm, SibNorm, ParentChildNorm, FareNorm)
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
#Again, we split our data into a training set and a hold out test set and use the training set for 
#cross-validation to determine the optimal parameter k, then see how we do on the held out test set.


set.seed(3)
vec <- sample(c(1:nrow(titanicNorm)), (nrow(titanicNorm)/5),replace = FALSE)
testNorm <- titanicNorm[vec,]
trainNorm <- titanicNorm[-vec,]


#We us 10-fold cross validation to find the optimal k value in terms of accuracy
set.seed(4)
control <- trainControl(method = "cv", number = 10, p = .9, classProbs = TRUE)
train_knn <- train(Survived ~ ClassNorm + SexNorm + AgeNorm + SibNorm + ParentChildNorm + FareNorm, method = "knn",
                   data = trainNorm,
                   tuneGrid = data.frame(k = seq(1, 71, 2)),
                   trControl = control,
                   metric = "Kappa") 

ggplot(train_knn, highlight = TRUE)+
  xlab("k (Neighbors)")+
  ylab("Kappa")+
  ggtitle("Kappa - Max/Min Normalization")

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

#####Experimenting with different predictor subsets#####

#Now let's return to the question of what to do about the correlation between the Fare and Class predictors.  Let's see if removing the Fare
#predictor or merging the two yields a higher test accuracy.  First let's try simply removing the Fare predictor.

titanicNorm_test1 <- titanicNorm[,c(1:6)]
set.seed(5)
vec <- sample(c(1:nrow(titanicNorm_test1)), (nrow(titanicNorm_test1)/5),replace = FALSE)
testNorm1 <- titanicNorm_test1[vec,]
trainNorm1 <- titanicNorm_test1[-vec,]



set.seed(6)
control <- trainControl(method = "cv", number = 10, p = .9, classProbs = TRUE)
train_knn1 <- train(Survived ~ ClassNorm + SexNorm + AgeNorm + SibNorm + ParentChildNorm, method = "knn",
                   data = trainNorm1,
                   tuneGrid = data.frame(k = seq(1, 71, 2)),
                   trControl = control,
                   metric = "Kappa") 

ggplot(train_knn1, highlight = TRUE)+
  xlab("k (Neighbors)")+
  ylab("Kappa")+
  ggtitle("Kappa - Max/Min Normalization")

train_knn1$bestTune
#Again, we get an interesting result in regards to optimal k.  The previous max/min test had k=3, now we go up to k=19 by removing the Fare predictor.


knn_maxmin_fit1 <- knn3(Survived ~ ., data = trainNorm1, k = train_knn1$bestTune$k)
y_hat_knn1 <- predict(knn_maxmin_fit1, newdata = testNorm1, type = "class")

MaxMinAccuracy1 <- confusionMatrix(y_hat_knn1, testNorm1$Survived)$overall[["Accuracy"]]
MaxMinKappa1 <- confusionMatrix(y_hat_knn1, testNorm1$Survived)$overall[["Kappa"]]
MaxMinAccuracy1
MaxMinKappa1


(MaxMinAccuracy-MaxMinAccuracy1)/MaxMinAccuracy
#Here we see that simply removing the Fare predictor lowered accuracy by about 8.6% indicating removing the Fare predictor may not be the best
#move.  It appears there is additional information in the Fare predictor that we need.
#For the sake of being thorough, let's try removing the Class predictor as well.

titanicNorm_test2 <- titanicNorm[,c(1,3,4,5,6,7)]
set.seed(7)
vec <- sample(c(1:nrow(titanicNorm_test2)), (nrow(titanicNorm_test2)/5),replace = FALSE)
testNorm2 <- titanicNorm_test2[vec,]
trainNorm2 <- titanicNorm_test2[-vec,]



set.seed(8)
control <- trainControl(method = "cv", number = 10, p = .9, classProbs = TRUE)
train_knn2 <- train(Survived ~ FareNorm + SexNorm + AgeNorm + SibNorm + ParentChildNorm, method = "knn",
                    data = trainNorm2,
                    tuneGrid = data.frame(k = seq(1, 71, 2)),
                    trControl = control,
                    metric = "Kappa") 

ggplot(train_knn2, highlight = TRUE)+
  xlab("k (Neighbors)")+
  ylab("Kappa")+
  ggtitle("Kappa - Max/Min Normalization")

train_knn2$bestTune
#The model with all of the predictors shows k=3, and this one is similar with k=5.

knn_maxmin_fit2 <- knn3(Survived ~ ., data = trainNorm2, k = train_knn2$bestTune$k)
y_hat_knn2 <- predict(knn_maxmin_fit2, newdata = testNorm2, type = "class")

MaxMinAccuracy2 <- confusionMatrix(y_hat_knn2, testNorm2$Survived)$overall[["Accuracy"]]
MaxMinKappa2 <- confusionMatrix(y_hat_knn2, testNorm2$Survived)$overall[["Kappa"]]
MaxMinAccuracy2
MaxMinKappa2


(MaxMinAccuracy-MaxMinAccuracy2)/MaxMinAccuracy

#Removing the class predictor only generates a very similar accuracy and kappa to the "baseline" model with both predictors present independently.


#Now let's try merging the two predictors by taking the average their normalized values as a new predictor that we'll call "Class_Detail".
#Note that since Fare and Class have a negative correlation (1st class is the most expensive, 3rd class is the least expensive) We'll need to flip
#the direction of one of the vectors before proceeding.  It's easiest to do with the Class predictor, so we'll start there.

titanicNorm_test3 <- titanicNorm
titanicNorm_test3$Class_revised <- abs(titanicNorm_test3$Class-1)
titanicNorm_test3 <- titanicNorm_test3 %>% mutate(Class_Detail = (Class_revised + FareNorm)/2)
titanicNorm_test3 <- titanicNorm_test3[,c(1,3,4,5,6,9)]


set.seed(9)
vec <- sample(c(1:nrow(titanicNorm_test3)), (nrow(titanicNorm_test3)/5),replace = FALSE)
testNorm3 <- titanicNorm_test3[vec,]
trainNorm3 <- titanicNorm_test3[-vec,]



set.seed(10)
control <- trainControl(method = "cv", number = 10, p = .9, classProbs = TRUE)
train_knn3 <- train(Survived ~ Class_Detail + SexNorm + AgeNorm + SibNorm + ParentChildNorm, method = "knn",
                    data = trainNorm3,
                    tuneGrid = data.frame(k = seq(1, 71, 2)),
                    trControl = control,
                    metric = "Kappa") 

ggplot(train_knn3, highlight = TRUE)+
  xlab("k (Neighbors)")+
  ylab("Kappa")+
  ggtitle("Kappa - Max/Min Normalization")

train_knn3$bestTune
#Note that k=17 in this context

knn_maxmin_fit3 <- knn3(Survived ~ ., data = trainNorm3, k = train_knn3$bestTune$k)
y_hat_knn3 <- predict(knn_maxmin_fit3, newdata = testNorm3, type = "class")

MaxMinAccuracy3 <- confusionMatrix(y_hat_knn3, testNorm3$Survived)$overall[["Accuracy"]]
MaxMinKappa3 <- confusionMatrix(y_hat_knn3, testNorm3$Survived)$overall[["Kappa"]]
MaxMinAccuracy3
MaxMinKappa3


(MaxMinAccuracy-MaxMinAccuracy3)/MaxMinAccuracy



#We see that we have a lower test accuracy with a merged predictor than if we were to keep the predictors separate; 8.55%% lower.  

Orthog <- data.frame()
Orthog[1,1] <- "Accuracy"
Orthog[2,1] <- "Kappa"
Orthog[1,2] <- MaxMinAccuracy
Orthog[2,2] <- MaxMinKappa
Orthog[1,3] <- MaxMinAccuracy1
Orthog[2,3] <- MaxMinKappa1
Orthog[1,4] <- MaxMinAccuracy2
Orthog[2,4] <- MaxMinKappa2
Orthog[1,5] <- MaxMinAccuracy3
Orthog[2,5] <- MaxMinKappa3
colnames(Orthog)[1] <- "Metric"
colnames(Orthog)[2] <- "AllPredictors"
colnames(Orthog)[3] <- "RemovedFare"
colnames(Orthog)[4] <- "RemovedClass"
colnames(Orthog)[5] <- "Merged"
View(Orthog)

#The tradeoff we're trying to optimize here is lost information by removing or merging predictors versus the risk of skewing results by including
#multiple predictors that have a correlation magnitude around 0.55.  By experimentation, we see that this tradeoff is optimized by keeping both
#predictors in the model independently.  Apparently, feature engineering forces us to lose information, and this hurts our model more that including the 
#predictors correlated at magnitude 0.55 would. We see that removing the class predictor does not cost us much accuracy, but it is still not the 
#best option in terms of cross-validated test results.  Therefore, we say the final model should include the full set of predictors initially defined.


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
