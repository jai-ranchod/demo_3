#####Loading Libraries and Introducing Data#####
#Here we use a logistic regression model as a means of binary classification to predict who will survive and who will not survive the infamous titanic wreck
#of 1912.  We use the built in "titanic_train" data set to train a logistic regression model which we then test against
#a held out test subset of the "titanic_train" data set.

#We plainly have a binary outcome, we can reasonably assume there is independence among the observations, and a brief examination
#of the data will clearly show that we have enough data points to pursue a logistic regression model.
#There are additional assumptions regarding logistic regression documented here:
#http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/#logistic-regression-assumptions

#which we will address as we build the model.


if (!require("titanic")) install.packages("titanic")
if (!require("splines")) install.packages("splines")
if (!require("broom")) install.packages("broom")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("tidyr")) install.packages("tidyr")
if (!require("pedometrics")) install.packages("pedometrics")
if (!require("car")) install.packages("car")



library(titanic)
library(splines)
library(broom)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pedometrics)
library(car)

#First we process the titanic_train data set to make it a little more logistic regression friendly
titanic <-  titanic_train %>%
  dplyr::select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
#####Dealing with NA values#####

#Next we need to deal with NA values.  If there is a pattern in regards to the rows holding NA values, we can 
#try to impute values, otherwise we may need to remove the rows.
mean(is.na(titanic$Age))
#Here we see that we have about 20% of our data where age is NA.



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


#####Explaining Data#####
#Let's now convert the 'sex' predictor to a factor and provide formal definitions for our data
titanic$sex_binary <- as.integer(titanic$Sex == "male")
titanic$sex_binary <- factor(titanic$sex_binary)

#Survived - 0=Did not survive, 1 = survived
#Pclass - Passenger class; 1st class being the most expensive, 2nd class, is cheaper, 3rd class is cheapest
#Sex - Sex of passenger, female or male
#Age - Age of passenger; notice occasional missing data here
#SibSp - Number of siblings/spouses on board
#Parch - Number of Parents/Children on board
#Fare - Passage fare in 1912 British Pounds
#sex_binary - a different expression of Sex


######Breaking down data into train and test split#####
#Note that we are only using the "titanic_train" set here, not utilizing the "titanic_test" set at all.
#This is because the "titanic_test" set does not have a column indicating survival status, and we want to 
#apply our final model to a holdout test set to see how it performs.
#For the purpose of model training in general, we'll use a 1/5 - 4/5 split.

titanic <- titanic[,c(1,2,4,5,6,7,8)]
set.seed(1)
vec <- sample(c(1:nrow(titanic)), (nrow(titanic)/5),replace = FALSE)
test <- titanic[vec,]
train <- titanic[-vec,]
#####Linearity-Logit Assumption Check#####
#We need to check the assumption of a linear relationship between our logit function and our continuous predictors
l <- (train$Fare)
q <- quantile(l, probs = c(0,0.2,0.4,0.6,0.8,1))

p1 <- mean(as.numeric(train$Survived[l < q[[2]]])-1)
p2 <- mean(as.numeric(train$Survived[l >= q[[2]] & l < q[[3]]])-1)
p3 <- mean(as.numeric(train$Survived[l >= q[[3]] & l < q[[4]]])-1)
p4 <- mean(as.numeric(train$Survived[l >= q[[4]] & l < q[[5]]])-1)
p5 <- mean(as.numeric(train$Survived[l >= q[[5]]])-1)
probs <- c(p1, p2, p3, p4, p5)
logits <- log(probs/(1-probs))
meds <- c(median(l[l<q[2]]),
          median(l[l>=q[2] & l < q[3]]),
          median(l[l>=q[3] & l < q[4]]),
          median(l[l>=q[4] & l < q[5]]),
          median(l[l>=q[5]]))

plot(meds, logits)
#It doesn't look like we meet the assumption of linearity of for the "Fare" predictor, but it looks like
#a logarithmic transformation of the "Fare" predictor might do the trick.
l <- log(train$Fare)
q <- quantile(l, probs = c(0,0.2,0.4,0.6,0.8,1))

p1 <- mean(as.numeric(train$Survived[l < q[[2]]])-1)

p2 <- mean(as.numeric(train$Survived[l >= q[[2]] & l < q[[3]]])-1)

p3 <- mean(as.numeric(train$Survived[l >= q[[3]] & l < q[[4]]])-1)

p4 <- mean(as.numeric(train$Survived[l >= q[[4]] & l < q[[5]]])-1)

p5 <- mean(as.numeric(train$Survived[l >= q[[5]]])-1)


probs <- c(p1, p2, p3, p4, p5)

logits <- log(probs/(1-probs))

meds <- c(median(l[l<q[2]]),
          median(l[l>=q[2] & l < q[3]]),
          median(l[l>=q[3] & l < q[4]]),
          median(l[l>=q[4] & l < q[5]]),
          median(l[l>=q[5]]))

plot(meds, logits)
#Let's make this transformation in the data set now. First we need to recall that log(0) = -Inf
#There are many methods for handling this issue as it is widely discussed in the data science community.
#For now, we will use a basic mean imputation to maintain simplicity.  Note that this method is supported by the
#relatively low proportion of zeros we have:
#https://aosmith.rbind.io/2018/09/19/the-log-0-problem/#:~:text=The%20log%20transformation%20tends%20to,rid%20of%20the%200%20values.

mean(train$Fare == 0)
mean(test$Fare == 0)  

nonzerofare_train <- train[-which(train$Fare == 0),]
x_a <- mean(nonzerofare_train$Fare)

nonzerofare_test <- test[-which(test$Fare == 0),]
x_b <- mean(nonzerofare_test$Fare)

train$Fare[train$Fare == 0] <- x_a
test$Fare[test$Fare == 0] <- x_b


train$LogFare <- log(train$Fare)
test$LogFare <- log(test$Fare)


#train <- train[-which(train$Fare == 0),]
#test <- test[-which(test$Fare == 0),]

#We need to repeat the same process with our "Age" predictor

l <- train$Age
q <- quantile(l, probs = c(0,0.2,0.4,0.6,0.8,1))

p1 <- mean(as.numeric(train$Survived[l < q[[2]]])-1)

p2 <- mean(as.numeric(train$Survived[l >= q[[2]] & l < q[[3]]])-1)

p3 <- mean(as.numeric(train$Survived[l >= q[[3]] & l < q[[4]]])-1)

p4 <- mean(as.numeric(train$Survived[l >= q[[4]] & l < q[[5]]])-1)

p5 <- mean(as.numeric(train$Survived[l >= q[[5]]])-1)

probs <- c(p1, p2, p3, p4, p5)

logits <- log(probs/(1-probs))

meds <- c(median(l[l<q[2]]),
          median(l[l>=q[2] & l < q[3]]),
          median(l[l>=q[3] & l < q[4]]),
          median(l[l>=q[4] & l < q[5]]),
          median(l[l>=q[5]]))

plot(meds, logits)
meds
#This relationship is definitely not linear and it does not appear that there is an easy way to transform
#the "Age" predictor to make the linearity/logit assumption work out. Let's look at a histogram and see if there is 
#another way to identify a useful pattern.
hist(titanic$Age, breaks = 50)



#We do notice, however, that the histogram appears to show natural breaks in the distribution of ages around 
#the particularly elderly and the particularly young.  This aligns with our historical knowledge of how positions on 
#life boats were prioritized. A reasonable estimate of cutoffs appears to be 18.5 and 63.5, with the rest of the population in 
#middle.
youngbreak <- 18.5
elderbreak <- 63.5

train$AgeLow <- as.numeric(train$Age < youngbreak)
train$AgeLow <- factor(train$AgeLow)

train$AgeMid <- as.numeric(train$Age >= youngbreak & train$Age < elderbreak)
train$AgeMid <- factor(train$AgeMid)

train$AgeHigh <- as.numeric(train$Age >= elderbreak)
train$AgeHigh <- factor(train$AgeHigh)

test$AgeLow <- as.numeric(test$Age < youngbreak)
test$AgeLow <- factor(test$AgeLow)

test$AgeMid <- as.numeric(test$Age >= youngbreak & test$Age < elderbreak)
test$AgeMid <- factor(test$AgeMid)

test$AgeHigh <- as.numeric(test$Age >= elderbreak)
test$AgeHigh <- factor(test$AgeHigh)

train <- train %>% dplyr::select(Survived, Pclass, SibSp, Parch, LogFare, sex_binary, AgeLow, AgeMid, AgeHigh)
test <- test %>% dplyr::select(Survived, Pclass, SibSp, Parch, LogFare, sex_binary, AgeLow, AgeMid, AgeHigh)
#####Addressing (Multi)Collinearity#####
#Here we create an initial model and correct any collinearity issues with the help of the vif() function.
model <- glm(Survived ~ Pclass + SibSp + Parch + LogFare + sex_binary + AgeHigh + AgeMid + AgeLow, data = train, family = "binomial")#AgeLow + AgeMid + AgeHigh, data = train, family = "binomial")
vif(model)
#Notice that we get an error regarding aliased coefficients.  We can investigate this further with the alias() function
alias(model)
#We see that we need to remove the "Age_Low" predictor as it is a linear combination fo AgeHigh and AgeMid
model <- glm(Survived ~ Pclass + SibSp + Parch + LogFare + sex_binary + AgeHigh + AgeMid, data = train, family = "binomial")#AgeLow + AgeMid + AgeHigh, data = train, family = "binomial")
vif(model)
#We see that none of our remaining features have a VIF higher than 4, indicating all of them should remain in the model
#####Assumption of No Influential Outliers#####
#Now we have to investigate the possibility of influential outlier values.
#First, let's look at Cook's distance for our most visually noticeable possible outliers.
plot(model, which = 4,id.n = 8)

#Now we pull the data for these points
model.data <- augment(model) %>% mutate(index = 1:dplyr::n())
model.data %>% top_n(8, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Survived), alpha = .5) +
  theme_bw()+
  xlab("Index")+
  ylab("Standard Residual")+
  ggtitle("Standardized Residuals of Model")+
  ylim(c(-5,5))+
  scale_color_discrete(name = "Survival Status", labels = c("Did Not Survive", "Survived"))

#We notice that we have no standardized residuals over absolute value of 3, so we can be confident that we do not have influential outliers.
#####Feature Selection#####
#Here we examine the impact of each of our features and determine which features to retain and which to discard.
#Note that p << n in this case. Given historical context and analysis done in the "Data Visualizations" portion of 
#this repository, we have reason to believe that each attribute in the data frame could potentially be a statistically
#significant predictor.  As such, we will use the z-statistic (and associated p-value) to determine feature selection.
summary(model)
#Here we notice that "LogFare", "AgeHigh", and "Parch" have relatively low magnitude z-statistics, indicating they may not be related to the
#outcome.  We see that the step() function for step-wise feature selection removes "LogFare" and "Parch" from our model.  The default
#step function, which we use here, is a backstep feature selection function based on the Akaike Information Criterion.
model <- step(model)
summary(model)

#####Final Prediction#####
#So far we have determined what the best possible model with a spline on the age feature would look like, but also that
#the best model will not have that spline; both using cross validation.  Now we take the model that we built from the training
#set accesssed in the first section and use it to make a prediction on the test set we held out in the beginning.
test <- test %>% mutate(predicted_percent_survival = predict(model, newdata = test, type = "response"))
test$predicted_percent_survival <- round(test$predicted_percent_survival) #Note that this assumes a cutoff of 0.5, which we will NOT assume below.
test$Survived <- as.numeric(test$Survived)
test$Survived <- (test$Survived-1)
sensitivity_final <- test %>% filter(Survived == 1) %>% summarise(sensitivity = sum(predicted_percent_survival)/dplyr::n())
specificity_final <- test %>% filter(Survived == 0) %>% summarise(specificity = (dplyr::n() - sum(predicted_percent_survival))/dplyr::n())

sensitivity_final

specificity_final

sensitivity_final*specificity_final

#We have now completed a successful logistic regression modeling of survival on the titanic.  
#We have checked all of the necessary assumptions associated with logistic regression and adapted our data accordingly.
#We then selected our feature set using the z-statistic.
#Finally, we tested our model against our hold out test set to determine that our model works quite well.
#####Further Model Evaluation#####
#Now that we've seen how our model performs against the hold out test set, we can go more in depth on performance
#evaluation and create an ROC curve and calculate the AUC.  
d <- 0.001
t <- seq(0,1,d)
spec <-c()
sens <-c()
for(i in 1:length(t))
{
  test <- test %>% mutate(predicted_percent_survival = predict(model, newdata = test, type = "response"))
  for(j in 1:nrow(test))
  {
    if(test$predicted_percent_survival[j] < t[i])
    {
      test$predicted_percent_survival[j] <- 0
    }else{
      test$predicted_percent_survival[j] <- 1
    }
  }

  sens[i] <- test %>% filter(Survived == 1) %>% summarise(sensitivity = sum(predicted_percent_survival)/dplyr::n()) 
  spec[i] <- test %>% filter(Survived == 0) %>% summarise(specificity = (dplyr::n() - sum(predicted_percent_survival))/dplyr::n())
}
df <- as.data.frame(cbind(t, sens, spec))
df$spec <- as.numeric(df$spec)
df$sens <- as.numeric(df$sens)
df$t <- as.numeric(df$t)

df <- df[order(df$sens),] 
#Here 
df %>% ggplot(aes(x = (1-spec), y = sens))+
  geom_point(size = 0.75)+
  geom_line()+
  labs(x = "False Positive Rate", y = "True Positive Rate")+
  ggtitle("ROC Graph")+
  geom_abline(intercept = 0)
#Our ROC curve looks quite good.  Let's now use this curve to approximate an ROC
                   
s <- c()
for(i in 1:nrow(df)-1)
{
  dx <- (1-df$spec[i+1]) - (1-df$spec[i])
  y <- df$sens[i]
  dy <- df$sens[i+1] - df$sens[i]
  t <- y*dx + (1/2)*dy*dx
  s[i] <- t
}
AUC <- sum(s)
AUC
#We note that we have done quite well in terms of AUC.

#Interestingly, we also note that the threshold yielding the optimal result here is right around the traditional
#threshold of 0.5 which we used above to calculate the sensitivity and specificity.
df <- df[order(df$t),]
df[which(df$sens*df$spec == max(df$sens*df$spec)),]

#####The End#####













