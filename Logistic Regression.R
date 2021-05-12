#####Initial Processing and Definitions#####
#Here we use a logistic regression model as a means of binary classification to predict who will survive and who will not survive the infamous titanic wreck
#of 1912
library(titanic)
library(splines)
library(broom)
library(dplyr)
library(ggplot2)
library(tidyr)
#First we process the titanic_train data set to make it a little more logistic regression friendly
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#We'll remove NA rows as there aren't very many
titanic <- na.omit(titanic)
#making the sex column binary with male = 1
titanic$sex_binary <- as.integer(titanic$Sex == "male")


#Survived - 0=Did not survive, 1 = survived
#Pclass - Passenger class; 1st class being the most expensive, 2nd class, is cheaper, 3rd class is cheapest
#Sex - Sex of passenger, female or male
#Age - Age of passenger; notice occasional missing data here
#SibSp - Number of siblings/spouses on board
#Parch - Number of Parents/Children on board
#Fare - Passage fare in 1912 British Pounds
#sex_binary - a different expression of Sex


#####Linearity-Logit Assumption Check#####
#Breaking down data into train and test split
titanic <- titanic[,c(1,2,4,5,6,7,8)]
set.seed(2)
vec <- sample(c(1:nrow(titanic)), (nrow(titanic)/3),replace = FALSE)
test <- titanic[vec,]
train <- titanic[-vec,]
#Now we can actually generate the model
model <- glm(Survived ~., data = train, 
             family = binomial)

#We need to generate a vector of probabilities associated with each row in order to perform diagnostics ensuring we are meeting the assumptions of
#logistic regression
probabilities <- predict(model, newdata = train, type = "response")

#First we check the assumption of linearity between continuous predictors and the outcome logit.  To do this we must select our numeric type predictors.
my_data <- train %>% select(Age, Fare)
numeric_predictors <- colnames(my_data)

#Now we separate out the predictors we have chosen along with their predicted percentage and logit
mydata <- my_data %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#Now we create a visualization of the predictors versus associate logit values to asses our assumption of linearity
ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
#Conclusion: we may need to apply a spline to the "age" predictor.  The "fare" predictor does not appear to need a spline.  Therefore, we will proceed
#with a model that DOES include a spline on the "age" predictor and one that does NOT include a spline on the "age" predictor.  We will then compare their
#respective performances via cross-validation and see if the inclusion of a spline is justified.
#####Assumption of No Influential Outliers#####
#Before we deal with the non-linearity of the age model, we have to investigate the possibility of influential outlier values.
#First, let's look at Cook's distance for our most visually noticeable possible outliers.
plot(model, which = 4,id.n = 8)

#Now we pull the data for these points
model.data <- augment(model) %>% 
  mutate(index = 1:n())
model.data %>% top_n(8, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Survived), alpha = .5) +
  theme_bw()+
  xlab("Index")+
  ylab("Standard Residual")+
  ggtitle("Standardized Residuals of Model")+
  ylim(c(-3,3))+
  scale_color_discrete(name = "Survival Status", labels = c("Did Not Survive", "Survived"))

#We notice that we have no standardized residuals over absolute value of 3, so we can be confident that we do not have influential outliers.
##########Introduction of Splines############


#Optimizing the number of degrees of freedom via cross-validation; we suspect there would be up to
# 3 regions (df = 2) for age based on graphics from the exploratory phase; we want to allow for the possibility that each area of the domain might need it's own piece
#of the piece-wise polynomial
#We'll add one more in the interest of exploring additional possibilities

#Histogram to provide some additional intuition about how many degrees of freedom may be appropriate
titanic %>% ggplot(aes(x = Age))+
  geom_density()+
  xlab("Age")+
  ylab("Density")+
  ggtitle("Estimating Initial Degrees of Freedom: Age")


#Here we use cross validation to find out what the ideal number of degrees of freedom for the Age spline is.  We randomly select 1/3 of the data to test and 2/3 to train
#For each iteration, each number of degrees of freedom (1-4) will be used to create a model and accuracy will be calculated.  We define accuracy as simply a 
#result that matches a prediction.  Inaccuracies are results that do not match a prediction; we do not distinguish between false positives and false negatives
#for these purposes.  The degree of freedom that is most frequently the most accurate will then be adopted by the model.
#setting seed and shuffling data
set.seed(1)
rows <- sample(nrow(titanic))
shuffled <- titanic[rows,]

#Then we split our data set for 5-fold cross validation
t <- nrow(shuffled)/5
df1 <- shuffled[1:t,]
df2 <- shuffled[(t+1):(2*t),]
df3 <- shuffled[((2*t)+1):(3*t),]
df4 <- shuffled[((3*t)+1):(4*t),]
df5 <- shuffled[((4*t)+1):(5*t),]

#Beginning first fold
train1 <- bind_rows(df2,df3, df4, df5)
test1 <- df1

test1 <- test1 %>% mutate(survived_numeric = as.numeric(Survived)-1)

accuracy_data_1 <- data.frame()

for(i in 1:4)
{
  
  
  model_1 <- glm(Survived ~ +SibSp + ns(Age, df = i) + Pclass + Parch + Fare,
                 data = train1,
                 family = binomial)
  model_1 <- step(model_1, trace = FALSE)
  
  df_1 <- test1 %>% mutate(predicted_percent_survival = predict(newdata = test1, model_1, type = "response"))
  df_1$predicted_percent_survival <- round(df_1$predicted_percent_survival)
  df_1 <- df_1 %>% mutate(diff = abs(survived_numeric-predicted_percent_survival))
  accuracy_data_1[i,1] <- 1-(sum(df_1$diff)/nrow(df_1))
  accuracy_data_1[i,2] <- i
  accuracy_data_1[i,3] <- df_1 %>% filter(survived_numeric == 1) %>% summarize(sensitivity = sum(predicted_percent_survival)/n()) %>% pull(sensitivity)
  accuracy_data_1[i,4] <- df_1 %>% filter(survived_numeric == 0) %>% summarize(specificity = 1-mean(predicted_percent_survival)) %>% pull(specificity)
  
  df_1 <- NA
}
colnames(accuracy_data_1)[1] <- "Accuracy1"
colnames(accuracy_data_1)[2] <- "Degrees of Freedom"
colnames(accuracy_data_1)[3] <- "Sensitivity"
colnames(accuracy_data_1)[4] <- "Specificity"
                          
#Beginning second fold
train2 <- bind_rows(df1,df3, df4, df5)
test2 <- df2
test2 <- test2 %>% mutate(survived_numeric = as.numeric(Survived)-1)


accuracy_data_2 <- data.frame()

for(i in 1:4)
{
  
  
  model_2 <- glm(Survived ~ +SibSp + ns(Age, df = i) + Pclass + Parch + Fare,
                 data = train2,
                 family = binomial)
  model_2 <- step(model_2, trace = FALSE)
  
  df_2 <- test2 %>% mutate(predicted_percent_survival = predict(newdata = test2, model_2, type = "response"))
  df_2$predicted_percent_survival <- round(df_2$predicted_percent_survival)
  df_2 <- df_2 %>% mutate(diff = abs(survived_numeric-predicted_percent_survival))
  accuracy_data_2[i,1] <- 1-(sum(df_2$diff)/nrow(df_2))
  accuracy_data_2[i,2] <- i
  accuracy_data_2[i,3] <- df_2 %>% filter(survived_numeric == 1) %>% summarize(sensitivity = sum(predicted_percent_survival)/n()) %>% pull(sensitivity)
  accuracy_data_2[i,4] <- df_2 %>% filter(survived_numeric == 0) %>% summarize(specificity = 1-mean(predicted_percent_survival)) %>% pull(specificity)
  
  df_2 <- NA
}
colnames(accuracy_data_2)[1] <- "Accuracy2"
colnames(accuracy_data_2)[2] <- "Degrees of Freedom"
colnames(accuracy_data_2)[3] <- "Sensitivity"
colnames(accuracy_data_2)[4] <- "Specificity"

#Beginning third fold
train3 <- bind_rows(df1,df2, df4, df5)
test3 <- df3
test3 <- test3 %>% mutate(survived_numeric = as.numeric(Survived)-1)


accuracy_data_3 <- data.frame()

for(i in 1:4)
{
  
  
  model_3 <- glm(Survived ~ +SibSp + ns(Age, df = i) + Pclass + Parch + Fare,
                 data = train3,
                 family = binomial)
  model_3 <- step(model_3, trace = FALSE)
  
  df_3 <- test3 %>% mutate(predicted_percent_survival = predict(newdata = test3, model_3, type = "response"))
  df_3$predicted_percent_survival <- round(df_3$predicted_percent_survival)
  df_3 <- df_3 %>% mutate(diff = abs(survived_numeric-predicted_percent_survival))
  accuracy_data_3[i,1] <- 1-(sum(df_3$diff)/nrow(df_3))
  accuracy_data_3[i,2] <- i
  accuracy_data_3[i,3] <- df_3 %>% filter(survived_numeric == 1) %>% summarize(sensitivity = sum(predicted_percent_survival)/n()) %>% pull(sensitivity)
  accuracy_data_3[i,4] <- df_3 %>% filter(survived_numeric == 0) %>% summarize(specificity = 1-mean(predicted_percent_survival)) %>% pull(specificity)
  
  df_3 <- NA
}

colnames(accuracy_data_3)[1] <- "Accuracy3"
colnames(accuracy_data_3)[2] <- "Degrees of Freedom"
colnames(accuracy_data_3)[3] <- "Sensitivity"
colnames(accuracy_data_3)[4] <- "Specificity"

#Starting fourth fold
train4 <- bind_rows(df1,df2, df3, df4)
test4 <- df4
test4 <- test4 %>% mutate(survived_numeric = as.numeric(Survived)-1)


accuracy_data_4 <- data.frame()

for(i in 1:4)
{
  
  
  model_4 <- glm(Survived ~ +SibSp + ns(Age, df = i) + Pclass + Parch + Fare,
                 data = train4,
                 family = binomial)
  model_4 <- step(model_4, trace = FALSE)
  
  df_4 <- test4 %>% mutate(predicted_percent_survival = predict(newdata = test4, model_4, type = "response"))
  df_4$predicted_percent_survival <- round(df_4$predicted_percent_survival)
  df_4 <- df_4 %>% mutate(diff = abs(survived_numeric-predicted_percent_survival))
  accuracy_data_4[i,1] <- 1-(sum(df_4$diff)/nrow(df_4))
  accuracy_data_4[i,2] <- i
  accuracy_data_4[i,3] <- df_4 %>% filter(survived_numeric == 1) %>% summarize(sensitivity = sum(predicted_percent_survival)/n()) %>% pull(sensitivity)
  accuracy_data_4[i,4] <- df_4 %>% filter(survived_numeric == 0) %>% summarize(specificity = 1-mean(predicted_percent_survival)) %>% pull(specificity)
  
  df_4 <- NA
}

colnames(accuracy_data_4)[1] <- "Accuracy4"
colnames(accuracy_data_4)[2] <- "Degrees of Freedom"
colnames(accuracy_data_4)[3] <- "Sensitivity"
colnames(accuracy_data_4)[4] <- "Specificity"

#Starting fifth fold

train5 <- bind_rows(df1,df2, df3, df4)
test5 <- df5
test5 <- test5 %>% mutate(survived_numeric = as.numeric(Survived)-1)


accuracy_data_5 <- data.frame()

for(i in 1:4)
{
  
  
  model_5 <- glm(Survived ~ +SibSp + ns(Age, df = i) + Pclass + Parch + Fare,
                 data = train5,
                 family = binomial)
  model_5 <- step(model_5, trace = FALSE)
  
  df_5 <- test5 %>% mutate(predicted_percent_survival = predict(newdata = test5, model_5, type = "response"))
  df_5$predicted_percent_survival <- round(df_5$predicted_percent_survival)
  df_5 <- df_5 %>% mutate(diff = abs(survived_numeric-predicted_percent_survival))
  accuracy_data_5[i,1] <- 1-(sum(df_5$diff)/nrow(df_5))
  accuracy_data_5[i,2] <- i
  accuracy_data_5[i,3] <- df_5 %>% filter(survived_numeric == 1) %>% summarize(sensitivity = sum(predicted_percent_survival)/n()) %>% pull(sensitivity)
  accuracy_data_5[i,4] <- df_5 %>% filter(survived_numeric == 0) %>% summarize(specificity = 1-mean(predicted_percent_survival)) %>% pull(specificity)
  
  df_5 <- NA
}

colnames(accuracy_data_5)[1] <- "Accuracy5"
colnames(accuracy_data_5)[2] <- "Degrees of Freedom"
colnames(accuracy_data_5)[3] <- "Sensitivity"
colnames(accuracy_data_5)[4] <- "Specificity"
##Evaluating Cross-validation


SENS <- sapply(c(1:4), function(j){
  Sensitivity_vec[j] <- (accuracy_data_1$Sensitivity[j] + accuracy_data_2$Sensitivity[j] + accuracy_data_3$Sensitivity[j] + accuracy_data_4$Sensitivity[j] + accuracy_data_5$Sensitivity[j])
})
Sensitivity <- as.data.frame(cbind(SENS, c(1:4)))
Sensitivity

SPC <- sapply(c(1:4), function(j){
  Specificity_vec[j] <- (accuracy_data_1$Specificity[j] + accuracy_data_2$Specificity[j] + accuracy_data_3$Specificity[j] + accuracy_data_4$Specificity[j] + accuracy_data_5$Specificity[j])
  })
Specificity <- as.data.frame(cbind(SPC, c(1:4)))
Specificity

ACC <- sapply(c(1:4), function(j){
  Specificity_vec[j] <- (accuracy_data_1$Accuracy1[j] + accuracy_data_2$Accuracy2[j] + accuracy_data_3$Accuracy3[j] + accuracy_data_4$Accuracy4[j] + accuracy_data_5$Accuracy5[j])
})
Accuracy <- as.data.frame(cbind(ACC, c(1:4)))
Accuracy

#Notice that the combination of sensitivity and specificity is optimized at 3 degrees of freedom, as is accuracy. Therefore, we will use 3 degrees of freedom in the spline going forward.


#####Addressing Collinearity with backward selection#####
#Now that we know how many degrees of freedom we want to use in the spline model, we can continue building our models, one with a spline on
#the "age" predictor, and one without.  We now address collinearity possibilities with
#a backward selection process similar to what we used in linear modeling. (note, backward selection is the default for the "step()" function.)
model_spline <- glm(Survived ~ SibSp + ns(Age, df = 3) + Pclass + Parch + Fare + sex_binary,
                    data = titanic,
                    family = binomial)
model_spline <- step(model_spline, trace = FALSE)
summary(model_spline)
#Spline model features include; Siblings; Age; Class; Sex

model_no_spline <- glm(Survived ~ SibSp + Age + Pclass + Parch + Fare + sex_binary,
                       data = titanic,
                       family = binomial)
model_no_spline <- step(model_no_spline, trace = FALSE)
summary(model_no_spline)

#No-spline model features included; siblings; Age; Class; Sex
#########Cross-Validation/Comparing Models###################
AIC(model_no_spline)
AIC(model_spline)

#cross validation to get additional information about how our models perform
#first we shuffle our rows randomly
set.seed(3)
rows <- sample(nrow(titanic))
shuffled <- titanic[rows,]

#Then we split our data set for 5-fold cross validation
t <- nrow(shuffled)/5
df1 <- shuffled[1:t,]
df2 <- shuffled[(t+1):(2*t),]
df3 <- shuffled[((2*t)+1):(3*t),]
df4 <- shuffled[((3*t)+1):(4*t),]
df5 <- shuffled[((4*t)+1):(5*t),]

#Beginning first fold
train1 <- bind_rows(df2,df3, df4, df5)
test1 <- df1

model_spline_1 <- glm(Survived ~SibSp + ns(Age, df = 3) + Pclass + sex_binary,
                      data = train1, 
                      family = binomial)
test1 <- test1 %>% mutate(predicted_percent_survival_no_spline = predict(model_spline_1, newdata = test1, type = "response"))

model_no_spline_1 <- glm(Survived ~SibSp + Age + Pclass + sex_binary,
                         data = train1, 
                         family = binomial)
test1 <- test1 %>% mutate(predicted_percent_survival_spline = predict(model_no_spline_1, newdata = test1, type = "response"))

test1$predicted_percent_survival_no_spline <- round(test1$predicted_percent_survival_no_spline)
test1$predicted_percent_survival_spline <- round(test1$predicted_percent_survival_spline)

test1 <- test1 %>% mutate(survived_numeric = as.numeric(Survived)-1)

test1 <- test1 %>% mutate(diff_no_spline = abs(survived_numeric-predicted_percent_survival_no_spline))
test1 <- test1 %>% mutate(diff_spline = abs(survived_numeric-predicted_percent_survival_spline))


sensitivity_no_spline1 <- test1 %>% filter(survived_numeric == 1) %>% summarise(sensitivity = sum(predicted_percent_survival_no_spline)/n())
specificity_no_spline1 <- test1 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_no_spline))/n())

sensitivity_spline1 <- test1 %>% filter(survived_numeric == 1) %>% summarise(sensivity = sum(predicted_percent_survival_spline)/n())
specificity_spline1 <- test1 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_spline))/n())

accuracy1_no_spline <- 1-(sum(test1$diff_no_spline)/nrow(test1))
accuracy1_spline <- 1-(sum(test1$diff_spline)/nrow(test1))


#Beginning second fold
train2 <- bind_rows(df1,df3, df4, df5)
test2 <- df2

model_spline_2 <- glm(Survived ~SibSp + ns(Age, df = 3) + Pclass + sex_binary,
                      data = train2, 
                      family = binomial)
test2 <- test2 %>% mutate(predicted_percent_survival_no_spline = predict(model_spline_2, newdata = test2, type = "response"))

model_no_spline_2 <- glm(Survived ~SibSp + Age + Pclass + sex_binary,
                         data = train2, 
                         family = binomial)
test2 <- test2 %>% mutate(predicted_percent_survival_spline = predict(model_no_spline_2, newdata = test2, type = "response"))

test2$predicted_percent_survival_no_spline <- round(test2$predicted_percent_survival_no_spline)
test2$predicted_percent_survival_spline <- round(test2$predicted_percent_survival_spline)

test2 <- test2 %>% mutate(survived_numeric = as.numeric(Survived)-1)

test2 <- test2 %>% mutate(diff_no_spline = abs(survived_numeric-predicted_percent_survival_no_spline))
test2 <- test2 %>% mutate(diff_spline = abs(survived_numeric-predicted_percent_survival_spline))


sensitivity_no_spline2 <- test2 %>% filter(survived_numeric == 1) %>% summarise(sensitivity = sum(predicted_percent_survival_no_spline)/n())
specificity_no_spline2 <- test2 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_no_spline))/n())

sensitivity_spline2 <- test2 %>% filter(survived_numeric == 1) %>% summarise(sensivity = sum(predicted_percent_survival_spline)/n())
specificity_spline2 <- test2 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_spline))/n())

accuracy2_no_spline <- 1-(sum(test2$diff_no_spline)/nrow(test2))
accuracy2_spline <- 1-(sum(test2$diff_spline)/nrow(test2))
#beginning third fold
train3 <- bind_rows(df1,df2, df4, df5)
test3 <- df3

model_spline_3 <- glm(Survived ~SibSp + ns(Age, df = 3) + Pclass + sex_binary,
                      data = train3, 
                      family = binomial)
test3 <- test3 %>% mutate(predicted_percent_survival_no_spline = predict(model_spline_3, newdata = test3, type = "response"))

model_no_spline_3 <- glm(Survived ~SibSp + Age + Pclass + sex_binary,
                         data = train3, 
                         family = binomial)
test3 <- test3 %>% mutate(predicted_percent_survival_spline = predict(model_no_spline_3, newdata = test3, type = "response"))

test3$predicted_percent_survival_no_spline <- round(test3$predicted_percent_survival_no_spline)
test3$predicted_percent_survival_spline <- round(test3$predicted_percent_survival_spline)

test3 <- test3 %>% mutate(survived_numeric = as.numeric(Survived)-1)

test3 <- test3 %>% mutate(diff_no_spline = abs(survived_numeric-predicted_percent_survival_no_spline))
test3 <- test3 %>% mutate(diff_spline = abs(survived_numeric-predicted_percent_survival_spline))


sensitivity_no_spline3 <- test3 %>% filter(survived_numeric == 1) %>% summarise(sensitivity = sum(predicted_percent_survival_no_spline)/n())
specificity_no_spline3 <- test3 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_no_spline))/n())

sensitivity_spline3 <- test3 %>% filter(survived_numeric == 1) %>% summarise(sensivity = sum(predicted_percent_survival_spline)/n())
specificity_spline3 <- test3 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_spline))/n())

accuracy3_no_spline <- 1-(sum(test3$diff_no_spline)/nrow(test3))
accuracy3_spline <- 1-(sum(test3$diff_spline)/nrow(test3))

#Starting fourth fold

train4 <- bind_rows(df1,df2, df3, df5)
test4 <- df4

model_spline_4 <- glm(Survived ~ SibSp + ns(Age, df = 3) + Pclass + sex_binary,
                      data = train4, 
                      family = binomial)
test4 <- test4 %>% mutate(predicted_percent_survival_no_spline = predict(model_spline_4, newdata = test4, type = "response"))

model_no_spline_4 <- glm(Survived ~SibSp + Age + Pclass + sex_binary,
                         data = train4, 
                         family = binomial)
test4 <- test4 %>% mutate(predicted_percent_survival_spline = predict(model_no_spline_4, newdata = test4, type = "response"))

test4$predicted_percent_survival_no_spline <- round(test4$predicted_percent_survival_no_spline)
test4$predicted_percent_survival_spline <- round(test4$predicted_percent_survival_spline)

test4 <- test4 %>% mutate(survived_numeric = as.numeric(Survived)-1)

test4 <- test4 %>% mutate(diff_no_spline = abs(survived_numeric-predicted_percent_survival_no_spline))
test4 <- test4 %>% mutate(diff_spline = abs(survived_numeric-predicted_percent_survival_spline))


sensitivity_no_spline4 <- test4 %>% filter(survived_numeric == 1) %>% summarise(sensitivity = sum(predicted_percent_survival_no_spline)/n())
specificity_no_spline4 <- test4 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_no_spline))/n())

sensitivity_spline4 <- test4 %>% filter(survived_numeric == 1) %>% summarise(sensivity = sum(predicted_percent_survival_spline)/n())
specificity_spline4 <- test4 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_spline))/n())

accuracy4_no_spline <- 1-(sum(test4$diff_no_spline)/nrow(test4))
accuracy4_spline <- 1-(sum(test4$diff_spline)/nrow(test4))

#starting fifth fold

train5 <- bind_rows(df1,df2, df3, df4)
test5 <- df5

model_spline_5 <- glm(Survived ~ SibSp + ns(Age, df = 3) + Pclass + sex_binary,
                      data = train5, 
                      family = binomial)
test5 <- test5 %>% mutate(predicted_percent_survival_no_spline = predict(model_spline_5, newdata = test5, type = "response"))

model_no_spline_5 <- glm(Survived ~SibSp + Age + Pclass + sex_binary,
                         data = train5, 
                         family = binomial)
test5 <- test5 %>% mutate(predicted_percent_survival_spline = predict(model_no_spline_5, newdata = test5, type = "response"))

test5$predicted_percent_survival_no_spline <- round(test5$predicted_percent_survival_no_spline)
test5$predicted_percent_survival_spline <- round(test5$predicted_percent_survival_spline)

test5 <- test5 %>% mutate(survived_numeric = as.numeric(Survived)-1)

test5 <- test5 %>% mutate(diff_no_spline = abs(survived_numeric-predicted_percent_survival_no_spline))
test5 <- test5 %>% mutate(diff_spline = abs(survived_numeric-predicted_percent_survival_spline))


sensitivity_no_spline5 <- test5 %>% filter(survived_numeric == 1) %>% summarise(sensitivity = sum(predicted_percent_survival_no_spline)/n())
specificity_no_spline5 <- test5 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_no_spline))/n())

sensitivity_spline5 <- test5 %>% filter(survived_numeric == 1) %>% summarise(sensivity = sum(predicted_percent_survival_spline)/n())
specificity_spline5 <- test5 %>% filter(survived_numeric == 0) %>% summarise(specificity = (n() - sum(predicted_percent_survival_spline))/n())

accuracy5_no_spline <- 1-(sum(test5$diff_no_spline)/nrow(test5))
accuracy5_spline <- 1-(sum(test5$diff_spline)/nrow(test5))


#####PLEASE READ THIS#####

#Before proceeding, it is also worth mentioning that by simply making assumptions about survival rate based on class and 
#sex, one can create a reasonably successful predictive model.  One can see this in the "additional data visualizations" file.  
#This methodology provides a lower sensitivity than this logistic regression model however, and the trade-off makes it less successful in ROC
#and balanced accuracy.
#I am aware of this, the purpose of this file is to illustrate the construction of a logistic regression model.

#####OK, keep going####

mean_accuracy_no_spline <- (accuracy1_no_spline + accuracy2_no_spline + accuracy3_no_spline + accuracy4_no_spline + accuracy5_no_spline)/5
mean_accuracy_no_spline

mean_accuracy_spline <- (accuracy1_spline + accuracy2_spline + accuracy3_spline + accuracy5_spline + accuracy5_spline)/5
mean_accuracy_spline

mean_specificity_no_spline <- (specificity_no_spline1 + specificity_no_spline2 + specificity_no_spline3 + specificity_no_spline4 + specificity_no_spline5)/5
mean_specificity_no_spline

mean_sensitivity_no_spline <- (sensitivity_no_spline1 + sensitivity_no_spline2 + sensitivity_no_spline3 + sensitivity_no_spline4 + sensitivity_no_spline5)/5
mean_sensitivity_no_spline

mean_Specificity_spline <- (specificity_spline1 + specificity_spline2 + specificity_spline3 + specificity_spline4 + specificity_spline5)/5
mean_Specificity_spline

mean_sensitivity_spline <- (sensitivity_spline1 + sensitivity_spline2 + sensitivity_spline3 + sensitivity_spline4 + sensitivity_spline5)/5
mean_sensitivity_spline

#Evaluation
#in general, we can feel confident that our model predicts who would survive or not survive the Titanic wreck with ~80% accuracy;
#however, given the relative prevalence of survivorship, it makes sense to compare sensitivity and specificity as well


#The introduction of the spline has a higher accuracy, however, due to the prevalences of surviving and not surviving, we need to be wary of moving forward
#with an overly specific model.  If we simply predict that everyone will note survive, we would have an accuracy of around 62%, with 100% specificity.
#Given the trade-off in sensitivity, it makes sense to use the model with no spline, even though it technically has a slightly lower raw accuracy.
#####Model Interpretation#####

model_no_spline <- glm(Survived ~ SibSp + Age + Pclass + sex_binary,
                       data = titanic, 
                       family = binomial)
summary(model_no_spline)

#Recall "sex_binary is 1 for male, 0 for female
#The log odds decrease as the number of siblings a passenger has increases
#the log odds decrease, in general, as the age increases
#The log odds of survival are lower for second class passengers as opposed to first class(the reference class) and the log odds of survival are MUCH
#lower for passengers in 3rd class than first class
#The log odds of survival are much lower for males than for females
#Recall that each feature increases or decrease in accordance with the logistic regression formula:
#p = exp(a)/(1+exp(a))
#where a = B_0 + B_1*x_1 + B_2*x_2...

#Notice that the results related to class and sex especially align with our analysis from the "Additional Data Visualizations" section
#Also notice that the "fare" predictor was eliminated during the backward selection process for both models, this is likely because
#that information is carried in the Pclass predictor.
