#####Abalone Data Set#####
if (!require("ApliedPredictiveModeling")) install.packages("AppliedPredictiveModeling")
if (!require("dplyr")) install.packages("dplyr")
if (!require("corrplot")) install.packages("corrplot")
if (!require("car")) install.packages("car")
if (!require("caret")) install.packages("caret")
if (!require("pedometrics")) install.packages("pedometrics")
if (!require("tidyr")) install.packages("tidyr")
if (!require("MASS")) install.packages("MASS")


library(AppliedPredictiveModeling)
library(dplyr)
library(corrplot)
library(car)
library(caret)
library(pedometrics)
library(tidyr)
library(MASS)
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
#for the "type" predictor.

abalone <- abalone %>% mutate(Infant = 0, Male = 0, Female = 0)
abalone$Infant[abalone$Type=="I"] <- 1
abalone$Male[abalone$Type == "M"] <- 1
abalone$Female[abalone$Type == "F"] <- 1

abalone <- abalone %>% dplyr::select(Rings, LongestShell, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight, Infant, Male, Female)

#####Exploratory Analysis#####
par(mfrow = c(1,1))

Matrix <- cor(abalone)
corrplot(Matrix, method = "color", order = "FPC")
#Our first correlation matrix is ordered by first principle component.

#Finally, we'll use the default values so you can get a closer look at individual predictor/outcome relationships
#a little more cleanly.
corrplot(Matrix)

#We definitely have strong relationships between predictors.  All of the weight and size related predictors are highly correlated
#which makes sense.  We see that "Infant" is negatively correlated with the predictors related to size, which again makes sense.
#We also note that Ring size does not seem to be correlated with sex much at all, but does seem to have a significant positive correlation
#with at least the shell weight predictor.

#Another thing we can do to get a sense of the relationship between predictors is create a universal model and give it as an
#argument to the vif() function.  the vif() function computes variance inflation factor calculations for each predictor in a given
#model.  For reference the variance inflation factor of a predictor i is defined as:
#VIF_i = 1/(1-(R_i)^2)
#where (R_i)^2 is the coefficient of determination of the regression of all other predictors onto the ith predictor.
vfModel <- lm(Rings ~.,data = abalone)
vif(vfModel)
#We see an error informing us of "aliased coefficients".  This means that one predictor is a perfect linear combination of at least
#one other predictor.  This is most likely related to the Infant, Male, and Female dummy variables, but we can use the alias() function to be
#sure.

alias(vfModel)
#Unsurprisingly we see the "Female" predictor and we proceed to remove if from the data set.
abalone <- abalone[,-which(colnames(abalone) == "Female")]
#Now let's try our VIF analysis again.
vfModel <- lm(Rings ~.,data = abalone)
vif(vfModel)
#We notice that "Longest Shell", "Diameter", "Whole Weight", "Shucked Weight", "Viscera Weight", and "Shell Weight" all have VIF
#scores above the conventional threshold for removal of 10.  This makes sense as these are all size related predictors.
#We'll return to the issue of collinearity later, running another VIF analysis after feature selection.

plot(vif(vfModel), xlab = "Index of Predictors", ylab = "VIF", main = "VIF of Predictors")
abline(h=10, col = "red")
abline(h=5, col = "red")
#This plot visually shows the comparatively high VIF values of most of the size and weight related predictors, and the comparatively small
#values of the other three.  Interestingly, "Height" does not seem to be inflated by the presence of other size related predictors.
#If you return to the correlation matrices, you'll notice that height does seem to be slightly less correlated with the other size related
#predictors than they are with themselves.
#####Feature Selection#####
#The first thing we need to do is make our "Infant", "Male", and "Female" predictors factors rather than numerical 
#dummy variables.

abalone$Infant <- factor(abalone$Infant)
abalone$Male <- factor(abalone$Male)

#The second thing we need to do is split our data set into a training set and a test set.  We'll perform feature selection on the training
#set, then evaluate the performance of the model on the held out test set.
set.seed(1)
y <- abalone$Rings
testIndex <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train <- abalone[-testIndex,]
test <- abalone[testIndex,]

MolluskModel <- lm(Rings~.,data = train)
MolluskModel <- stepAIC(MolluskModel, direction = "both", trace = FALSE)
summary(MolluskModel)
#Notice that none of the predictors are eliminated via this process, despite the fact that we know a universal
#model is prone to collinearity from our prior analysis.  Therefore, we will continue our feature selection proces
#with the stepVIF() function.  This function uses VIF to perform step-wise feature selection with a default of 
#10.
MolluskModel <- stepVIF(MolluskModel)
summary(MolluskModel)
vif(MolluskModel)
#Now we have successfully addressed collinearity in our data set.  We see that shell weight is a the most significant
#predictor, followed by height.  
#Also notice that the "Male" predictor specifically does not have a p-value below our conventional threshold of 0.05.  This means that we cannot
#be confident that the "true" coefficient among the entire abalone population between "Male" and "Rings" is not actually 0.
#Given the low correlation between "Rings" and the sex predictors we observed in our exploratory analysis, we can justify removing the "Male" predictor
#as well.

MolluskModel <- lm(Rings~Diameter+Height+ShellWeight+Infant,data = train)
summary(MolluskModel)
#Now we have a statistically significant model with statistically significant predictors, although we notice that we appear to only
#be capturing about 41% of the variance in number of rings with our training model.  Let's move on, keeping in this in mind.
#####Model Evaluation#####
lmPredictions <- predict(MolluskModel, newdata = test, type = "response")
test <- as.data.frame(bind_cols(test, lmPredictions))

colnames(test)[11] <- "Predicted"

lmRMSE <- sqrt(sum((test$Predicted-test$Rings)^2)/length(lmPredictions))
lmRMSE

max(abalone$Rings)
min(abalone$Rings)

abalone %>% ggplot(aes(x = Rings))+
  geom_density(fill = "blue", alpha = 0.2)+
  xlab("Number of Rings")+
  ggtitle("Density Plot of Abalone Ring Count")

#a test RMSE of ~2.4 over this distribution is pretty good, but not as good as the 2.13 we got in the 
#"Random Forest Modeling" file in this repository.
#####Model Interpretation#####
summary(MolluskModel)
#As mentioned above, we have a statistically significant model comprised of statistically significant individual predictors.
#We notice that shell weight and height are the most influential predictors and have positive correlations.  Perhaps most interestingly,
#we see that the "Diameter" predictor has a negative coefficient despite having a positive correlation with "Rings" in our
#correlation matrix above.  We also note that although the diameter predictor is statistically significant, it's p-value is the highest in
#the model.  It indicates that there is a 3.46% chance that the "true" population coefficient for "Diameter" is actually >=0.  This is low, but
#interesting all the same.
#We can do a brief experiment to gain more insight.  Let's make a separate model with all the same predictors minus "Diameter" using the
#training data set and evaluate it on the test set.  We cna then compare test RMSEs to isolate the impact of the diameter predictor.
TestModel <- lm(Rings~Height+ShellWeight+Infant,data = train)
TestPredictions <- predict(TestModel, newdata = test, type = "response")
TestRMSE <- sqrt(sum((TestPredictions-test$Rings)^2)/length(TestPredictions))
TestRMSE
lmRMSE
#Notice that our test RMSE actually increases a little if we do not include the diameter predictor.  We will therefore
#include this predictor in our model.


#####Model Diagnostics#####
finalModel <- lm((Rings)~Diameter+Height+ShellWeight+Infant, data = abalone)
par(mfrow = c(2,2))
plot(finalModel, labels.id = FALSE)
par(mfrow = c(1,1))
#The above created diagnostic plots reveal some interesting information about our model.  Our residuals plot shows no
#distinct pattern between fitted values and residuals, indicating we can rely on our assumptions of linearity and
#homoscedasticity.  Our leverage plot in the lower left shows that we have one particular data point well beyond Cook's distance.
#We do not have enough information to determine that we can remove this data point; so we keep it in.

#Finally, we notice that the qq plot in the upper right hand corner has one outlier at the bottom and an upward curve 
#at the top.  This indicates that we cannot rely on our assumption of normality.  So what are the consequences of the 
#assumption of normality failing?

#According to 
#Statistics Solutions. (2013). Normality.
#Retrieved from https://www.statisticssolutions.com/academic-solutions/resources/directory-of-statistical-analyses/normality/

#we need not worry too much.  Given the size of our data set, we can be assured that via the central limit theorem
#that the distribution of residuals will approximate normality.  We can see this in action by bootstrapping a larger data set later on.
#First, however, we should try another common technique for dealing with this situation; data transformation.

#####Transforming Outcomes#####
set.seed(2)
y <- abalone$Rings
testIndex2 <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train2 <- abalone[-testIndex2,]
test2 <- abalone[testIndex2,]

MolluskModel2 <- lm(log(Rings)~.,data = train2)
MolluskModel2 <- stepAIC(MolluskModel2, direction = "both", trace = FALSE)
summary(MolluskModel2)
vif(MolluskModel2)
#Again, we see that we have high VIF values remaining in our model and must apply the stepVIF() function
MolluskModel2 <- stepVIF(MolluskModel2)
summary(MolluskModel2)
#Again we see that we cannot rely on the "Male" predictor having an impact on the model and therefore remove it.
MolluskModel2 <- lm(log(sqrt(Rings))~Diameter+ShellWeight+Height+Infant, data = abalone)
par(mfrow = c(2,2))
plot(MolluskModel2, labels.id = TRUE)
par(mfrow = c(1,1))
#Notice the residuals plot again shows no evidence of a pattern indicating the transformation still allows us 
#to rely on our assumptions of linearity and homoscedasticity.  Likewise we still see our one leverage point
#beyond Cook's distance, but again leave it in place.
#Finally, we notice that our qq plot looks much better, albeit with some deviation still in place on the upper
#end.  


#Given the deviation we experience after a transformation of the outcome, this avenue does not appear to be the most fruitful.
#Therefore, we move on to our next plan; bootstrapping.  Here we leverage the Central Limit Theorem by supposing that the
#4,177 abalone in our data set represent only a small sample of a larger population (not only a reasonable assumption, but a 
#verifiable fact in this case.) We take 4,177 sample data points (with replacement), average them out, and use that average as 1 row in our new
#data set.  By the definition of the central limit theorem, the distribution of all vectors will be approximately normal, which
#should eliminate our issues with the normality assumption encountered above.




#####Building Bootstrap Model#####
#First, let's re-build the data set for clarity.

data(abalone)
abalone <- abalone %>% mutate(Infant = 0, Male = 0, Female = 0)
abalone$Infant[abalone$Type=="I"] <- 1
abalone$Male[abalone$Type == "M"] <- 1
abalone$Female[abalone$Type == "F"] <- 1

abalone <- abalone %>% dplyr::select(Rings, LongestShell, Diameter, Height, WholeWeight, ShuckedWeight, VisceraWeight, ShellWeight, Infant, Male, Female)





B <- 10000
R <- c()
LS <- c()
D <- c()
H <- c()
WW <- c()
SW <- c()
VW <- c()
ShW <- c()
I <- c()
M <- c()
Fe <- c()
Rings_Bootstrap <- c()
LongestShell_Bootstrap <- c()
Diameter_Bootstrap <- c()
Height_Bootstrap <- c()
WholeWeight_Bootstrap <- c()
ShuckedWeight_Bootstrap <- c()
VisceraWeight_Bootstrap <- c()
ShellWeight_Bootstrap <- c()
Infant_Bootstrap <- c()
Male_Bootstrap <- c()
Female_Bootstrap <- c()

L <- c()
set.seed(3)

for(i in 1:B)
{
  for(j in 1:nrow(abalone))
  {
    k <- sample(1:nrow(abalone), 1, replace = TRUE)
    R[j] <- abalone$Rings[k]
    LS[j] <- abalone$LongestShell[k]
    D[j] <- abalone$Diameter[k]
    H[j] <- abalone$Height[k]
    WW[j] <- abalone$WholeWeight[k]
    SW[j] <- abalone$ShuckedWeight[k]
    VW[j] <- abalone$VisceraWeight[k]
    ShW[j] <- abalone$ShellWeight[k]
    I[j] <- abalone$Infant[k]
    Fe[j] <- abalone$Female[k]
    M[j] <- abalone$Male[k]
  }
  Rings_Bootstrap[i] <- mean(R)
  LongestShell_Bootstrap[i] <- mean(LS)
  Diameter_Bootstrap[i] <- mean(D)
  Height_Bootstrap[i] <- mean(H)
  WholeWeight_Bootstrap[i] <- mean(WW)
  ShuckedWeight_Bootstrap[i] <- mean(SW)
  VisceraWeight_Bootstrap[i] <- mean(VW)
  ShellWeight_Bootstrap[i] <- mean(ShW)
  Infant_Bootstrap[i] <- mean(I)
  Female_Bootstrap[i] <- mean(Fe)
  Male_Bootstrap[i] <- mean(M)
}
Bootstrap <- as.data.frame(cbind(Rings_Bootstrap, LongestShell_Bootstrap, Diameter_Bootstrap, Height_Bootstrap, WholeWeight_Bootstrap, ShuckedWeight_Bootstrap, VisceraWeight_Bootstrap, ShellWeight_Bootstrap, Female_Bootstrap, Infant_Bootstrap, Male_Bootstrap))

#Notice that we are creating one specific bootstrapped data set to simulate a larger population that our initial 
#sample came from.  This is *not* bootstrap aggregation ("bagging") which entails estimating model coefficients via 
#re-sampling.

#####Feature Selection - Bootstrap Model#####
#We'll jump straight into feature selection given that we've already performed our exploratory analysis
#and we would not expect to gain any new information.
#First we break the data into a training set and a test set again.
set.seed(4)
y <- Bootstrap$Rings_Bootstrap
testIndex <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train <- Bootstrap[-testIndex,]
test <- Bootstrap[testIndex,]

#Then we check for collinearity issues with the vif() function.
Bootstrap1 <- lm(Rings_Bootstrap~.,data = train)
vif(Bootstrap1)
#We see that we have an aliased predictor, just as before.
alias(Bootstrap1)
#The fact that it is the "Male" predictor is not terribly surprising, this is likely just due to how the predictors
#are arranged in the data frame, otherwise we would have seen the "Female" or "Infant" predictor be the aliased one given that the are
#set up to be mutually exclusive.
Bootstrap2 <- lm(Rings_Bootstrap~Diameter_Bootstrap+Height_Bootstrap+VisceraWeight_Bootstrap+WholeWeight_Bootstrap+ShuckedWeight_Bootstrap+ShellWeight_Bootstrap+LongestShell_Bootstrap+Infant_Bootstrap+Female_Bootstrap, data = Bootstrap)
vif(Bootstrap2)
#Notice that again we have several predictors above the default R threshold of 10.  We will return to this issue shortly.
Bootstrap3 <- stepAIC(Bootstrap2, direction = "both", trace = FALSE)
summary(Bootstrap3)
#Now let's check for colinearity again using the vif() function.
vif(Bootstrap3)
#We see several VIF values greater than the default R threshold of 10, so we should apply the stepVIF() function
#as a further measure of feature selection.
Bootstrap4 <- stepVIF(Bootstrap3)
summary(Bootstrap4)
#Finally we see that our "Female" predictor has a p-value that does not quite meet the threshold of significance.  This indicates that
#we cannot reject the null hypothesis that the "Female" predictor has no influence on the outcome.  Recall that in the above model, it was the 
#"Female" predictor that was aliased and the "Male" predictor that was excluded for lack of statistical significance.  This is again just a 
#reflection of the order of the columns in the data frames.

Bootstrap5 <- lm(Rings_Bootstrap~Diameter_Bootstrap+Height_Bootstrap+ShellWeight_Bootstrap+Infant_Bootstrap, data = train)


#####Model Evaluation - Bootstrap Model#####
lmPredictions <- predict(Bootstrap5, newdata = test, type = "response")
test <- as.data.frame(bind_cols(test, lmPredictions))

colnames(test)[12] <- "Predicted"

lmRMSE <- sqrt(sum((test$Predicted-test$Rings_Bootstrap)^2)/length(lmPredictions))
lmRMSE
#Notice the RMSE is significantly lower than before.  Strictly speaking, this model is predicting the average
#number of rings on a sample of 4,177 abalone given the average characteristics of that set.  As such we expect the 
#test RMSE to be significantly smaller that our initial model.
#This approach does still let us interpret the predictors of the model, however, and is much more likely to
#stand up to an inspection of the assumptions of linear modeling.

#####Model Interpretation - Bootstrap Model#####
summary(Bootstrap5)
#Although we are using a bootstrapped data set, notice that we are still only explaining about 41% of the 
#variation in number of rings with our model.


#We can still analyze the predictors, however. We first notice that height and shell weight are positive coefficients,
#indicating that the number of rings increases as shell weight and height increase.  We notice that diameter again has a
#negative influence on our predicted number of rings.  Finally, we see that infants are likely to have a smaller
#number of rings, which we expect.

#This analysis is best used for inference given the fact that we got a higher test RMSE with a linear model than with our
#non-parametrics model (random forest, see the "Random Forest" file in this repository).  We also notice the compartively small
#R-squared value when indicating that this analysis should only be used for cautious inference.  The truth is that linear modeling 
#does not always prove to be an appropriate fit for a given problem, which is why the ability to employe multiple techniques is
#important.


#####Model Diagnostics - Bootstrap#####
finalModelBootstrap <- lm(Rings_Bootstrap~Diameter_Bootstrap+Height_Bootstrap+ShellWeight_Bootstrap+Infant_Bootstrap, data = Bootstrap)
par(mfrow = c(2,2))
plot(finalModelBootstrap, labels.id = FALSE)
par(mfrow = c(1,1))
#We clearly see that all of our assumptions of linear modeling are met this time.  Our residuals plot shows
#no sign of a pattern between the residuals and the predicted outcome.  This indicates we can rely on our assumptions of 
#linearity and homoscedasticity.  Our qq plot in the upper right clearly shows that our assumption of linearity holds.
#Agaain, we are technically predicting the average number of rings in a given sample of 4,177 abalone using the attributes
#associated with that sample.  We know that the re-sampling to create the bootstrap data set was done randomly, and 
#we can reasonably assume that the selection of the original sample of 4,177 was done randomly, indicating that we can 
#assume independent observations in this case.
