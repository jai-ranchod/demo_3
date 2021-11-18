#####Introducing Data#####

if (!require("car")) install.packages("car")
if (!require("dplyr")) install.packages("dplyr")
if (!require("pedometrics")) install.packages("pedmetrics")
if (!require("caret")) install.packages("caret")
if (!require("MASS")) install.packages("MASS")
if (!require("corrplot")) install.packages("corrplot")
library(corrplot)
library(dplyr)
library(car)
library(pedometrics)
library(caret)
library(MASS)
#Here we try to predict the gas mileage of various cars using the mtcars data set that is built into R

data("mtcars")
str(mtcars)

#Clear definition of the attributes in the mtcars data set can be found here:
#https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/mtcars.html
#But we will also show a brief introduction here:
#mpg	Miles/(US) gallon - this one is the outcome
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

#####Exploratory Analysis#####
#First, let's create a correlation matrix to get a visual representation of the relationships between
#predictors and the outcome.
mtcars <- mtcars[order(mtcars$mpg),]
Matrix <- cor(mtcars)
corrplot(Matrix, method = "color", order = "FPC")
#Our first correlation matrix is ordered by first principle component.  For the next matrix, we'll order by
#angular order of eigenvectors or "AOE".
corrplot(Matrix, method = "color", order = "AOE")

#Finally, we'll use the default values so you can get a closer look at individual predictor/outcome relationships
#a little more cleanly.
corrplot(Matrix)


#First, we notice that there are several predictors that have a strong relationship with the outcome, such as number of cylinders,
#engine displacement, number of cylinders, weight, and v/inline engine.  We also notice that some predictors have relationships with 
#each other such as weight and engine displacement, and number of cylinders and v/inline engine, for example.
#Another thing we can do to get a sense of the relationship between predictors is create a universal model and give it as an
#argument to the vif() function.  the vif() function computes variance inflation factor calculations for each predictor in a given
#model.  For reference the variance inflation factor of a predictor i is defined as:
#VIF_i = 1/(1-(R_i)^2)
#where (R_i)^2 is the coefficient of determination of the regression of all other predictors onto the ith predictor.
vfModel <- lm(mpg ~.,data = mtcars)
vif(vfModel)
plot(vif(vfModel), xlab = "Index of Predictors", ylab = "VIF", main = "VIF of Predictors")
abline(h=10, col = "red")
abline(h=5, col = "red")
#The default threshold for eliminating predictors from a model based on VIF in the stepVIF() function in R is 10.  We see
#that our universal model has 3 predictors over 10, and another 4 between 5 and 10.  We should keep an eye on this and definitely
#check the final model for signs of collinearity.
#Having gained some insight into our data set, lets proceed to training our model.

#####Feature Selection####
#In order to prepare our data set for linear model training we should make our categorical variables factors.
df <- mtcars
df$am <- factor(df$am)
df$vs <- factor(df$vs)

#Next, we need to splot our data set into a training set and a test set.  We train the model on the training set, then
#evaluate our performance against the test set.
set.seed(1)
y <- df$mpg
testIndex <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train <- mtcars[-testIndex,]
test <- mtcars[testIndex,]

carsModel <- lm(mpg~.,data = train)
carsModel <- stepAIC(carsModel, direction = "both", trace = FALSE)
summary(carsModel)
#Note that we are using the stepAIC() function to perform feature selection.  This function performs step-wise feature
#selection using the Akaike Information Criterion.  Learn more using
?stepAIC
#Now that we've created our model, let's see how it performs on the held-out validation set.

#####Model Evaluation#####
lmPredictions <- predict(carsModel, newdata = test, type = "response")
test <- as.data.frame(bind_cols(test, lmPredictions))
colnames(test)[12] <- "Predicted"

lmRMSE <- sqrt(sum((test$Predicted-test$mpg)^2)/length(lmPredictions))
lmRMSE

max(df$mpg)
min(df$mpg)


#Here we see an RMSE of ~2.93 over the distribution of outcomes shown in the density plot below, 
#so we can see we've built a good model.

df %>% ggplot(aes(x = mpg))+
  geom_density(fill = "blue", alpha = 0.2)+
  ggtitle("Distribution of mpg")+
  xlab("mpg")+
  theme_bw()
#Note that the test RMSE in the "Random Forest Modeling" file of this repository is ~2.43, indicating that 
#a random forest is a better model for predicting out of sample mpg.
#####Model Interpretation#####
summary(carsModel)

#Let's now interpret the model fit to analyze what influences miles per gallon.

#First we notice that we are explaining about 84% of the variance in the outcome
#with our model, and a clearly statistically significant F-statistic, which is a great start.
#Next we notice that we only have two predictors finally included.  We see that as we
#increase horsepower and weight, fuel efficiency decreases.  We also note that as we move from automatic to
#manual transmission, we gain fuel efficiency.

#It may be surprising that there only three out of the original ten predictors which became part of
#the final model.  Examining the principal component analysis in the
#"Principal Component Analysis" file in this repository will help bring clarity.
#Basically the number of factors (or principal components) required to explain the 
#variation in miles per gallon is quite small, and we know that moving from automatic to manual transmission
#increases fuel efficiency (although that's not as true now as it once was), but as a binary predictor this is not included in the principle component analysis.
#https://www.consumerreports.org/cro/2012/01/save-gas-and-money-with-a-manual-transmission/index.htm#:~:text=In%20our%20tests%2C%20we've,also%20improve%20acceleration%2C%20sometimes%20significantly.
















#####Model Diagnostics#####
finalModel <- lm(mpg~hp+wt+factor(am), data = mtcars)
par(mfrow = c(2,2))
plot(finalModel, labels.id = FALSE)
par(mfrow = c(1,1))
#Our residuals plot indicates no specific pattern linking residuals to fitte values, implying that our assumptions of 
#homoscedasticity and linearity are intact.  Our qq plot similarly indicates that we can rely on our assumption of 
#normality.  The variation in manufacturers, makes, and models of the cars implies that we can reasonably assume independent
#data points.  Finally, we also note that we do not appear to have any residuals beyond Cook's distance in the lower
#left hand corner.

#We also need to return to the issue of collinearity that we set aside earlier.  We again use the vif() function
#to evaluate the possibility of collinearity in the model.
vif(finalModel)
#We see relatively low VIF values, which are well under our default threshold and certianly under the values we saw previously.
#We can be confident that we do not have issues with collinearity in this model.

#Note that we used our complete data set here.  This is because of the objective of creating diagnostic plots.  
#If we want to ensure a linear relationship between our outcome and our chosen predictors, it's better to look at
#the full data set.  Likewise, if we want to check that the error terms between prediction and actual value are normally distributed
#it is better to use the entire data set. The train-test split is designed to allow us to perform feature selection and
#evaluate our model without having to worry about over-fitting.  For the diagnostic purpose of evaluating assumptions
#of linear modeling, we can use the entire data set.  That is to say, we would use the model "finalModel" to predict mpg of 
#any car outside of this data set.



