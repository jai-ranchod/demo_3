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
#Here we build a model to relate gas mileage to the other features in the dataset.
#Note that the purpose of this script is to evaluate the assumptions of linear modeling on
#a fitted linear model.

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
Matrix <- cor(mtcars)
corrplot(Matrix, method = "color", order = "FPC")
#Our first correlation matrix is ordered by first principle component.  For the next matrix, we'll order by
#angular order of eigen vectors or "AOE".
corrplot(Matrix, method = "color", order = "AOE")

#Finally, we'll use the default values so you can get a closer look at individual predictor/outcome relationships
#a little more cleanly.
corrplot(Matrix)


#First, we notice that there are several predictors that have a strong relationship with the outcome, such as number of cylinders,
#engine displacement, number of cylinders, weight, and v/inline engine.  We also notice that some predictors have relationships with 
#each other such as weight and engine displacement, and number of cylinders and v/inline engine, for example.
#Another thing we can do to get a sense of the relationship between predictors is create a universal model and give it as an
#argument to the vif() function.  The vif() function computes variance inflation factor calculations for each predictor in a given
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

#Next, we need to plot our data set into a training set and a test set.  We train the model on the training set, then
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

lmRMSE <- sqrt(mean((test$Predicted-test$mpg)^2))#/length(lmPredictions))
lmRMSE

max(df$mpg)
min(df$mpg)


#Here we see an RMSE of ~4.26 over the distribution of outcomes shown in the density plot below, 
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

#First we notice that we are explaining about 81% of the variance in the outcome
#with our model, and a clearly statistically significant F-statistic, which is a great start.
#Next we notice that we only have two predictors finally included.  We see that as we
#increase horsepower, fuel efficiency decreases.  We also note that as we move from automatic to
#manual transmission, we gain fuel efficiency.

#It may be surprising that there only five out of the original ten predictors which became part of
#the final model.  Examining the principal component analysis in the
#"Principal Component Analysis" file in this repository will help bring clarity.
#Basically the number of factors (or principal components) required to explain the 
#variation in miles per gallon is quite small, and we know that moving from automatic to manual transmission
#increases fuel efficiency (although that's not as true now as it once was), but as a binary predictor this is not included in the principle component analysis.
#https://www.consumerreports.org/cro/2012/01/save-gas-and-money-with-a-manual-transmission/index.htm#:~:text=In%20our%20tests%2C%20we've,also%20improve%20acceleration%2C%20sometimes%20significantly.



#####Model Diagnostics#####
finalModel <- lm(mpg~drat+wt+qsec+factor(vs)+factor(am), data = mtcars)
par(mfrow = c(2,2))
plot(model, labels.id = FALSE)
par(mfrow = c(1,1))
#Assumption #1: Linearity: 
#We check the residuals plot (upper left) looking for any specific pattern in the residuals that might
#indicate our assumption of linearity is not reliable.  we clearly see a pattern present, indicating we have failed on this count.

#Assumption #2: Normality Assumption (residuals are normally distributed):
#We check the qq-plot (upper right) to determine whether or not the residuals are normally distributed.
#Given that a straight line indicates normal residual distribution, we cannot safely make that assumption 
#given the distortion of the data at the high and low ends of the plot.

#Assumption #3: Homoscedasticity:
#We check the Scale-Location plot (lower left) hoping to see a pattern of points that is generally evenly
#spaced with a horizontal red line.  Not seeing this, we seem to fail on the assumption of homoscedasticity as well.

#Assumption #4: Collinearity
#We assume that our predictors to not significantly correlate with each other.  To evaluate this assumption, we 
#recall the vif() function
vif(finalModel)
#We see relatively low VIF values, which are well under our default threshold and certainly under the values we saw previously.
#We can be confident that we do not have issues with collinearity in this model.

#Additional Consideration: Outliers
#Although it is not always considered one of the core assumptions of linear modeling, one should
#be on the lookout for influential outliers that can cause issues with the model.  To investigate
#this, we look at the Residuals vs Leverage plot in the lower right.  Notice that we do indeed have points
#out beyond Cook's distance, indicating that we have outliers that we should be mindful of.


#Note that we fail several of the assumptions of linear modeling here. This indicates to us that a 
#linear model is perhaps not the best way to go about estimating gas mileage with this data.
#Find the "R - Random Forest Modeling - mtcars data" file in this repository to see a better estimation
#approach applied to this data.



