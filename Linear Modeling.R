#####Introducing Data#####

if (!require("car")) install.packages("car")
if (!require("dplyr")) install.packages("dplyr")
library(dplyr)
library(car)
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

#####Assumptions of Linear Modeling####
#To check the assumption of no collinearity among our predictors, we use variance inflation factors, specifically the stepVIF() function.
#The stepVIF() function uses a step-wise approach to feature selection based on 
#variance inflation factors.  The default threshold is 10, which we leave in place here.  For reference
#the variance inflation factor of a predictor i is defined as:
#
#VIF_i = 1/(1-(R_i)^2)
#where (R_i)^2 is the coefficient of determination of the regression of all other predictors onto the ith predictor.

vfModel <- lm(mpg ~.,data = mtcars)
vif(vfModel)
stepVIF(vfModel)
#Our variance inflation factor analysis shows that the cylinder, displacement, and weight predictors exceed the threshold and should be
#removed from the model.
#The information contained in these predictors is thought to be contained in the rest of the predictors, hence the high VIF scores.
#Therefore, we really don't lose information, and we are reducing the potential for error attributable to variance.
df <- mtcars %>% select(mpg, hp, drat, qsec, vs, am, gear, carb)

par(mfrow = c(2,2))
plot(vfModel)
par(mfrow = c(1,1))
#The residuals plot in the upper left-hand corner is not perfect, but should not severally violate our linearity and homoscedasticity assumptions.
#The qq plot in the upper right hand corner shows us that we are meeting our normality assumption as well.
#We also notice that our residuals vs. leverage plot shows a potential leverage point concern with the Ford Pantera L
#We can plot Cooks Distance to investigate further.  To avoid misleading axis lengths we plot the original
#default, then also a plot on a fixed scale of 0-1

plot(cooks.distance(vfModel), main = "Cook's Distance Plot")

plot(cooks.distance(vfModel), ylim=c(0,1), main = "Cook's Distance Plot")

#We have definitely identified an anomalous data point, but we have no particularly good reason to remove the data point.
#We will leave the data point in, and keep it in mind throughout our analysis.
#####Training Model#####
#Here we split the data into a training set and a test set.  We will train the model
#on the larger training set, then evaluate its performance on the held out test set.
#This helps us avoid over fitting the model.
#We will select which features to include in this model using the step() function which 
#performs backward step-wise feature selection using the Akaike Information Criterion.
set.seed(1)
y <- df$mpg
testIndex <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train <- df[-testIndex,]
test <- df[testIndex,]

set.seed(2)
LinearModel <- lm(mpg~.,data = train)
LinearModel <- step(LinearModel)

#####Model Evaluation and Interpretation#####
lmPredictions <- predict(LinearModel, newdata = test, type = "response")
test <- as.data.frame(bind_cols(test, lmPredictions))
colnames(test)[9] <- "Predicted"

lmRMSE <- sqrt(sum((test$Predicted-test$mpg)^2)/length(lmPredictions))
lmRMSE

#Here we see an RMSE of ~3.21 over a range of outcomes of 23.5, so we've built
#a good model.  
#Let's now interpret the model fit to analyze what influences miles per gallon.
summary(LinearModel)
#First we notice that we are explaining about 78% of the variance in the outcome
#with our model, and a clearly statistically significant F-statistic, which is a great start.
#Next we notice that we only have two predictors finally included.  We see that as we
#increase horsepower, fuel efficiency decreases, and as we move from an automatic
#transmission to a manual transmission, we expect to increase miles per gallon by slightly
#over 5.

#It may be surprising that there only two out of the original ten predictors became part of
#the final model.  Examining the principal component analysis in the
#"Principal Component Analysis" file in this repository will help bring clarity.
#Basically the number of factors (or principal components) required to explain the 
#variation in miles per gallon is quite small, and we know transmission significantly
#impacts gas mileage as well.










