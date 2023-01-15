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
#The conventional VIF threshold, above which a feature may need to be removed from the model, is 10:
#https://quantifyinghealth.com/vif-threshold/#:~:text=Most%20research%20papers%20consider%20a,of%205%20or%20even%202.5.

#Here we note that "cyl", "disp", and "wt" are above this threshold.
#We can also use the stepVIF() function on bootstrapped data. This function removes features from a model on the basis
#of VIF score in a step-wise fashion.  

k <- 100
v <- c()
for(i in 1:k)
{
  set.seed(seed = i) 
  vifdf <- data.frame()
  for(j in 1:nrow(mtcars))
  {
    n <- sample(1:nrow(mtcars),size = 1,replace = TRUE)
    vifdf <- rbind(vifdf, mtcars[n,])
  }
  vfModel <- lm(mpg ~.,data = vifdf)
  vfModel <- stepVIF(vfModel)
  
  vfterms <- attr(vfModel$terms, "term.labels")
  v <- c(v,vfterms)
  print(i)
}


hp <- sum(v == "hp")/k
drat <- sum(v == "drat")/k
wt <- sum(v == "wt")/k
qsec <- sum(v == "qsec")/k
vs <- sum(v == "vs")/k
am <- sum(v == "am")/k
gear <- sum(v == "gear")/k
carb <- sum(v == "carb")/k
cyl <- sum(v == "cyl")/k
disp <- sum(v == "disp")/k


hp
drat
wt
qsec
vs
am
gear
carb
cyl
disp
#Here we see that "cyl" and "disp" appear in very few models, and "qsec" also appears in fewer than half of the models.
#Given that both methods indicate that "cyl" and "disp" should be removed we can take them out of our model and move on.

#Having gained some insight into our data set, lets proceed to training our model.

#####Feature Selection####
#In order to prepare our data set for linear model training we should make our categorical variables factors.
df <- mtcars
df$am <- factor(df$am)
df$vs <- factor(df$vs)

#Next, we need to plot our data set into a training set and a test set.  We train the model on the training set, then
#evaluate our performance against the test set.
set.seed(2)
y <- df$mpg
testIndex <- createDataPartition(y, times = 1, p = 0.2, list = FALSE)

train <- mtcars[-testIndex,]
test <- mtcars[testIndex,]
k <- 1000
v <- c()
for(i in 1:k)
  
{
  set.seed(seed = i+2) #we used seed equals one, two above
  traindf <- data.frame()
  for(j in 1:nrow(train))
  {
    #set.seed(seed = 100 + j)
    n <- sample(1:nrow(train),size = 1,replace = TRUE)
    
    traindf <- rbind(traindf, train[n,])
  }
  
  #Notice we do not include "cyl" and "disp" as both collinearity analysis approaches implied their removal.
  carsModel <- lm(mpg ~ hp + drat + qsec + vs + am + gear + carb + wt,data = traindf)
  carsModel <- step(carsModel, trace = 0)
  
  m <- attr(carsModel$terms, "term.labels")
  v <- c(v,m)
  print(i)
}




hp <- sum(v == "hp")/k
drat <- sum(v == "drat")/k
wt <- sum(v == "wt")/k
qsec <- sum(v == "qsec")/k
vs <- sum(v == "vs")/k
am <- sum(v == "am")/k
gear <- sum(v == "gear")/k
carb <- sum(v == "carb")/k


hp
drat
wt
qsec
vs
am
gear
carb

#We see that the following features are included in a majority of models and will thus
#be included in our final model:
#drat
#wt
#qsec
#vs
#am

summary(carsModel)
carsModelFinal <- lm(mpg ~ drat + wt + qsec + vs + am, data = train)
#Note that we are using the step() function to perform feature selection.  This function performs step-wise feature
#selection using the Akaike Information Criterion.  Learn more using
# ?step()
#Now that we've created our model, let's see how it performs on the held-out validation set.

#####Model Evaluation#####
lmPredictions <- predict(carsModelFinal, newdata = test, type = "response")
test <- as.data.frame(bind_cols(test, lmPredictions))
colnames(test)[12] <- "Predicted"

lmRMSE <- sqrt(mean((test$Predicted-test$mpg)^2))#/length(lmPredictions))
lmRMSE

max(df$mpg)
min(df$mpg)


#Here we see a reasonable RMSE over the distribution of outcomes shown in the density plot below, 
#so we can see we've built a decently good model.

df %>% ggplot(aes(x = mpg))+
  geom_density(fill = "blue", alpha = 0.2)+
  ggtitle("Distribution of mpg")+
  xlab("mpg")+
  theme_bw()
#Note that the test RMSE in the "Random Forest Modeling" file of this repository is ~2.43, indicating that 
#a random forest is a better model for predicting out of sample mpg.
#####Model Interpretation#####
summary(carsModelFinal)

#Let's now interpret the model fit to analyze what influences miles per gallon.

#First we notice that we are explaining about 81% of the variance in the outcome
#with our model, and a clearly statistically significant F-statistic, which is a great start.
#Next we notice that we only have five predictors finally included.  

#Notice that as the rear axle ratio (drat) increases, so does our prediction of fuel efficiency.
#The same can be said for quarter mile time (qsec) which intuitively makes sense.
#We know that moving from automatic to manual transmission increases fuel efficiency (although that's not as true now as it once was).
#https://www.consumerreports.org/cro/2012/01/save-gas-and-money-with-a-manual-transmission/index.htm#:~:text=In%20our%20tests%2C%20we've,also%20improve%20acceleration%2C%20sometimes%20significantly.

#We see that as weight increases the predicted fuel efficiency drops which again makes intuitive sense.
#Finally, we note that as we move from a v-shaped engine to an inline engine our predicted fuel efficiency drops.

#It may be surprising that there only five out of the original ten predictors which became part of
#the final model.  Examining the principal component analysis in the
#"Principal Component Analysis" file in this repository will help bring clarity.
#Basically the number of factors (or principal components) required to explain the 
#variation in miles per gallon is fairly small.



#####Model Diagnostics#####
finalModel <-  lm(mpg ~ drat + wt + qsec + vs + am, data = mtcars)
par(mfrow = c(2,2))
plot(finalModel, labels.id = FALSE)
par(mfrow = c(1,1))

#Now we check the 4 basic assumption of linear modeling found here
#https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/R/R5_Correlation-Regression/R5_Correlation-Regression4.html
#as well as a few additional considerations.

#Assumption #1: Linearity: 
#We check the residuals plot (upper left) looking for any specific pattern in the residuals that might
#indicate our assumption of linearity is not reliable.  We notice a slight "u-shaped" pattern here that is not
#dramatically out of line but does indicate taht we may not have a linear phenomena at play here.

#Assumption #2: Homoscedasticity:
#We check the Scale-Location plot (lower left) hoping to see a pattern of points that is generally evenly
#spaced with a horizontal red line.  Seeing instead a gradually increasing trend, we seem to fail on the
#assumption of homoscedasticity as well.

#Assumption #3: Independence:
#Given the variance in make and model it is reasonable to assume that our selection represents an independent
#representation of cars.

#Assumption #4: Normality Assumption (residuals are normally distributed):
#We check the qq-plot (upper right) to determine whether or not the residuals are normally distributed.
#We see a distrotion in the upper right hand side of the plot, but it does not appear to be anything severe.


#Additional Considerations
#1: Outliers
#Although it is not always considered one of the core assumptions of linear modeling, one should
#be on the lookout for influential outliers that can cause issues with the model.  To investigate
#this, we look at the Residuals vs Leverage plot in the lower right.  Notice that we do indeed have points
#out beyond Cook's distance, indicating that we have outliers that we should be mindful of.

#2: Collinearity
#We assume that our predictors to not significantly correlate with each other.  To evaluate this assumption, we 
#recall the vif() function
vif(finalModel)
#We see relatively low VIF values, which are well under our default threshold and certainly under the values we saw previously.
#We can be confident that we do not have issues with collinearity in this model.



#Note that we fail several of the assumptions of linear modeling here. This indicates to us that a 
#linear model is perhaps not the best way to go about estimating gas mileage with this data.
#Find the "R - Random Forest Modeling - mtcars data" file in this repository to see a better estimation
#approach applied to this data.

#####Conclusion#####
#All things considered it looks like a simple linear model may not be the best way forward to estimate miles per gallon in this
#dataset.  One could initiate transformations to make some of the diagnostics of linear modeling come more into line.  However,
#I will pivot to another model type found here:
# https://github.com/jai-ranchod/demo_3/blob/main/R%20-%20Random%20Forest%20Modeling%20-%20mtcars%20data.R
#which we see produces a significantly better RMSE.


