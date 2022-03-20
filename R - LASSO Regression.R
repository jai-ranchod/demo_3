#####Introduction#####
if (!require("ISLR")) install.packages("ISLR")
if (!require("glmnet")) install.packages("glmnet")

library(ISLR)
library(glmnet)
#We'll be using the "Hitters" data set found in the ISLR library to illustrate the Lasso technique for
#linear model regularization.  Specifically, we'll be modeling salary.  The "Hitters" data set contains baseball statistics for a wide variety of 
#players from the 1986 and 1987 seasons.  The statistics should be self explanatory with the caveat that 
# the letter "C" in front of a statistic indicates it is a career total number. Use ?Hitters to learn more.

#The Lasso technique is similar to ridge regression in that both add a term to the cost function that penalizes
#model complexity by adding a term to the cost function that is proportional to some measure of the magnitude of the 
#coefficient vector.  In ridge regression this is the l2 norm, in the lasso it is the l1 norm.  The advantage of the 
#lasso is that it will actually perform feature selection.  That is, it will shrink some predictors coefficients down to
#zero, which does not happen in ridge regression.

#####Creating Training Set#####
data(Hitters)
mean(is.na(Hitters))
#With less than 1% of our data listed as NA, we can remove the NA rows with the na.omit() function
Hitters <- na.omit(Hitters)
#First, we use the convenient model.matrix() function to expand our factors into dummy variables.
x <- model.matrix(Salary~., Hitters)[,-1]
y <- Hitters$Salary

#The we make a correlation matrix.  There are quite a few variables, so I recommend expanding the plot.
Matrix <- cor(x)
corrplot(Matrix, method = "color", order = "FPC")
#We see that we have multicollinnearity to deal with.  Fortunately, the LASSO technique is well suited to
#handle this kind of situation.  


set.seed(1)
train <- sample(1:nrow(x), 0.8*nrow(x))
#notice we are using a 1/5 to 4/5 test to train ratio here
test <- (-train)
y.test <- y[test]
x.test <- x[test,]
y.train <- y[train]
x.train <- x[train,]

#####Fitting Model#####
#Next we use the glmnet() function to fit the lasso model.  Note that when using the glmnet() function, the parameter
#alpha is the mixing parameter for the penalty function where alpha = 1 implies a lasso (which is the default).
#Note that we must also define a grid of tuning parameters over which we search of the optimal lambda.

grid <- 10^seq(10,-3,length=100)
#We scan all the way from 10^10 (essentiall null model) to 10^(-2) (essentially a traditional least squares model)

lasso.model <- glmnet(x.train, y.train, lambda = grid)

plot(lasso.model, "lambda", label = TRUE)
#Notice that our choice of lambda will clearly impact the model.  AS we increase the(log) level of lambda
#one can clearly see the coefficient values collapsing to 0.  Also notice the number of non-zero coefficients remaining
#in the model for values of (log) lambda going across the top.

k <- as.data.frame(cbind(lasso.model$df, lasso.model$lambda))
colnames(k)[1] <- "df"
colnames(k)[2] <- "lambda"

#If we apply the same scale to data from the model output, we can see as the optimal degrees of freedom
#in the model declines as (log) lambda increases.

k %>% ggplot(aes(x = log(lambda), y = df))+
  geom_point()+
  xlim(c(0,10))+
  ggtitle("Degrees of Freedom by \n Tuning Parameter")+
  theme_bw()

#Now let's apply cross-validation to determine the optimal lambda value.  The default number of folds is 10
#in the cv.glmnet() function which should be fine.
set.seed(2)
cv.output <- cv.glmnet(x.train, y.train, alpha = 1)
plot(cv.output)
#Here we see that as we increase (log) lamdba, our cross-validation MSE slightly decreases until dramatically
#increasing.  Also notice the two dashed vertical lines.  The line on the left marks the log(lambda) value
#for which we have the minimum MSE.  Again, the numbers across the top of the graph show the numebr of non=zero
#coefficients remaining for that value of (log) lambda.

cv.output$lambda.min

#The line on the right shows the (log) lambda value that is the highest we can go while still keeping the error
#within 1 standard error of the minimum.  This is known as the "1se" value.

#####Generating Predictions#####

#Let's now take our optimized regularization value and make predictions for our held out test set.

preds <- predict(lasso.model, s=cv.output$lambda.min, newx = x.test)
sqrt(mean((preds-y.test)^2))

#Let's review the coefficients of the final model.

final <- glmnet(x,y,alpha = 1, lambda = grid)
lasso.coef <- predict(final, type = "coefficients", s = cv.output$lambda.min)[1:20,]
lasso.coef

#Notice that 12 of the coefficients were shrunk entirely to zero.  Before we go on with interpreting the
#model, let's check a residuals plot.

#####Analyzing Assumptions of Linearity#####

#We should look at a residuals plot to ensure our we are meeting our assumptions of linearity and
#homoscedasticity.

lasso.coef[lasso.coef !=0]

test.model <- glmnet(x,y,alpha = 1, lambda = cv.output$lambda.min)
y.hat <- predict(test.model, s=cv.output$lambda.min, newx = x)

errordf <- as.data.frame(cbind(y, y.hat))
colnames(errordf)[1] <- "Y"
colnames(errordf)[2] <- "Y_Hat"
errordf <- errordf %>% mutate(res = Y-Y_Hat)

errordf %>% ggplot(aes(x = Y_Hat, y = res))+
  geom_point()
#We notice a clear "fanning out" of the residuals, albeit in a relatively symmetric way.  This means we
#got halfway there.  Our assumption of linearity is intact, but we are clearly not meeting our assumption of 
#homoscedasticity.  

#####Transforming the Outcome#####
#One of the potential remedies for heteroscedasticity is a tranformation of the outcome (source linked)

#https://academic.macewan.ca/burok/Stat378/notes/remedies.pdf

#This kind of transformation will come at the cost of some interpretability of our model, but the fit 
#may be much better.  Let's find out.
#I'll do this one in a compact format given that I've already provided commentary.

data(Hitters)
Hitters <- na.omit(Hitters)
x.log <- model.matrix(Salary~., Hitters)[,-1]
y.log <- log(Hitters$Salary)
#^the only difference is the log transformation
set.seed(3)
train.log <- sample(1:nrow(x.log), 0.8*nrow(x.log))
test.log <- (-train.log)
y.test.log <- y[test.log]
x.test.log <- x[test.log,]
y.train.log <- y[train.log]
x.train.log <- x[train.log,]

set.seed(4)
cv.output.log <- cv.glmnet(x.train.log, y.train.log, alpha = 1)
cv.output.log$lambda.min

grid <- 10^seq(10,-3,length=100)
final.log <- glmnet(x.log,y.log,alpha = 1, lambda = grid)
lasso.coef.log <- predict(final.log, type = "coefficients", s = cv.output.log$lambda.min)[1:20,]
lasso.coef.log

test.model.log <- glmnet(x.log,y.log,alpha = 1, lambda = cv.output.log$lambda.min)
y.hat.log <- predict(test.model.log, s=cv.output.log$lambda.min, newx = x.log)
errordf.log <- as.data.frame(cbind(y.log, y.hat.log))
colnames(errordf.log)[1] <- "Y"
colnames(errordf.log)[2] <- "Y_Hat"
errordf.log <- errordf.log %>% mutate(res = Y-Y_Hat)
errordf.log %>% ggplot(aes(x = Y_Hat, y = res))+
  geom_point()+
  xlab("Predicted Value")+
  ylab("Residual")+
  ggtitle("Residual Plot - Log \n Transformed Outcome")+
  theme_bw()
#By making the transformation of the outcome, we are sacrificing some model interpretability for
#model fit.  In this case, it appears to be worth it as the original residuals plot showed quite 
#clear heteroscedasticity.

final.log <- glmnet(x.log,y.log,alpha = 1, lambda = grid)
lasso.coef.log <- predict(final.log, type = "coefficients", s = cv.output.log$lambda.min)[1:20,]
lasso.coef.log

#Notice that predictors such as "At Bat", "RBI", "Career Walks", "New League 'National'", "Career Home Runs",
#"Career At Bats", and "Assists are all reduced to 0.  Unsurprisingly we see that errors has a negative influence,
#as does being in a western division interestingly.  We also notice that "Years" has a significant influence
#on the outcome, which may have contributed to our heteroscedasticity earlier.  The longer players stay in the
#league, the more their salaries get spread out as the become "known" entities, while generally rising above their
#rookie salaries.  At the same time, salaries for younger players are understandably much more tightly
#packed together.  Aside from that, we only note that national league players generally get paid more and the
#rest of the non-zero predictors are ones that a casual baseball fan might predcit like home runs, hits, etc.


