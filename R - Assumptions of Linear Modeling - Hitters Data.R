#####Introduction#####

if (!require("ISLR")) install.packages("ISLR")
if (!require("glmnet")) install.packages("glmnet")

library(ISLR)
library(glmnet)

#We'll be using the "Hitters" data set found in the ISLR library to illustrate the Lasso technique for
#linear model regularization.  Specifically, we'll be modeling salary.  The "Hitters" data set contains baseball
#statistics for a wide variety of players from the 1986 and 1987 seasons.  The statistics should be self explanatory 
#with the caveat that  the letter "C" in front of a statistic indicates it is a career total number. Use ?Hitters to 
#learn more.

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
#We see that we have some multicollinnearity to deal with.  Fortunately, the LASSO technique is well suited to
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

grid <- 10^seq(10,-2,length=10000)
#We scan all the way from 10^10 (essentially null model) to 10^(-2) (essentially a traditional least squares model)

#Now let's apply cross-validation to determine the optimal lambda value.  The default number of folds is 10
#in the cv.glmnet() function which should be fine.
set.seed(2)
cv.output <- cv.glmnet(x.train, y.train, lambda = grid) #alpha = 1 is default value
plot(cv.output)
#Here we see that as we increase (log) lamdba, our cross-validation MSE slightly decreases until dramatically
#increasing.  Also notice the two dashed vertical lines.  The line on the left marks the log(lambda) value
#for which we have the minimum MSE.  Again, the numbers across the top of the graph show the number of non-zero
#coefficients remaining for that value of (log) lambda.

cv.output$lambda.min

#The line on the right shows the (log) lambda value that is the highest we can go while still keeping the error
#within 1 standard error of the minimum.  This is known as the "1se" value.

#####Generating coefficients#####

final <- glmnet(x,y,alpha = 1, lambda = cv.output$lambda.min)
lasso.coef <- predict(final, type = "coefficients")[1:20,]
lasso.coef

####Plotting Regularization####

#Here we make a plot to observe the effect of regularization on coefficient value
lasso.mod <- glmnet(x,y,alpha = 1, lambda = grid)
beta=coef(lasso.mod)
tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- lasso.mod$lambda[tmp$variable+1] # extract the lambda values
tmp$norm <- apply(abs(beta[-1,]), 2, sum)[tmp$variable+1] # compute L1 norm


ggplot(tmp[tmp$coef != "(Intercept)",], aes(lambda, value, color = coef, linetype = coef)) + 
  geom_line() + 
  scale_x_log10() + 
  xlab("Lambda (log scale)") + 
  guides(color = guide_legend(title = ""), 
         linetype = guide_legend(title = "")) +
  theme_bw() + 
  theme(legend.key.width = unit(3,"lines"))

####Creating Linear Model####

#L1 and L2 regularization are, after all, just methods of helping us find the best beta (coefficient) vector.
#As such, let's return to our linear modeling framework.
s <- as.data.frame(lasso.coef[which(lasso.coef != 0)])
colnames(s)[1] <- "Coefficient"
xFrame <- as.data.frame(x) #x - optimized matrix helping with categorical variables from above.

intercept <- rep(s["(Intercept)",], length(y))
hits <- rep(s["Hits",], nrow(x))
walks <- rep(s["Walks",], nrow(x))
c_runs <- rep(s["CRuns",], nrow(x))
c_rbi <- rep(s["CRBI",], nrow(x))
leaguen <- rep(s["LeagueN",], nrow(x))
putouts <- rep(s["PutOuts",], nrow(x))
divisionw <- rep(s["DivisionW",], nrow(x))


xFrame$Hits <- xFrame$Hits*hits
xFrame$Walks <- xFrame$Walks*walks
xFrame$CRuns <- xFrame$CRuns*c_runs
xFrame$CRBI <- xFrame$CRBI*c_rbi
xFrame$LeagueN <- xFrame$LeagueN*leaguen
xFrame$DivisionW <- xFrame$DivisionW*divisionw
xFrame$PutOuts <- xFrame$PutOuts*putouts
df <- as.data.frame(cbind(Hitters$Salary, xFrame$Hits, xFrame$Walks, xFrame$CRuns, xFrame$CRBI, xFrame$LeagueN, xFrame$DivisionW, xFrame$PutOuts))
colnames(df) <- c("Salary",                "Hits",      "Walks",     "c_Runs",     "c_RBI",    "NationalLeague", "DivisionWest", "Putouts")

t <- lm(Salary ~ . , data = df)
vif(t)
#This is interesting as LASSO regularization is usually pretty good about mitigating multicollinearity effects.
#Note that VIF scores are still below the traditional threshold of 10, but are still a little higher than
#we might expect. That is why it is always important to analyze the assumptions of your model, which we get into
#below.

par(mfrow = c(2,2))
plot(t, labels.id = FALSE)
par(mfrow = c(1,1))

#####Analyzing Assumptions of Linearity#####

#Assumption #1: Linearity: 
#We notice that we have a clear "fanning out" of the residuals, albeit in a relatively symmetric way. 
#We can therefore keep our assumption of linearity intact, but we may have an issue with homoscedasticity.

#Assumption #2: Homoscedasticity:
#We check the Scale-Location plot (lower left) hoping to see a pattern of points that is generally evenly
#spaced with a horizontal red line.  Seeing instead a gradually increasing trend, we seem to fail on the
#assumption of homoscedasticity, as was indicated in the residual plot.

#Assumption #3: Independence:
#Given the variance in individuals, stadiums, and teams, it is reasonable to assume that our selection represents an independent
#representation of players.

#Assumption #4: Normality Assumption (residuals are normally distributed):
#We check the qq-plot (upper right) to determine whether or not the residuals are normally distributed.
#We see a distortion in the upper right hand side of the plot, indicating that we may not meet our normality 
#assumption either.

#Additional Considerations
#1: Outliers
#Although it is not always considered one of the core assumptions of linear modeling, one should
#be on the lookout for influential outliers that can cause issues with the model.  To investigate
#this, we look at the Residuals vs Leverage plot in the lower right.  It looks like we do not have
#points out beyond cooks distance.

#2: Collinearity
#In general, we expect L1 and L2 regularization methods to handle multicollinearity fairly well.
#Here, notice that career runs and career RBI each have scores that are elevated, but still below
#the default stepVIF() threshold of 10 that is used widely in the literature.  
vif(t)
#Anyone familiar with baseball will not be surprised that these two metrics correlate to each other.
#The gold standard is experimentation.  Let's run an linear model with our coefficients including and
#excluding the feature with the highest VIF score, career runs.

#With Career runs
set.seed(5)
dftrainindex <- sample(1:nrow(df), 0.8*nrow(df))
#notice we are using a 1/5 to 4/5 test to train ratio here
dftestindex <- (-dftrainindex)
dftrain <- df[dftrainindex,]
dftest <- df[dftestindex,]

t_df <- lm(Salary~., data = dftrain)
vif(t_df)
lmPredictions <- predict(t_df, newdata = dftest, type = "response")

test <- as.data.frame(bind_cols(dftest, lmPredictions))
colnames(test)[9] <- "Predicted"
lmRMSE <- sqrt(mean((test$Predicted-test$Salary)^2))#/length(lmPredictions))
lmRMSE

#Without Career Runs

set.seed(6)
df2 <- df[,-which(colnames(df) == "c_Runs")]
dftrainindex <- sample(1:nrow(df2), 0.8*nrow(df2))
#notice we are using a 1/5 to 4/5 test to train ratio here
dftestindex <- (-dftrainindex)
dftrain <- df2[dftrainindex,]
dftest <- df2[dftestindex,]

t_df <- lm(Salary~., data = dftrain)
vif(t_df)
#Notice here that we have quite low VIF scores
lmPredictions <- predict(t_df, newdata = dftest, type = "response")

test <- as.data.frame(bind_cols(dftest, lmPredictions))
colnames(test)[8] <- "Predicted"
lmRMSE <- sqrt(mean((test$Predicted-test$Salary)^2))#/length(lmPredictions))
lmRMSE
#It looks like excluding career runs yields a significantly better model.  To ensure that both features made
#it through the lasso process to begin with, we can narrow down our earlier plot:

ggplot(tmp[tmp$coef %in% c("CRuns", "CRBI"),], aes(lambda, value, color = coef, linetype = coef)) + 
  geom_line() + 
  scale_x_log10() + 
  xlab("Lambda (log scale)") + 
  guides(color = guide_legend(title = ""), 
         linetype = guide_legend(title = "")) +
  theme_bw() + 
  theme(legend.key.width = unit(3,"lines"))+
  geom_vline(xintercept = cv.output$lambda.min)
#Indeed we see that we should have both as per the lasso process, but a final model would certainly
#exclude career runs based on the above analysis.  The lasso technique is useful, but results should 
#always be interpreted and critically analyzed before being deployed.
t2 <- lm(Salary ~ Hits + Walks + c_RBI + NationalLeague + DivisionWest + Putouts, data = df)
par(mfrow = c(2,2))
plot(t2, labels.id = FALSE)
par(mfrow = c(1,1))
#Simply noting that removing career runs does not solve our linear model assumptions issues.

#It looks like we may have some issues with a few of our linear modeling assumptions here.
#One potential route would be to simply not use a linear model to predict salary.  There are other, less parameterized
#regression techniques available.  A random forest model, for example, might work well.

#Alternatively, if we really wanted to stick with the linear model, we could perform a transformation of some
#kind on the outcome and try again.  A natural log transformation is a popular choice in this kind of situation.
#We continue the analysis with such a transformation here:
#https://github.com/jai-ranchod/demo_3/blob/main/R%20-%20Assumptions%20of%20Linear%20Modeling%20-%20Outcome%20Transformation.R
