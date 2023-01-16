#####Introduction#####
#Here we re-create the previous lasso regresion model found here:
#https://github.com/jai-ranchod/demo_3/blob/main/R%20-%20Assumptions%20of%20Linear%20Modeling%20-%20Hitters%20Data.R
#but with a log-transformed outcome.  We'll skip over some of the previous analysis if necessary.

if (!require("ISLR")) install.packages("ISLR")
if (!require("glmnet")) install.packages("glmnet")

library(ISLR)
library(glmnet)

#####Creating Training Set#####
data(Hitters)
mean(is.na(Hitters))
#With less than 1% of our data listed as NA, we can remove the NA rows with the na.omit() function
Hitters <- na.omit(Hitters)
#First, we use the convenient model.matrix() function to expand our factors into dummy variables.
Hitters2 <- Hitters
Hitters2$Salary <- log(Hitters2$Salary) #Here is where we make the transformation
x <- model.matrix(Salary~., Hitters2)[,-1]
y <- Hitters2$Salary

set.seed(1)
train <- sample(1:nrow(x), 0.8*nrow(x))
#notice we are using a 1/5 to 4/5 test to train ratio here
test <- (-train)
y.test <- y[test]
x.test <- x[test,]
y.train <- y[train]
x.train <- x[train,]

#####Fitting Model#####
grid <- 10^seq(10,-2,length=10000)
set.seed(2)
cv.output <- cv.glmnet(x.train, y.train, lambda = grid) #alpha = 1 is default value
plot(cv.output)

cv.output$lambda.min

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
hmRun <- rep(s["HmRun",], nrow(x))
Runs <- rep(s["Runs",], nrow(x))
Walks <- rep(s["Walks",], nrow(x))
Years <- rep(s["Years",], nrow(x))
CHits <- rep(s["CHits",], nrow(x))
LeagueN <- rep(s["LeagueN",], nrow(x))
DivisionW <- rep(s["DivisionW",], nrow(x))
PutOuts <- rep(s["PutOuts",], nrow(x))
Errors <- rep(s["Errors",], nrow(x))

xFrame$Hits <- xFrame$Hits*hits
xFrame$HmRun <- xFrame$HmRun*hmRun
xFrame$Runs <- xFrame$Runs*Runs
xFrame$Walks <- xFrame$Walks*Walks
xFrame$Years <- xFrame$Years*Years
xFrame$CHits <- xFrame$CHits*CHits
xFrame$LeagueN <- xFrame$LeagueN*LeagueN
xFrame$DivisionW <- xFrame$DivisionW*DivisionW
xFrame$PutOuts <- xFrame$PutOuts*PutOuts
xFrame$Errors <- xFrame$Errors*Errors

df <- as.data.frame(cbind(Hitters2$Salary, xFrame$Hits, xFrame$HmRun, xFrame$Runs, xFrame$Walks, xFrame$Years, xFrame$CHits, xFrame$LeagueN, xFrame$DivisionW, xFrame$PutOuts, xFrame$Errors))
colnames(df) <- c("Salary",                "Hits",     "HmRun",     "Runs",      "Walks",     "Years",        "CHits",      "NationalLeague", "DivisionWest",   "Putouts",     "Errors")

t <- lm(Salary ~ . , data = df)
vif(t)
#This time we do get a VIF score explicitly over 10, so we will outright remove it from the final model
t <- lm(Salary ~ Hits + HmRun + Walks + Years + CHits + NationalLeague + DivisionWest + Putouts + Errors , data = df)


par(mfrow = c(2,2))
plot(t, labels.id = FALSE)
par(mfrow = c(1,1))
#####Analyzing Assumptions of Linearity#####
#In general our diagnostic plots look a lot better this time.

#Assumption #1: Linearity: 
#We do not appear to have the "fanning out" phenomena we observed prior to the log transformation.

#Assumption #2: Homoscedasticity:
#We check the Scale-Location plot (lower left) hoping to see a pattern of points that is generally evenly
#spaced with a horizontal red line.  Here, instead of a steadily increasing line, we do actually see a fairly even
#line.

#Assumption #3: Independence:
#Again, given the variance in individuals, stadiums, and teams, it is reasonable to assume that our selection represents an independent
#representation of players.

#Assumption #4: Normality Assumption (residuals are normally distributed):
#We check the qq-plot (upper right) to determine whether or not the residuals are normally distributed.
#This time we see a fairly stable qq-plot with only a few points out of line.  This is much better than prior
#to the transformation.

#Additional Considerations
#1: Outliers
#Although it is not always considered one of the core assumptions of linear modeling, one should
#be on the lookout for influential outliers that can cause issues with the model.  To investigate
#this, we look at the Residuals vs Leverage plot in the lower right.  Once again, it looks like we do not have
#points out beyond cooks distance.

#2: Collinearity
#In general, we expect L1 and L2 regularization methods to handle multicollinearity fairly well.
#Here, notice that career runs and career RBI each have scores that are elevated, but still below
#the default stepVIF() threshold of 10 that is used widely in the literature.  
vif(t)
#Notice that we have quite low VIF scores after removing the "Runs" feature.
#It appears that a log transformation could yield a reasonable linear model, if the 
#use case allows for such a transformation.








