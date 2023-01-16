# In this file, we take the 'Hitters' dataset and construct a regularized linear model predicting
# player salary. We do this with lasso regression (l1 regularization) which penalizes the objective
# function with the absolute value of the sum of the model coefficients.  Please note that we are
# assuming that this dataset is conducive to a linear model in this case.  Plesae also note that this is not the 
# same data set that is embedded in R.  The purpose here is to
# illustrate the creation of a regularized linear model.  Evaluating the diagnostics and assumptions
# of linear modeling is explored elsewhere in this repository.

# importing packages
import pandas as pd

# Importing and trimming data set
df = pd.read_csv('./Hitters.csv')
# The 'Hitters' dataset can be found on kaggle here:
# https://www.kaggle.com/datasets/floser/hitters

df = df.dropna()


# encoding categorical variables as dummy variables:
dummies = pd.get_dummies(df[['League', 'Division', 'NewLeague']])

# dropping the outcome and the variables for which we created dummmy variables.
# Note that we isolate and define our target variable first.
y = df['Salary']
x_numerical = df.drop(['Salary', 'League', 'Division', 'NewLeague'], axis=1).astype('float64')

# making a list of numerical features so we have the column name list for later
list_numerical = x_numerical.columns

# concatonating our dummy variables and numeric variables into one cohesive dataset
x = pd.concat([x_numerical, dummies[['League_N', 'Division_W', 'NewLeague_N']]], axis=1)

# x.info()

# Now we slit the data into a training set and a testing set.
# Note that we use 1/5 of the data for testing and the rest for training.
from sklearn.model_selection import train_test_split

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size=0.2, random_state=10)

# Next we standardize the training data to prepare for the use of regularization.  We do this using the
# standardscaler() function from sklean, which subtracts the mean and divides the difference by the standard
# deviation for each feature to which it is applied.  Note that we only apply it to the numerical features.

from sklearn.preprocessing import StandardScaler

scaler = StandardScaler().fit(x_train[list_numerical])
x_train[list_numerical] = scaler.transform(x_train[list_numerical])
x_test[list_numerical] = scaler.transform(x_test[list_numerical])

# Now we apply a lasso fit to the training set with an arbitrarily set alpha = 1
from sklearn.linear_model import Lasso

reg = Lasso(alpha=1)
reg.fit(x_train, y_train)

print('R squared training set', round(reg.score(x_train, y_train) * 100, 2))
print('R squared test set', round(reg.score(x_test, y_test) * 100, 2))

# Let's look into the MSE for the training and test set with this alpha value.

from sklearn.metrics import mean_squared_error

# Training data
train_prediction = reg.predict(x_train)
mse_train = mean_squared_error(y_train, train_prediction)
print('MSE training set', round(mse_train, 2))

# Test data
test_prediction = reg.predict(x_test)
mse_test = mean_squared_error(y_test, test_prediction)
print('MSE test set', round(mse_test, 2))

import numpy as np
import matplotlib.pyplot as plt

alphas = np.linspace(0.01, 500, 100)
lasso = Lasso(max_iter=10000)
coefs = []

# Note that the regularization parameter for the lasso model in python is alpha
for a in alphas:
    lasso.set_params(alpha=a)
    lasso.fit(x_train, y_train)
    coefs.append(lasso.coef_)

ax = plt.gca()

ax.plot(alphas, coefs)
ax.set_xscale('log')
plt.axis('tight')
plt.xlabel('alpha')
plt.ylabel('Standardized Coefficients')
plt.title('Lasso coefficients as a function of alpha')
plt.axvline(x=1, color='black', linestyle='dashed')
# Un-comment the line below to view the graph illustrating the effect of the alpha parameter on coefficient value.
# plt.show()

# Note that unlike ridge regression, lasso regression can shrink coefficients down to 0 as
# we see on the right-hand side of the plot.  Since lasso regression penalizes the objective function
# with the sum of the absolute values of the coefficients, it is no surprise that eventually as you move
# right on the x-axis the all the coefficient values drop to zero.
# Also notice that our dashed vertical line is set at alpha = 1; exactly where we set our arbitrary
# value in the previous section.  You can see that we still have several non-zero predictors, but also
# notice that we have significantly reduced the absolute value of our predictors.

# Now we find the optimal alpha value via cross-validation.  Specifically, we will use 5-fold cross-validation.

from sklearn.linear_model import LassoCV

model = LassoCV(cv=5, random_state=0, max_iter=10000)
model.fit(x_train, y_train)

print(model.alpha_)

# Now we build a lasso model with our optimized alpha coefficient.
lasso_best = Lasso(alpha=model.alpha_)
lasso_best.fit(x_train, y_train)

# Before we move on to model evaluation, let's take a look at our coefficients and our values:
print(list(zip(lasso_best.coef_, x)))

# Let's evaluate the model now.
print('R squared training set', round(lasso_best.score(x_train, y_train) * 100, 2))
print('R squared test set', round(lasso_best.score(x_test, y_test) * 100, 2))

# Note that with the optimized alpha, our training R squared value decreases but our test R squared value
# increases in comparison to our evaluation with our earlier arbitrary alpha = 1 model.

print('Test MSE with alpha-optimized model', mean_squared_error(y_test, lasso_best.predict(x_test)))

# Also note that our test MSE is significantly reduced using the optimal alpha value.
# Next we make a plot illustrating the effect of varying alphas on MSE in our cross-validation
# process
plt.semilogx(model.alphas_, model.mse_path_, ":")
plt.plot(
    model.alphas_,
    model.mse_path_.mean(axis=-1),
    "k",
    label="Average across the folds",
    linewidth=2,
)

plt.axvline(
    model.alpha_, linestyle="--", color="k", label="alpha: CV estimate"
)

plt.legend()
plt.xlabel("alphas")
plt.ylabel("Mean square error")
plt.title("Mean square error on each fold")
plt.axis("tight")

ymin, ymax = 50000, 250000
xmin, xmax = 0.1, 1000
plt.ylim(ymin, ymax)
plt.xlim(xmin, xmax)
plt.show()

# Note that this plotting structure assumes that the earlier plot showing the effect of alpha
# on coefficient value was uncommented.



# Final Notes:
# Change the size of the test set
