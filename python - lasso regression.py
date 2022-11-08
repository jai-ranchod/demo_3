# importing packages
import pandas as pd
import math as mt
import numpy as np
from statsmodels.stats.outliers_influence import variance_inflation_factor
import seaborn as sns
import matplotlib.pyplot as plt
import random
from random import sample
from sklearn import linear_model

# Note that for the purposes of this exercise we are going to assume that this dataset is conducive to a linear model.
# The objective here is to demonstrate the linear model regularization process specifically.


# Importing and trimming data set
df = pd.read_csv('./Hitters.csv')
# The 'Hitters' dataset can be found on kaggle here:
# https://www.kaggle.com/datasets/floser/hitters
df = pd.get_dummies(df, drop_first=True)
df = df.dropna()

# creating hold-out validation set and training set for cross-validation
l = len(df)
r = int(np.floor(l / 10))

all_rows = []
for i in range(l):
    all_rows.append(i)

random.seed(1)
test_indices = sample(range(l), r)
train_indices = []
for i in range(l):
    if all_rows[i] not in test_indices:
        train_indices.append(i)

test_holdout = df.iloc[test_indices]
df = df.iloc[train_indices]

# training simple linear regression model
x = df.drop('Salary', axis=1)
y = df['Salary']
holdout_x = test_holdout.drop('Salary', axis=1)
holdout_y = test_holdout['Salary']

from sklearn.model_selection import train_test_split

train_x, test_x, train_y, test_y = train_test_split(x, y, test_size=0.3, random_state=2)

from sklearn.linear_model import LinearRegression

reg = LinearRegression().fit(train_x, train_y)

# illustrating the need for regularization

print('Simple linear model test Score:', ' ', reg.score(test_x, test_y))
print('Simple linear model training Score:', ' ', reg.score(train_x, train_y))
print('Clearly the simple model over-fits and we can benefit from regularization.')
# regularized model
z = 100
max_iter = 10000
tol = 0.00001
tuning = pd.DataFrame(columns=['Alpha', 'Score'])
for i in range(1, z):
    lasso_reg = linear_model.Lasso(alpha=i, max_iter=max_iter, tol=tol)
    lasso_reg.fit(train_x, train_y)
    tuning.loc[i] = (i, round(lasso_reg.score(test_x, test_y), 6))

# finding optimal regularization parameter

alpha_max = float(tuning.Alpha[tuning.Score == tuning.Score.max()])
lasso_fin = linear_model.Lasso(alpha=alpha_max, max_iter=max_iter, tol=tol)
lasso_fin.fit(train_x, train_y)

print('Test score of regularized model:', ' ', lasso_fin.score(holdout_x, holdout_y))
print('Notice that we increase our test score by', ' ',
      (lasso_fin.score(holdout_x, holdout_y) - reg.score(test_x, test_y)), 'by using L1 (Lasso) regularization')
print('Our final score is nearly as good as our training score.')



