# Repository Overview
This github repository is designed to give demonstrations of my expertise in a variety of data science and data analysis contexts using the R, SQL, and Python programming languages.

You can download R here from the Case Western Reserve University mirror here:
https://cran.case.edu/

and you can download the RStudio IDE here:
https://www.rstudio.com/products/rstudio/download/

Both are free.

The data sets referenced are all built into R.

You can download the python scripting language here:
https://www.python.org/downloads/

and you can download the PyCharm IDE here:
https://www.jetbrains.com/pycharm/download/#section=mac

Finally, you can download Microsoft SQL Server here:
https://docs.microsoft.com/en-us/sql/ssms/sql-server-management-studio-ssms?view=sql-server-ver15

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
**SQLite Demo (SQL in R)**:

This script uses the RSQLite package which embeds an SQLite database engine in R.  I add the tables from the "SQL Practice Problems" book by Sylvia Mosilik.  I then use SQLite to execute the queries that are mey answers to some of the "Intermediate" and "Challenge" problems.  The original code was written in MS SQL, and what you see in this repository is my adaptation to SQLite.

**Assumptions of Linear Modeling - Hitters Data (R)**: 

This script uses the "Hitters" data set to illustrate the creation of a linear regression model. The primary purpose of this script, however, is to evaluate the assumptions of linear models against a fitted linear model.

**Assumptions of Linear Modeling - Outcome Transformation (R)**:

This is an extension of the initial assumptions of linear modeling file in which we apply a log transformation to the outcome.

**Data Visualizations - Titanic Data(R)**: 

This script uses the built-in "Titanic" data set.  The focus is on data visualizations that illustrate different factors influencing survival rates of the Titanic wreck.

**Exponential Forecasting(R)**:

This script uses a data set from Yahoo Finance that will be referenced directly within the script; no alterations will be necessary.  I use an optimized Holt model to forecast the value of the PHLX semiconductor index 21 business days (one month) out.

**KNN - Titanic (R)**:

This script uses the "titanic_train" data set to illustrate the use of the k-nearest neighbors algorithm to create a binary classifier on the survival status of passengers from the infamous Titanic Oceanliner tragedy.

**Logistic Regression (R)**:

In this script, we return to the "Titanic" data set to build a logistic regression model.  The "titanic_train" set is used to create the binary predictor model which is then evaluated on the "titanic_test" set.

**Principal Component Analysis (R)**:

In this script, I use the built-in "mtcars" data set to demonstrate principal component analysis as an exploratory analysis technique.

**Random Forest - abalone data (R)**: 

This script applies a random forest method to the abalone dataset to estimate abalone age given the other data.

**Random Forest - mtcars data (R)**:

This script applies a random forest method to the mtcars dataset to estiamte fuel efficiency given the other data.

**Statistical Analysis - Cancer Statistics (R)**: 

In this script, I use data from the Cleveland Clinic that is directly referenced in the script, no alterations are necessary.  The analysis focuses on the odds ratios associated with Thyroid cancer for men vs. women, and contains analysis across both time and gender.

**Master Class Exercises (SQL)**:

This script contains selected problems from an advanced topics SQL Server class I took.  The problems focus on the "Adventure Works 2019" dataset that can be found here:

https://docs.microsoft.com/en-us/sql/samples/adventureworks-install-configure?view=sql-server-ver15&tabs=ssms

**Recursive Query - Employee Lineage (SQL)**:

Here we create a table of employees and managers.  We then use a recursive query to create a table identifying the top level managers and the number of employees under each.

**Automated Stock Analysis (python)**:

This is an automation script that pulls data from Yahoo Finance and outputs a file to a specified directory containing data related to the top x% of stocks from a chosen exchange.  The user chooses the top %, the exchange, the lookback period, and the file output location.

**Birthday Paradox(python)**:

This files explores the "Birthday Paradox" which states that the probability of having two or more people in the same room with the same birthday exceeds 50% with only 23 people in the room.

**Dietary Linear Program (python)**:

This is a simple linear program designed to optimize ones food budget subject to the constraint that the food purchased must contain sufficient amounts of eight selected vitamins and nutrients.  Prices (as of 2022) and nutritional content are all researched and links are commented in the code.

**LASSO Regression (python)**:

This is a script showing the necessity and utility of the L1 regularization technique (LASSO) on the "Hitters" dataset found here:

https://www.kaggle.com/datasets/floser/hitters

----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

I look forward to sharing my work with you. There is a lot of information here, so if you would like a pdf with more detailed explanations, or if you have any other questions or comments, please do not hesitate to reach out to me at:

jsranchod@gmail.com

and have a nice day!

-Jairaj Ranchod
