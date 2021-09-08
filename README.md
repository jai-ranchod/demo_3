# Repository Overview
This github repository is designed to give demonstrations of my expertise in a variety of data science and data analysis contexts using the R (and SQLite) programming language(s).

You can download R here from the Case Western Reserve University mirror here:
https://cran.case.edu/

and you can download the RStudio IDE here:
https://www.rstudio.com/products/rstudio/download/

Both are free.

The data sets referenced are either built into R or referenced in a self-contained way within the script.  Data sets that are not directly built into R are also held in this repository:

https://github.com/jai-ranchod/demo_2

as a backup.  All scripts are self-contained, so it is not necessary to download these files, but you are free to do so if you would like.

Data Visualizations - Titanic Data: 

This script uses the built-in "Titanic" data set.  The focus is on data visualizations that illustrate different factors influencing survival rates of the Titanic wreck.

Exponential Forecasting: 
 
This script uses a data set from Yahoo Finance that will be referenced directly within the script; no alterations will be necessary.  I use an optimized Holt model to forecast the value of the PHLX semiconductor index 21 business days (one month) out.

Linear and Random Forest Modeling: 

This script uses a data set from the Marin Stats program to predict lung capacity in adolescents and teens.  The continuous lung capacity outcome is predicted with a linear model, with a random forest model, and finally with an ensemble of both.  The data set is refenced directly within the script, no alterations are necessary.

Logistic Regression: 

In this script, we return to the "Titanic" data set to build a logistic regression model.  The "titanic_train" set is used to create the binary predictor model which is then evaluated on the "titanic_test" set.  Cubic splines are considered but ultimately not included after cross-validation determines they are not helpful to the model.

Principal Component Analysis:

In this script, I use the built-in "mtcars" data set to demonstrate principal component analysis as an exploratory analysis technique.

Statistical Analysis - Cancer Statistics: 

In this script, I use data from the Cleveland Clinic that is directly referenced in the script, no alterations are necessary.  The analysis focuses on the odds ratios associated with Thyroid cancer for men vs. women, and contains analysis across both time and gender.

SQLite Demo:
This script uses the RSQLite package which embeds an SQLite database engine in R.  I add the tables from the "SQL Practice Problems" book by Sylvia Mosilik.  I then use SQLite to execute the queries that are mey answers to some of the "Intermediate" and "Challenge" problems.  The original code was written in MS SQL, and what you see in this repository is my adaptation to SQLite.  For your reference, please find attached a photo of the database schema.

I look forward to sharing my work with you. There is a lot of information here, so if you would like a pdf with more detailed explanations, or if you have any other questions or comments, please do not hesitate to reach out to me at:

jsranchod@gmail.com

and have a nice day!

-Jairaj Ranchod
