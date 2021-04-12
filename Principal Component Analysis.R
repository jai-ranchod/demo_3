library(factoextra)
data(mtcars)
#Here we use principal components analysis to gain a better understanding of the relationship between the columns of the "mtcars" dataset.

#mpg - Miles per Gallon
#cyl - number of cylinders
#disp - engine displacement; combined volume of cylinders
#hp - horsepower
#drat - rear axle ratio
#wt - weight
#qsec - quarter mile time
#vs - is engine "V" shaped (1) of straight (0)
#am - automatic transmission (0) or manual transmission (1) (this dataset is from 1974, so manual transmission could very well have a positive impact on feul economy)
#gear - number of forward gears
#carb - number of carburetors
#First, we remove the factors(categorical variables) from the data set.
PCA_mtcars <- mtcars[,c(1,2,3,4,5,6,7,10,11)]

#since not all variables are in the same scale, it becomes necessary to scale the data to provide (along with centering) an appropriate
#covariance matrix
mtcars_PCA_Object <- prcomp(PCA_mtcars, center = TRUE, scale. = TRUE)
summary(mtcars_PCA_Object)

fviz_eig(mtcars_PCA_Object)
#Here we see that following the Kaiser-Guttman rule would leave us with 7 principal components.
#However, about 86% of variance is cumulatively explained by the first two principal components, so we will use the percentage of common variance
#rule instead to better viualize our data.

#Now we can create a graphic to illustrate the relationship between our predictors and the top two principal components identified.
#Note that a full analysis in this style with more principal components would involve analyzing all possible combinations of 2 principal components
#graphically

fviz_pca_var(mtcars_PCA_Object,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



#Since we will later attempt to build a linear model estimating fuel efficiency, it is worthwhile to simply create a second graphic without the "mpg" vector.
PCA_mtcars_02 <- mtcars[,c(2,3,4,5,6,7,10,11)]
mtcars_PCA_Object_02 <- prcomp(PCA_mtcars_02, center = TRUE, scale. = TRUE)
fviz_pca_var(mtcars_PCA_Object_02,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#Here we see an interesting result. We clearly see vectors like weight, displacement, number of cylinders, and horepower clustering along dimension 1.
#These are all size and engine power related vectors, and it is not surprising to see them going in the opposite direction of mpg (from the first graphic).
#We also see that vectors such as quarter mile time, number of forward gears, and number of carburetors are more aligned with dimension 2 than dimension 1.
#A higher number of forward gears is typically associated with sports cars, a higher number of carburetors is associated with higher engine power, and the  
#quarter mile time is negatively associated with acceleration (higher time means slower acceleration).  This dimension measures the cars ability to efficiently
#accelerate (notice that more forward gears, faster acceleration, and more carburetors extend in the negative direction of dimension 2). It may seem counterintuitive
#to consider acceleration or "quickness" independently of such things as wieght and horsepower, but upon deeper reflection, it actually makes sense.  Consider
#the 016 Rio de Janero Olympics as an example, specifically the 100m dash.  The three medalists from this event were Usain Bolt (Jamaica), Justin Gatlin (United States)
#and Andre de Grasse (Canada).  Usain Bolt weighs 207 lbs, Gatlin 176 lbs, and de Grasse 154 lbs.  None of these men are carrying weight they're not using
#(they olympic medalists after all) so we can see a variety of "horsepower" among the quickest men in the world. Returning to our Principal Components Analysis,
#we note that the mpg extends in the opposite directino of the weight, displacement, cylinders, and horespower cluster, but the "quickness" dimension is made up of
#vectors that may or may not have some limited correlation with mpg.
