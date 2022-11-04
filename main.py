# This problem focuses on how to get the most nutritional value on a budget.
# Specifically we will optimize the purchase of three foods subject to meeting the minimum
# recommended amount of eight vitamins and nutrients.  Given that we are focused on avoiding malnutrition
# we will assume that getting slightly more than enough of any given vitamin or nutrient is not an issue.

#Average food consumed per day in weight
#https://www.precisionnutrition.com/what-are-your-4-lbs#:~:text=People%20generally%20eat%20between%20three,Want%20to%20lose%20weight%3F


# Recommended Amounts per day --- https://www.fda.gov/food/new-nutrition-facts-label/daily-value-new-nutrition-and
# -supplement-facts-labels
# Niacin; 16 mg
# Pantothenic Acid; 5 mg
# Thiamin; 1.2 mg
# Vitamin B6; 1.7 mg
# Magnesium; 420 mg
# Iron 18 mg
# Vitamin C; 90 mg
# Manganese; 2.3 mg

# Food #1; Chicken Breast - $0.23875/ounce --- https://costniche.com/cost-of-chicken-breast-per-pound/
# Niacin; 3.8 mg/ounce
# https://www.healthline.com/nutrition/foods-high-in-niacin
# Pantothentic Acid; 0.43 mg/ounce
# https://ods.od.nih.gov/factsheets/PantothenicAcid-HealthProfessional/
# Thiamin; 0 mg/ounce
# https://www.medicalnewstoday.com/articles/219545#benefits
# Vitamin B6; 0.26 mg/ounce
# https://www.livestrong.com/article/267826-vitamins-in-chicken/
# Magnesium; 9.63 mg/ounce
# https://www.nutritionvalue.org/Chicken%2C_grilled%2C_cooked%2C_meat_only%2C_boneless%2C_skinless%2C_breast
# %2C_broiler_or_fryers_nutritional_value.html
# Iron; 0.286 mg/ounce
# https://healthyeating.sfgate.com/chicken-source-iron-4824.html
# Vitamin C; 0 mg/ounce
# https://www.healthline.com/nutrition/chicken-diet#downsides
# Manganese; 0.0048 mg/ounce
# https://fdc.nal.usda.gov/fdc-app.html#/food-details/171477/nutrients

# Food #2; Oranges - $0.520375/orange (on average)
# https://frugalanswers.com/average-cost-of-oranges/
# Average size of an orange (4.6 ounces)
# https://www.domeats.com/how-much-does-an-orange-weigh/#:~:text=On%20average%20oranges%20weigh
# %204.6,from%202%20to%2010%20ounces
# Niacin; 0.6 mg/orange
# https://www.eatthismuch.com/food/nutrition/oranges,1465/
# Pantothentic Acid; 0.33 mg/orange
# https://whfoods.com/genpage.php?pfriendly=1&tname=foodspice&dbid=37
# Thiamin; 0.11 mg/orange
# https://www.winchesterhospital.org/health-library/article?id=26273
# Vitamin B6; 0.1 mg/orange
# https://www.eatthismuch.com/food/nutrition/oranges,1465/
# Magnesium; 15.4 mg/orange
# https://www.eatthismuch.com/food/nutrition/oranges,1465/
# Iron; 0.131 mg/orange
# https://fdc.nal.usda.gov/fdc-app.html#/food-details/169097/nutrients
# Vitamin C; 70 mg/orange
# https://www.webmd.com/food-recipes/health-benefits-oranges
# Manganese; 0.03 mg/orange
# https://whfoods.com/genpage.php?tname=foodspice&dbid=37

# Food #3; White Rice - $0.7068/cup (186 grams) --- https://www.globalproductprices.com/USA/rice_prices/
# Niacin; 2.7 mg/cup
#  https://www.verywellfit.com/rice-nutrition-facts-calories-and-health-benefits-4119792
#  Pantothentic Acid; 0.76 mg/cup
#  https://www.uhhospitals.org/health-information/health-and-wellness-library/article/nutritionfacts-v1/rice-white-medium-grain-cooked-unenriched-1-cup
# Thiamin; 0.04 mg/cup
# https://www.uhhospitals.org/health-information/health-and-wellness-library/article/nutritionfacts-v1/rice-white-medium-grain-cooked-unenriched-1-cup
# Vitamin B6; 0.09 mg/cup ---
# https://www.uhhospitals.org/health-information/health-and-wellness-library/article/nutritionfacts-v1/rice-white-medium-grain-cooked-unenriched-1-cup
# Magnesium; 24.18 mg/cup
# https://www.uhhospitals.org/health-information/health-and-wellness-library/article/nutritionfacts-v1/rice-white-medium-grain-cooked-unenriched-1-cup
# Iron 0.37 mg/cup
# https://www.uhhospitals.org/health-information/health-and-wellness-library/article/nutritionfacts-v1/rice-white-medium-grain-cooked-unenriched-1-cup
# Vitamin C; 0 mg/cup
# https://www.uhhospitals.org/health-information/health-and-wellness-library/article/nutritionfacts-v1/rice-white-medium-grain-cooked-unenriched-1-cup
# Manganese; 0.6 mg/ounce
# https://www.motherearthstorehouse.com/Healthy-Living/Healthy-Foods/Grains/WhiteRice#:~:text=You'll%20also%20consume%20a,by%20the%20Institute%20of%20Medicine.
import scipy
from scipy.optimize import linprog

obj = [0.23875, 0.520375, 0.7068]  # coefficients of cost function  we attempt to minimize
# note that the sign of each element of the "A" matrix and "b" vector are all negative - this is because we adjust the
# constraint function to be of the form Ax - b <= 0
Amat = [[-3.8, -0.6, -2.7],  # Matrix holding nutrient values for each food options
        [-0.43, -0.33, -0.76],
        [0, -0.11, -0.04],
        [-0.16, -0.1, -0.09],
        [-9.63, -15.4, -24.18],
        [-0.286, -0.2, -0.37],
        [0, -70, 0],
        [-0.0048, -0.03, -0.6]]
bvec = [-16, -5, -1.2, -1.7, -420, -18, -90, -2.3]

bnd = [(0, float("inf")),
       # stipulating that values of foods must be positive; this is actually redundant in this function
       (0, float("inf")),  # but it won't hurt to explicitly call it out
       (0, float("inf"))]
opt = scipy.optimize.linprog(c=obj, A_ub=Amat, b_ub=bvec, bounds=bnd)

print("Based on this analysis, you should consume approximately" + "\n" +
      str(round(opt.x[0],4)) + " ounces of chicken" + "\n" +
      str(round(opt.x[1],4)) + " oranges" + "\n" +
      str(round(opt.x[2],4)) + " cups of rice" + "\n" +
      "per day in order to optimize your budget under these dietary restrictions." + "\n" +
      "This may sound like a lot, but note that we only consider three foods here." + "\n" +
      "Also note that the average person consumes ~3-4 pounds of food per day (see commented code for link)." + "\n" +
      "Depending on the kind of oranges in question, this diet would roughly fall in line with that figure.")

