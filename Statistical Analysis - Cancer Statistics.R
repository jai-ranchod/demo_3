library(readxl)
library("class") #needed for knn
library("tidyverse")
library("plotly")
library("forecast")
library("reldist")
library("corrplot")
library("ineq")
library("gglorenz")
library("lubridate")
library("BSDA")
library("car")
library("pedometrics")
library("caret")
library("gtools")
library("readxl")
library("stringr")
library("pdftools")
library("dplyr")
library("tidyverse")
library("scales")
#Here we are using a dataset from the Cleveland Clinic regarding cancer rates from 2009 to 2011. Specifically we will analyize the odds
#ratios of incidences of Thyroid cancer for men and women across multiple age groups.

Cancer_Stats <- read_excel("~/MBA Classes/Spring Semester 2020/Business Analytics/Data Sets/Cancer_Stats.xlsx")
head(Cancer_Stats)

#"NewCount" = New Count of either incidences or mortalities
#"Event_Type" = Incidence or Mortality
#"Population" = The population in the specified race, sex, and age group for the specified year
#"Race" = Racial identification of the specified sub-group
#"Sex" = Sex of the specified subgroup
#"Site" = Site of Cancer
#"Year" = Year of data
#"AGE_GRP" = Age group 

#For this data set, we will be analyzing incidences of thyroid cancer between males and females for all races. 
#We consider each year to contain an independent population for each age group.  One could make the case that the population in the 70-74 age group, for example, may be influenced by the occurences in prior years when those patient were in other age groups.  We consider these effects to be small, and combined with the changing population, we are confident in our ability to define each year and age group as independent.
#We start off with a simple density plot showing differences based on sex:
#####Initial Analysis#####

Thyroid <- Cancer_Stats %>% filter(EVENT_TYPE == "Incidence", RACE == "All Races", SITE == "Thyroid")

Male_Thyroid <- Thyroid %>% filter(SEX == "Male")
Female_Thyroid <- Thyroid %>% filter(SEX == "Female")



ggplot()+
  geom_density(aes(x = Male_Thyroid$RATE, fill = "Male"), alpha = 0.6)+
  geom_density(aes(x = Female_Thyroid$RATE, fill = "Female"), alpha = 0.6)+
  scale_fill_manual(name = "Sex", breaks = c("Female","Male"), values = c("pink","blue"))+
  xlab("Rate of Thyroid Cancer per 10,000 People")+
  ylab("Probability Density")+
  ggtitle("Probability of Thyroid Cancer Rate (per 10,000) Regardless of Age or Year")

#This shows that in general women face higher rates of thyroid cancer than men.  However, this graph does not provide any age specific information, which we find next.

Thyroid %>% filter(SEX == "Male and Female") %>% group_by(AGE_GRP) %>% summarise(Population = sum(POPULATION), Count = sum(NewCount), Rate = 100000*(Count/Population)) %>% ggplot(aes(x = AGE_GRP, y = Rate))+
  geom_bar(stat = "identity")+
  geom_label(aes(label = round(Rate,2)), nudge_y = 1, size = 3)+
  xlab("Age Group")+
  ylab("Rate of Thyroid Cancer")+
  ggtitle("Rate of Thyroid Cancer by Age Group")

#Here we see that rates are highest among the general population between ages 50 and 75.  
#Breaking the rates down by both gender and age group, however, we see that women see higher rates across all age groups, and that women face the highest incidence rates between ages 40 - 69 as opposed to the 60 - 84 range which is highest for men.

Thyroid %>% filter(SEX == "Male" | SEX == "Female") %>% group_by(AGE_GRP, SEX) %>% summarise(Population = sum(POPULATION), Count = sum(NewCount), Rate = 100000*(Count/Population)) %>% ggplot(aes(x = AGE_GRP, y = Rate))+
  geom_line(aes(color = SEX, group = SEX))+
  geom_point(aes(color = SEX))+
  xlab("Age Group")+
  ylab("Rate of Thyroid Cancer (per 100,000)")+
  ggtitle("Rate of Thyroid Cancer by Age Group and Sex")

#####Odds of Thyroid Cancer######
#Next, we can calculate the odds of Thyroid cancer for females and males, and calculated the odds ratios for each age group.  R does have available odds ratio functions, but for these purposes, I prefer to demonstrate manual calculation.
#First, we need to filter in the relevant data from our initial data set.

Age_Group_Sex <- Thyroid %>% filter(SEX == "Male" | SEX == "Female") %>% group_by(AGE_GRP, SEX) %>% summarise(Population = sum(POPULATION), Count = sum(NewCount), Rate = 100000*(Count/Population))


Female <- Age_Group_Sex %>% filter(SEX == "Female")
Male <- Age_Group_Sex %>% filter(SEX == "Male")
New_Age_Group_Sex <- inner_join(Male, Female, by = "AGE_GRP", suffix = c("_Male", "_Female"))

#Next we create the functions to calculate the odds ratio, p-value of the odds ration, and standard error for each age group.

odds_ratio <- function(positive_male, negative_male, positive_female, negative_female){
  ratio <- (positive_female/negative_female)/(positive_male/negative_male)
  ratio
}
se <- function(positive_male, negative_male, positive_female, negative_female){
  s <- sqrt((1/positive_male)+(1/negative_male)+(1/positive_female)+(1/negative_female))
  s
}
p_value <- function(positive_male, negative_male, positive_female, negative_female){
  se <- sqrt((1/positive_male)+(1/negative_male)+(1/positive_female)+(1/negative_female))
  
  ratio <- (positive_female/negative_female)/(positive_male/negative_male)
  log_or <- log(ratio)
  pv <- 2*(1-pnorm(log_or, 0, se))
  pv
}

#####Odds Ratios#####

#With these functions in place, now we add the odds ratio, p-value, and upper and lower 95% confidence intervals.
#My personal preference is to perform mutations one at a time to keep things more organized.

New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(odds_ratio_female_to_male = odds_ratio(positive_male = Count_Male, negative_male = Population_Male-Count_Male, positive_female = Count_Female, negative_female = Population_Female-Count_Female))
New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(p_value_odds_ratio_female_to_male = p_value(positive_male = Count_Male, negative_male = Population_Male-Count_Male, positive_female = Count_Female, negative_female = Population_Female-Count_Female))        
New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(CI_Lower = odds_ratio_female_to_male*exp(-1*qnorm(0.975)*se(positive_male = Count_Male, negative_male = Population_Male-Count_Male, positive_female = Count_Male, negative_female = Population_Female-Count_Female)))
New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(CI_Upper = odds_ratio_female_to_male*exp(qnorm(0.975)*se(positive_male = Count_Male, negative_male = Population_Male-Count_Male, positive_female = Count_Male, negative_female = Population_Female-Count_Female)))

sample <- New_Age_Group_Sex[c(1,10,11,12,13)]
colnames(sample)[1] <- "Age Group"
colnames(sample)[2] <- "Odds Ratio"
colnames(sample)[3] <- "p-value"
colnames(sample)[4] <- "Lower 95% CI"
colnames(sample)[5] <- "Upper 95% CI"
sample

#Note that in the above table, the odds ratio is odds for females divided by odd for males.
#We see that the p-values indicate statistically significant odds ratios.  The cell values of positive and negative females and males certainly indicate that we are safe making the normality assumption for the odds ratios.

sample_2 <- New_Age_Group_Sex[c(1,3,4,7,8)]
colnames(sample_2)[1] <- "Age Group"
colnames(sample_2)[2] <- "Male Population"
colnames(sample_2)[3] <- "Male Incidences"
colnames(sample_2)[4] <- "Female Population"
colnames(sample_2)[5] <- "Female Incidences"
sample_2

#However, we can make a Haldane-Anscombe correction to our data set and calculate the differences in odds ratio and confidence interval just to be sure.
#This involves adding 0.5 to each cell of the 2x2 matrix for each age group and performing our calculations again.


New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(odds_ratio_female_to_male_test = odds_ratio(positive_male = (Count_Male+0.5), negative_male = (Population_Male-Count_Male+0.5), positive_female = (Count_Female+0.5), negative_female = (Population_Female-Count_Female+0.5)))
New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(CI_Lower_test = odds_ratio_female_to_male_test*exp(-1*qnorm(0.975)*se(positive_male = Count_Male+0.5, negative_male = Population_Male-Count_Male+0.5, positive_female = Count_Male+0.5, negative_female = Population_Female-Count_Female+0.5)))                                                  
New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(CI_Upper_test = odds_ratio_female_to_male_test*exp(qnorm(0.975)*se(positive_male = Count_Male+0.5, negative_male = Population_Male-Count_Male+0.5, positive_female = Count_Male+0.5, negative_female = Population_Female-Count_Female+0.5)))

New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(odd_ratio_difference = odds_ratio_female_to_male_test-odds_ratio_female_to_male)
New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(CI_Lower_difference = CI_Lower_test - CI_Lower)
New_Age_Group_Sex <- New_Age_Group_Sex %>% mutate(CI_Upper_difference = CI_Upper_test - CI_Upper)

sample_3 <- New_Age_Group_Sex[c(1,17,18, 19)]
colnames(sample_3)[1] <- "Age Group"
colnames(sample_3)[2] <- "Odds Ratio - Difference before and after HA Adjustment"
colnames(sample_3)[3] <- "Lower 95% CI - Difference before and after HA Adjustment"
colnames(sample_3)[4] <- "Upper 95% CI - Difference before and after HA Adjustment"
sample_3

#Note that "difference" here means measurement before Haldane - Anscombe adjustment minus measurement after.
#We see that even our largest differences are only on the order of one-thousandths, giving us confidence in our odds ratios and confidence intervals.
#Finally, we can create a chart illustrating the difference in odds ratio of thyroid cancer for females and males between 1999 and 2011.

a <- 0.2
v <- c(-a, a, a, -a, -a, -a, -a, -a, -a, -a, -a, -a, -a, -a, -a)

b <- 0.48
w <- c(b, -b, b, -b, -b, -b, -b, -b, -b, -b, -b, -b, -b, -b, -b)

New_Age_Group_Sex %>% ggplot(aes(x = AGE_GRP, y = odds_ratio_female_to_male))+
  geom_line(group = 1)+
  geom_point()+
  xlab("Age Group")+
  ylab("Odds Ratio (Female to Male)")+
  ggtitle("Odds Ratios of Thyroid Cancer by Age Group (Female to Male)")+
  geom_label(aes(label = round(odds_ratio_female_to_male,2)), position = position_nudge(y = v, x = w), size = 2.3)+
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2)

#From this graph we can see that the odds ratios are highest in the younger age groups, with the most precise confidence intervals occuring between ages 55-85.
#####Comparing odds ratios over time#####
Male_Thyroid_1999 <- Cancer_Stats %>% filter(EVENT_TYPE == "Incidence", RACE == "All Races", SITE == "Thyroid", SEX == "Male", YEAR == 1999)
Female_Thyroid_1999 <- Cancer_Stats %>% filter(EVENT_TYPE == "Incidence", RACE == "All Races", SITE == "Thyroid", SEX == "Female", YEAR == 1999)
Thyroid_1999 <- inner_join(Male_Thyroid_1999, Female_Thyroid_1999, by = "AGE_GRP", suffix = c("_Male", "_Female"))
Thyroid_1999 <- Thyroid_1999[,c(9,1,3,5,10,12,14)]
Thyroid_1999 <- Thyroid_1999 %>% mutate(OR = odds_ratio(positive_male = NewCount_Male, negative_male = POPULATION_Male-NewCount_Male, positive_female = NewCount_Female, negative_female = POPULATION_Female-NewCount_Female))
OR_1999 <- Thyroid_1999$OR

Male_Thyroid_2011 <- Cancer_Stats %>% filter(EVENT_TYPE == "Incidence", RACE == "All Races", SITE == "Thyroid", SEX == "Male", YEAR == 2011)
Female_Thyroid_2011 <- Cancer_Stats %>% filter(EVENT_TYPE == "Incidence", RACE == "All Races", SITE == "Thyroid", SEX == "Female", YEAR == 2011)
Thyroid_2011 <- inner_join(Male_Thyroid_2011, Female_Thyroid_1999, by = "AGE_GRP", suffix = c("_Male", "_Female"))
Thyroid_2011 <- Thyroid_2011[,c(9,1,3,5,10,12,14)]
Thyroid_2011 <- Thyroid_2011 %>% mutate(OR = odds_ratio(positive_male = NewCount_Male, negative_male = POPULATION_Male-NewCount_Male, positive_female = NewCount_Female, negative_female = POPULATION_Female-NewCount_Female))
OR_2011 <- Thyroid_2011$OR



#Finally, we can see how the odds ratios have changed for each age group from the beginning of the data set in 1999 to the end in 2011

vec_1999 <- c(1:15)
vec_2011 <- c(1:15)

vec_1999[1:15] <- "1999"
vec_2011[1:15] <- "2011"

vec_age_group <- Thyroid_2011$AGE_GRP

df_2009 <- as.data.frame(bind_cols(vec_1999, OR_1999, vec_age_group))
df_2011 <- as.data.frame(bind_cols(vec_2011, OR_2011,vec_age_group))

df <-as.data.frame(bind_rows(df_2009, df_2011))
df$Year <- as.factor(df$Year)

colnames(df)[1] <- "Year"
colnames(df)[2] <- "OR"
colnames(df)[3] <- "Age_Group"

df %>% ggplot(aes(x = Age_Group))+
  geom_line(aes(y = OR, color = Year, group = Year))+
  geom_point(aes(y = OR, color = Year))+
  xlab("Age Group")+
  ylab("Odds Ratio")+
  ggtitle("Odds Ratio of Thyroid Cancer (Female to Male) by Year")+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))
#Notice that in 1999, the odds ratios are all above 1, indicating the odds of thyroid cancer are higher for women in all age groups, although the odds ratio
#does appear to approace 1 in the older age groups.  However, in 2011, the odds ratio actually does fall below 1 in all age groups over age 65, indicating
#the odds of thyroid cancer are higher for males in these age groups in 2011.
