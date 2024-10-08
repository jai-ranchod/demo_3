#####Loading Libraries and Introducing Data#####
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggrepel")) install.packages("ggrepel")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("rvest")) install.packages("rvest")
if (!require("reldist")) install.packages("reldist")
if (!require("stringr")) install.packages("stringr")

library("ggplot2")
library("ggrepel")
library("readxl")
library("dplyr")
library("rvest")
library("reldist")
library("stringr")
#Here we are using a data set from the Cleveland Clinic regarding cancer rates from 1999 to 2011.
#Specifically we will analyze the odds ratios of incidences of Thyroid cancer for men and women 
#across multiple age groups and all races.

#The Data can be loaded by accessing the .csv file I've loaded into github;  the intent is to make this R file 
#self-contained so the user can simply run the script and observe the output.  However, if there is any trouble
#reading the data in from github, please feel free to directly download the .xlsx or .csv file from my github
#repository. (linked in line 28)

#Accessing the data via web scraping

url <-"https://github.com/jai-ranchod/demo_2/blob/dd072b722fc735a87d03a88ee8d63784137f764e/Cancer_Stats.csv"
h <- read_html(url)
Nodes <- h %>% html_nodes("table")
table <- html_table(Nodes[[1]])
table <- str_split(table[,2], ",", simplify = TRUE)
colnames(table) <- table[1,]
table <- table[-1,]
Cancer_Stats <- as.data.frame(table)

#Making numeric columns

Cancer_Stats$NewCount <- as.numeric(Cancer_Stats$NewCount)
Cancer_Stats$POPULATION <- as.numeric(Cancer_Stats$POPULATION)
Cancer_Stats$RATE <- as.numeric(Cancer_Stats$RATE)
Cancer_Stats$YEAR <- as.numeric(Cancer_Stats$YEAR)

head(Cancer_Stats)

#"NewCount" = New Count of either incidences or mortalities
#"Event_Type" = Incidence or Mortality
#"Population" = The population in the specified race, sex, and age group for the specified year
#"Race" = Racial identification of the specified sub-group
#"Sex" = Sex of the specified subgroup
#"Site" = Site of Cancer
#"Year" = Year of data
#"AGE_GRP" = Age group 

#Note that we are only analyzing incidence data here, and since each record in that case represents a new case
#of (in this case) thyroid cancer, we can consider the observations independent.

#####Initial Analysis#####
#First we need to filter our data.  This analysis will consider incidences of thyroid cancer across all
#races.  We will create splits along gender lines to analyze the effect of thyroid cancer on men and women.

Thyroid <- Cancer_Stats %>% filter(EVENT_TYPE == "Incidence", RACE == "All Races", SITE == "Thyroid")

Male_Thyroid <- Thyroid %>% filter(SEX == "Male")
Female_Thyroid <- Thyroid %>% filter(SEX == "Female")

ggplot()+
  geom_density(aes(x = Male_Thyroid$RATE, fill = "Male"), alpha = 0.6)+
  geom_density(aes(x = Female_Thyroid$RATE, fill = "Female"), alpha = 0.6)+
  scale_fill_manual(name = "Sex", breaks = c("Female","Male"), values = c("pink","blue"))+
  xlab("Thyroid Cancers per 10,000 People")+
  ylab("Probability Density")+
  ggtitle("Probability of Thyroid Cancer \n Rate (per 10,000) Regardless \n of Age or Year")

#This shows that in general women face higher rates of thyroid cancer than men, a finding backed here:
# https://www.cancer.gov/news-events/cancer-currents-blog/2021/thyroid-cancer-diagnosed-more-in-women
# Note that this covers cases over the entire time range as well.
#However, this graph does not provide any age specific information, which we find next.

#Note that this graph looks best when zoomed in.

Thyroid %>% filter(SEX == "Male and Female") %>% group_by(AGE_GRP) %>% summarise(Population = sum(POPULATION), Count = sum(NewCount), Rate = 100000*(Count/Population)) %>% ggplot(aes(x = AGE_GRP, y = Rate))+
  geom_bar(stat = "identity")+
  geom_label_repel(aes(label = round(Rate,2)), nudge_y = 1, size = 2)+
  xlab("Age Group")+
  ylab("Thyroid Cancers per 100,000")+
  ggtitle("Thyroid Cancers per 100,000 \n by Age Group")+
  theme(axis.text.x = element_text(angle = 90))
  

#Here we see that rates are highest among the general population between ages 50 and 75.  
#Breaking the rates down by both gender and age group, however, we see that women see higher rates across all age groups,
#and that women face the highest incidence rates between ages 40 - 69 as opposed to the 60 - 84 range which is highest for men.
#Again, this data applies to the population of all years in the dataset.

Thyroid %>% filter(SEX == "Male" | SEX == "Female") %>% group_by(AGE_GRP, SEX) %>% summarise(Population = sum(POPULATION), Count = sum(NewCount), Rate = 100000*(Count/Population)) %>% ggplot(aes(x = AGE_GRP, y = Rate))+
  geom_line(aes(color = SEX, group = SEX))+
  geom_point(aes(color = SEX))+
  xlab("Age Group")+
  ylab("Thyroid Cancers per 100,000")+
  ggtitle("Rate of Thyroid Cancer \n by Age Group and Sex")+
  theme(axis.text.x = element_text(angle = 90))

#####Odds of Thyroid Cancer######
#Next, we can calculate the odds of Thyroid cancer for females and males, and calculated the odds ratios for each age group.
#R does have available odds ratio functions, but for these purposes, I prefer to demonstrate manual calculation.
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
#We see that the p-values indicate statistically significant odds ratios.  
#Examining the "New_Age_Group_Sex" table will confirm that we have enough data to make an odds ratio calculation.
#Note that the "rate" column in this table is cases per 100,000.

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
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2)+
  theme(axis.text.x = element_text(angle = 90))

#From this graph we can see that the odds ratios are highest in the younger age groups, with the most precise confidence intervals occurring between ages 55-85.
#This indicates that at younger age groups women are more prone to thyroid cancer than men, but the disparity evens out as 
#age increases.

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
  ggtitle("Odds Ratio of Thyroid \n Cancer (Female to Male) by Year")+
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))

#Notice that in 1999, the odds ratios are all above 1, indicating the odds of thyroid cancer are higher for women in all age groups, although the odds ratio
#does appear to approach 1 in the older age groups.  However, in 2011, the odds ratio actually does fall below 1 in all age groups over age 65, indicating
#the odds of thyroid cancer are higher for males in these age groups in 2011.
