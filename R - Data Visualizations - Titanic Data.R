#####Loading Libraries and Introducing Data#####

#We will be using the built in "Titanic" data set to show some visualizations that can help us better understand
#the data.  The "Titanic" data set shows who survived the infamous crash as well as demographic and commercial
#data about the passengers.

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("titanic")) install.packages("titanic")
library(tidyverse)
library(titanic)

options(digits = 3)    # report 3 significant digits


titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#First we convert the "survived" indicator from data type of "factor" to data type of "numeric"
titanic <- titanic %>% mutate(Survived_YN = as.numeric(Survived) - 1)
View(titanic)

#Survived - 0 = Did not survive, 1 = survived
#Pclass - Passenger class; 1st class being the most expensive, 2nd class, is cheaper, 3rd class is cheapest
#Sex - Sex of passenger, female or male
#Age - Age of passenger; notice occasional missing data here
#SibSp - Number of siblings/spouses on board
#Parch - Number of Parents/Children on board
#Fare - Passage fare in 1912 British Pounds
#Survived_YN - a different expression of survival status

#####Graphics#####

#The first thing that's worth looking at is the breakdown by sex of the population of the ship in general. 
titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, group = Sex, fill = Sex)) + 
  geom_density(alpha = 0.2, bw = 10) +
  xlab("Age")+
  ylab("Density")+
  ggtitle("Distribution of Sex on the Titanic")

#Here we see that younger passengers are more likely to be female while older passengers are more likely to be male with the transition happening around
#the early 20s.

titanic %>% filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)+
  xlab("Age")+
  ylab("Count of People")+
  ggtitle("Survival Status by Age (Age = NA excluded)")+
  scale_fill_discrete(name = "Survival Status", labels = c("Did Not Survive", "Survived"))
#Here we see that the only age groups more likely to survive than die are the younger age group between 0-9 or so
#and the very oldest segment of the population.  The older age groups up until the oldest individuals are more likely to die
#than survive, but not by much.  Unsurprisingly, the age group most likely to die than survive is the middle age group of about 15-35 years old.

titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar()+
  scale_fill_discrete(name = "Survival Status", labels = c("Did Not Survive", "Survived"))+
  xlab("Passenger Class (1st, 2nd, 3rd)")+
  ylab("Count of People")+
  ggtitle("Survival Count by Passenger Class")

#We also see that probability of survival is related to passenger class, with 1st class being most likely to survive, and 3rd class least likely to survive.
#We can illustrate this a little more clearly with a pair of position fill bar plots.

titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = position_fill())+
  scale_fill_discrete(name = "Survival Status", labels = c("Did Not Survive", "Survived"))+
  xlab("Passenger Class (1st, 2nd, 3rd)")+
  ylab("Proportion of People in Class")+
  ggtitle("Survival Proportion by Passenger Class")

titanic %>%
  ggplot(aes(x = Survived_YN, fill = Pclass)) +
  geom_bar(position = position_fill())+
  scale_fill_discrete(name = "Passenger Class", labels = c("1st Class", "2nd Class", "3rd Class"))+
  xlab("Survival Class")+
  ylab("Proportion of People in Survival Status")+
  ggtitle("Survival Proportion by Passenger Class")

#With both of these visualizations we see that passenger class is clearly related to survival rate. Since passenger class is closely related to fare, it also makes
#sense to analyze survival status by fare.  Recall fare is represented in British Pounds as of 1912.

titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived_YN, Fare, group = Survived_YN)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2") +
  geom_point() + 
  geom_jitter()+
  xlab("Survival Status")+
  ylab("Fare (British Pounds in 1912)")+
  ggtitle("Relationship Between Survival Status and Fare")
#Notice that of those who did not survive, there is clearly a cluster at the lower fare range.  Those who did survive are distributed more evenly across the
#entire range of fares.  Notice that we only use data where some fare is present.  

#We can pull together data on sex, survival rate, and passenger class by generating multiple plots simultaneously.

titanic %>% filter(!(is.na(Age))) %>%
  ggplot(aes(x = Age,y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2) +
  scale_fill_discrete(name = "Survival Status", labels = c("Did Not Survive", "Survived"))+
  xlab("Age")+
  ylab("Count Adjusted Proportion of Age Group/Passenger Class")+
  ggtitle("Sex and Passenger Class Survival")+
  facet_grid(Pclass ~ Sex)
#Note that we show first class in the top row, second class in the middle row, and third class in the bottom row.  We show females on the left and
#males on the right.  These visualizations show that first and second class females are significantly more likely to survive than die.  Third class females are
#about equally likely to survive or die, with the older age group (starting ~30 y.o.) being more likely to die.  Second and third
#class males are much more likely to die than survive, with the exception of very young second class males.  Finally we see that
#first class males are about equally likely to survive or die up until about age 40 at which point they begin to become more
#likely to die.

