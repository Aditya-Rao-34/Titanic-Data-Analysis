install.packages("ggplot2")
install.packages("ggthemes")
install.packages("scales")
install.packages("dplyr")
install.packages("mice")
install.packages("randomForest")
install.packages("readr")
install.packages("stringr")


# LOADING PACKAGE

list.files("../input")

library('ggplot2')
library('ggthemes')
library('scales')
library('dplyr')
library('mice')
library('randomForest')
library('readr')
library('stringr')


# Read Train and Test Data

print(getwd())
setwd("F:/R-Lang Project")
train<-read.csv("train.csv",stringsAsFactors = F)
test<-read.csv('test.csv',stringsAsFactors = F)
test$survived<- NA
ncol(train)
ncol(test)


## Combine both test and train data


titanic<-bind_rows(train,test)



# DATA CHECK

str(titanic)
summary(titanic)
head(titanic)

## Checking for missing values in Numerical type columns

table(is.na(titanic$Embarked))
titanic[titanic$Embarked == '', "Embarked"]
table(titanic$Embarked)
table(is.na(titanic$Age))
table(is.na(titanic$Fare))

### with the help of this we get the sense of what type of variables
### and how many are we working. Basically helps in making sense of 
### the data



# DATA CLEANING

## Cleaning the Embarked Column

titanic[titanic$Embarked == '', "Embarked"]
table(titanic$Embarked)

### Assigning the mode to fill the missing values

titanic[titanic$Embarked == '', "Embarked"]<- 'S'

## Cleaning the Age Column

table(is.na(titanic$Age))

### To clean age, we must replace the NA with the median of the dataset

age.median<- median(titanic$Age, na.rm= TRUE)
titanic[is.na(titanic$Age), "Age"]<- age.median
table(is.na(titanic$Age))

## Cleaning the Fare Column

table(is.na(titanic$Fare))
Fare.median<- median(titanic$Fare, na.rm= TRUE)
titanic[is.na(titanic$Fare), "Fare"]<- Fare.median
table(is.na(titanic$Fare))



# FEATURE ENGINEERING 

colnames(titanic)

## Retrieve title from passenger names

titanic$title<- NA
titanic$title <- str_sub(titanic$Name, str_locate(titanic$Name, ",")[ , 1] + 2, str_locate(titanic$Name, "\\.")[ , 1] - 1)
titanic$title

## Show title counts by sex

table(titanic$Sex, titanic$title)

### Convert title with low count into new title

unusual_title<-c('Dona','Lady','the Countess','Capt','Col','Don'
                 ,'Dr','Major','Sir', "Rev", "Jonkheer")

### reassign Mlle, Mme, Ms

titanic$title[titanic$title == "Mlle"]<- 'Miss'
titanic$title[titanic$title == "Ms"]<- 'Miss'
titanic$title[titanic$title == "Mme"]<- 'Miss'



# CATEGORICAL CASTING

titanic$Pclass<- as.factor(titanic$Pclass)
titanic$Sex<- as.factor(titanic$Sex)
titanic$Embarked<- as.factor(titanic$Embarked)
train$Survived<- as.factor(train$Survived)



# DATA EXPLORATION - ASSESSING SURVIVAL RATE

## Overall Survival Rate?

summarise(train, SurvivalRate = sum(Survived)/nrow(train)*100)

ggplot(titanic, aes(x = Survived)) +
  geom_bar(color="firebrick") +
  labs(x= "Survival Count", y= "NUmber of people", title = "Titanic Survival Rates") +
  theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())

prop.table(table(titanic$Survived))

## Survival Rate by Gender?

table(titanic$Survived)

ggplot(train, aes(x = Sex, fill = Survived)) +
  theme_bw() +
  geom_bar() +
  labs(y= "Passenger Count", title = "Titanic Survival rates by Sex")

## Survival Rate by Class?

ggplot(train, aes(x = Pclass, fill = Survived)) +
  theme_bw() +
  geom_bar() +
  labs(y= "Passenger Count", title = "Titanic Survival rates by Class")

## Survival Rate by Class and Sex?

### drilling down into the data using facet_wrap
ggplot(train, aes(x = Sex, fill = Survived)) +
  theme_bw() +
  facet_wrap(~ Pclass) +
  geom_bar() +
  labs(y= "Passenger Count", title = "Titanic Survival rates by Sex")

### This gives a really good insight on how the different classes were treated 
### Since it can be clearly observed that the 3rd class suffered highest fatalities
### in pure magnitude and in percentage of dead

## Survival Rate by Age?

ggplot(train, aes(x = Age, fill = Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Age(binwidth = 5",
       title = "Titanic Survival rates by Age")

## Survival Rate by age when segmented by sex and Class?

ggplot(train, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_density(alpha = 0.5) +
  labs(y = "Survived",
       x = "Age",
       title = "Titanic Survival Rates by Age, Pclass and Sex")

### For better understanding, lets use histograms

ggplot(train, aes(x = Age, fill = Survived)) +
  theme_bw() +
  facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 5) +
  labs(y = "Survived",
       x = "Age",
       title = "Titanic Survival Rates by Age, Class and Sex")




# SUMMARY AND CONCLUSION

## While exploring the data after its checking and cleaning, the following 
## insights can be drawn :
## 1. The survivability rates of men were lesser than that of women
## 2. Most fatalities were suffered by men of 3rd class followed by men of 2nd class, women of 3rd class, then men of 1st class and so on
## 3. Infant survivability was higher except in the 3rd class
## 4. Their survivability can be ranked in the descending order as such : 
## women(1st class)>children(1st class)>children(2nd class)>women(2nd class)>men(1st class)>children(3rd class)
## >women(3rd class)>men(2nd class)>men(3rd class)

## Conclusion: The data given on class which points to the socio-economic difference amongst passengers did play a significant role 
## as a factor contributing to the survivability of an individual. It can be clearly observed if an individual belonged to the first class
## and was female, then that individual would've the highest chance of survivability





