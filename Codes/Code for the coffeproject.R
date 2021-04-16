


# Some of the code for the coffee projects: 

#Rami WD
setwd("~/Desktop/MSc Economics and Business Adm. in Business Intelligence/Business Intelligence/Data Science Project/Coffee project/Data")
data = read_xlsx("dataset_complete_only drinkers.xlsx")

#Dasha WD
dataset_complete_only_drinkers <- read_excel("C:/Users/Dasha/OneDrive - Aarhus Universitet/AU/Business Intelligence/semester 2/Data Science Project/Data Science Project/Shkatulka/dataset_complete_only drinkers.xlsx")

#Mahdi WD
data = read_excel("C:/Users/mahdi/Dropbox/Business Intelligence (cand.merc.)/2. semester/Data Science Project/Project - Group 13/Shkatulka/dataset_complete_only drinkers.xlsx")

library(FSA)
library(readxl)
library(caret)
library(tidyverse)



view(data)


######### 1.Charachterization of the variabels. ####### 

str(data)

# The only correct one is coloumn 26, which is a character.
# Everything else is wrongly classlified as either numerical or character since they need to be categoricals.
# transform the variables 1 to 25 to factors:
# OBS. We might have to change them to numercial again in order to do a boxplot. 

data[,1:25] = lapply(data[,1:25], as.factor)


###### 2. Renameing some of the variables and combine the coloumns to one  ######## 
# We will make an additional coloumn us to get an overview of all of the cities
# this is done by making a new coloumn ##### 



# Renaming the data$Location factors to city names
data$Location = factor(data$Location,
       levels = c("1","2","3","4","5","6"),
       labels = c("Copenhagen","Aarhus","Odense", "Aalborg", "Esbjerg", "Others"))

data$Gender = factor(data$Gender,
                       levels = c("0","1"),
                       labels = c("Male","Female"))



# The sample: 
table(data$Location)

# Did not include Grenoble which is why I divide with 288. 
# Copenhagen: 106/288 ≈ 36,8% 
# Aarhus: 92/288 ≈ 31,9% 
# Odense: 42/288 ≈ 14,5% 
# Aalborg: 7/288 ≈ 2,4% 
# Esbjerg: 0
# Others: 42/288 ≈ 31,9% 



# Combining the two location coloumns into one coloumn. (The names will be combined)

data$Location2 <- paste(data$Location, "-", data$Location_other)

# Renaming the coloumns so we only have the city names.  
data$Location2 = data$Location2 %>% str_replace("- NA","")
data$Location2 = data$Location2 %>% str_replace("Others - ","")

data$Location2 = as.factor(data$Location2)


# Getting the mean and median of people based upon location.
data$Willingness = as.integer(data$Willingness)
p=Summarize(data$Willingness ~ data$Location2) # This will give us the median of the willingness in the city-samples 
p

# We could use the median information to colour the different pin-points (cities) according to their willingness to get a subscription.  

######## 3. Examining the sample #############

# The sample: 
table(data$Location2)

# Did not include Grenoble which is why I divide with 288. 
# Copenhagen: 106/288 ≈ 36,8% 
# Aarhus: 92/288 ≈ 31,9% 
# Odense: 42/288 ≈ 14,5% 
# Aalborg: 7/288 ≈ 2,4% 
# Esbjerg: 0
# Others: 42/288 ≈ 31,9% 

# Gives an overview in the relation between Willingness and Location. 
ggplot(data, aes(Location2, Willingness)) + 
  geom_jitter(width = 0.25)

ggplot(data, aes(Willingness, Education_level)) + 
  geom_jitter()

# Will show the educational level based upon location(city)
table(data$Location2,data$Education_level)


# Examning the different genders 
table(data$Willingness,data$Gender)
summary(data$Gender)

data$Willingness = as.numeric(data$Willingness) # Need at continous variable to create a boxplot


# Gender-basis mean values in the willingness of coffe subscriptions. (the white dot emphazises the mean)
means = aggregate(Willingness ~ Gender, data = data, mean)
ggplot(data=data, aes(x=Gender, y=Willingness, fill=Gender)) + geom_boxplot() +
  stat_summary(fun=mean, colour="white", geom="point", 
               shape=18, size=3, show.legend=FALSE) + 
  geom_text(data = means, aes(label = Willingness, y = Willingness + 0.10))

# It can be seen that the mean is significantly higher for Male (roughly 4) compared to females (3,4). 
# Furthermore, the classs distribution is different. 
# The male category is normally distributed since median = mean. 
# The female category is right-skewed. 

# The different willingness distributions across genders can potentially be a "decision making factor"
# If we are dealing with two almost identical cities with two different but with different population composition.
# Since a Male is more likely to do a coffee-subscription according to the data.


################## 3. Possibly adding a demographical element into the sample based on the location variable. ######## 
# Have not done it yet, what like to hear what you guys have to say.
# The intuition is that cities alike will have similar population-growth rates.
table(data$Location2)

# We will use data from "danmarks statistik" covering the population of the cities in the period 2015-2020;
# number of total people in the city 
# number of females in the city
# number of males in the city

# We will try to extract the mean growth of the city population from the five years (overall and gender specific). 
# 


table(data$Location2)

#########  removing Grenoble right away, so we will not create a factor out of it. 

# R creates alot of missing values, when we use the code below to remove Grenoble, so we need another way. 
# data = data[!(data$Location_other == "Grenoble"),]

str(data)

# View(data)











