# CIND810 Capstone Project - Place of Residence and Vote Choice in the 2019 Canadian Federal Election
# Student: James R. Howard
# Supervisor: Bilgehan Erdem
# Semester: Fall 2020


# Step 1: Obtaining the 2019 Canadian Election Study (CES) dataset
# 1.1 Using the guide from https://osf.io/preprints/socarxiv/a29h8/download, we use the cesR package
# to download the 2019 Canadian Election Study. We will be using the 2019 web survey.

# install cesR package from GitHub
devtools::install_github("hodgettsp/cesR")
# install labelled package from CRAN
install.packages("labelled")
# load cesR and labelled packages
library(cesR)
library(labelled)

# request 2019 CES online survey
get_ces("ces2019_web")
# convert dataframe values to factor type
ces2019_web <- as_factor(ces2019_web)
# check column heads for dataframe
head(ces2019_web)

# Step 2: Obtaining the necessary packages to analyze the data
install.packages("dplyr")
library(dplyr)
install.packages("forcats")
library(forcats)
install.packages("tidyverse")
library(tidyverse)

# Step 3: Sorting the variables of interest in your data into a new data.frame
# The big question here is how to handle NA values in your data. You were operating under the assumption they should be removed 
# for your analysis, but this has presented problems when trying to create a new data.frame that has no NA values. 
# Recall that the cps is tje campaign-period study and pes is the post-election study of the Canadian Election Study.

new.ces2019_web <- data.frame(
  "Education" = ces2019_web$cps19_education,
  "Employment" = ces2019_web$cps19_employment,
  "Religion's Importance" = ces2019_web$cps19_rel_imp,
  "Religion" = ces2019_web$cps19_religion,
  "Vote Choice" = ces2019_web$cps19_votechoice, #So this variable needs to be changed
  "Place of Residence" = ces2019_web$pes19_rural_urban,
  "Left/Right Belief Scale, Self" = ces2019_web$pes19_lr_self_1,
  "Family Values" = ces2019_web$pes19_famvalues,
  "Women's Place in the Home" = ces2019_web$pes19_womenhome,
  "Racial Minorities" = ces2019_web$cps19_groups_therm_1,
  "Immigrants" = ces2019_web$cps19_groups_therm_2,
  "Feminists" = ces2019_web$cps19_groups_therm_4, 
  "Politicians in General" = ces2019_web$cps19_groups_therm_5,
  "Aboriginal Peoples" = ces2019_web$pes19_groups1_1,
  "Gays and Lesbians" = ces2019_web$pes19_groups1_2,
  "Muslims living in Canada" = ces2019_web$pes19_groups1_3,
  "Politicians in General" = ces2019_web$pes19_groups1_4,
  "Times Volunteering for a Group or Organization" = ces2019_web$cps19_volunteer,
  "Owns a Residence" = ces2019_web$cps19_property_1,
  "Owns a business, a piece of property, a farm or livestock" = ces2019_web$cps19_property_2,
  "Owns stock or bonds" = ces2019_web$cps19_property_3,
  "Have Canadian Citizenship" = ces2019_web$cps19_citizenship,
  "Age" = ces2019_web$cps19_age,
  "Importance of Voting" = ces2019_web$pes19_diff_happens_1,
  "Nativism - Immigration" = ces2019_web$pes19_nativism5,
  "Do you work in the public, private, or not-for-profit sector?" = ces2019_web$cps19_sector,
  "Trust in Others" = ces2019_web$pes19_trust,
  "Newer lifestyles" = ces2019_web$pes19_newerlife,
  "Is income inequality a problem in Canada?" = ces2019_web$pes19_inequal,
  "Does not own property, stocks, or bonds" = ces2019_web$cps19_property_5,
  "Do you have any savings" = ces2019_web$cps19_property_4
)


# Step 4: Cleaning the variables of interest in your new data.frame.
# Remember, first we need to find missing values. To do that, we use the function is.na().
#### DISCLAIMER #### The number of NAs in these variables from ces2019_web can be assumed to be the same
#### number of NAs of the same variables in the new.ces2019_web data.frame

# Let's see if we can count the number of Missing values in cps19_education. Summarize and print. 
sum(is.na(ces2019_web$cps19_education))
# this approach worked. We can move forward knowing there are no missing values in this variable.
# The same needs to be done with every other variable we intend to use in our dataset.
# The ones that have missing values need to be removed. 
# Should the be removed one variable at a time, or can it be done to an entire data.frame?

# The next variable is cps19_employment
sum(is.na(ces2019_web$cps19_employment))
# 0 NA values observed

# The next value is cps19_rel_imp
sum(is.na(ces2019_web$cps19_rel_imp))
# 9834 NA values were recorded. We will want those removed from out analysis.
# To do this, we will need to create a new variable that has been Processed of NA values.
# We want to use cps19_rel_imp_Processed, but we will do this in our NEW data.frame, so as not to 
# affect the original dataset. 

# The next variable is cps19_religion
sum(is.na(ces2019_web$cps19_religion))
# 0 NA values observed

# The next variable is cps19_votechoice - Riyad Nov 12, probably take them out.
# work on them later on. 

sum(is.na(ces2019_web$cps19_votechoice))
# 6258 NA values observed
# This variable needs to be changed, because its label is "which party do you THINK you will vote for"
# It is not "who did you vote for", it is the cps variable
# This is the variable you should be using instead
sum(is.na(ces2019_web$pes19_votechoice2019))
# 29215 NAs observed. 

# The next variable is pes19_rural_urban
sum(is.na(ces2019_web$pes19_rural_urban))
# 27482 NA values recorded

# The next variable is cps19_lr_scale_bef_1
sum(is.na(ces2019_web$cps19_lr_scale_bef_1))
# 23319 NA values recorded

# The next variable is cps19_lr_scale_aft_1
sum(is.na(ces2019_web$cps19_lr_scale_aft_1))
# 22989 NA values recorded

# The next variable is pes19_lr_self_1
sum(is.na(ces2019_web$pes19_lr_self_1))
# 33453 NA values recorded

# The next variable is pes19_famvalues
sum(is.na(ces2019_web$pes19_famvalues))
# 27482 NA values recorded

# The next variable is pes19_womenhome
sum(is.na(ces2019_web$pes19_womenhome))
# 27482 NA values

# The next variable is cps19_groups_therm_1
sum(is.na(ces2019_web$cps19_groups_therm_1))
# 6734

# The next variable is cps19_groups_therm_2
sum(is.na(ces2019_web$cps19_groups_therm_2))
# 6248 NA values

#The next varibale is cps19_groups_therm_3
sum(is.na(ces2019_web$cps19_groups_therm_3))
# 6876 NA values

# the next variable is cps19_groups_therm_4
sum(is.na(ces2019_web$cps19_groups_therm_4))
# 6183 NA values

# The next variable is cps19_groups_therm_5
sum(is.na(ces2019_web$cps19_groups_therm_5))
# 5834 NA values

# The next variable pes19_groups1_1
sum(is.na(ces2019_web$pes19_groups1_1))
# 28610 NA values

# The next variable is pes19_groups1_2
sum(is.na(ces2019_web$pes19_groups1_2))
# 28740 NA values

# The next variable is pes19_groups1_3
sum(is.na(ces2019_web$pes19_groups1_3))
# 28707 NA values

# The next variable is pes19_groups1_4
sum(is.na(ces2019_web$pes19_groups1_4))
# 28503 NA values

# The next variable is cps19_volunteer
sum(is.na(ces2019_web$cps19_volunteer))
# 0 NA values recorded 

# The next variable is cps19_property_1
sum(is.na(ces2019_web$cps19_property_1))
# 13023 NA values

# The next variable is cps19_property_2
sum(is.na(ces2019_web$cps19_property_2))
# 34452 NA values

# cThe next variable is ps19_property_3
sum(is.na(ces2019_web$cps19_property_3))
# 27205

# The next variable is cps19_citizenship
sum(is.na(ces2019_web$cps19_citizenship))
# 0 NA values

# The next vairable is cps19_age
sum(is.na(ces2019_web$cps19_age))
# 0 NA values

# The next variable is pes19_diff_happens
sum(is.na(ces2019_web$pes19_diff_happens_1))
# 28032 NA values

# The next variable is pes19_nativism5
sum(is.na(ces2019_web$pes19_nativism5))
# 27482 NS values

#the next variable is cps19_sector 
sum(is.na(ces2019_web$cps19_sector))
# 18283 NA values

# Next variable is pes19_trust 
sum(is.na(ces2019_web$pes19_trust)) 
# 27482 NA values

# Next variable is pes19_newerlife 
sum(is.na(ces2019_web$pes19_newerlife))
# NA 27482 

# Next variable is pes19_inequal
sum(is.na(ces2019_web$pes19_inequal))
# NA 27482 

# Great work! You have identified the variables with NAs you are going to put into your new data.frame. 
# The next step is to put them into your data.frame. From there, you will go on to remove the NA values.
# November 14, 2020
# You can have these variables cleaned, have their NAs replaced, without having to create new variables or a new data.frame!
# this would be much easier for you. The first step in this direction would be to do a test and confirm this works.

#To replace NAs with the mode, for factor variables, we want to use the following equation
########################################
# To Recap
# This is how you can replace NAs for numeric variables.
sum(is.na(new.ces2019_web$Feminists))
new.ces2019_web$Feminists[which(is.na(new.ces2019_web$Feminists))] <- median(new.ces2019_web$Feminists, na.rm = TRUE)
# Success!

#This is how you rpelace NAs for categorical variables
Rel.Imp.mx = table(new.ces2019_web$Religion.s.Importance) 
Rel.Imp.mx 
names(Rel.Imp.mx) ### 
Rel.Imp.mx[Rel.Imp.mx==max(Rel.Imp.mx)] ### Display the max values which is the mode
# Check for NAs
summary(is.na(new.ces2019_web$Religion.s.Importance))
#replace NAs with Mode
new.ces2019_web$Religion.s.Importance[is.na(new.ces2019_web$Religion.s.Importance)] <- "Somewhat important"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Religion.s.Importance))


# The following variables need to be cleaned. A new data.frame is NOT NECESSARY.

# Question 1
#Vote Choice - Categorical

pes.vote.choice.mx = table(ces2019_web$cps19_votechoice) ### Get mtcars$mpg distribution
names(pes.vote.choice.mx) ### Display the names, which are the original values of religious importance
pes.vote.choice.mx[pes.vote.choice.mx==max(pes.vote.choice.mx)] ### Display the max values which is the mode
summary(is.na(ces2019_web$cps19_votechoice))
summary(is.na(ces2019_web$cps19_votechoice)) # this variable has more data... despite it being the Post-election Study.
# Here you are again wondering if this is the right variable to use... Didn't Riyad prefer the larger variable from the cps??
# Yes he did, but you issue is that it wasn't as accurate as the current one you're using. Maybe you need to compare the two.
fct_count(ces2019_web$cps19_votechoice)
# The thing is, the totals look pretty even across the two. Given that fact, I am making the executive decision to use the 
# cps19 varaible instead, as I believe it will be more accurate. You could always compare 
ces2019_web$cps19_votechoice[is.na(ces2019_web$cps19_votechoice)] <- "Liberal Party"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(ces2019_web$cps19_votechoice))
# The thing is, the totals look pretty even across the two 


# Place of Residence - Categorical

Residence.mx = table(new.ces2019_web$Place.of.Residence) ### Get mtcars$mpg distribution
Residence.mx ### Display religious importance values distribution. Values are the frequencies of original values
names(Residence.mx) ### Display the names, which are the original values of religious importance
Residence.mx[Residence.mx==max(Residence.mx)] ### Display the max values which is the mode
summary(is.na(new.ces2019_web$Place.of.Residence))
new.ces2019_web$Place.of.Residence[is.na(new.ces2019_web$Place.of.Residence)] <- "A large town or city (more than 50K people)"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Place.of.Residence))


# Question 2
# Education - Categorical
Education.mx = table(new.ces2019_web$Education) ### Get mtcars$mpg distribution
Education.mx ### Display religious importance values distribution. Values are the frequencies of original values
names(Education.mx) ### Display the names, which are the original values of religious importance
Education.mx[Education.mx==max(Education.mx)] ### Display the max values which is the mode
summary(is.na(new.ces2019_web$Education))
new.ces2019_web$Education[is.na(new.ces2019_web$Education)] <- "Bachelor's degree"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Education))

#Employment - Categorical
# No NAs

# Religion - Categorical
# No NAs
# Age ... numeric?
Processed.Age <- na.omit(new.ces2019_web$Age)
sum(is.na(new.ces2019_web$Age))

# The Ownership questions were recoded using fct_explicit_na(), as it replaces NA with a new Factor level.
# Make sure your variables are converted to factors
# Owns.a.Residence - Logical - you did this WRONG
new.ces2019_web$Owns.a.Residence <- factor(new.ces2019_web$Owns.a.Residence)
summary(is.na(new.ces2019_web$Owns.a.Residence))
new.ces2019_web$Owns.a.Residence <- fct_explicit_na(new.ces2019_web$Owns.a.Residence, na_level = "Does not own a residence")
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Owns.a.Residence))
fct_count(new.ces2019_web$Owns.a.Residence)

# Owns.a.business..a.piece.of.property..a.farm.or.livestock - Categorical - convert to factor
new.ces2019_web$Owns.a.business..a.piece.of.property..a.farm.or.livestock <- factor(new.ces2019_web$Owns.a.business..a.piece.of.property..a.farm.or.livestock)
# Check for NAs
summary(is.na(new.ces2019_web$Owns.a.business..a.piece.of.property..a.farm.or.livestock))
#replace NAs with Mode
new.ces2019_web$Owns.a.business..a.piece.of.property..a.farm.or.livestock <- fct_explicit_na(new.ces2019_web$Owns.a.business..a.piece.of.property..a.farm.or.livestoc, na_level = "Does not own a business, a piece of property, a farm or livestock")
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Owns.a.business..a.piece.of.property..a.farm.or.livestock))
fct_count(new.ces2019_web$Owns.a.business..a.piece.of.property..a.farm.or.livestock)

# Owns.stock.or.bonds - Categorical- convert to factor
new.ces2019_web$Owns.stock.or.bonds <- factor(new.ces2019_web$Owns.stock.or.bonds) 
# Check for NAs
summary(is.na(new.ces2019_web$Owns.stock.or.bonds))
#replace NAs with Mode
new.ces2019_web$Owns.stock.or.bonds <- fct_explicit_na(new.ces2019_web$Owns.stock.or.bonds, na_level = "Does not own stocks or bonds")
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Owns.stock.or.bonds))
fct_count(new.ces2019_web$Owns.stock.or.bonds)

# Do you have any savings
new.ces2019_web$Do.you.have.any.savings <- factor(new.ces2019_web$Do.you.have.any.savings)
summary(new.ces2019_web$Do.you.have.any.savings)
new.ces2019_web$Do.you.have.any.savings <- fct_explicit_na(new.ces2019_web$Do.you.have.any.savings, na_level = "Does not have any savings")
fct_count(new.ces2019_web$Do.you.have.any.savings)

# Question 3
# Religion's Importance - Categorical
# Completed in the example above
# Family.Values - Categorical
Family.Values.mx = table(new.ces2019_web$Family.Values) ### Get mtcars$mpg distribution
Family.Values.mx ### Display religious importance values distribution. Values are the frequencies of original values
names(Family.Values.mx) ### Display the names, which are the original values of religious importance
Family.Values.mx[Family.Values.mx==max(Family.Values.mx)] ### Display the max values which is the mode
summary(is.na(new.ces2019_web$Family.Values))
new.ces2019_web$Family.Values[is.na(new.ces2019_web$Family.Values)] <- "Somewhat agree"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Family.Values))

# Women.s.Place.in.the.Home - Categorical
Women.s.Place.in.the.Home.mx = table(new.ces2019_web$Women.s.Place.in.the.Home) ### Get mtcars$mpg distribution
Women.s.Place.in.the.Home.mx ### Display religious importance values distribution. Values are the frequencies of original values
names(Women.s.Place.in.the.Home.mx) ### Display the names, which are the original values of religious importance
Women.s.Place.in.the.Home.mx[Women.s.Place.in.the.Home.mx==max(Women.s.Place.in.the.Home.mx)] ### Display the max values which is the mode
summary(is.na(new.ces2019_web$Women.s.Place.in.the.Home))
new.ces2019_web$Women.s.Place.in.the.Home[is.na(new.ces2019_web$Women.s.Place.in.the.Home)] <- "Strongly disagree"
# confirm NAs have been replaces by checking again for NA values
sum(is.na(new.ces2019_web$Women.s.Place.in.the.Home))


# Racial.Minorities - Numeric, not categorical
sum(is.na(new.ces2019_web$Racial.Minorities))
new.ces2019_web$Racial.Minorities[which(is.na(new.ces2019_web$Racial.Minorities))] <- median(new.ces2019_web$Racial.Minorities, na.rm = TRUE)
sum(is.na(new.ces2019_web$Racial.Minorities))


# Immigrants - numeric 
sum(is.na(new.ces2019_web$Immigrants))
new.ces2019_web$Immigrants[which(is.na(new.ces2019_web$Immigrants))] <- median(new.ces2019_web$Immigrants, na.rm = TRUE)
sum(is.na(new.ces2019_web$Immigrants))


# Feminists - Numeric
sum(is.na(new.ces2019_web$Immigrants))
new.ces2019_web$Immigrants[which(is.na(new.ces2019_web$Immigrants))] <- median(new.ces2019_web$Immigrants, na.rm = TRUE)
sum(is.na(new.ces2019_web$Immigrants))

# Politicians.in.General - Numeric
sum(is.na(new.ces2019_web$Politicians.in.General))
new.ces2019_web$Politicians.in.General[which(is.na(new.ces2019_web$Politicians.in.General))] <- median(new.ces2019_web$Politicians.in.General, na.rm = TRUE)
sum(is.na(new.ces2019_web$Politicians.in.General))

# Aboriginal.Peoples - numeric
sum(is.na(new.ces2019_web$Aboriginal.Peoples))
new.ces2019_web$Aboriginal.Peoples[which(is.na(new.ces2019_web$Aboriginal.Peoples))] <- median(new.ces2019_web$Aboriginal.Peoples, na.rm = TRUE)
sum(is.na(new.ces2019_web$Aboriginal.Peoples))

# Gays.and.Lesbians - Numeric
sum(is.na(new.ces2019_web$Gays.and.Lesbians))
new.ces2019_web$Gays.and.Lesbians[which(is.na(new.ces2019_web$Gays.and.Lesbians))] <- median(new.ces2019_web$Gays.and.Lesbians, na.rm = TRUE)
sum(is.na(new.ces2019_web$Gays.and.Lesbians))

# Muslims.living.in.Canada - Numeric
sum(is.na(new.ces2019_web$Muslims.living.in.Canada))
new.ces2019_web$Muslims.living.in.Canada[which(is.na(new.ces2019_web$Muslims.living.in.Canada))] <- median(new.ces2019_web$Muslims.living.in.Canada, na.rm = TRUE)
sum(is.na(new.ces2019_web$Muslims.living.in.Canada))

# Politicians.in.General.1 - Numeric
sum(is.na(new.ces2019_web$Politicians.in.General.1))
new.ces2019_web$Politicians.in.General.1[which(is.na(new.ces2019_web$Politicians.in.General.1))] <- median(new.ces2019_web$Politicians.in.General.1, na.rm = TRUE)
sum(is.na(new.ces2019_web$Politicians.in.General.1))

# Times.Volunteering.for.a.Group.or.Organization - Categorical
# No NAs


# Importance.of.Voting - Numeric 
sum(is.na(new.ces2019_web$Importance.of.Voting))
new.ces2019_web$Importance.of.Voting[which(is.na(new.ces2019_web$Importance.of.Voting))] <- median(new.ces2019_web$Importance.of.Voting, na.rm = TRUE)
sum(is.na(new.ces2019_web$Importance.of.Voting))

# Nativism...Immigration - Categorical
sum(is.na(new.ces2019_web$Nativism...Immigration))
Nativism...Immigration.mx = table(new.ces2019_web$Nativism...Immigration) 
Nativism...Immigration.mx 
names(Nativism...Immigration.mx) ### 
Nativism...Immigration.mx[Nativism...Immigration.mx==max(Nativism...Immigration.mx)] ### Display the max values which is the mode
# Check for NAs
summary(is.na(new.ces2019_web$Nativism...Immigration))
#replace NAs with Mode
new.ces2019_web$Nativism...Immigration[is.na(new.ces2019_web$Nativism...Immigration)] <- "Strongly disagree"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Nativism...Immigration))

# Do.you.work.in.the.public..private..or.not.for.profit.sector. - Categorical
Do.you.work.in.the.public..private..or.not.for.profit.sector..mx = table(new.ces2019_web$Do.you.work.in.the.public..private..or.not.for.profit.sector.) 
Do.you.work.in.the.public..private..or.not.for.profit.sector..mx 
names(Do.you.work.in.the.public..private..or.not.for.profit.sector..mx) ### 
Do.you.work.in.the.public..private..or.not.for.profit.sector..mx[Do.you.work.in.the.public..private..or.not.for.profit.sector..mx==max(Do.you.work.in.the.public..private..or.not.for.profit.sector..mx)] ### Display the max values which is the mode
# Check for NAs
summary(is.na(new.ces2019_web$Do.you.work.in.the.public..private..or.not.for.profit.sector.))
#replace NAs with Mode
new.ces2019_web$Do.you.work.in.the.public..private..or.not.for.profit.sector.[is.na(new.ces2019_web$Do.you.work.in.the.public..private..or.not.for.profit.sector.)] <- "Private sector"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Do.you.work.in.the.public..private..or.not.for.profit.sector.))

# Trust.in.Others - Categorical
Trust.in.Others.mx = table(new.ces2019_web$Trust.in.Others) 
Trust.in.Others.mx 
Trust.in.Others.mx[Trust.in.Others.mx==max(Trust.in.Others.mx)] ### Display the max values which is the mode
# Check for NAs
summary(is.na(new.ces2019_web$Trust.in.Others))
#replace NAs with Mode
new.ces2019_web$Trust.in.Others[is.na(new.ces2019_web$Trust.in.Others)] <- "You need to be very careful when dealing with people"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Trust.in.Others))
fct_count(new.ces2019_web$Trust.in.Others)
# Newer.lifestyles - Categorical
Newer.lifestyles.mx = table(new.ces2019_web$Newer.lifestyles) 
Newer.lifestyles.mx 
Newer.lifestyles.mx[Newer.lifestyles.mx==max(Newer.lifestyles.mx)] ### Display the max values which is the mode
# Check for NAs
summary(is.na(new.ces2019_web$Newer.lifestyles))
#replace NAs with Mode
new.ces2019_web$Newer.lifestyles[is.na(new.ces2019_web$Newer.lifestyles)] <- "Somewhat agree"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Newer.lifestyles))

#Does not own stocks or bonds... is a logical verctor that you've turned into a factor. 
# You also need to create a new level to store the NAs you're changing, using fct_explicit_na 
new.ces2019_web$Does.not.own.property..stocks..or.bonds <- factor(new.ces2019_web$Does.not.own.property..stocks..or.bonds)
# Check for NAs
summary(is.na(new.ces2019_web$Does.not.own.property..stocks..or.bonds))
#replace NAs with another level
new.ces2019_web$Does.not.own.property..stocks..or.bonds <- fct_explicit_na(new.ces2019_web$Does.not.own.property..stocks..or.bonds, na_level = "Owns one of more of these")
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Does.not.own.property..stocks..or.bonds))
fct_count(new.ces2019_web$Does.not.own.property..stocks..or.bonds)

# You definitely need to add the other responses to this question that you included in your new table. YES!!! You should be excited about this progress
# If you keep up the pace of your work then you may get all your cleaning done by tonight!!

# Owns stocks or bonds...
new.ces2019_web$Owns.a.Residence <- factor(new.ces2019_web$Owns.a.Residence)
# Check for NAs
summary(is.na(new.ces2019_web$Owns.a.Residence))
#replace NAs with another level
new.ces2019_web$new.ces2019_web$Owns.a.Residence <- fct_explicit_na(new.ces2019_web$Does.not.own.property..stocks..or.bonds, na_level = "Owns one of more of these")
# confirm NAs have been replaces by checking again for NA values
sum(is.na(new.ces2019_web$new.ces2019_web$Owns.a.Residence))
str(new.ces2019_web$Owns.a.Residence)
fct_count(new.ces2019_web$Does.not.own.property..stocks..or.bonds)



# Is.income.inequality.a.problem.in.Canada. - Categorical
Is.income.inequality.a.problem.in.Canada.mx = table(new.ces2019_web$Is.income.inequality.a.problem.in.Canada) 
Is.income.inequality.a.problem.in.Canada.mx 
Is.income.inequality.a.problem.in.Canada.mx[Is.income.inequality.a.problem.in.Canada.mx==max(Is.income.inequality.a.problem.in.Canada.mx)] ### Display the max values which is the mode
# Check for NAs
summary(is.na(new.ces2019_web$Is.income.inequality.a.problem.in.Canada))
#replace NAs with Mode
new.ces2019_web$Is.income.inequality.a.problem.in.Canada[is.na(new.ces2019_web$Is.income.inequality.a.problem.in.Canada)] <- "Probably yes"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Is.income.inequality.a.problem.in.Canada))

# Left.Right.Belief.Scale..Self - numeric
sum(is.na(new.ces2019_web$Left.Right.Belief.Scale..Self))
new.ces2019_web$Left.Right.Belief.Scale..Self[which(is.na(new.ces2019_web$Left.Right.Belief.Scale..Self))] <- median(new.ces2019_web$Left.Right.Belief.Scale..Self, na.rm = TRUE)
sum(is.na(new.ces2019_web$Left.Right.Belief.Scale..Self))
# Let's now check the whole data.frame for NAs
sum(is.na(new.ces2019_web$Left.Right.Belief.Scale..Self))
summary(new.ces2019_web$Left.Right.Belief.Scale..Self)
# Now, let's confirm there are no more NAs in your data.frame
summary(is.na(new.ces2019_web))
#v 56222... how is this possible?
# You need to remove htis variable Is.income.inequality.a.problem.in.Canada.
# You need to clean this variable, that for some reason doesn't want to be clean even though it has been cleaned...
names(new.ces2019_web)
new.ces2019_web <- new.ces2019_web[,-29]




##### What are your options in this case? Should you just be trying to remove NA values, or inputing them? #####
##### November 14th, 2020:
##### You need to be inputing or replacing NA values!! You have talked to Riyad, and this is 
##### the best mode of action. Numeric columns with NA values will be replaced with the median, 
##### and factor columns need to be replaced with the mode 
# Step 5: Recoding variables of interest for logit regression
# 5.1 We want to make sure our dependent variable, Vote Choice, is binomial. That is, we wish to recode
# the variable to "Voted Conservative", and "Did Not Vote Conservative". 
# We need to collapse the levels in this factoral variable using fct_collapse(). In this way, you are trying to break down
# the levels of the Independent variables you wish to use in your model into individual levels, 

# Vote Choice - Dependent Variable, November 16 2020
new.ces2019_web$Vote.Choice <- fct_collapse(new.ces2019_web$Vote.Choice,
                                      Conservative = c("Conservative Party", "People's Party"),
                                      Did.not.Vote.Conservative  = c("Liberal Party", "ndp", "Bloc Québécois",
                                                          "Green Party", "Another party (please specify)",
                                                          "Don't know/ Prefer not to answer"))

# Step 6: Research Questions and Performing Logistic Regression
# 6.1 Question 1: Are rural and/or suburban residents more likely to support conservative parties compared to their urban counterparts? 
# You would like to be able to break down the results by province. You recall being able to do this
# 6.2 Question 2: Are rural and/or suburban residents more likely to support conservative parties than their urban counterparts even after taking into account socio-demographic characteristics? 
# 6.3 Question 3: Are rural and/or suburban residents more likely to support conservative parties relative to their urban counterparts even after taking into account individual level values and beliefs? 

# GLM() testing model
names(new.ces2019_web)
new.ces2019_web <- new.ces2019_web[-33]
model.A <- glm(Vote.Choice ~., data = new.ces2019_web, family="binomial")
summary(model.A)
####### The Caret package will alow us to split our data into a training set, and a test set, to help us evalutae our model.
install.packages("caret")
library(caret)
library(tidyverse)
#### Now we want to begin by splitting our data into a training and a test set
#1. Begin with setting your seed
set.seed(234)
train.samples <-createDataPartition(new.ces2019_web$Vote.Choice, p= 0.75, list = FALSE)

training.dataset <- new.ces2019_web[train.samples,]
testing.dataset <- new.ces2019_web[-train.samples,]
# Split Rule is the ne
splitrule <- trainControl(method = "repeatedcv", number=10, repeats = 10, classProbs = T, summaryFunction = twoClassSummary)
# Now we are going to build the model
gbmModel <- train(Vote.Choice~., data = training.dataset, trControl=splitrule, method="gbm", preProcess("center", "scale"), metric="ROC")
# error in model = variable lengths differ (founr for 'Education'), whAat does this mean? 
# Answer: removed new.ces2019_web from Vote.Choice, model was able to run, asked to DL gbm package. Got a different error, 
# Error: Matrices or data frames are required for preprocessing. I suspect this is because some of the variables are factors. 
# It is suggested that, to use the preprocess(), you will need to convert the factors to dummy variables, using for example caret/s dummyVars; 
# I guess you need to have the factor variabels excluded from preprocess... ugh
# NOPE! You only need to create dummy variables of your factor variables. You can acheive this by using the dummyVars() to create a new table from which you can 
# THEN perform your model 
# So in this case we need to create dummy variables for our varaibles that are factors. We do this by creating dummy variables.
dummy <- dummyVars("~.", data=new.ces2019_web)
# Now we create a data.frame to house our variables. It won't tranform the ID, but will create binary columns of our factor variables.
transform <- data.frame(predict(dummy, newdata = new.ces2019_web))
# Now we need to confirm that all of our variables are binary.
str(transform)
# Success! Now I believe we have to run our model through with this new data. So this may require that we develop a training and test set with the newly transformed data.frame.
set.seed(234)
transform.train.samples <-createDataPartition(transform$Vote.Choice.Conservative, p= 0.75, list = FALSE)

transform.training.dataset <- transform[transform.train.samples,]
transform.testing.dataset <- transform[-transform.train.samples,]
# Split Rule is the ne
splitrule <- trainControl(method = "repeatedcv", number=10, repeats = 10, classProbs = T, summaryFunction = twoClassSummary)
# Now we are going to build the model
gbmModel1 <- train(Vote.Choice.Conservative~., data = transform.training.dataset, trControl=splitrule, method="gbm", preProcess("center", "scale"), metric="ROC")
# The model is asking us to use a 2 level factor as out outcome column. So then I think we need to convert Vote.Choice.Conservative to a factor
transform$Vote.Choice.Conservative <- as.numeric(transform$Vote.Choice.Conservative)
str(transform)
# Now I get the same error, that matrices or data frames are required for preprocessing... Do I switch my DV back to numeric?
# It didn't make a difference. What I don't understand is that in my working environment, transform.training.dataset IS listed as a data.frame.

