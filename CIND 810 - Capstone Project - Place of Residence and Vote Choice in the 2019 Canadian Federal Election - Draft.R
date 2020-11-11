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
ces2019_web <- to_factor(ces2019_web)
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
  "Vote Choice" = ces2019_web$cps19_votechoice,
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
  "Province or territory currently living in" = ces2019_web$pes19_province,
  "Does not own property, stocks, or bonds" = ces2019_web$cps19_property_5,
  "Do you have any savings" = ces2019_web$cps19_property_4
)

# Step 4: Cleaning the variables of interest in your new data.frame.
# Remember, first we need to find missing values. To do that, we use the function is.na().
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
# To do this, we will need to create a new variable that has been cleaned of NA values.
# We want to use cps19_rel_imp_cleaned, but we will do this in our NEW data.frame, so as not to 
# affect the original dataset. 

# The next variable is cps19_religion
sum(is.na(ces2019_web$cps19_religion))
# 0 NA values observed

# The next variable is cps19_votechoice
sum(is.na(ces2019_web$cps19_votechoice))
# 6258 NA values observed

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
# Question 1
#Vote Choice
Cleaned.Vote.Choice <- na.omit(new.ces2019_web$Vote.Choice)
# Place of Residence
Cleaned.Place.of.Residence <- na.omit(new.ces2019_web$Place.of.Residence)

# Question 2
# Education
Cleaned.Education <- na.omit(new.ces2019_web$Education)
#Employment
Cleaned.Employment <- na.omit(new.ces2019_web$Employment)
# Religion
Cleaned.Religion <- na.omit(new.ces2019_web$Religion)
# Age
Cleaned.Age <- na.omit(new.ces2019_web$Age)
# The Ownership questions ought to be collapsed together into one variable
# Owns.a.Residence
Cleaned.Owns.a.Residence <- na.omit(new.ces2019_web$Owns.a.Residence)
# Owns.a.business..a.piece.of.property..a.farm.or.livestock
Cleaned.Owns.a.business..a.piece.of.property..a.farm.or.livestock <- na.omit(new.ces2019_web$Owns.a.business..a.piece.of.property..a.farm.or.livestock)
# Owns.stock.or.bonds
Cleaned.Owns.stock.or.bonds <- na.omit(new.ces2019_web$Owns.stock.or.bonds)
Cleaned.Owns.Savings <- na.omit(new.ces2019_web$do.you.have.any.savings)
Cleaned.Does.Not.Own.Property.Stocks.Bonds.Savings <- na.omit(new.ces2019_web$Does.Not.Own.Property..Stocks..Or.Bonds)

# Question 3
# Religion's Importance
Cleaned.Religion.s.Importance <- na.omit(new.ces2019_web$Religion.s.Importance)
# Family.Values
Cleaned.Family.Values <- na.omit(new.ces2019_web$Family.Values)
# Women.s.Place.in.the.Home
Cleaned.Women.s.Place.in.the.Home <- na.omit(new.ces2019_web$Women.s.Place.in.the.Home)
# Racial.Minorities - Numeric, not categorical
Cleaned.Racial.Minorities <- na.omit(new.ces2019_web$Racial.Minorities)
# Immigrants - numeric 
Cleaned.Immigrants <- na.omit(new.ces2019_web$Immigrants)
# Feminists - Numeric
Cleaned.Feminists <- na.omit(new.ces2019_web$Feminists)
# Politicians.in.General - Numeric
Cleaned.Politicians.in.General <- na.omit(new.ces2019_web$Politicians.in.General)
# Aboriginal.Peoples - numeric
Cleaned.Aboriginal.Peoples <- na.omit(new.ces2019_web$Aboriginal.Peoples)
# Gays.and.Lesbians - Numeric
Cleaned.Gays.and.Lesbians <- na.omit(new.ces2019_web$Gays.and.Lesbians)
# Muslims.living.in.Canada
Cleaned.Muslims.living.in.Canada <- na.omit(new.ces2019_web$Muslims.living.in.Canada)
# Politicians.in.General.1 
Cleaned.Politicians.in.General.1 <- na.omit(new.ces2019_web$Politicians.in.General.1)
# Times.Volunteering.for.a.Group.or.Organization
Cleaned.Times.Volunteering.for.a.Group.or.Organization <- na.omit(new.ces2019_web$Times.Volunteering.for.a.Group.or.Organization)

# Importance.of.Voting - Numeric 
Cleaned.Importance.of.Voting <- na.omit(new.ces2019_web$Importance.of.Voting)
# Nativism...Immigration
Cleaned.Nativism...Immigration <- na.omit(new.ces2019_web$Nativism...Immigration)
# Do.you.work.in.the.public..private..or.not.for.profit.sector.
Cleaned.Do.you.work.in.the.public..private..or.not.for.profit.sector. <- na.omit(new.ces2019_web$Do.you.work.in.the.public..private..or.not.for.profit.sector.)
# Trust.in.Others
Cleaned.Trust.in.Others <- na.omit(new.ces2019_web$Trust.in.Others)
# Newer.lifestyles
Cleaned.Newer.lifestyles <- na.omit(new.ces2019_web$Newer.lifestyles)
# Is.income.inequality.a.problem.in.Canada.
Cleaned.Is.income.inequality.a.problem.in.Canada <- na.omit(new.ces2019_web$Is.income.inequality.a.problem.in.Canada.)
# Left.Right.Belief.Scale..Self - numeric
Cleaned.Left.Right.Belief.Scale..Self <- na.omit(new.ces2019_web$Left.Right.Belief.Scale..Self)

# Create your new data.frame with these cleaned variables
Cleaned.new.ces2019_Web <- data.frame(Cleaned.Vote.Choice, Cleaned.Education, Cleaned.Is.income.inequality.a.problem.in.Canada,
                                      Cleaned.Nativism...Immigration, Cleaned.Newer.lifestyles, Cleaned.Place.of.Residence,
                                      Cleaned.Religion, Cleaned.Left.Right.Belief.Scale..Self, Cleaned.Trust.in.Others,
                                      Cleaned.Importance.of.Voting, Cleaned.Times.Volunteering.for.a.Group.or.Organization,
                                      Cleaned.Politicians.in.General.1, Cleaned.Feminists, Cleaned.Muslims.living.in.Canada,
                                      Cleaned.Gays.and.Lesbians, Cleaned.Aboriginal.Peoples, Cleaned.Racial.Minorities,
                                      Cleaned.Women.s.Place.in.the.Home, Cleaned.Family.Values, Cleaned.Religion.s.Importance,
                                      Cleaned.Does.Not.Own.Property.Stocks.Bonds.Savings, Cleaned.Owns.Savings, Cleaned.Owns.stock.or.bonds,
                                      Cleaned.Owns.a.business..a.piece.of.property..a.farm.or.livestock, Cleaned.Owns.a.Residence, Cleaned.Age,
                                      Cleaned.Employment)
##### Problem #####
# The problem at this point is figuring out what to do with the NA vaues you wish to test.
# There are several thousand, sometimes tens of thousands, of NA values being observed in the variables you wish to use in your dataset
# You have tried to remove them all from a new dataset, and that removed an unfortunate amount of rows from your total dataset.
# You then decided to remove NA values from variables on an individual basis, in order to preserve the data as much as possible, 
# but you have received the following error from trying to create the current data.frame;

# Error in data.frame(Cleaned.Vote.Choice, Cleaned.Education, Cleaned.Is.income.inequality.a.problem.in.Canada,  : 
#                     arguments imply differing number of rows: 31564, 37822, 10340, 4369, 9790, 9319, 31639, 9115, 9082, 9212, 31088, 27988, 0, 10617, 3370, 24799


##### What are your options in this case? Should you just be trying to remove NA values, or inputing them? #####


# Step 5: Recoding variables of interest for logit regression
# 5.1 We want to make sure our dependent variable, Vote Choice, is binomial. That is, we wish to recode
# the variable to "Voted Conservative", and "Did Not Vote Conservative". 
# We need to collapse the levels in this factoral variable using fct_collapse(). In this way, you are trying to break down
# the levels of the Independent variables you wish to use in your model into individual levels, 

# Vote Choice - Dependent Variable 
Cleaned.new.ces2019_Web$Vote.Choice <- fct_collapse(Cleaned.Vote.Choice,
                                      Conservative = c("Conservative Party", "People's Party"),
                                      Did.not.Vote.Conservative  = c("Liberal Party", "ndp", "Bloc Québécois",
                                                          "Green Party", "Another party (please specify)",
                                                          "Don't know/ Prefer not to answer"))

##### Update Nov 10, 2020 #####
# R considers each level, or category an independent binary variable by the glm(), so all this work is probably unnecessary.
#Education
Cleaned.new.ces2019_Web$hs_dropout <- fct_collapse(Cleaned.Education,
                                    hs_dropout = c("No Schooling", "Some elementary school", "Completed elementary school", "Some secondary/ high school"))
Cleaned.new.ces2019_Web$Highschool <- fct_collapse(Cleaned.Education,                                   
                                     High_school = c("Completed secondary/ high school"))
Cleaned.new.ces2019_Web$Technical_community_college <- fct_collapse(Cleaned.Education,                                    
                                    Technical_community_college = c("Some technical, community college, CEGEP, College Classique", "Completed technical, community college, CEGEP, College Classique"))
Cleaned.new.ces2019_Web$University <- fct_collapse(Cleaned.Education,                                   
                                     University = c("Some university", "Bachelor's degree", "Master's degree"))
Cleaned.new.ces2019_Web$Professional_degree <- fct_collapse(Cleaned.Education,                                   
                                     Professional_degree = c("Professional degree or doctorate"))

# Inequality
Cleaned.new.ces2019_Web$Cleaned.Is.income.inequality.a.problem.in.Canada.1 < - fct_collapse(Cleaned.Is.income.inequality.a.problem.in.Canada, 
                                                                    Yes = c("Definitely yes", "Probably yes"),
                                                                    Not_sure = c("Not sure"),
                                                                    No = c("Probably not", "Definitely not"),
                                                                    Dont_know_noanswer = c("Don't know/ Prefer not to answer"))

#Nativism
Cleaned.new.ces2019_Web$Cleaned.Nativism...Immigration.1 <- fct_collapse(Cleaned.Nativism...Immigration, 
                                                Disagree = c("Strongly disagree", "Somewhat disagree "),
                                                Not_sure = c("Neither agree nor disagree"),
                                                Agree = c("Somewhat agree", "Strongly agree"),
                                                Dontknow_noanswer = c("Don't know/ Prefer not to answer"))

#Newer lifestyles
Cleaned.new.ces2019_Web$Cleaned.Newer.lifestyles.1 <- fct_collapse(Cleaned.Newer.lifestyles, 
                                           Disagree = c("Strongly disagree", "Somewhat disagree"),
                                           Not_sure = c("Neither agree nor disagree", "Don't know/ Prefer not to answer"),
                                           Agree = c("Somewhat agree", "Strongly agree"))

# Place of Residence. 
# You will need to condense the levels into three, rural, suburban, and rural.
# You will then need to create your own variables for these individual levels. 
Cleaned.new.ces2019_Web$Cleaned.Place.of.Residence.1 <- fct_collapse(Cleaned.Place.of.Residence,
                                             Rural = c("A rural area or village (less than1000 people)",
                                                       "A small town (more than 1000 people but less than 15K)"),
                                                       Suburban = c("A middle-sized town (15K-50K people) not attached to a city",
                                                      "A suburb of a large town or city"),
                                             Urban = c("A large town or city (more than 50K people)"),
                                             Dont_know.Prefer_not_to_answer = c("Don't know / Prefer not to answer"))
#Religion
Cleaned.new.ces2019_Web$Cleaned.Religion.1 <- fct_collapse(Cleaned.Religion, 
                                   None.atheist = c("None/ Don't have one/ Atheist"),
                                   Agnostic = c("Agnostic"),
                                   Buddhist.Buddhism = c("Buddhist/ Buddhism"),
                                   Hindu = c("Hindu"),
                                   Jewish = c("Jewish/ Judaism/ Jewish Orthodox"),
                                   Muslim = c("Muslim/ Islam"),
                                   Sikh = c("Muslim/ Islam"),
                                   Protestant.Christian = c("Anglican/ Church of England", "Baptist", "Greek Orthodox/ Ukrainian Orthodox/ Russian Orthodox/ Easter",
                                                            "Jehovah's Witness", "Lutheran", "Mormon/ Church of Jesus Christ of the Latter Day Saints", "Pentecostal/ Fundamentalist/ Born Again/ Evangelical",
                                                            "Presbyterian", "Protestant", "United Church of Canada", "Christian Reformed", "Salvation Army", "Mennonite"),
                                   Catholic = c("Catholic/ Roman Catholic/ RC"))
  
# Step 6: Research Questions and Performing Logistic Regression
# 6.1 Question 1: Are rural and/or suburban residents more likely to support conservative parties compared to their urban counterparts? 
# You would like to be able to break down the results by province. You recall being able to do this
# 6.2 Question 2: Are rural and/or suburban residents more likely to support conservative parties than their urban counterparts even after taking into account socio-demographic characteristics? 
# 6.3 Question 3: Are rural and/or suburban residents more likely to support conservative parties relative to their urban counterparts even after taking into account individual level values and beliefs? 
####### 
ces2019_web.logit.1 <- glm(Cleaned.Vote.Choice.1 ~ Cleaned.Aboriginal.Peoples + Cleaned.Age + Cleaned.Do.you.work.in.the.public..private..or.not.for.profit.sector. +
Cleaned.Education.1 + Cleaned.Employment.1 + Cleaned.Family.Values.1calling  + Cleaned.Feminists + Cleaned.Gays.and.Lesbians +
Cleaned.Immigrants + Cleaned.Muslims.living.in.Canada + Cleaned.Nativism...Immigration + Cleaned.Newer.lifestyles +
Cleaned.Religion + Cleaned.Times.Volunteering.for.a.Group.or.Organization + Cleaned.Importance.of.Voting +
Cleaned.Is.income.inequality.a.problem.in.Canada. + Cleaned.Left.Right.Belief.Scale..Self + Cleaned.Nativism...Immigration +
Cleaned.Owns.a.business..a.piece.of.property..a.farm.or.livestock + Cleaned.Owns.a.Residence + Cleaned.Owns.stock.or.bonds +
Cleaned.Place.of.Residence + Cleaned.Politicians.in.General + Cleaned.Politicians.in.General.1 + Cleaned.Racial.Minorities +
Cleaned.Religion.s.Importance + Cleaned.Trust.in.Others + Cleaned.Vote.Choice + Cleaned.Women.s.Place.in.the.Home, data = new.ces2019_web, family = "binomial")

# Step 7: Analyzing the results of the model
# Step 8: Performing decision tree *check notes from supervisor's meeting for clarification
# 8.1 the BART model? 
# Step 9: Analyzing the results of the model
# Step 10: Compare the results of the logit model to the decision tree model
# Step 11: Determine and create interesting and marketable visualizations of your results
# Step 12: Upload project to Github page, distribute to your supervisor