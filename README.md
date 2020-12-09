# CIND810 Capstone Project - Place of Residence and Vote Choice in the 2019 Canadian Federal Election - December 4, 2020
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
  "Times Volunteering for a Group or Organization" = ces2019_web$cps19_volunteer,
  "Owns a Residence" = ces2019_web$cps19_property_1,
  "Owns a business, a piece of property, a farm or livestock" = ces2019_web$cps19_property_2,
  "Owns stock or bonds" = ces2019_web$cps19_property_3,
  "Age" = ces2019_web$cps19_age,
  "Importance of Voting" = ces2019_web$pes19_diff_happens_1,
  "Do you work in the public, private, or not-for-profit sector?" = ces2019_web$cps19_sector,
  "Trust in Others" = ces2019_web$pes19_trust,
  "Newer lifestyles" = ces2019_web$pes19_newerlife,
  "Does not own property, stocks, or bonds" = ces2019_web$cps19_property_5,
  "Do you have any savings" = ces2019_web$cps19_property_4
)


# Step 4:Cleaning the variables of interest in your new data.frame.
# We replace the NAs in the new data.frame. We replace NAs with numeric variables with the median.We replace 
# categorical variables with the mode. 

# This is how you can replace NAs for numeric variables.
sum(is.na(new.ces2019_web$Feminists))
new.ces2019_web$Feminists[which(is.na(new.ces2019_web$Feminists))] <- median(new.ces2019_web$Feminists, na.rm = TRUE)
# Success!

#This is how you rpelace NAs for categorical variables
Rel.Imp.mx = table(new.ces2019_web$Religion.s.Importance) 
Rel.Imp.mx 
names(Rel.Imp.mx) ### 
Rel.Imp.mx[Rel.Imp.mx==max(Rel.Imp.mx)] ### Display the max values i.e., the mode
# Check for NAs
summary(is.na(new.ces2019_web$Religion.s.Importance))
#replace NAs with Mode
new.ces2019_web$Religion.s.Importance[is.na(new.ces2019_web$Religion.s.Importance)] <- "Somewhat important"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Religion.s.Importance))
fct_count(new.ces2019_web$Religion.s.Importance)

# The following variables need to be cleaned.
#Vote Choice - Categorical
pes.vote.choice.mx = table(ces2019_web$cps19_votechoice) 
names(pes.vote.choice.mx)
pes.vote.choice.mx[pes.vote.choice.mx==max(pes.vote.choice.mx)] ### Display the max values which is the mode
ces2019_web$cps19_votechoice[is.na(ces2019_web$cps19_votechoice)] <- "Liberal Party"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(ces2019_web$cps19_votechoice))

# Place of Residence - Categorical
Residence.mx = table(new.ces2019_web$Place.of.Residence) 
Residence.mx
names(Residence.mx) ### Display the names
Residence.mx[Residence.mx==max(Residence.mx)] ### Display the max values which is the mode
summary(is.na(new.ces2019_web$Place.of.Residence))
new.ces2019_web$Place.of.Residence[is.na(new.ces2019_web$Place.of.Residence)] <- "A large town or city (more than 50K people)"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Place.of.Residence))

# Question 2
# Education - Categorical
Education.mx = table(new.ces2019_web$Education) 
Education.mx 
names(Education.mx) ### Display the names
Education.mx[Education.mx==max(Education.mx)] ### Display the max values which is the mode
summary(is.na(new.ces2019_web$Education))
new.ces2019_web$Education[is.na(new.ces2019_web$Education)] <- "Bachelor's degree"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Education))

#Employment - Categorical
# No NAs

# Religion - Categorical
# No NAs

# Age - Numeric 
# No NAs

# The Ownership questions were recoded using fct_explicit_na(), as it replaces NA with a new Factor level.

# Owns.a.Residence - Categorical - convert to factor
new.ces2019_web$Owns.a.Residence <- factor(new.ces2019_web$Owns.a.Residence)
summary(is.na(new.ces2019_web$Owns.a.Residence))
new.ces2019_web$Owns.a.Residence <- fct_explicit_na(new.ces2019_web$Owns.a.Residence, na_level = "Does not own a residence")
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Owns.a.Residence))
fct_count(new.ces2019_web$Owns.a.Residence)
table(new.ces2019_web$Owns.a.Residence, new.ces2019_web$Vote.Choice)
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
# Family.Values - Categorical
Family.Values.mx = table(new.ces2019_web$Family.Values)
Family.Values.mx 
names(Family.Values.mx)  
Family.Values.mx[Family.Values.mx==max(Family.Values.mx)] ### Display the max values which is the mode
summary(is.na(new.ces2019_web$Family.Values))
new.ces2019_web$Family.Values[is.na(new.ces2019_web$Family.Values)] <- "Somewhat agree"
# confirm NAs have been replaces by checking again for NA values
summary(is.na(new.ces2019_web$Family.Values))

# Women.s.Place.in.the.Home - Categorical
Women.s.Place.in.the.Home.mx = table(new.ces2019_web$Women.s.Place.in.the.Home) 
Women.s.Place.in.the.Home.mx 
names(Women.s.Place.in.the.Home.mx) 
Women.s.Place.in.the.Home.mx[Women.s.Place.in.the.Home.mx==max(Women.s.Place.in.the.Home.mx)] ### Display the max values which is the mode
summary(is.na(new.ces2019_web$Women.s.Place.in.the.Home))
new.ces2019_web$Women.s.Place.in.the.Home[is.na(new.ces2019_web$Women.s.Place.in.the.Home)] <- "Strongly disagree"
# confirm NAs have been replaces by checking again for NA values
sum(is.na(new.ces2019_web$Women.s.Place.in.the.Home))

# Racial.Minorities - Numeric
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

# Times.Volunteering.for.a.Group.or.Organization - Categorical
# No NAs

# Importance.of.Voting - Numeric 
sum(is.na(new.ces2019_web$Importance.of.Voting))
new.ces2019_web$Importance.of.Voting[which(is.na(new.ces2019_web$Importance.of.Voting))] <- median(new.ces2019_web$Importance.of.Voting, na.rm = TRUE)
sum(is.na(new.ces2019_web$Importance.of.Voting))

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

# Left.Right.Belief.Scale..Self - numeric
sum(is.na(new.ces2019_web$Left.Right.Belief.Scale..Self))
new.ces2019_web$Left.Right.Belief.Scale..Self[which(is.na(new.ces2019_web$Left.Right.Belief.Scale..Self))] <- median(new.ces2019_web$Left.Right.Belief.Scale..Self, na.rm = TRUE)
sum(is.na(new.ces2019_web$Left.Right.Belief.Scale..Self))
# Let's now check the whole data.frame for NAs
sum(is.na(new.ces2019_web$Left.Right.Belief.Scale..Self))
summary(new.ces2019_web$Left.Right.Belief.Scale..Self)
# Now, let's confirm there are no more NAs in your data.frame
summary(is.na(new.ces2019_web))
names(new.ces2019_web)
# for some reason, "new.ces2019_web" was identfied as a variable. The following code removes that variable.
new.ces2019_web <- new.ces2019_web[,-27]

# Step 5: Recoding variables of interest for our models
# 5.1 We want to make sure our dependent variable, Vote Choice, is binomial. That is, we wish to recode
# the variable to "Voted Conservative", and "Did Not Vote Conservative".
# We also want to make the Age variable a factor and collapse its levels into three smaller levels, that resemble Roy et. al's approach (2015)
# We need to collapse the levels in this factoral variable using fct_collapse(). 

# Vote Choice - Dependent Variable
new.ces2019_web$Vote.Choice <- fct_collapse(new.ces2019_web$Vote.Choice,
                                            Conservative = c("Conservative Party", "People's Party"),
                                            Did.not.Vote.Conservative  = c("Liberal Party", "ndp", "Bloc Québécois",
                                                                           "Green Party", "Another party (please specify)",
                                                                           "Don't know/ Prefer not to answer"))
# Age - Independent Variable
new.ces2019_web$Age <-as.factor(new.ces2019_web$Age)
fct_count(new.ces2019_web$Age)
levels(new.ces2019_web$Age)
new.ces2019_web$Age <- fct_collapse(new.ces2019_web$Age,
                                    Under35 = c("18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28",
                                                "29", "30", "31", "32", "33", "34"),
                                    Between35_and_54 = c("35", "36", "37", "38", "39", "40", "41", "42", "43", "44",
                                                         "45", "46", "47", "48", "49", "50", "51", "52", "53", "54"),
                                    Over54 = c("55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65", "66",
                                               "67", "68", "69", "70", "71", "72", "73", "74", "75", "76", "77", "78",
                                               "79", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90",
                                               "91", "92", "93", "94", "95", "96", "97", "98", "99"))

# Step 5.2: Checking for class imbalance
levels(new.ces2019_web$Vote.Choice)
# What we see in this case is a disproportionate amount of respondents not voting for a conservative party. So there may be a class imbalance problem.
# To address this problem, we will be installing and using the SMOTE package. In both our models, the confusion matrices for our questions aren't good 
# predictors of "Conservative" vote choice, and one way to solve for this problem is to use SMOTE(), which can help us with unbalanced data. 
install.packages("DMwR")
library("DMwR")
new.ces2019_web <- SMOTE(Vote.Choice~., new.ces2019_web, perc.over = 100, perc.under = 200)
fct_count(new.ces2019_web$Vote.Choice)
# Our results show an even number of responses for each level, 18636 for each. 

# Now we begin designing our models, and determing how best they will be analyzed. We will be using the output of the models via the summary() to etermine coefficient estimate, z-values, and P-values.
# We will be using a confusion matrix to assess the accuracy, Recall and Precision of our model, and we will also be computing the R2 for each model.
install.packages("caret")
library(caret)
# 6.1 Question 1: Are rural and/or suburban residents more likely to support conservative parties compared to their urban counterparts? 
# 1.1 Logistic Regression - For this model we are going to create a training and a test set, using K-fold cross-validation
# to assess the accuracy of our model.
set.seed(123)
#Create Data Partition
Train.Q <- createDataPartition(new.ces2019_web$Vote.Choice, p=.75, list = F)
training.dataQ <- new.ces2019_web[ Train.Q, ]
testing.dataQ <- new.ces2019_web[-Train.Q,]

#K-FOLD CROSS VALIDATION
set.seed(123)
Q.1.train.control <- trainControl(method = "repeatedcv", number=10, repeats = 3)

Q.1 <- train(Vote.Choice~Place.of.Residence, data = new.ces2019_web, method="glm",family="binomial", trControl=Q.1.train.control)
# Now we want to make predictions using our trained model.
pred <- predict(Q.1, newdata=testing.dataQ)
# Using the prediction made with our model, we evaluate the accuracy of our prediction using a Confusion Matrix.
confusionMatrix(data=pred, testing.dataQ$Vote.Choice, positive = "Conservative")

# 6.2 Question 2: Are rural and/or suburban residents more likely to support conservative parties than their urban counterparts even after taking into account socio-demographic characteristics? 
set.seed(123)
Q.2 <- train(Vote.Choice ~ Education + Employment+ Place.of.Residence+ Owns.a.Residence+ Owns.stock.or.bonds+ 
               Owns.a.business..a.piece.of.property..a.farm.or.livestock+ Does.not.own.property..stocks..or.bonds+
               Do.you.work.in.the.public..private..or.not.for.profit.sector.+ Do.you.have.any.savings, 
             data = new.ces2019_web, method="glm",family="binomial", trControl=Q.1.train.control)

# Now we want to make predictions using our trained model.
pred2 <- predict(Q.2, newdata=testing.dataQ)
# Using the prediction made with our model, we evaluate the accuracy of our prediction using a Confusion Matrix.
confusionMatrix(data=pred2, testing.dataQ$Vote.Choice, positive = "Conservative")
# The results again show that the model can't correctly predict "Conservative" vote choice. 

# 6.3 Question 3: Are rural and/or suburban residents more likely to support conservative parties relative to their urban counterparts even after taking into account individual level values and beliefs? 
set.seed(123)
Q.3 <- train(Vote.Choice~., data = new.ces2019_web, method="glm",family="binomial", trControl=Q.1.train.control)
pred3 <- predict(Q.3, newdata=testing.dataQ)
# Using the prediction made with our model, we evaluate the accuracy of our prediction using a Confusion Matrix.
confusionMatrix(data=pred3, testing.dataQ$Vote.Choice, positive = "Conservative")

# Step 7: Performing decision tree 
library(rpart)
library(MLmetrics)
# Step 7.1: Question 1: Decision Tree
set.seed(123)
Control=trainControl(method= "repeatedcv",number=10,repeats=10,classProbs=TRUE,summaryFunction =multiClassSummary)
dt_model <- caret :: train(Vote.Choice ~ Place.of.Residence, data = testing.dataQ, method = "rpart", trControl = Control, tuneLength=10)
summary(dt_model)
# Using the prediction made with our model, we evaluate the accuracy of our prediction using a Confusion Matrix.
predict_dt <- predict(dt_model, testing.dataQ)
confusionMatrix(data=predict_dt, testing.dataQ$Vote.Choice, positive = "Conservative")
# The model results were the same, no significant difference in it's ability to predict "Conservative" Vote Choice. 
# Accuracy, Sensitivity and Specificity were also no different.

# Step 7.2 Question 2: Decision Tree
set.seed(123)
Control=trainControl(method= "repeatedcv",number=10,repeats=10,classProbs=TRUE,summaryFunction =multiClassSummary)
dt_model2 <- caret :: train(Vote.Choice ~ Education + Employment+ Place.of.Residence+ Owns.a.Residence+ Owns.stock.or.bonds+ 
                              Owns.a.business..a.piece.of.property..a.farm.or.livestock+ Does.not.own.property..stocks..or.bonds+
                              Do.you.work.in.the.public..private..or.not.for.profit.sector.+ Do.you.have.any.savings,
                            data = testing.dataQ, method = "rpart", trControl = Control, tuneLength=10)

# Using the prediction made with our model, we evaluate the accuracy of our prediction using a Confusion Matrix.
predict_dt2 <- predict(dt_model2, testing.dataQ)
confusionMatrix(data=predict_dt2, testing.dataQ$Vote.Choice, positive = "Conservative")
# The model results were the same, no significant difference in it's ability to predict "Conservative" Vote Choice. 
# Accuracy, Sensitivity and Specificity were also no different.

# Step 7.3: Question 3: Decision Tree
set.seed(123)
Control=trainControl(method= "repeatedcv",number=10,repeats=10,classProbs=TRUE,summaryFunction =multiClassSummary)
dt_model3 <- caret :: train(Vote.Choice ~ ., data = testing.dataQ, method = "rpart", trControl = Control, tuneLength=10)
summary(dt_model3)
# Using the prediction made with our model, we evaluate the accuracy of our prediction using a Confusion Matrix.
predict_dt3 <- predict(dt_model3, testing.dataQ)
confusionMatrix(data=predict_dt3, testing.dataQ$Vote.Choice, positive = "Conservative")

# Step 9: Review Results
# Step 9.1.1 Question 1
summary(Q.1)
print(Q.1)

# Step 9.1.2 Question 2
summary(Q.2)
print(Q.2)

# Step 9.1.3 Question 3 
summary(Q.3)
print(Q.3)

# Step 10: Compare the results of the logit model to the decision tree model
# Step 10.1.1: Question 1 
print(dt_model)

# Step 10.1.2: QUestion 2
print(dt_model2)

#Step 10.1.3: Question 3
print(dt_model3)

# Step 11: Determine and create interesting and marketable visualizations of your results
# the package coefplot will be able to plot to coefficients of our models  
install.packages("coefplot")
library(coefplot)
#We run this function to deal with a minor error preventing coefplot() from running
dev.off()
# Now we plot the results of our logistic regression model. Unfortunately this does not work with our CART models, for reasons unknown.
coefplot(Q.1)
coefplot(Q.2)
coefplot(Q.3)
