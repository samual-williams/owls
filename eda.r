###################################################################
#### Exploratory data analysis for owls eductaion paper ###########
###################################################################


rm(list=ls())  

library(ggplot2)
library(dplyr)
library(MASS)
library(bbmle)
library(DataExplorer)

data <- read.csv("owlData.csv")

create_report(data)

dim(data)
summary(data$hear.talk)
ggplot(data, aes(fill=hear.talk, x = stage)) + geom_bar()

data <- filter(data, hear.talk != "") # removes missing values in whether resondents heard talk

ggplot(data, aes(fill=hear.talk, x = stage)) + geom_bar()

###################################################################
############# Exploratory data analysis ###########################
###################################################################

###################################################################
# Compare models fitted using using various independant variables 
###################################################################

###################################################################
# Q1: Would you like to have an owl nesting near your home? (yes/no)
###################################################################

# fliter out the blanks in resonse variable 
library(dplyr)
questionData <- filter(data, home != "") # removes missing values in response variable
ggplot(questionData, aes(fill=home, x = stage)) + geom_bar() # missing responses gone

# Now remove respondents that did not hear the talk
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(questionTreatmentData, aes(fill=hear.talk, x = stage)) + geom_bar() # All post resondents heard talk

# Check plot of resonse variable
p1 <- ggplot(questionTreatmentData, aes(fill=home, x = stage)) + geom_bar() # Could be a relationship
p1

m1 <- glm(home ~ stage, family = binomial (link = "cloglog"), data = questionTreatmentData)
m2 <- glm(home ~ village, family = binomial (link = "cloglog"), data = questionTreatmentData)
m3 <- glm(home ~ grade, family = binomial (link = "cloglog"), data = questionTreatmentData) 
m4 <- glm(home ~ gender, family = binomial (link = "cloglog"), data = questionTreatmentData) 
m5 <- glm(home ~ school, family = binomial (link = "cloglog"), data = questionTreatmentData) 
mNull <- glm(home ~ 1, family = binomial (link = "cloglog"), data = questionTreatmentData)

# Compare models
AICtab(mNull, m1, m2, m3, m4, m5)
# Stage, school, and gender fit better than null

###################################################################
# Q2: Would you like to have an owl nesting in the roof of your home? (yes/no)
###################################################################

# fliter out the blanks in resonse variable 
questionData <- filter(data, like.nesting.in.roof != "") # removes missing values in response variable
ggplot(questionData, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # missing responses gone

# Now remove respondents that did not hear the talk
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(questionTreatmentData, aes(fill=hear.talk, x = stage)) + geom_bar() # All post resondents heard talk

# Check plot of resonse variable
ggplot(questionTreatmentData, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # Could be a relationship

m1 <- glm(like.nesting.in.roof ~ stage, family = binomial (link = "cloglog"), data = questionTreatmentData)
m2 <- glm(like.nesting.in.roof ~ village, family = binomial (link = "cloglog"), data = questionTreatmentData)
m3 <- glm(like.nesting.in.roof ~ grade, family = binomial (link = "cloglog"), data = questionTreatmentData) 
m4 <- glm(like.nesting.in.roof ~ gender, family = binomial (link = "cloglog"), data = questionTreatmentData) 
m5 <- glm(like.nesting.in.roof ~ school, family = binomial (link = "cloglog"), data = questionTreatmentData) 
mNull <- glm(like.nesting.in.roof ~ 1, family = binomial (link = "cloglog"), data = questionTreatmentData)

# Compare models
AICtab(mNull, m1, m2, m3, m4, m5)

###################################################################
# Q3: Would you put up an artificial nest for owls to nest in, in your yard (compound) near your home? (yes/no)
###################################################################
# fliter out the blanks & errors in resonse variable
boxData <- filter(data, would.put.nest.box.in.yard != "")
boxData <- filter(boxData, would.put.nest.box.in.yard != "the owls. No owls")
boxData <- filter(boxData, would.put.nest.box.in.yard != "witchcraft")
ggplot(boxData, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() # missing values gone. 

questionData <- boxData

# Now remove respondents that did not hear the talk
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(questionTreatmentData, aes(fill=hear.talk, x = stage)) + geom_bar() # All post resondents heard talk

# Check plot of resonse variable
ggplot(questionTreatmentData, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() # Could be a relationship

m1 <- glm(would.put.nest.box.in.yard ~ stage, family = binomial (link = "cloglog"), data = questionTreatmentData)
m2 <- glm(would.put.nest.box.in.yard ~ village, family = binomial (link = "cloglog"), data = questionTreatmentData)
m3 <- glm(would.put.nest.box.in.yard ~ grade, family = binomial (link = "cloglog"), data = questionTreatmentData) 
m4 <- glm(would.put.nest.box.in.yard ~ gender, family = binomial (link = "cloglog"), data = questionTreatmentData) 
m5 <- glm(would.put.nest.box.in.yard ~ school, family = binomial (link = "cloglog"), data = questionTreatmentData) 
mNull <- glm(would.put.nest.box.in.yard ~ 1, family = binomial (link = "cloglog"), data = questionTreatmentData)

# Compare models
AICtab(mNull, m1, m2, m3, m4, m5)
# Gender could also be an important variable here, with males more likely than females 

ggplot(questionTreatmentData, aes(fill=would.put.nest.box.in.yard, x = gender)) + geom_bar()

m6 <- glm(would.put.nest.box.in.yard ~ stage + gender, family = binomial (link = "cloglog"), data = questionTreatmentData) 
AICtab(mNull, m1, m2, m3, m4, m5, m6)
# Adding gender improves model fit

###################################################################
# Q4: Which of the three choices best describes your feeling towards owls?
# Like/No feeling/Not like
###################################################################
ggplot(data, aes(fill=attitude.to.owls, x = stage)) + geom_bar() # plot shows missing values
# Note - need to change csv file so that attitude.to.owls responses are labelled with correct order (change text from "not like" to "1 - not like". Otherwise model predicts influence of talk on not like, rather than like. 

# fliter out the blanks in resonse variable & delete rows that did not hear talk
questionData <- filter(data, attitude.to.owls != "") # removes missing values in response variable
ggplot(questionData, aes(fill=attitude.to.owls, x = stage)) + geom_bar() # missing values gone

# First test those that did hear the talk
ggplot(questionData, aes(fill=hear.talk, x = stage)) + geom_bar() 
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(questionTreatmentData, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(questionTreatmentData, aes(fill=attitude.to.owls, x = stage)) + geom_bar() # Looks like a relationship

# Ordinal ordered logistic regression using MASS - see https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
m1 <- polr(attitude.to.owls ~ stage, data = questionTreatmentData, Hess=TRUE)
summary(mTreatment)
m2 <- polr(attitude.to.owls ~ village, data = questionTreatmentData, Hess=TRUE)
m3 <- polr(attitude.to.owls ~ grade, data = questionTreatmentData, Hess=TRUE) 
m4 <- polr(attitude.to.owls ~ gender, data = questionTreatmentData, Hess=TRUE) 
m5 <- polr(attitude.to.owls ~ school, data = questionTreatmentData, Hess=TRUE) 
mNull <- polr(attitude.to.owls ~ 1, data = questionTreatmentData, Hess=TRUE)

# Compare models
AICtab(mNull, m1, m2, m3, m4, m5)
# Stage seems like best model 

###################################################################
# Q5: Which of the three choices best describes your response to seeing an owl?
# Not afraid/Very afraid/Terrified
###################################################################

ggplot(data, aes(fill=afraid.of.owls, x = stage)) + geom_bar() # plot shows missing values

# fliter out the blanks in resonse variable & delete rows that did not hear talk
questionData <- filter(data, afraid.of.owls != "") # removes missing values in response variable
ggplot(questionData, aes(fill=afraid.of.owls, x = stage)) + geom_bar() # missing values gone

# First test those that did hear the talk
ggplot(questionData, aes(fill=hear.talk, x = stage)) + geom_bar() 
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(questionTreatmentData, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(questionTreatmentData, aes(fill=afraid.of.owls, x = stage)) + geom_bar() # Looks like a relationship

# Fit ordered logistic regression models
m1 <- polr(afraid.of.owls ~ stage, data = questionTreatmentData, Hess=TRUE)
m2 <- polr(afraid.of.owls ~ village, data = questionTreatmentData, Hess=TRUE)
m3 <- polr(afraid.of.owls ~ grade, data = questionTreatmentData, Hess=TRUE) 
m4 <- polr(afraid.of.owls ~ gender, data = questionTreatmentData, Hess=TRUE) 
m5 <- polr(afraid.of.owls ~ school, data = questionTreatmentData, Hess=TRUE) 
mNull <- polr(afraid.of.owls ~ 1, data = questionTreatmentData, Hess=TRUE)

AICtab(mNull, m1, m2, m3, m4, m5)
# Gender, stage, school and grade fit better than null
ggplot(questionTreatmentData, aes(fill=afraid.of.owls, x = gender)) + geom_bar()
# males less likely to be scared
# need to fix errors in gender


###################################################################
# Q6: What do you do if an owl lands on the roof of your home?
# Run away/Nothing/Try and kill the owl/Chase the owl away.
###################################################################

ggplot(data, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar() # plot shows missing values

# fliter out the blanks in resonse variable & delete rows that did not hear talk
questionData <- filter(data, what.do.if.owl.lands.on.the.roof != "") # removes missing values in response variable
ggplot(questionData, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar() # missing values gone

# First test those that did hear the talk
ggplot(questionData, aes(fill=hear.talk, x = stage)) + geom_bar() 
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(questionTreatmentData, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar() # Looks like a relationship

# Fit ordered logistic regression models
m1 <- polr(what.do.if.owl.lands.on.the.roof ~ stage, data = questionTreatmentData, Hess=TRUE)
m2 <- polr(what.do.if.owl.lands.on.the.roof ~ village, data = questionTreatmentData, Hess=TRUE)
m3 <- polr(what.do.if.owl.lands.on.the.roof ~ grade, data = questionTreatmentData, Hess=TRUE) 
m4 <- polr(what.do.if.owl.lands.on.the.roof ~ gender, data = questionTreatmentData, Hess=TRUE)
m5 <- polr(what.do.if.owl.lands.on.the.roof ~ school, data = questionTreatmentData, Hess=TRUE)
mNull <- polr(what.do.if.owl.lands.on.the.roof ~ 1, data = questionTreatmentData, Hess=TRUE)

AICtab(mNull, m1, m2, m3, m4, m5)
# school, grade, stage and gender all fit better than null

ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = school)) + geom_bar() 
ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = grade)) + geom_bar() 
ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar()
ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = gender)) + geom_bar()


###################################################################
# Q7:What did you do the last time you saw an owl?
# Run away/Nothing/Try and kill the owl
###################################################################

ggplot(data, aes(fill=what.did.when.last.saw.owl, x = stage)) + geom_bar() # plot shows missing values

# fliter out the blanks in resonse variable & delete rows that did not hear talk
questionData <- filter(data, what.did.when.last.saw.owl != "") # removes missing values in response variable
ggplot(questionData, aes(fill=what.did.when.last.saw.owl, x = stage)) + geom_bar() # missing values gone

# First test those that did hear the talk
ggplot(questionData, aes(fill=hear.talk, x = stage)) + geom_bar() 
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(questionTreatmentData, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(questionTreatmentData, aes(fill=what.did.when.last.saw.owl, x = stage)) + geom_bar() # Looks like a relationship

# Fit ordered logistic regression models
m1 <- polr(what.did.when.last.saw.owl ~ stage, data = questionTreatmentData, Hess=TRUE)
m2 <- polr(what.did.when.last.saw.owl ~ village, data = questionTreatmentData, Hess=TRUE)
m3 <- polr(what.did.when.last.saw.owl ~ grade, data = questionTreatmentData, Hess=TRUE)
m4 <- polr(what.did.when.last.saw.owl ~ gender, data = questionTreatmentData, Hess=TRUE)
m5 <- polr(what.did.when.last.saw.owl ~ school, data = questionTreatmentData, Hess=TRUE)
mNull <- polr(what.did.when.last.saw.owl ~ 1, data = questionTreatmentData, Hess=TRUE)

AICtab(mNull, m1, m2, m3, m4, m5)
# school, gender, and grade fit better than null. Stage not better than null.

ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = school)) + geom_bar() 
ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = grade)) + geom_bar() 
ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = gender)) + geom_bar()

