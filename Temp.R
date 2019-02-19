rm(list=ls()) 

library(ggplot2)
library(dplyr)

data <- read.csv("owlData.csv")

# library(DataExplorer)
# create_report(data)

dim(data)
summary(data$hear.talk)
ggplot(data, aes(fill=hear.talk, x = stage)) + geom_bar()


library(dplyr)
data <- filter(data, hear.talk != "") # removes missing values in whether resondents heard talk
library(ggplot2)
ggplot(data, aes(fill=hear.talk, x = stage)) + geom_bar()


# Most questions of interest have either binary responses, or responses that fit into more than two ordinal categories. 
# - For questions with binary responses fit Bernoulli GLMs with listening to the talk as independant variable
# - For quiestions with more than two ordinal categorical responses fit multinomial logistic regression models with listening to the talk as independant variable

###################################################################
############ Questions with BINARY resonses #######################
###################################################################

###################################################################
# Would you like to have an owl nesting near your home? (yes/no)
###################################################################
library(ggplot2)
ggplot(data, aes(fill=home, x = stage)) + geom_bar() # plot shows missing values

# fliter out the blanks in resonse variable 
library(dplyr)
questionData <- filter(data, home != "") # removes missing values in response variable
ggplot(questionData, aes(fill=home, x = stage)) + geom_bar() # missing responses gone

# Now remove respondents that did not hear the talk
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(questionTreatmentData, aes(fill=hear.talk, x = stage)) + geom_bar() # All post resondents heard talk

# Check plot of resonse variable
ggplot(questionTreatmentData, aes(fill=home, x = stage)) + geom_bar() # Could be a relationship

# Fit Bernoulli glm
mTreatment <- glm(home ~ stage, family = binomial (link = "cloglog"), data = questionTreatmentData) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mTreatment)
# Treatment model has significant p value

# Model validation
devresid <- resid(mTreatment, type = "deviance")
hist(devresid) # No deviance residuals >2, so no evidence of pooor model fit

# Fit null model
mTreatmentNull <- glm(home ~ 1, family = binomial (link = "cloglog"), data = questionTreatmentData) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mHomeNull)

# Model validation
devresid <- resid(mHomeNull, type = "deviance") 
hist(devresid) # No deviance residuals >2, so no evidence of pooor model fit

# Compare treatment and null model
library(bbmle)
AICtab(mTreatmentNull, mTreatment)
# Treatment fits better than null

# Now fit same models for control group
questionControl <- filter(questionData, hear.talk != "yes") # remove respondents that did  hear talk
ggplot(questionControl, aes(fill=hear.talk, x = stage)) + geom_bar() # all post respondents did not hear talk

# Check plot of resonse variable
ggplot(questionControl, aes(fill=home, x = stage)) + geom_bar() 

# Fit model with controls 
mQuestionControl <- glm(home ~ stage, family = binomial (link = "cloglog"), data = questionControl)
summary(mQuestionControl)
devresid <- resid(mQuestionControl, type = "deviance") # for model validation
hist(devresid) # No deviance residuals >2, so no evidence of pooor model fit

# Fit null model
mQuestionControlNull <- glm(home ~ 1, family = binomial (link = "cloglog"), data = questionControl) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mQuestionControlNull)
devresid <- resid(mQuestionControlNull, type = "deviance") # for model validation
hist(devresid) # No deviance residuals >2, so no evidence of pooor model fit

# Compare control vs null models
library(bbmle)
AICtab(mQuestionControlNull, mQuestionControl)
# Controls no better than null

# So there is no difference in response to this question among prior- and post- talk groups those that did NOT hear the talk (controls). Although sample size is small for the control group. 

###################################################################
# Would you like to have an owl nesting in the roof of your home? (yes/no)
###################################################################

ggplot(data, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar()

# fliter out the blanks in resonse variable & delete rows that did not hear talk
roofData <- filter(data, like.nesting.in.roof != "") # removes missing values in response variable
ggplot(roofData, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # missing values gone

# First test those that did hear the talk
ggplot(roofData, aes(fill=hear.talk, x = stage)) + geom_bar() 
roofTalk <- filter(roofData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(roofTalk, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(roofTalk, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # Looks like a relationship

# Fit binomial (Bernoulli) glm
mRoofTalk <- glm (like.nesting.in.roof ~ stage, family = binomial (link = "cloglog"), data = roofTalk)
summary(mRoofTalk) # There is a relationship
devresid <- resid(mRoofTalk, type = "deviance") # for model validation
hist(devresid) # Some deviance residuals >2, so may indicate poor model fit

# Now test controls for same question
roofNoTalk <- filter(roofData, hear.talk != "yes") # remove respondents that did hear talk
ggplot(roofNoTalk, aes(fill=hear.talk, x = stage)) + geom_bar() # all post respondents did not hear talk

# Check plot of resonse variable
ggplot(roofNoTalk, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # relationship seems unlikely

mRoofNoTalk <- glm(like.nesting.in.roof ~ stage, family = binomial (link = "cloglog"), data = roofNoTalk)
summary(mRoofNoTalk) # No relationship among controls
devresid <- resid(mRoofNoTalk, type = "deviance") # for model validation
hist(devresid) # Some deviance residuals >2, so may indicate poor model fit

###################################################################
# Testing whether listning to talk effects responses to "Would you put up an artificial nest for owls to nest in, in your yard (compound) near your home?" (y/n)
###################################################################
ggplot(data, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar()

# fliter out the blanks & errors in resonse variable
boxData <- filter(data, would.put.nest.box.in.yard != "")
boxData <- filter(boxData, would.put.nest.box.in.yard != "the owls. No owls")
boxData <- filter(boxData, would.put.nest.box.in.yard != "witchcraft")
ggplot(boxData, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() # missing values gone. 

# First test those that did hear the talk
ggplot(boxData, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone
boxTalk <- filter(boxData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(boxTalk, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(boxTalk, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() # Looks like a relationship

# Fit binomial (Bernoulli) glm
mBoxTalk <- glm (would.put.nest.box.in.yard ~ stage, family = binomial (link = "cloglog"), data = boxTalk)
summary(mBoxTalk) # There is a relationship
devresid <- resid(mBoxTalk, type = "deviance") # for model validation
hist(devresid) # No deviance residuals >2, so no evidence of poor model fit

# Now test controls for same question
boxNoTalk <- filter(boxData, hear.talk != "yes") # remove respondents that did hear talk
ggplot(boxNoTalk, aes(fill=hear.talk, x = stage)) + geom_bar() # all post respondents did not hear talk

# Check plot of resonse variable
ggplot(boxNoTalk, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() # relationship seems unlikely

mBoxNoTalk <- glm(would.put.nest.box.in.yard ~ stage, family = binomial (link = "cloglog"), data = boxNoTalk)
summary(mBoxNoTalk) # No relationship among controls
devresid <- resid(mBoxNoTalk, type = "deviance") # for model validation
hist(devresid) # Some deviance residuals >2, so may indicate evidence of poor model fits

###################################################################
# Testing categorial responses with more than 2 levels - multinomial 
###################################################################

###################################################################
# Which of the three choices best describes your feeling towards owls?
# Like/No feeling/Not like
###################################################################
names(data)
library(ggplot2)
ggplot(data, aes(fill=attitude.to.owls, x = stage)) + geom_bar() # plot shows missing values
# Note - need to change csv file so that attitude.to.owls responses are labelled with correct order (change text from "not like" to "1 - not like". Otherwise model predicts influence of talk on not like, rather than like. 

# fliter out the blanks in resonse variable & delete rows that did not hear talk
attitudeData <- filter(data, attitude.to.owls != "") # removes missing values in response variable
ggplot(attitudeData, aes(fill=attitude.to.owls, x = stage)) + geom_bar() # missing values gone

# First test those that did hear the talk
ggplot(attitudeData, aes(fill=hear.talk, x = stage)) + geom_bar() 
attitudeTalk <- filter(attitudeData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(attitudeTalk, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(attitudeTalk, aes(fill=attitude.to.owls, x = stage)) + geom_bar() # Lookks like a relationship

# Try ordered logistic regression using MASS https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
library(MASS)
mAttitude<- polr(attitude.to.owls ~ stage, data = attitudeTalk, Hess=TRUE)
summary(mAttitude)
m <- mAttitude
dat <- attitudeTalk

# Calculate p values
## store table
(ctable <- coef(summary(m)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0
confint.default(m) # CIs assuming normality

# The coefficients from the model can be somewhat difficult to interpret because they are scaled in terms of logs. Another way to interpret logistic regression models is to convert the coefficients into odds ratios. To get the OR and confidence intervals, we just exponentiate the estimates and confidence intervals.
exp(coef(m)) # stage2 post 1.881341. 
# So we would say that for a one unit increase in stage, i.e., going from 1- prior to 2 - post, the odds of “like” is 1.88 times greater than “no feeling” or “not like” *combined* [given that all of the other variables in the model are held constant, if had multile variables in model]. Likewise, the odds “like” or “no feeling” versus “not like” is 2.85 times greater, given that all of the other variables in the model are held constant. 

# exp(cbind(OR = coef(m), ci))

# Model validation
# One of the assumptions underlying ordinal logistic (and ordinal probit) regression is that the relationship between each pair of outcome groups is the same. In other words, ordinal logistic regression assumes that the coefficients that describe the relationship between, say, the lowest versus all higher categories of the response variable are the same as those that describe the relationship between the next lowest category and all higher categories, etc. This is called the proportional odds assumption or the parallel regression assumption. Because the relationship between all pairs of groups is the same, there is only one set of coefficients. 

# create the function that estimates the values that will be graphed to validate model
library(Hmisc)
sf <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

(s <- with(dat, summary(as.numeric(attitude.to.owls) ~ stage, fun=sf))) ###### Can't validate model due to INF values in table #######

glm(I(as.numeric(attitude.to.owls) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(attitude.to.owls) >= 3) ~ stage, family="binomial", data = dat)

# Fit null model
mAttitudeNull <- polr(attitude.to.owls ~ 1, data = attitudeTalk, Hess=TRUE)
summary(mAttitudeNull)

# compare full and null model
library(bbmle)
AICtab(mAttitudeNull, mAttitude)
# full model has best fit

# Test responses to same question using controls

# First test those that did hear the talk
ggplot(attitudeData, aes(fill=hear.talk, x = stage)) + geom_bar() 
attitudeNoTalk <- filter(attitudeData, hear.talk != "yes") # remove respondents that did not hear talk
ggplot(attitudeNoTalk, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(attitudeNoTalk, aes(fill=attitude.to.owls, x = stage)) + geom_bar() # Lookks like a relationship

# Try ordered logistic regression using MASS https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
library(MASS)
mAttitudeNoTalk <- polr(attitude.to.owls ~ stage, data = attitudeNoTalk, Hess=TRUE)
summary(mAttitudeNoTalk)
m <- mAttitudeNoTalk
dat <- attitudeNoTalk

# Calculate p values
## store table
(ctable <- coef(summary(m)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0
confint.default(m) # CIs assuming normality

(s <- with(dat, summary(as.numeric(attitude.to.owls) ~ stage, fun=sf))) ###### Can't validate model due to INF values in table #######

glm(I(as.numeric(attitude.to.owls) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(attitude.to.owls) >= 3) ~ stage, family="binomial", data = dat)


###################################################################
# Which of the three choices best describes your response to seeing an owl?
# Not afraid/Very afraid/Terrified
###################################################################

names(data)
# "afraid.of.owls"
ggplot(data, aes(fill=afraid.of.owls, x = stage)) + geom_bar() # plot shows missing values

# fliter out the blanks in resonse variable & delete rows that did not hear talk
dataTemp <- filter(data, afraid.of.owls != "") # removes missing values in response variable
ggplot(dataTemp, aes(fill=afraid.of.owls, x = stage)) + geom_bar() # missing values gone

# First test those that did hear the talk
ggplot(dataTemp, aes(fill=hear.talk, x = stage)) + geom_bar() 
dataTemp <- filter(dataTemp, hear.talk != "no") # remove respondents that did not hear talk
ggplot(dataTemp, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(dataTemp, aes(fill=afraid.of.owls, x = stage)) + geom_bar() #  Relationship looks unlikely

# Try ordered logistic regression using MASS https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
mAfraid<- polr(attitude.to.owls ~ stage, data = attitudeTalk, Hess=TRUE)
summary(mAfraid)
m <- mAfraid
dat <- dataTemp

# Calculate p values
## store table
(ctable <- coef(summary(m)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - doesn't cross 0
confint.default(m) # CIs assuming normality

# Validate model
(s <- with(dat, summary(as.numeric(afraid.of.owls) ~ stage, fun=sf))) # Can't validate due to Inf values
glm(I(as.numeric(afraid.of.owls) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(afraid.of.owls) >= 3) ~ stage, family="binomial", data = dat)

# Fit null model
mAfraidNull <- polr(afraid.of.owls ~ 1, data = dataTemp, Hess=TRUE)
summary(mAfraidNull)

# compare full and null model
library(bbmle)
AICtab(mAfraidNull, mAfraid)
# Null model has best fit

###################################################################
# What do you do if an owl lands on the roof of your home?
# Run away/Nothing/Try and kill the owl/Chase the owl away.
###################################################################

names(data)
# what.do.if.owl.lands.on.the.roof
ggplot(data, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar() # plot shows missing values

# fliter out the blanks in resonse variable & delete rows that did not hear talk
dataTemp <- filter(data, what.do.if.owl.lands.on.the.roof != "") # removes missing values in response variable
ggplot(dataTemp, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar() # missing values gone

# First test those that did hear the talk
ggplot(dataTemp, aes(fill=hear.talk, x = stage)) + geom_bar() 
dataTemp <- filter(dataTemp, hear.talk != "no") # remove respondents that did not hear talk
ggplot(dataTemp, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(dataTemp, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar() #  Relationship looks posible?

# Try ordered logistic regression using MASS https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
mRoofAction <- polr(what.do.if.owl.lands.on.the.roof ~ stage, data = attitudeTalk, Hess=TRUE)
summary(mRoofAction)
m <- mRoofAction
dat <- dataTemp

# Calculate p values
## store table
(ctable <- coef(summary(m)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - crosses 0
confint.default(m) # CIs assuming normality

# Validate model
(s <- with(dat, summary(as.numeric(what.do.if.owl.lands.on.the.roof) ~ stage, fun=sf))) # Can't validate due to Inf values
glm(I(as.numeric(what.do.if.owl.lands.on.the.roof) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(what.do.if.owl.lands.on.the.roof) >= 3) ~ stage, family="binomial", data = dat)

# Fit null model
mRoofActionNull <- polr(what.do.if.owl.lands.on.the.roof ~ 1, data = dataTemp, Hess=TRUE)
summary(mRoofActionNull)

# compare full and null model
library(bbmle)
AICtab(mRoofActionNull, mRoofAction)
# Null model has best fit


###################################################################
# What did you do the last time you saw an owl?
# Run away/Nothing/Try and kill the owl
###################################################################

names(data)
# what.did.when.last.saw.owl
ggplot(data, aes(fill=what.did.when.last.saw.owl, x = stage)) + geom_bar() # plot shows missing values

# fliter out the blanks in resonse variable & delete rows that did not hear talk
dataTemp <- filter(data, what.did.when.last.saw.owl != "") # removes missing values in response variable
ggplot(dataTemp, aes(fill=what.did.when.last.saw.owl, x = stage)) + geom_bar() # missing values gone

# First test those that did hear the talk
ggplot(dataTemp, aes(fill=hear.talk, x = stage)) + geom_bar() 
dataTemp <- filter(dataTemp, hear.talk != "no") # remove respondents that did not hear talk
ggplot(dataTemp, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(dataTemp, aes(fill=what.did.when.last.saw.owl, x = stage)) + geom_bar() #  Relationship looks posible?

# Try ordered logistic regression using MASS https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
mTreatment <- polr(what.did.when.last.saw.owl ~ stage, data = attitudeTalk, Hess=TRUE)
summary(mTreatment)
m <- mTreatment
dat <- dataTemp

# Calculate p values
## store table
(ctable <- coef(summary(m)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
## combined table
(ctable <- cbind(ctable, "p value" = p))

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - crosses 0
confint.default(m) # CIs assuming normality

# Validate model
(s <- with(dat, summary(as.numeric(what.do.if.owl.lands.on.the.roof) ~ stage, fun=sf))) 
glm(I(as.numeric(what.do.if.owl.lands.on.the.roof) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(what.do.if.owl.lands.on.the.roof) >= 3) ~ stage, family="binomial", data = dat)

# Fit null model
mNull <- polr(what.do.if.owl.lands.on.the.roof ~ 1, data = dataTemp, Hess=TRUE)
summary(mNull)

# compare full and null model
library(bbmle)
AICtab(mNull, mTreatment)
# Treatment model has best fit


















