rm(list=ls())  

library(ggplot2)
library(dplyr)
library(MASS)
library(bbmle)

data <- read.csv("owlData.csv")

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
# Q1: Would you like to have an owl nesting near your home? (yes/no)
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
p1 <- ggplot(questionTreatmentData, aes(fill=home, x = stage)) + geom_bar() # Could be a relationship
p1

# Fit Bernoulli glm
mTreatment <- glm(home ~ stage, family = binomial (link = "cloglog"), data = questionTreatmentData) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mTreatment)
# Treatment model has significant p value

# Calculate CIs and Odds ratio
m <- mTreatment
confint(m)
exp(coef(m))

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

# Calculate CIs and Odds ratio
m <- mQuestionControl
confint(m)
exp(coef(m))

# Fit null model
mQuestionControlNull <- glm(home ~ 1, family = binomial (link = "cloglog"), data = questionControl) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mQuestionControlNull)
devresid <- resid(mQuestionControlNull, type = "deviance") # for model validation
hist(devresid) # No deviance residuals >2, so no evidence of pooor model fit

# Compare control vs null models
library(bbmle)
AICtab(mQuestionControlNull, mQuestionControl)
# Controls no better than null

###################################################################
# Q2: Would you like to have an owl nesting in the roof of your home? (yes/no)
###################################################################
ggplot(data, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # plot shows missing values

# fliter out the blanks in resonse variable 
questionData <- filter(data, like.nesting.in.roof != "") # removes missing values in response variable
ggplot(questionData, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # missing responses gone

# Now remove respondents that did not hear the talk
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk
ggplot(questionTreatmentData, aes(fill=hear.talk, x = stage)) + geom_bar() # All post resondents heard talk

# Check plot of response variable
ggplot(questionTreatmentData, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # Could be a relationship

# Fit Bernoulli glm
mTreatment <- glm(like.nesting.in.roof ~ stage, family = binomial (link = "cloglog"), data = questionTreatmentData) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mTreatment)
# Treatment model has significant p value

# Calculate CIs and Odds ratio
m <- mTreatment
confint(m)
exp(coef(m))

# Model validation
devresid <- resid(mTreatment, type = "deviance")
hist(devresid) # Some deviance residuals >2, so could be evidence of pooor model fit

# Fit null model
mTreatmentNull <- glm(like.nesting.in.roof ~ 1, family = binomial (link = "cloglog"), data = questionTreatmentData) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mTreatmentNull)

# Model validation
devresid <- resid(mHomeNull, type = "deviance") 
hist(devresid) # No deviance residuals >2, so no evidence of pooor model fit

# Compare treatment and null model
AICtab(mTreatmentNull, mTreatment)
# Treatment fits better than null

# Now fit same models for control group
questionControl <- filter(questionData, hear.talk != "yes") # remove respondents that did  hear talk
ggplot(questionControl, aes(fill=hear.talk, x = stage)) + geom_bar() # all post respondents did not hear talk

# Check plot of resonse variable
ggplot(questionControl, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() 

# Fit model with controls 
mQuestionControl <- glm(like.nesting.in.roof ~ stage, family = binomial (link = "cloglog"), data = questionControl)
summary(mQuestionControl)
devresid <- resid(mQuestionControl, type = "deviance") # for model validation
hist(devresid) # Some deviance residuals >2, so some evidence of pooor model fit

# Calculate CIs and Odds ratio
m <- mQuestionControl
confint(m)
exp(coef(m))

# Fit null model
mQuestionControlNull <- glm(like.nesting.in.roof ~ 1, family = binomial (link = "cloglog"), data = questionControl) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mQuestionControlNull)
devresid <- resid(mQuestionControlNull, type = "deviance") # for model validation
hist(devresid) # Some deviance residuals >2, so some evidence of pooor model fit

# Compare control vs null models
AICtab(mQuestionControlNull, mQuestionControl)
# Controls no better than null


###################################################################
# Q3: Would you put up an artificial nest for owls to nest in, in your yard (compound) near your home? (yes/no)
###################################################################
ggplot(data, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() # plot shows missing values

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

# Fit Bernoulli glm
mTreatment <- glm(would.put.nest.box.in.yard ~ stage, family = binomial (link = "cloglog"), data = questionTreatmentData) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mTreatment)
# Treatment model has significant p value

# Calculate CIs and Odds ratio
m <- mTreatment
confint(m)
exp(coef(m))

# Model validation
devresid <- resid(mTreatment, type = "deviance")
hist(devresid) # No deviance residuals >2, so no evidence of pooor model fit

# Fit null model
mTreatmentNull <- glm(would.put.nest.box.in.yard ~ 1, family = binomial (link = "cloglog"), data = questionTreatmentData) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mTreatmentNull)

# Model validation
devresid <- resid(mHomeNull, type = "deviance") 
hist(devresid) # No deviance residuals >2, so no evidence of pooor model fit

# Compare treatment and null model
AICtab(mTreatmentNull, mTreatment)
# Treatment fits better than null

# Now fit same models for control group
questionControl <- filter(questionData, hear.talk != "yes") # remove respondents that did  hear talk
ggplot(questionControl, aes(fill=hear.talk, x = stage)) + geom_bar() # all post respondents did not hear talk

# Check plot of resonse variable
ggplot(questionControl, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() 

# Fit model with controls 
mQuestionControl <- glm(would.put.nest.box.in.yard ~ stage, family = binomial (link = "cloglog"), data = questionControl)
summary(mQuestionControl)
devresid <- resid(mQuestionControl, type = "deviance") # for model validation
hist(devresid) # Some deviance residuals >2, so some evidence of pooor model fit

# Calculate CIs and Odds ratio
m <- mQuestionControl
confint(m)
exp(coef(m))

# Fit null model
mQuestionControlNull <- glm(would.put.nest.box.in.yard ~ 1, family = binomial (link = "cloglog"), data = questionControl) # used cloglog link over default logit as allows for more asymmetry in response 
summary(mQuestionControlNull)
devresid <- resid(mQuestionControlNull, type = "deviance") # for model validation
hist(devresid) # Some deviance residuals >2, so some evidence of pooor model fit

# Compare control vs null models
AICtab(mQuestionControlNull, mQuestionControl)
# Controls no better than null


###################################################################
############ Questions with >2 ordinal responses ##################
###################################################################

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
library(MASS)
mTreatment <- polr(attitude.to.owls ~ stage, data = questionTreatmentData, Hess=TRUE)
summary(mTreatment)
m <- mTreatment
dat <- questionTreatmentData

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
mTreatmentNull <- polr(attitude.to.owls ~ 1, data = questionTreatmentData, Hess=TRUE)
summary(mTreatmentNull)
m <- mTreatmentNull

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0

# Odds ratio
exp(coef(m))

# compare full and null model
AICtab(mTreatmentNull, mTreatment)
# Treatment model has best fit

# Test responses to same question using controls
# First test those that did hear the talk
ggplot(questionData, aes(fill=hear.talk, x = stage)) + geom_bar() 
questionControlData <- filter(questionData, hear.talk != "yes") # remove respondents that did not hear talk
ggplot(questionControlData, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(questionControlData, aes(fill=attitude.to.owls, x = stage)) + geom_bar() 

# Fit control model
mControl <- polr(attitude.to.owls ~ stage, data = questionControlData, Hess=TRUE)
summary(mControl)
m <- mControl
dat <- questionControlData

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

# Odds ratio
exp(coef(m))

(s <- with(dat, summary(as.numeric(attitude.to.owls) ~ stage, fun=sf))) ###### Can't validate model due to INF values in table #######

glm(I(as.numeric(attitude.to.owls) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(attitude.to.owls) >= 3) ~ stage, family="binomial", data = dat)

# Fit null control model
mControlNull <- polr(attitude.to.owls ~ 1, data = questionControlData, Hess=TRUE)
summary(mControlNull)
m <- mControlNull

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0

# Odds ratio
exp(coef(m))

# compare full and null model
AICtab(mControlNull, mControl)
# Null model has best fit


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

# Fit ordered logistic regression model
mTreatment <- polr(afraid.of.owls ~ stage, data = questionTreatmentData, Hess=TRUE)
summary(mTreatment)
m <- mTreatment
dat <- questionTreatmentData

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
(s <- with(dat, summary(as.numeric(afraid.of.owls) ~ stage, fun=sf))) ###### Can't validate model due to INF values in table #######

glm(I(as.numeric(afraid.of.owls) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(afraid.of.owls) >= 3) ~ stage, family="binomial", data = dat)

# Fit null model
mTreatmentNull <- polr(afraid.of.owls ~ 1, data = questionTreatmentData, Hess=TRUE)
summary(mTreatmentNull)
m <- mTreatmentNull

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0

# Odds ratio
exp(coef(m))

# compare full and null model
AICtab(mTreatmentNull, mTreatment)
# Treatment model has best fit

# Test responses to same question using controls
# First test those that did hear the talk
ggplot(questionData, aes(fill=hear.talk, x = stage)) + geom_bar() 
questionControlData <- filter(questionData, hear.talk != "yes") # remove respondents that did not hear talk
ggplot(questionControlData, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(questionControlData, aes(fill=afraid.of.owls, x = stage)) + geom_bar() 

# Fit control model
mControl <- polr(afraid.of.owls ~ stage, data = questionControlData, Hess=TRUE)
summary(mControl)
m <- mControl
dat <- questionControlData

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

# Odds ratio
exp(coef(m))

(s <- with(dat, summary(as.numeric(afraid.of.owls) ~ stage, fun=sf))) ###### Can't validate model due to INF values in table #######

glm(I(as.numeric(afraid.of.owls) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(afraid.of.owls) >= 3) ~ stage, family="binomial", data = dat)

# Fit null control model
mControlNull <- polr(afraid.of.owls ~ 1, data = questionControlData, Hess=TRUE)
summary(mControlNull)
m <- mControlNull

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0

# Odds ratio
exp(coef(m))

# compare full and null model
AICtab(mControlNull, mControl)
# Null model has best fit


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

# Fit ordered logistic regression model
mTreatment <- polr(what.do.if.owl.lands.on.the.roof ~ stage, data = questionTreatmentData, Hess=TRUE)
summary(mTreatment)
m <- mTreatment
dat <- questionTreatmentData

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
(s <- with(dat, summary(as.numeric(what.do.if.owl.lands.on.the.roof) ~ stage, fun=sf))) ###### Can't validate model due to INF values in table #######

glm(I(as.numeric(what.do.if.owl.lands.on.the.roof) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(what.do.if.owl.lands.on.the.roof) >= 3) ~ stage, family="binomial", data = dat)

# Fit null model
mTreatmentNull <- polr(what.do.if.owl.lands.on.the.roof ~ 1, data = questionTreatmentData, Hess=TRUE)
summary(mTreatmentNull)
m <- mTreatmentNull

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0

# Odds ratio
exp(coef(m))

# compare full and null model
AICtab(mTreatmentNull, mTreatment)
# Treatment model has best fit

# Test responses to same question using controls
# First test those that did hear the talk
ggplot(questionData, aes(fill=hear.talk, x = stage)) + geom_bar() 
questionControlData <- filter(questionData, hear.talk != "yes") # remove respondents that did not hear talk
ggplot(questionControlData, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(questionControlData, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar() 

# Fit control model
mControl <- polr(what.do.if.owl.lands.on.the.roof ~ stage, data = questionControlData, Hess=TRUE)
summary(mControl)
m <- mControl
dat <- questionControlData

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

# Odds ratio
exp(coef(m))

(s <- with(dat, summary(as.numeric(what.do.if.owl.lands.on.the.roof) ~ stage, fun=sf))) ###### Can't validate model due to INF values in table #######

glm(I(as.numeric(what.do.if.owl.lands.on.the.roof) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(what.do.if.owl.lands.on.the.roof) >= 3) ~ stage, family="binomial", data = dat)

# Fit null control model
mControlNull <- polr(what.do.if.owl.lands.on.the.roof ~ 1, data = questionControlData, Hess=TRUE)
summary(mControlNull)
m <- mControlNull

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0

# Odds ratio
exp(coef(m))

# compare full and null model
AICtab(mControlNull, mControl)
# Null model has best fit

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

# Fit ordered logistic regression model
mTreatment <- polr(what.did.when.last.saw.owl ~ stage, data = questionTreatmentData, Hess=TRUE)
summary(mTreatment)
m <- mTreatment
dat <- questionTreatmentData

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
(s <- with(dat, summary(as.numeric(what.did.when.last.saw.owl) ~ stage, fun=sf))) ###### Can't validate model due to INF values in table #######

glm(I(as.numeric(what.did.when.last.saw.owl) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(what.did.when.last.saw.owl) >= 3) ~ stage, family="binomial", data = dat)

# Fit null model
mTreatmentNull <- polr(what.did.when.last.saw.owl ~ 1, data = questionTreatmentData, Hess=TRUE)
summary(mTreatmentNull)
m <- mTreatmentNull

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0

# Odds ratio
exp(coef(m))

# compare full and null model
AICtab(mTreatmentNull, mTreatment)
# Null model has best fit

# Test responses to same question using controls
# First test those that did hear the talk
ggplot(questionData, aes(fill=hear.talk, x = stage)) + geom_bar() 
questionControlData <- filter(questionData, hear.talk != "yes") # remove respondents that did not hear talk
ggplot(questionControlData, aes(fill=hear.talk, x = stage)) + geom_bar() # No talk gone

# Check plot of resonse variable
ggplot(questionControlData, aes(fill=what.did.when.last.saw.owl, x = stage)) + geom_bar() 

# Fit control model
mControl <- polr(what.did.when.last.saw.owl ~ stage, data = questionControlData, Hess=TRUE)
summary(mControl)
m <- mControl
dat <- questionControlData

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

# Odds ratio
exp(coef(m))

(s <- with(dat, summary(as.numeric(what.did.when.last.saw.owl) ~ stage, fun=sf))) ###### Can't validate model due to INF values in table #######

glm(I(as.numeric(what.did.when.last.saw.owl) >= 2) ~ stage, family="binomial", data = dat)
glm(I(as.numeric(what.did.when.last.saw.owl) >= 3) ~ stage, family="binomial", data = dat)

# Fit null control model
mControlNull <- polr(what.did.when.last.saw.owl ~ 1, data = questionControlData, Hess=TRUE)
summary(mControlNull)
m <- mControlNull

# 95% CIs
(ci <- confint(m)) # default method gives profiled CIs - shouldn't cross 0

# Odds ratio
exp(coef(m))

# compare full and null model
AICtab(mControlNull, mControl)
# Null model has best fit


###
# Side note - plots for EcoRodMan report
library(ggplot2)

p1 <- ggplot(questionTreatmentData, aes(fill=home, x = stage)) + geom_bar() 
p1

p2 <- ggplot(questionTreatmentData, aes(fill=like.nesting.in.roof, x = stage)) + geom_bar() # Could be a relationship
p2

p3 <- ggplot(questionTreatmentData, aes(fill=would.put.nest.box.in.yard, x = stage)) + geom_bar() # Could be a relationship
p3

fig1 <- plot_grid(p1, p2, p3, ncol = 1, labels = c("1", "2", "3"))
fig1

p4 <- ggplot(questionTreatmentData, aes(fill=attitude.to.owls, x = stage)) + geom_bar() # Looks like a relationship
p4

p5 <- # Check plot of resonse variable
  ggplot(questionTreatmentData, aes(fill=afraid.of.owls, x = stage)) + geom_bar() # Looks like a relationship
p5

p6 <- ggplot(questionTreatmentData, aes(fill=what.do.if.owl.lands.on.the.roof, x = stage)) + geom_bar() # Looks like a relationship
p6

p7 <- ggplot(questionTreatmentData, aes(fill=what.did.when.last.saw.owl, x = stage)) + geom_bar() # Looks like a relationship
p7

fig2 <- plot_grid(p4, p5, p6, p7, ncol = 1, labels = c("4", "5", "6", "7"))
fig2

###

# Publication plots 

# 100% stacked bar plots 

###################################################################
# Q1: Would you like to have an owl nesting near your home? (yes/no)
###################################################################

# fliter out the blanks in resonse variable 
library(dplyr)
questionData <- filter(data, home != "") # removes missing values in response variable
ggplot(questionData, aes(fill=home, x = stage)) + geom_bar() # missing responses gone

# Now remove respondents that did not hear the talk
questionTreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk

df <- questionTreatmentData 
library(janitor)
a <- df %>% tabyl(home, stage) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
a
a <- a[-1, ] # remove first row

library(tidyr)
a <- a %>% gather(stage, percent, 2:3) # convert to long format
a

a$percent <- as.numeric(a$percent) # convert percent to numeric
class(a$percent)

# Calculate sample size

df <- questionTreatmentData 
library(janitor)

b <- df %>% tabyl(home, stage) # create n table 
b

b <- b[-1, ] # remove first row
b

library(tidyr)
b <- b %>% gather(stage, n, 2:3) # convert to long format
b

a$n <- b$n # add n column to percent table
a

# Add controls
library(dplyr)
questionControl <- filter(questionData, hear.talk != "yes") # remove respondents that did  hear talk
df <- questionControl

c <- df %>% tabyl(home, stage) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
c
c <- c[-1, ] # remove first row
c

names(c) <- c("home", "1 prior", "3 control")
c

library(tidyr)
c <- c %>% gather(stage, percent, 2:3) # convert to long format
c

c$percent <- as.numeric(a$percent) # convert percent to numeric
class(c$percent)

d <- df %>% tabyl(home, stage) # create n table 
d

d <- d[-1, ] # remove first row
d

names(d) <- c("home", "1 prior", "3 control")
d

library(tidyr)
d <- d %>% gather(stage, n, 2:3) # convert to long format
d

c$n <- d$n # add n column to percent table
c

e <- rbind(a, c[3:4,])
e

e$percent <- as.numeric(e$percent)

# plot

cbp2<- c("#56B4E9", "#009E73") # create colour blind palatte with 2 colours (used palatte from https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/)

library(ggplot2)
p <- ggplot(e, aes(x = stage, y = percent,fill = home)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = cbp2, name = "Would you like to have \nan owl nesting \nnear your home?", labels = c("No", "Yes")) + 
  ylim(0,100) +
  xlab("Group") + ylab("Percent") +
  scale_x_discrete(labels=c("Before \npresentation", "After \npresentation", "Control")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))
p

p1 <- p

# ggsave("p.png", dpi = 600)

###################################################################
# Q2: Would you like to have an owl nesting in the roof of your home? (yes/no)
###################################################################
questionData <- filter(data, like.nesting.in.roof != "") # removes missing values in response variable
Q2TreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk

df <- Q2TreatmentData 
a <- df %>% tabyl(like.nesting.in.roof, stage) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
a
a <- a[-1, ] # remove first row

a <- a %>% gather(stage, percent, 2:3) # convert to long format
a

a$percent <- as.numeric(a$percent) # convert percent to numeric
class(a$percent)

# Calculate sample size
b <- df %>% tabyl(like.nesting.in.roof, stage) # create n table 
b

b <- b[-1, ] # remove first row
b

library(tidyr)
b <- b %>% gather(stage, n, 2:3) # convert to long format
b

a$n <- b$n # add n column to percent table
a

# Add controls
Q2ControlData <- filter(questionData, hear.talk != "yes") # remove respondents that did  hear talk
df <- Q2ControlData

c <- df %>% tabyl(like.nesting.in.roof, stage) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
c
c <- c[-1, ] # remove first row
c

names(c) <- c("like.nesting.in.roof", "1 prior", "3 control")
c

c <- c %>% gather(stage, percent, 2:3) # convert to long format
c

c$percent <- as.numeric(c$percent) # convert percent to numeric
class(c$percent)

d <- df %>% tabyl(like.nesting.in.roof, stage) # create n table 
d

d <- d[-1, ] # remove first row
d

names(d) <- c("like.nesting.in.roof", "1 prior", "3 control")
d

d <- d %>% gather(stage, n, 2:3) # convert to long format
d

c$n <- d$n # add n column to percent table
c

e <- rbind(a, c[3:4,])
e

e$percent <- as.numeric(e$percent)
e

# plot

cbp2<- c("#56B4E9", "#009E73") # create colour blind palatte with 2 colours (used palatte from https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/)

library(ggplot2)
p <- ggplot(e, aes(x = stage, y = percent,fill = like.nesting.in.roof)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = cbp2, name = "Would you like to have \nan owl nesting \nin the roof of your home?", labels = c("No", "Yes")) + 
  ylim(0,100) +
  xlab("Group") + ylab("Percent") +
  scale_x_discrete(labels=c("Before \npresentation", "After \npresentation", "Control")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))
p

p2 <- p

# ggsave("p.png", dpi = 600)

library(cowplot)
plot_grid(p1,p2, ncol = 1)

###################################################################
# Q3: Would you put up an artificial nest for owls to nest in, in your yard (compound) near your home? (yes/no)
###################################################################
# fliter out the blanks & errors in resonse variable
boxData <- filter(data, would.put.nest.box.in.yard != "")
boxData <- filter(boxData, would.put.nest.box.in.yard != "the owls. No owls")
boxData <- filter(boxData, would.put.nest.box.in.yard != "witchcraft")

questionData <- boxData

# Now remove respondents that did not hear the talk
Q3TreatmentData <- filter(questionData, hear.talk != "no") # remove respondents that did not hear talk

df <- Q3TreatmentData 
a <- df %>% tabyl(would.put.nest.box.in.yard, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
a

a <- a %>% gather(stage, percent, 2:3) # convert to long format
a

a$percent <- as.numeric(a$percent) # convert percent to numeric
class(a$percent)

# Calculate sample size
b <- df %>% tabyl(would.put.nest.box.in.yard, stage, show_missing_levels = FALSE) # create n table 
b

b <- b %>% gather(stage, n, 2:3) # convert to long format
b

a$n <- b$n # add n column to percent table
a

# Add controls
Q3ControlData <- filter(questionData, hear.talk != "yes") # remove respondents that did  hear talk
df <- Q3ControlData

c <- df %>% tabyl(would.put.nest.box.in.yard, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
c

names(c) <- c("would.put.nest.box.in.yard", "1 prior", "3 control")
c

c <- c %>% gather(stage, percent, 2:3) # convert to long format
c

c$percent <- as.numeric(c$percent) # convert percent to numeric
class(c$percent)

d <- df %>% tabyl(would.put.nest.box.in.yard, stage, show_missing_levels = FALSE) # create n table 
d

names(d) <- c("would.put.nest.box.in.yard", "1 prior", "3 control")
d

d <- d %>% gather(stage, n, 2:3) # convert to long format
d

c$n <- d$n # add n column to percent table
c

a
c

e <- rbind(a, c[3:4,])
e

e$percent <- as.numeric(e$percent)
e

# plot

cbp2<- c("#56B4E9", "#009E73") # create colour blind palatte with 2 colours (used palatte from https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/)

library(ggplot2)
p <- ggplot(e, aes(x = stage, y = percent,fill = would.put.nest.box.in.yard)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = cbp2, name = "Would you put up an \nartificial nest for owls \nin your yard?", labels = c("No", "Yes")) + 
  ylim(0,100) +
  xlab("Group") + ylab("Percent") +
  scale_x_discrete(labels=c("Before \npresentation", "After \npresentation", "Control")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))
p

p3 <- p

# Combine plots
plot1 <- plot_grid(p1,p2, p3, ncol = 1, labels = "auto")
plot1
save_plot("plot1.png", plot1, dpi = 600, base_width = 9, base_height = 7)

###################################################################
############ Questions with >2 ordinal responses ##################
###################################################################

###################################################################
# Q4: Which of the three choices best describes your feeling towards owls?
# Like/No feeling/Not like
###################################################################
library(dplyr)
Q4Data <- filter(data, attitude.to.owls != "") # removes missing values in response variable

# First test those that did hear the talk
Q4TreatmentData <- filter(Q4Data, hear.talk != "no") # remove respondents that did not hear talk

df <- Q4TreatmentData 

library(janitor)
a <- df %>% tabyl(attitude.to.owls, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
a

library(tidyr)
a <- a %>% gather(stage, percent, 2:3) # convert to long format
a

a$percent <- as.numeric(a$percent) # convert percent to numeric
class(a$percent)

# Calculate sample size
b <- df %>% tabyl(attitude.to.owls, stage, show_missing_levels = FALSE) # create n table 
b

b <- b %>% gather(stage, n, 2:3) # convert to long format
b

a$n <- b$n # add n column to percent table
a

# Add controls
Q4ControlData <- filter(Q4Data, hear.talk != "yes") # remove respondents that did  hear talk
df <- Q4ControlData

library(janitor)
c <- df %>% tabyl(attitude.to.owls, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
c

names(c) <- c("attitude.to.owls", "1 prior", "3 control")
c

c <- c %>% gather(stage, percent, 2:3) # convert to long format
c

c$percent <- as.numeric(c$percent) # convert percent to numeric
class(c$percent)

d <- df %>% tabyl(attitude.to.owls, stage, show_missing_levels = FALSE) # create n table 
d

names(d) <- c("attitude.to.owls", "1 prior", "3 control")
d

d <- d %>% gather(stage, n, 2:3) # convert to long format
d

c$n <- d$n # add n column to percent table
c

e <- rbind(a, c[4:6,])
e

e$percent <- as.numeric(e$percent)
e

# plot

cbp3<- c("#56B4E9", "#009E73", "#E69F00") # create colour blind palatte with 3 colours (used palatte from https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/)

library(ggplot2)
p <- ggplot(e, aes(x = stage, y = percent,fill = attitude.to.owls)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = cbp3, name = "Which best describes \nyour feeling towards owls?", labels = c("Not like", "No feeling", "Like")) + 
  ylim(0,100) +
  xlab("Group") + ylab("Percent") +
  scale_x_discrete(labels=c("Before \npresentation", "After \npresentation", "Control")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))
p

p4 <- p

###################################################################
# Q5: Which of the three choices best describes your response to seeing an owl?
# Not afraid/Very afraid/Terrified
###################################################################
Q5Data <- filter(data, afraid.of.owls != "") # removes missing values in response variable

# First test those that did hear the talk
Q5TreatmentData <- filter(Q5Data, hear.talk != "no") # remove respondents that did not hear talk

df <- Q5TreatmentData 

a <- df %>% tabyl(afraid.of.owls, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
a

a <- a %>% gather(stage, percent, 2:3) # convert to long format
a

a$percent <- as.numeric(a$percent) # convert percent to numeric
class(a$percent)

# Calculate sample size
b <- df %>% tabyl(afraid.of.owls, stage, show_missing_levels = FALSE) # create n table 
b

b <- b %>% gather(stage, n, 2:3) # convert to long format
b

a$n <- b$n # add n column to percent table
a

# Add controls
Q5ControlData <- filter(Q5Data, hear.talk != "yes") # remove respondents that did  hear talk
df <- Q5ControlData

c <- df %>% tabyl(afraid.of.owls, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
c

names(c) <- c("afraid.of.owls", "1 prior", "3 control")
c

c <- c %>% gather(stage, percent, 2:3) # convert to long format
c

c$percent <- as.numeric(c$percent) # convert percent to numeric
class(c$percent)

d <- df %>% tabyl(afraid.of.owls, stage, show_missing_levels = FALSE) # create n table 
d

names(d) <- c("afraid.of.owls", "1 prior", "3 control")
d

d <- d %>% gather(stage, n, 2:3) # convert to long format
d

c$n <- d$n # add n column to percent table
c

e <- rbind(a, c[4:6,])
e

e$percent <- as.numeric(e$percent)
e

# plot

cbp3<- c("#56B4E9", "#009E73", "#E69F00") # create colour blind palatte with 3 colours (used palatte from https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/)

library(ggplot2)
p <- ggplot(e, aes(x = stage, y = percent,fill = afraid.of.owls)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = cbp3, name = "Which best describes \nyour response to seeing an \nowl?", labels = c("Terrified", "Very afraid", "Not afraid")) + 
  ylim(0,100) +
  xlab("Group") + ylab("Percent") +
  scale_x_discrete(labels=c("Before \npresentation", "After \npresentation", "Control")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))
p

p5 <- p

library(cowplot)
plot_grid(p4, p5, ncol = 1)


###################################################################
# Q6: What do you do if an owl lands on the roof of your home?
# Run away/Nothing/Try and kill the owl/Chase the owl away.
###################################################################

Q6Data <- filter(data, what.do.if.owl.lands.on.the.roof != "") # removes missing values in response variable

# First test those that did hear the talk
Q6TreatmentData <- filter(Q6Data, hear.talk != "no") # remove respondents that did not hear talk

df <- Q6TreatmentData 

a <- df %>% tabyl(what.do.if.owl.lands.on.the.roof, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
a

a <- a %>% gather(stage, percent, 2:3) # convert to long format
a

a$percent <- as.numeric(a$percent) # convert percent to numeric
class(a$percent)

# Calculate sample size
b <- df %>% tabyl(what.do.if.owl.lands.on.the.roof, stage, show_missing_levels = FALSE) # create n table 
b

b <- b %>% gather(stage, n, 2:3) # convert to long format
b

a$n <- b$n # add n column to percent table
a

# Add controls
Q6ControlData <- filter(Q6Data, hear.talk != "yes") # remove respondents that did  hear talk
df <- Q6ControlData

c <- df %>% tabyl(what.do.if.owl.lands.on.the.roof, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
c

names(c) <- c("what.do.if.owl.lands.on.the.roof", "1 prior", "3 control")
c

c <- c %>% gather(stage, percent, 2:3) # convert to long format
c
a
c$percent <- as.numeric(c$percent) # convert percent to numeric
class(c$percent)

d <- df %>% tabyl(what.do.if.owl.lands.on.the.roof, stage, show_missing_levels = FALSE) # create n table 
d

names(d) <- c("what.do.if.owl.lands.on.the.roof", "1 prior", "3 control")
d

d <- d %>% gather(stage, n, 2:3) # convert to long format
d

c$n <- d$n # add n column to percent table
c

e <- rbind(a, c[5:8,])
e

e$percent <- as.numeric(e$percent)
e

# plot

cbp4<- c("#56B4E9", "#009E73", "#E69F00", "#D55E00") # create colour blind palatte with 3 colours (used palatte from https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/)

library(ggplot2)
p <- ggplot(e, aes(x = stage, y = percent,fill = what.do.if.owl.lands.on.the.roof)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = cbp4, name = "What do you do if an \nowl lands on the roof of \nyour home?", labels = c("Try and kill the owl", "Chase the owl away", "Run away", "Do nothing")) + 
  ylim(0,101) +
  xlab("Group") + ylab("Percent") +
  scale_x_discrete(labels=c("Before \npresentation", "After \npresentation", "Control")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))
p

p6 <- p

library(cowplot)
plot_grid(p4, p5, p6, ncol = 1)

###################################################################
# Q7:What did you do the last time you saw an owl?
# Run away/Nothing/Try and kill the owl
###################################################################
# fliter out the blanks in resonse variable & delete rows that did not hear talk
Q7Data <- filter(data, what.did.when.last.saw.owl != "") # removes missing values in response variable

# First test those that did hear the talk
Q7TreatmentData <- filter(Q7Data, hear.talk != "no") # remove respondents that did not hear talk

df <- Q7TreatmentData 

a <- df %>% tabyl(what.did.when.last.saw.owl, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
a

a <- a %>% gather(stage, percent, 2:3) # convert to long format
a

a$percent <- as.numeric(a$percent) # convert percent to numeric
class(a$percent)

# Calculate sample size
b <- df %>% tabyl(what.did.when.last.saw.owl, stage, show_missing_levels = FALSE) # create n table 
b

b <- b %>% gather(stage, n, 2:3) # convert to long format
b

a$n <- b$n # add n column to percent table
a

# Add controls
Q7ControlData <- filter(Q7Data, hear.talk != "yes") # remove respondents that did  hear talk
df <- Q7ControlData

c <- df %>% tabyl(what.did.when.last.saw.owl, stage, show_missing_levels = FALSE) %>% adorn_percentages("col") %>% adorn_pct_formatting(digits = 1, affix_sign = FALSE) # create percentage table 
c

names(c) <- c("what.did.when.last.saw.owl", "1 prior", "3 control")
c

c <- c %>% gather(stage, percent, 2:3) # convert to long format
c

c$percent <- as.numeric(c$percent) # convert percent to numeric
class(c$percent)

d <- df %>% tabyl(what.did.when.last.saw.owl, stage, show_missing_levels = FALSE) # create n table 
d

names(d) <- c("what.did.when.last.saw.owl", "1 prior", "3 control")
d

d <- d %>% gather(stage, n, 2:3) # convert to long format
d

c$n <- d$n # add n column to percent table
c

e <- rbind(a, c[4:6,])
e

e$percent <- as.numeric(e$percent)
e

# plot

cbp3<- c("#56B4E9", "#009E73", "#E69F00") # create colour blind palatte with 3 colours (used palatte from https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/)

library(ggplot2)
p <- ggplot(e, aes(x = stage, y = percent,fill = what.did.when.last.saw.owl)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_manual(values = cbp3, name = "What did you do the last \ntime you saw an owl?", labels = c("Try and kill the owl", "Run away", "Nothing")) + 
  ylim(0,100) +
  xlab("Group") + ylab("Percent") +
  scale_x_discrete(labels=c("Before \npresentation", "After \npresentation", "Control")) +
  geom_text(aes(label = n), position = position_stack(vjust = 0.5))
p

p7 <- p


# Combine plots
plot2 <- plot_grid(p4, p5, p6, p7, ncol = 1, labels = "auto")
plot2
save_plot("plot2.png", plot2, dpi = 500, base_width = 9, base_height = 9)

