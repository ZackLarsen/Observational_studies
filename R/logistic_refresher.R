
## Logistic regression example using Adult dataset fom UC Irvine machine 
## learning repository
## Data available at:
# https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data

## Helpful info at
# https://machinelearningmastery.com/logistic-regression-for-machine-learning/
# https://www.predictiveanalyticsworld.com/patimes/on-variable-importance-in-logistic-regression/9649/

## Excellent FREE book at:
# https://web.stanford.edu/~hastie/Papers/ESLII.pdf
## AND
# http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Seventh%20Printing.pdf



library(pacman)
p_load(tidyverse, xgboost, data.table, fastDummies, ROCR, glmnet, ISLR)



# Notes -------------------------------------------------------------------

## From ESLII.pdf:

# Model building can be costly for logistic regression models, because
# each model fitted requires iteration. Popular shortcuts are the Rao
# score test which tests for inclusion of a term, and the Wald test which
# can be used to test for exclusion of a term. Neither of these require
# iterative fitting, and are based on the maximum-likelihood fit of the
# current model. It turns out that both of these amount to adding
# or dropping a term from the weighted least squares fit, using the
# same weights. Such computations can be done efficiently, without
# recomputing the entire weighted least squares fit

# The L1 penalty used in the lasso (Section 3.4.2) can be used for variable
# selection and shrinkage with any linear regression model

# As with the lasso, we typically do not penalize the intercept term, 
# and standardize the predictors for the penalty to be meaningful.

# NOTE: The above line says not to penalize the intercept term, but scikit-learn
# does this by default in python. Make sure the software you are using, whether
# in R or Python, does not do this as a default option, or make a note of it
# so you can override the default.

# It is generally felt that logistic
# regression is a safer, more robust bet than the LDA model, relying on fewer
# assumptions. It is our experience that the models give very similar results,
# even when LDA is used inappropriately, such as with qualitative predictors



## From ISLR.pdf:
## Speaking about the output of a logistic regression model:
# (paraphrasing - the z-score associated with a coefficient beta 1 is equal to beta 1 divided 
# its standard error) ... so a large (absolute) value of the z-statistic indicates evidence 
# against the null hypothesis H0 : β1 = 0. (paraphrasing again - this means that we have
# sufficient evidence to reject the null hypothesis that this variable doesn't affect the
# outcome variable)

# Paraphrasing here - the coefficients in the model represent, for FIXED values of the
# other variables, how much the probability of the outcome is affected by the variable
# corresponding to that coefficient.


# As in the linear regression setting, the
# results obtained using one predictor may be quite different from those obtained
# using multiple predictors, especially when there is correlation among
# the predictors. In general, the phenomenon seen in Figure 4.3 is known as
# confounding.

# If you want to perform a classification model with more than two outputs (binary),
# use discriminant analysis instead of extending logistic regression to multinomial outputs.

















# ISLR examples -----------------------------------------------------------

# https://rpubs.com/tdib03/StockMarketLab

# Summarise the data
summary(Smarket)

# Look at pairwise scatter plot matrix
pairs(Smarket)


# Train the model
glm.fits=glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data=Smarket,
             family =binomial )

summary(glm.fits)

# Assess just the coefficients
coef(glm.fits)


# access particular aspects of the fitted model, such as the p-values for the coefficients.
summary(glm.fits)$coef





# predict the probability that the market will go up, given values of the predictors:
glm.probs = predict(glm.fits, type = "response")
# If no data set is supplied to the predict() function, then the probabilities
# are computed for the training data that was used to fit the logistic regression model.

# Examine first 10:
glm.probs[1:10]


# In order to make a prediction as to whether the market will go up or
# down on a particular day, we must convert these predicted probabilities
# into class labels, Up or Down. The following two commands create a vector
# of class predictions based on whether the predicted probability of a market
# increase is greater than or less than 0.5.
glm.pred=rep("Down", 1250)
glm.pred[glm.probs > .5]="Up"
# The first command creates a vector of 1,250 Down elements. The second line
# transforms to Up all of the elements for which the predicted probability of a
# market increase exceeds 0.5. Given these predictions, the table() function
# can be used to produce a confusion matrix in order to determine how many
# observations were correctly or incorrectly classified.


table(glm.pred, Direction)

mean(glm.pred==Direction)





train=(Year < 2005)
Smarket.2005 = Smarket[! train ,]
dim(Smarket.2005)

Direction.2005 = Direction[! train]
























# Adult data prep ---------------------------------------------------------


adult <- fread('C:\\Users\\U383387\\Zack_master\\Datasets\\adult.csv', nrows = 1000, header = FALSE)
adult %>% 
  head()

colnames(adult) <- c(
  "age",
  "workclass",
  "fnlwgt",
  "education",
  "education-num",
  "marital-status",
  "occupation",
  "relationship",
  "race",
  "sex",
  "capital-gain",
  "capital-loss",
  "hours-per-week",
  "native-country",
  "income"
)


colnames(adult) 
adult %>% 
  head()

glimpse(adult)

adult$workclass <- as.factor(adult$workclass)
adult$education <- as.factor(adult$education)
adult$marital_status <- as.factor(adult$`marital-status`)
adult$occupation <- as.factor(adult$occupation)
adult$relationship <- as.factor(adult$relationship)
adult$race <- as.factor(adult$race)
adult$sex <- as.factor(adult$sex)
adult$native_country <- as.factor(adult$`native-country`)
adult$income <- as.factor(adult$income)
adult$income <- as.integer(ifelse(adult$income == "<=50K",0,1))

glimpse(adult)

adult_dummies <- fastDummies::dummy_cols(adult)
colnames(adult_dummies)

glimpse(adult_dummies)



# Train logistic regression model
fit = glm(income ~ age + `workclass_Private` + `workclass_Self-emp-inc` + 
            `education_Bachelors` + `education_Masters` + `education_Doctorate` +
            sex + race + `marital-status_Divorced` + 
            `marital-status_Married-spouse-absent` + 
            `occupation_Exec-managerial` + `occupation_?` + 
            `occupation_Handlers-cleaners` +
            `occupation_Adm-clerical` + `occupation_Farming-fishing` + 
            `occupation_Other-service` +
            `relationship_Wife` + `relationship_Husband` + `race_White` + 
            `race_Black` +
            `sex_Male` + `sex_Female` + `native-country_United-States` +
            `native-country_Mexico` + 
            `native-country_India` + `native-country_South`, 
          data=adult_dummies,
          family=binomial())

#show results
summary(fit)


# Plot the different levels of income by age
plot(income ~ age, data=adult, lwd=2)

# Plot the different levels of income by race
plot(income ~ race, data=adult, lwd=2)

# Plot the different levels of income by education
plot(income ~ education, data=adult, lwd=2)

# add regression "line" to plot
abline(fit, col="red")
title(main="Using Simple Linear Regression")


lines(adult$income, fit$fitted, type="l", col="red", lwd=2)
title(main="Using Logistic Regression")





# find several predicted values. Reminder: output is logits
pred=predict(fit, data.frame(INCOME=c(20000, 40000, 60000, 80000, 100000)))

# convert logits to probabilities
prob = exp(pred) / (1 + exp(pred))
prob


# show ROC curve (must install ROCR package)
pred = prediction(fit$fitted.values, adult$)
perf = performance(pred,"tpr","fpr")
plot(perf, main="ROC Curve")
abline(a=0, b= 1)






#computes deviance / likelihood ratio test
anova(fit, update(fit, ~1), test="Chisq") 

confint(fit) # 95% CI for the coefficients
exp(coef(fit)) # to analyze change in odds for changes in X
exp(confint(fit)) # 95% CI for exp(coefficients)
predict(fit, type="response") # predicted probability values
residuals(fit, type="deviance") # residuals








