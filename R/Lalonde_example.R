##################################################
## Project: Matching
## Script purpose: Run through examples with Lalonde data
## Date: January 1, 2021
## Author: Zack Larsen
##################################################






# Steps -------------------------------------------------------------------


Match

Assess balance

Re-match as needed until balance is satisfied

Run post-match analysis

Test if matching with ratio affects results
- match with ratio of 2:1 and then subset the dataset
- so that the ratio is 1:1 and compare results





# Resources ---------------------------------------------------------------



#https://docs.zeligproject.org/articles/quickstart.html

#https://docs.zeligproject.org/articles/using_Zelig_with_MatchIt.html

#https://docs.zeligproject.org/articles/att.html

#https://docs.zeligproject.org/articles/bootstraps.html







#http://freerangestats.info/blog/2017/04/09/propensity-v-regression

#Compared to the older style propensity matching to create a pseudo
#control sample, it may be better to weight the full data by inverse
# propensity score because it doesn't discard data. Performing a 
# regression (rather than simple cross tabs) after the weighting 
# or matching is a good idea to handle inevitable imperfections. 
# The whole family of methods doesn't necessarily deliver big gains 
# over more straightforward single stage regressions. 
# And if you have omitted material variables you're in 
# trouble whatever you do.





# https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html#after-pair-matching-with-replacement-1

# Several methods involve weights that are to be used in estimating the
# treatment effect. With full matching and stratification matching
# (when analyzed using MMWS), the weights do the entire work of balancing 
# the covariates across the treatment groups. Omitting weights essentially 
# ignores the entire purpose of matching. Some cases are less obvious. 
# When performing matching with replacement and estimating the treatment 
# effect using the match.data() output, weights must be included to
# ensure control units matched to multiple treated units are weighted accordingly. 
# Similarly, when performing k:1 matching where not all treated units 
# receive k matches, weights are required to account for the differential 
# weight of the matched control units. The only time weights can be
# omitted after pair matching is when performing 1:1 matching without 
# replacement. Including weights even in this scenario will not affect 
# the analysis and it can be good practice to always include weights to 
# prevent this error from occurring. There are some scenarios where weights 
# are not useful because the conditioning occurs through some other means, 
# such as when using the pooling strategy rather than MMWS for estimating 
# marginal effects after stratification.









# Data io -----------------------------------------------------------------


data("lalonde")
attach(lalonde)


# Data prep ---------------------------------------------------------------


lalonde <- as.data.frame(lalonde)


# lalonde %<>% 
#   mutate(
#     black = ifelse(race == "black", 1, 0),
#     hispan = ifelse(race == "hispan", 1, 0)
#   )





# EDA ---------------------------------------------------------------------


lalonde %>% 
  group_by(treat) %>% 
  summarise_all(list(mean))





# Dataviz -----------------------------------------------------------------



hp <- ggplot(lalonde, aes(x=age)) + 
  geom_histogram(binwidth=2,colour="white",fill="skyblue")
ggplotly(hp)
# Divide by levels of treatment, in the horizontal direction
hp + facet_grid(. ~ treat)



hp <- ggplot(lalonde, aes(x=educ)) + 
  geom_histogram(binwidth=2,colour="white",fill="skyblue")
hp + facet_grid(. ~ treat)




hp <- ggplot(lalonde, aes(x=treat)) + 
  geom_histogram(binwidth=2,colour="white",fill="skyblue")
hp + facet_grid(. ~ re75)







# Boxplots with facet grid for treatment indicator:

dfmelt<-melt(lalonde, measure.vars = 2:10)
dfmelt

p <- ggplot(dfmelt, aes(x=factor(round(treat,0.5)), y=value,fill=variable))+
  geom_boxplot()+
  facet_grid(.~variable)+
  labs(x="X (binned)")+
  theme(axis.text.x=element_text(angle=-90, vjust=0.4,hjust=1))

p <- ggplotly(p)
p


p + facet_grid(treat ~ .)









# Pre-match standardized differences --------------------------------------



stddiff.numeric(lalonde, vcol = "treat")




# Matching ----------------------------------------------------------------





colnames(lalonde)
prop_vars <- c("age","educ","black","hisp","married","nodegr","re74","re75")
F1 <- f.build("treat", prop_vars)

nn <- matchit(formula = F1,
              data = lalonde,
              method = "nearest")

summary(nn)
#plot(nn)
plot(nn, type = "jitter", interactive = FALSE)
plot(nn, type = "hist")

love.plot(bal.tab(nn))









prop_vars <- c("age","educ","black","hisp","married","nodegr","re74","re75")
F1 <- f.build("treat",prop_vars)
ratio <- matchit(formula = F1,
                 data = lalonde,
                 method = "nearest", 
                 ratio = 2)

summary(ratio)
plot(ratio, type = "jitter", interactive = FALSE)
plot(ratio, type = "hist")

love.plot(bal.tab(ratio))

#Plot balance
plot(summary(ratio))











# Post-matching standardized differences ----------------------------------










# Match pairs -------------------------------------------------------------




match.data(nn)

nn$match.matrix



nn_matches <- get_matches(nn)

dim(nn_matches)



match.data(nn, "control")

match.data(nn, "treat")






# Post-matching analysis --------------------------------------------------








# LM ----------------------------------------------------------------------






# GLM ---------------------------------------------------------------------




earnings_formula <- f.build("re78", c("treat"))

earnings_formula_with_ps <- f.build("re78", c("treat", "distance"))



earnings_glm <- glm(formula = earnings_formula, data = nn_matches, family = "gaussian")

coef(earnings_glm)

confint(earnings_glm)
#689.6568 3346.082





earnings_glm_ps <- glm(formula = earnings_formula_with_ps, data = nn_matches, family = "gaussian")

coef(earnings_glm_ps)

confint(earnings_glm_ps)
#623.6155  3278.166







# RLM ---------------------------------------------------------------------





coef(rlm(re78 ~ age + treat + educ + black + hisp + nodegr + married + re74 + re75, 
         data = nn_matches))["treat"]
1089.641 






# Zelig -------------------------------------------------------------------


# https://docs.zeligproject.org/articles/using_Zelig_with_MatchIt.html

# https://r.iq.harvard.edu/docs/matchit/2.4-15/Examples2.html





# When the best balance is achieved, we run the parametric analysis:

z.out <- zelig(re78 ~ treat + age + educ + black + hisp + nodegr + 
                 married + re74 + re75, 
               data = as.data.frame(match.data(nn)), 
               model = "ls")

z.out

# and then set the explanatory variables at their means (the default)
# and change the treatment variable from a 0 to a 1:

x.out <- setx(z.out, treat=0)
x1.out <- setx(z.out, treat=1)

# and finally compute the result and examine a summary:
s.out <- sim(z.out, x = x.out, x1 = x1.out)
summary(s.out)
# The summary gives us the expected and predicted values at each
# level of the predictor that we set, plus the first-differences
# (the difference in expected values at the 
# set levels of the treatment assignment variable)

# First differences:
# 557.6148 3191.303

plot(s.out)






# Same model, bootstrapped
z.out.boot <- zelig(re78 ~ treat + age + educ + black + hisp + nodegr + 
                      married + re74 + re75, 
                    data = as.data.frame(match.data(nn)), 
                    model = "ls",
                    bootstrap = 500)
z.out.boot
summary(z.out.boot)






# Without extra covariate adjustment:
z.out.treat <- zelig(re78 ~ treat, 
                    data = as.data.frame(match.data(nn)), 
                    model = "ls")
z.out.treat
summary(z.out.treat)

# and then set the explanatory variables at their means (the default)
# and change the treatment variable from a 0 to a 1:

x.out.treat <- setx(z.out.treat, treat=0)
x1.out.treat <- setx(z.out.treat, treat=1)

# and finally compute the result and examine a summary:
s.out.treat <- sim(z.out.treat, x = x.out.treat, x1 = x1.out.treat)
summary(s.out.treat)
















z.out1 <- zelig(re78 ~ age + educ + black + hisp + nodegr + 
                  married + re74 + re75, data = match.data(nn, "control"), 
                model = "ls")

x.out1 <- setx(z.out1, data = match.data(m.out1, "treat"), cond = TRUE)
s.out1 <- sim(z.out1, x = x.out1)

# Finally, we obtain a summary of the results by
summary(s.out1)

plot(s.out1)









# Zelig Poisson ATT -------------------------------------------------------



data(sanction)
sanction
zqi.out <- zelig(num ~ target + coop + mil, 
                 model = "poisson", data = sanction, cite = FALSE)
# find the ATT where the treatment is mil = 1
z.att <- zqi.out %>%
  ATT(treatment = "mil", treat = 1) %>% 
  get_qi(qi = "ATT", xvalue = "TE")

z.att

plot(z.att)

plot(zqi.out)


# summarize the results
hist(z.att, 
     main = NULL,
     xlab ="Averege Treatment Effect of mil on the Treated")








# Zelig examples ----------------------------------------------------------



# Zelig:

#https://r.iq.harvard.edu/docs/matchit/2.4-15/Examples2.html


# We now give four examples using the Lalonde data. 
# They are meant to be read sequentially. You can run these 
# example commands by typing demo(analysis). Although we use 
# the linear least squares model in these examples, a wide 
# range of other models are available in Zelig 
# (for the list of supported models, see 
#   http://gking.harvard.edu/zelig/docs/Models_Zelig_Can.html.
# To load the Zelig package after installing it, type

library(Zelig)




# Model-Based Estimates
# In our first example, we conduct a standard parametric analysis and 
# compute quantities of interest in the most common way. 
# We begin with nearest neighbor matching with a logistic 
# regression-based propensity score, discarding control
# units outside the convex hull of the treated units (King and Zeng, 2007; King and Zeng, 2006):

m.out <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
                   method = "nearest", discard = "hull.control", data = lalonde)

# Then we check balance using the summary and plot procedures (which we don't show here). 

# When the best balance is achieved, we run the parametric analysis:

z.out <- zelig(re78 ~ treat + age + educ + black + hispan + nodegree + 
                      married + re74 + re75, data = match.data(m.out), 
                      model = "ls")

# and then set the explanatory variables at their means (the default)
# and change the treatment variable from a 0 to a 1:

x.out <- setx(z.out, treat=0)
x1.out <- setx(z.out, treat=1)

# and finally compute the result and examine a summary:
s.out <- sim(z.out, x = x.out, x1 = x1.out)
summary(s.out)







# Average Treatment Effect on the Treated
# We illustrate now how to estimate the average treatment effect on the treated 
# in a way that is quite robust. We do this by estimating the coefficients 
# in the control group alone.
# We begin by conducting nearest neighbor matching with a logistic 
# regression-based propensity score:

m.out1 <- matchit(treat ~ age + educ + black + hispan + nodegree + 
                    married + re74 + re75, method = "nearest", data = lalonde)

# Then we check balance using the summary and plot procedures 
# (which we don't show here). We re-estimate the matching procedure
# until we achieve the best balance possible. (The running examples 
# here are meant merely to illustrate, not to suggest that we've achieved 
# the best balance.) Then we go to Zelig, and in this case choose to fit 
# a linear least squares model to the control group only:

z.out1 <- zelig(re78 ~ age + educ + black + hispan + nodegree + 
                      married + re74 + re75, data = match.data(m.out1, "control"), 
                      model = "ls")

# where the "control" option in match.data() extracts only the matched control
# units and ls specifies least squares regression. In a smaller data set, 
# this example should probably be changed to include all the data in this 
# estimation (using data = match.data(m.out1) for the data) and by including
# the treatment indicator (which is excluded in the example since its a 
# constant in the control group.) Next, we use the coefficients estimated
# in this way from the control group, and combine them with the values of
# the covariates set to the values of the treated units. 
# We do this by choosing conditional prediction 
# (which means use the observed values) in setx(). 
# The sim() command does the imputation.

x.out1 <- setx(z.out1, data = match.data(m.out1, "treat"), cond = TRUE)
s.out1 <- sim(z.out1, x = x.out1)

# Finally, we obtain a summary of the results by
summary(s.out1)



# Average Treatment Effect (Overall)
# To estimate the average treatment effect, we continue with the previous
# example and fit the linear model to the treatment group:

z.out2 <- zelig(re78 ~ age + educ + black + hispan + nodegree + 
                      married + re74 + re75, data = match.data(m.out1, "treat"), 
                      model = "ls")

# We then conduct the same simulation procedure in order to impute the 
# counterfactual outcome for the control group

x.out2 <- setx(z.out2, data = match.data(m.out1, "control"), cond = TRUE)
s.out2 <- sim(z.out2, x = x.out2)

# In this calculation, Zelig is computing the difference between observed
# and the expected values. This means that the treatment effect for the 
# control units is the effect of control (observed control outcome minus
# the imputed outcome under treatment from the model). Hence, to combine 
# treatment effects just reverse the signs of the estimated treatment 
# effect of controls.

ate.all <- c(s.out1$qi$att.ev, -s.out2$qi$att.ev)

# The point estimate, its standard error, and the $ 95\%$ confidence interval is given by
mean(ate.all)
sd(ate.all)
quantile(ate.all, c(0.025, 0.975))




# Subclassification
# In subclassification, the average treatment effect estimates are obtained 
# separately for each subclass, and then aggregated for an overall estimate. 
# Estimating the treatment effects separately for each subclass,
# and then aggregating across subclasses, can increase the robustness of the
# ultimate results since the parametric analysis within each subclass requires
# only local rather than global assumptions. However, fewer observations are
# obviously available within each subclass, and so this option is normally 
# chosen for larger data sets.

# We begin this example by conducting subclassification with four subclasses,

m.out2 <- matchit(treat ~ age + educ + black + hispan + nodegree + 
                        married + re74 + re75, data = lalonde, 
                        method = "subclass", subclass = 4)

# When balance is as good as we can get it, we then fit a linear regression 
# within each subclass by controlling for the estimated propensity score 
# (called distance) and other covariates. In most software, this would 
# involve running four separate regressions and then combining the results.
# In Zelig, however, all we need to do is to use the by option:

z.out3 <- zelig(re78 ~ re74 + re75 + distance, 
                      data = match.data(m.out2, "control"), 
                      model = "ls", by = "subclass")

# The same set of commands as in the first example are used to do the
# imputation of the counterfactual outcomes for the treated units:

x.out3 <- setx(z.out3, data = match.data(m.out2, "treat"), fn = NULL, 
                     cond = TRUE)

s.out3 <- sim(z.out3, x = x.out3)

summary(s.out3)

# It is also possible to get the summary result for each subclass. 
# For example, the following command summarizes the result for the second subclass.

summary(s.out3, subset = 2)

# How Adjustment After Exact Matching Has No Effect
# Regression adjustment after exact one-to-one exact matching gives the 
# identical answer as a simple, unadjusted difference in means. 
# General exact matching, as implemented in MatchIt, 
# allows one-to-many matches, so to see the same result 
# we must weight when adjusting. 
# In other words: 
# weighted regression adjustment after general exact matching gives the 
# identical answer as a simple, unadjusted weighted difference in means. 
# For example:

m.out <- matchit(treat ~ educ + black + hispan, data = lalonde, 
                       method = "exact")
m.data <- match.data(m.out)

## weighted diff in means 
weighted.mean(mdata$re78[mdata$treat == 1], mdata$weights[mdata$treat==1]) 
            - weighted.mean(mdata$re78[mdata$treat==0], mdata$weights[mdata$treat==0])

## weighted least squares without covariates
zelig(re78 ~ treat, data = m.data, model = "ls", weights = "weights")

## weighted least squares with covariates
zelig(re78 ~ treat + black + hispan + educ, data = m.data, model = "ls", 
            weights = "weights")


