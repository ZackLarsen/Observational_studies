##################################################
## Project:
## Script purpose:
## Date: `r paste(date)`
## Author: Zack Larsen
##################################################

# Econometric sense -------------------------------------------------------


https://econometricsense.blogspot.com/2015/03/using-r-matchit-package-for-propensity.html

Saturday, March 28, 2015
Using the R MatchIt package for propensity score analysis
Descriptive analysis between treatment and control groups can reveal interesting patterns or relationships, but we cannot always take descriptive statistics at face value. Regression and matching methods allow us to make controlled comparisons to reduce selection bias in observational studies.
For a couple good references that I am basically tracking in this post see here and here. These are links to the pages of the package authors and a nice paper (A Step by Step Guide to Propensity Score Matching in R) from higher education evaluation research respectively.

In both Mostly Harmless Econometrics and Mastering Metrics Angrist and Pischke discuss the similarities between matching and regression. From MM:
  
  "Specifically, regression estimates are weighted averages of multiple matched comparisons"

In this post I borrow from some of the previous references, and try to follow closely the dialogue in chapter 3 of MHE. So, conveniently the R matchit propensity score matching package comes with a subset of the Lalonde data set referenced in MHE. Based on descriptives, it looks like this data matches columns (1) and (4) in table 3.3.2. The Lalonde data set basically consists of a treatment variable indicator, an outcome re78 or real earnings in 1978 as well as other data that can be used for controls. (see previous links above for more details). If we use regression to look at basic uncontrolled raw differences between treatment and control groups, it appears that the treatment (a job training program) produces negative results (on the order of $635):
  
  R code:
  summary(lm(re78~treat,data=lalonde))

Output:
  Coefficients:
  Estimate Std. Error t value            Pr(>|t|)   
(Intercept)     6984        361   19.36 <0.0000000000000002 ***
  treat           -635  <---      657   -0.97                0.33 

Once we implement matching in R, the output provides comparisons between the balance in covariates for the treatment and control groups before and after matching. Matching is based on propensity scores estimated with logistic regression. (see previous post on propensity score analysis for further details). The output below indicates that the propensity score matching creates balance among covariates/controls as if we were explicitly trying to match on the controls themselves.

R Code:
  m.out1 <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, data = lalonde, method = "nearest", distance = "logit")

Output:
  Summary of balance for all data:
  Means Treated Means Control SD Control  Mean Diff   eQQ Med  eQQ Mean   eQQ Max
distance        0.5774        0.1822     0.2295     0.3952    0.5176    0.3955    0.5966
age            25.8162       28.0303    10.7867    -2.2141    1.0000    3.2649   10.0000
educ           10.3459       10.2354     2.8552     0.1105    1.0000    0.7027    4.0000
black           0.8432        0.2028     0.4026     0.6404    1.0000    0.6432    1.0000
hispan          0.0595        0.1422     0.3497    -0.0827    0.0000    0.0811    1.0000
nodegree        0.7081        0.5967     0.4911     0.1114    0.0000    0.1135    1.0000
married         0.1892        0.5128     0.5004    -0.3236    0.0000    0.3243    1.0000
re74         2095.5737     5619.2365  6788.7508 -3523.6628 2425.5720 3620.9240 9216.5000
re75         1532.0553     2466.4844  3291.9962  -934.4291  981.0968 1060.6582 6795.0100

Summary of balance for matched data:
  Means Treated Means Control SD Control Mean Diff  eQQ Med eQQ Mean    eQQ Max
distance        0.5774        0.3629     0.2533    0.2145   0.1646   0.2146     0.4492
age            25.8162       25.3027    10.5864    0.5135   3.0000   3.3892     9.0000
educ           10.3459       10.6054     2.6582   -0.2595   0.0000   0.4541     3.0000
black           0.8432        0.4703     0.5005    0.3730   0.0000   0.3730     1.0000
hispan          0.0595        0.2162     0.4128   -0.1568   0.0000   0.1568     1.0000
nodegree        0.7081        0.6378     0.4819    0.0703   0.0000   0.0703     1.0000
married         0.1892        0.2108     0.4090   -0.0216   0.0000   0.0216     1.0000
re74         2095.5737     2342.1076  4238.9757 -246.5339 131.2709 545.1182 13121.7500
re75         1532.0553     1614.7451  2632.3533  -82.6898 152.1774 349.5371 11365.7100

Estimation of treatment effects can be obtained via paired or matched comparisons (Lanehart et al, 2012; Austin, 2010-see previous posts here and here)

Rcode:
  t.test(m.data1$re78[m.data1$treat==1],m.data1$re78[m.data1$treat==0],paired=TRUE)

Output:
  t = 1.2043, df = 184, p-value = 0.23
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -579.6904 2396.0948
sample estimates:
  mean of the differences
908.2022 <---
  
  This indicates an estimated treatment effect of about $900.00, which is quite a reversal from the raw uncontrolled/unmatched comparisons. In Mostly Harmless Econometrics, as part of the dialogue relating regression to matching, Angrist and Pischke present results in table 3.3.3 for regressions utilizing data that has been 'screened' by eliminating observations where ps > .90 or < .10. Similar results were obtained in R below:
  
  Rcode:
  summary(lm(re78~treat + age + educ + black + hispan + nodegree + married + re74 + re75, data = m.data3))

Output:
  Coefficients:
  Estimate Std. Error t value Pr(>|t|) 
(Intercept)    23.6095  3920.6599    0.01    0.995 
treat        1229.8619 <---  849.1439    1.45    0.148 
age            -3.4488    45.4815   -0.08    0.940 
educ          506.9958   240.1989    2.11    0.036 *
  black       -1030.4883  1255.1766   -0.82    0.412 
hispan        926.5288  1498.4450    0.62    0.537

Again we get a reversal of the values from the raw comparisons, and a much larger estimated treatment effect than the non-paramterically matched comparisons above.

Note: Slightly different results were obtained from A&P, partly because I am not sure the exact specification of their ps model, which may have impacted the screening and ultimately differences in the data used.  Full R code is provided below with the ps model specification. Also, I have replicated similar results in SAS using the Mayo Clinic %gmatch macro as well as using approaches outlined by Lanehart et al (2012).  These results may be shared in a later post or white paper. 

See also: A Toy Instrumental Variable Application

R Program:
  
  # *------------------------------------------------------------------
# | PROGRAM NAME: ex ps match mostly harmless R
# | DATE: 3/24/15 
# | CREATED BY: MATT BOGARD
# | PROJECT FILE: Tools and References/Rcode             
# *----------------------------------------------------------------
# | PURPOSE: Use R matchit and glm to mimic the conversation in 
# | chapter 3 of Mostly Harmless Econometrics              
# | NOTE: because of random sorting within matching application different results may be
# | obtained with each iteration of matchit in R
# *------------------------------------------------------------------

rm(list=ls()) # get rid of any existing data 

ls() # view open data sets

library(MatchIt) # load matching package

options("scipen" =100, "digits" = 4) # override R's tendency to use scientific notation

# raw differences in means for treatments and controls using regression

summary(lm(re78~treat,data=lalonde))

t.test(lalonde$re78[lalonde$treat==1],lalonde$re78[lalonde$treat==0],paired=FALSE)

#-----------------------------------------------------
# nearest neighbor matching via MatchIt
#-----------------------------------------------------

# estimate propensity scores and create matched data set using 'matchit' and lalonde data


m.out1 <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, data = lalonde, method = "nearest", distance = "logit")

summary(m.out1) # check balance

m.data1 <- match.data(m.out1,distance ="pscore") # create ps matched data set from previous output

hist(m.data1$pscore) # distribution of propenisty scores
summary(m.data1$pscore)

#-----------------------------------------------------
# perform paired t-tests on matched data
#-----------------------------------------------------

t.test(m.data1$re78[m.data1$treat==1],m.data1$re78[m.data1$treat==0],paired=TRUE)

#-----------------------------------------------------------------------------------------
# lets look at regression on the ps restricted data on the entire sample per MHE chapter 3
#-----------------------------------------------------------------------------------------

m.data2 <- lalonde # copy lalonde for ps estimation
dim(m.data2)

# generate propensity scores for all of the data in Lalonde 
ps.model <- glm(treat ~ age + educ + black + hispan + nodegree + married + re74 +  
                  re75, data = m.data2, family=binomial(link="logit"),
                na.action=na.pass)

summary(ps.model)

# add pscores to study data
m.data2$pscore <- predict(ps.model, newdata = m.data2, type = "response")

hist(m.data2$pscore) # distribution of ps
summary(m.data2$pscore)
dim(m.data2)

# restrict data to ps range .10 <= ps <= .90

m.data3 <- m.data2[m.data2$pscore >= .10 & m.data2$pscore <=.90,]
summary(m.data3$pscore)

# regression with controls on propensity score screened data set
summary(lm(re78~treat + age + educ + black + hispan + nodegree + married + re74 + re75, data = m.data3))

# unrestricted regression with controls
summary(lm(re78~treat + age + educ + black + hispan + nodegree + married + re74 + re75, data = lalonde))



