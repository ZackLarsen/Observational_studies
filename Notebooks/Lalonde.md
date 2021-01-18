Lalonde
================

## Data io

``` r
data(lalonde)
```

## Lalonde EDA pt. 1

``` r
summary(lalonde)
```

    ##      treat             age             educ           black       
    ##  Min.   :0.0000   Min.   :16.00   Min.   : 0.00   Min.   :0.0000  
    ##  1st Qu.:0.0000   1st Qu.:20.00   1st Qu.: 9.00   1st Qu.:0.0000  
    ##  Median :0.0000   Median :25.00   Median :11.00   Median :0.0000  
    ##  Mean   :0.3013   Mean   :27.36   Mean   :10.27   Mean   :0.3958  
    ##  3rd Qu.:1.0000   3rd Qu.:32.00   3rd Qu.:12.00   3rd Qu.:1.0000  
    ##  Max.   :1.0000   Max.   :55.00   Max.   :18.00   Max.   :1.0000  
    ##      hispan          married          nodegree           re74      
    ##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :    0  
    ##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:    0  
    ##  Median :0.0000   Median :0.0000   Median :1.0000   Median : 1042  
    ##  Mean   :0.1173   Mean   :0.4153   Mean   :0.6303   Mean   : 4558  
    ##  3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.: 7888  
    ##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :35040  
    ##       re75              re78        
    ##  Min.   :    0.0   Min.   :    0.0  
    ##  1st Qu.:    0.0   1st Qu.:  238.3  
    ##  Median :  601.5   Median : 4759.0  
    ##  Mean   : 2184.9   Mean   : 6792.8  
    ##  3rd Qu.: 3249.0   3rd Qu.:10893.6  
    ##  Max.   :25142.2   Max.   :60307.9

``` r
skim(lalonde)
```

<table style='width: auto;'
        class='table table-condensed'>

<caption>

Data summary

</caption>

<thead>

<tr>

<th style="text-align:left;">

</th>

<th style="text-align:left;">

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

Name

</td>

<td style="text-align:left;">

lalonde

</td>

</tr>

<tr>

<td style="text-align:left;">

Number of rows

</td>

<td style="text-align:left;">

614

</td>

</tr>

<tr>

<td style="text-align:left;">

Number of columns

</td>

<td style="text-align:left;">

10

</td>

</tr>

<tr>

<td style="text-align:left;">

\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Column type frequency:

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

numeric

</td>

<td style="text-align:left;">

10

</td>

</tr>

<tr>

<td style="text-align:left;">

\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

</td>

<td style="text-align:left;">

</td>

</tr>

<tr>

<td style="text-align:left;">

Group variables

</td>

<td style="text-align:left;">

None

</td>

</tr>

</tbody>

</table>

**Variable type: numeric**

<table>

<thead>

<tr>

<th style="text-align:left;">

skim\_variable

</th>

<th style="text-align:right;">

n\_missing

</th>

<th style="text-align:right;">

complete\_rate

</th>

<th style="text-align:right;">

mean

</th>

<th style="text-align:right;">

sd

</th>

<th style="text-align:right;">

p0

</th>

<th style="text-align:right;">

p25

</th>

<th style="text-align:right;">

p50

</th>

<th style="text-align:right;">

p75

</th>

<th style="text-align:right;">

p100

</th>

<th style="text-align:left;">

hist

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

treat

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.30

</td>

<td style="text-align:right;">

0.46

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:left;">

▇▁▁▁▃

</td>

</tr>

<tr>

<td style="text-align:left;">

age

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

27.36

</td>

<td style="text-align:right;">

9.88

</td>

<td style="text-align:right;">

16

</td>

<td style="text-align:right;">

20.00

</td>

<td style="text-align:right;">

25.00

</td>

<td style="text-align:right;">

32.00

</td>

<td style="text-align:right;">

55.00

</td>

<td style="text-align:left;">

▇▅▂▂▁

</td>

</tr>

<tr>

<td style="text-align:left;">

educ

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

10.27

</td>

<td style="text-align:right;">

2.63

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

9.00

</td>

<td style="text-align:right;">

11.00

</td>

<td style="text-align:right;">

12.00

</td>

<td style="text-align:right;">

18.00

</td>

<td style="text-align:left;">

▁▂▆▇▁

</td>

</tr>

<tr>

<td style="text-align:left;">

black

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.40

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:left;">

▇▁▁▁▅

</td>

</tr>

<tr>

<td style="text-align:left;">

hispan

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.12

</td>

<td style="text-align:right;">

0.32

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:left;">

▇▁▁▁▁

</td>

</tr>

<tr>

<td style="text-align:left;">

married

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.42

</td>

<td style="text-align:right;">

0.49

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:left;">

▇▁▁▁▆

</td>

</tr>

<tr>

<td style="text-align:left;">

nodegree

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

0.63

</td>

<td style="text-align:right;">

0.48

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:right;">

1.00

</td>

<td style="text-align:left;">

▅▁▁▁▇

</td>

</tr>

<tr>

<td style="text-align:left;">

re74

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

4557.55

</td>

<td style="text-align:right;">

6477.96

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

1042.33

</td>

<td style="text-align:right;">

7888.50

</td>

<td style="text-align:right;">

35040.07

</td>

<td style="text-align:left;">

▇▂▁▁▁

</td>

</tr>

<tr>

<td style="text-align:left;">

re75

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

2184.94

</td>

<td style="text-align:right;">

3295.68

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

0.00

</td>

<td style="text-align:right;">

601.55

</td>

<td style="text-align:right;">

3248.99

</td>

<td style="text-align:right;">

25142.24

</td>

<td style="text-align:left;">

▇▁▁▁▁

</td>

</tr>

<tr>

<td style="text-align:left;">

re78

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

6792.83

</td>

<td style="text-align:right;">

7470.73

</td>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

238.28

</td>

<td style="text-align:right;">

4759.02

</td>

<td style="text-align:right;">

10893.59

</td>

<td style="text-align:right;">

60307.93

</td>

<td style="text-align:left;">

▇▂▁▁▁

</td>

</tr>

</tbody>

</table>

``` r
df_status(lalonde)
```

    ##    variable q_zeros p_zeros q_na p_na q_inf p_inf    type unique
    ## 1     treat     429   69.87    0    0     0     0 integer      2
    ## 2       age       0    0.00    0    0     0     0 integer     40
    ## 3      educ       3    0.49    0    0     0     0 integer     19
    ## 4     black     371   60.42    0    0     0     0 integer      2
    ## 5    hispan     542   88.27    0    0     0     0 integer      2
    ## 6   married     359   58.47    0    0     0     0 integer      2
    ## 7  nodegree     227   36.97    0    0     0     0 integer      2
    ## 8      re74     243   39.58    0    0     0     0 numeric    358
    ## 9      re75     245   39.90    0    0     0     0 numeric    356
    ## 10     re78     143   23.29    0    0     0     0 numeric    457

## Lalonde EDA, pt. 2

``` r
lalonde %>% 
  group_by(treat) %>% 
  count(sort = TRUE, wt = age, name = "subjects")
```

    ## # A tibble: 2 x 2
    ## # Groups:   treat [2]
    ##   treat subjects
    ##   <int>    <int>
    ## 1     0    12025
    ## 2     1     4776

``` r
lalonde %>% 
  group_by(treat) %>% 
  count(decile = 10 * (age %/% 10))
```

    ## # A tibble: 9 x 3
    ## # Groups:   treat [2]
    ##   treat decile     n
    ##   <int>  <dbl> <int>
    ## 1     0     10   115
    ## 2     0     20   166
    ## 3     0     30    71
    ## 4     0     40    51
    ## 5     0     50    26
    ## 6     1     10    38
    ## 7     1     20   108
    ## 8     1     30    24
    ## 9     1     40    15

## Lalonde EDA, pt. 3

``` r
summary(lalonde$re78)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     0.0   238.3  4759.0  6792.8 10893.6 60307.9

``` r
describe(lalonde$re78)
```

    ## lalonde$re78 
    ##        n  missing distinct     Info     Mean      Gmd      .05      .10 
    ##      614        0      457    0.987     6793     7807      0.0      0.0 
    ##      .25      .50      .75      .90      .95 
    ##    238.3   4759.0  10893.6  17800.1  21691.8 
    ## 
    ## lowest :     0.00000    31.03226    33.98771    54.67588    94.57450
    ## highest: 26372.28000 26817.60000 34099.28000 36646.95000 60307.93000

``` r
hist(lalonde$re78, )
```

![](Lalonde_files/figure-gfm/eda3-1.png)<!-- -->

``` r
hist(log(lalonde$re78))
```

![](Lalonde_files/figure-gfm/eda3-2.png)<!-- -->

## Distribution Fitting

``` r
rgamma(100,2,11)
```

    ##   [1] 0.271000363 0.074853333 0.067171978 0.451041096 0.317903592 0.311172210
    ##   [7] 0.565519246 0.306116987 0.251879485 0.186329118 0.072198058 0.510544431
    ##  [13] 0.377408632 0.165204725 0.178883943 0.065924985 0.070228140 0.197006248
    ##  [19] 0.036262816 0.243034653 0.133258641 0.097906069 0.212007424 0.396288040
    ##  [25] 0.417738827 0.101071986 0.148136494 0.247201136 0.231103717 0.323920245
    ##  [31] 0.154293402 0.096522121 0.136233232 0.033502900 0.185310283 0.531883975
    ##  [37] 0.215201692 0.526886871 0.085307076 0.225147073 0.211388712 0.232337003
    ##  [43] 0.050577607 0.337005358 0.101209903 0.111540108 0.044954816 0.399030330
    ##  [49] 0.053348543 0.145246129 0.008357667 0.256365622 0.238819097 0.448762907
    ##  [55] 0.248160160 0.158080525 0.105187914 0.131486324 0.020172599 0.424904716
    ##  [61] 0.036736755 0.126171624 0.181207781 0.086800952 0.098360571 0.526442104
    ##  [67] 0.138589832 0.230844746 0.044525265 0.414276236 0.130649636 0.145114670
    ##  [73] 0.347356274 0.247881839 0.116533326 0.443920335 0.051698061 0.069521054
    ##  [79] 0.220130985 0.420607414 0.049704048 0.013903069 0.052901533 0.337733578
    ##  [85] 0.174617469 0.304948968 0.160655205 0.129432284 0.158413201 0.129855725
    ##  [91] 0.150241629 0.068261093 0.096343530 0.070768210 0.084504501 0.322515352
    ##  [97] 0.405956636 0.121424880 0.058432813 0.091220928

``` r
rnorm(100,5,.01)
```

    ##   [1] 5.003497 4.991681 5.004970 4.991794 5.014865 4.998047 4.991034 5.004079
    ##   [9] 5.016878 5.008710 5.001067 4.999301 5.012737 5.013834 4.976757 5.004525
    ##  [17] 4.994585 5.012747 5.001750 5.006378 5.002604 4.990882 4.996349 5.000061
    ##  [25] 4.983337 5.018076 5.012398 4.983502 5.005733 4.996382 4.997649 4.979476
    ##  [33] 4.997726 4.977838 5.000684 5.002316 4.988167 5.006600 5.006043 4.992347
    ##  [41] 5.004628 5.000701 4.988140 5.004362 4.992951 5.002947 5.002059 5.004466
    ##  [49] 5.003822 5.000335 5.000203 4.980479 5.006906 4.999294 5.007704 5.006235
    ##  [57] 5.012758 5.022418 5.002190 4.997490 5.018566 5.002784 5.004571 5.004050
    ##  [65] 5.007795 4.982974 5.003426 5.000727 5.000631 5.012801 4.975933 5.000767
    ##  [73] 4.986195 5.027828 4.982038 5.011472 5.005147 5.001743 5.000532 5.000919
    ##  [81] 5.015817 5.013211 5.008897 5.001090 5.002921 4.997264 4.995642 4.989397
    ##  [89] 4.985361 4.995551 4.997818 4.982182 4.996592 4.987977 4.989475 5.005679
    ##  [97] 5.009534 5.013038 5.005170 4.998338

``` r
x <- rgamma(10000,2,11) + rnorm(10000,5,.01)
fit.gamma <- fitdist(x, distr = "gamma", method = "mle")
summary(fit.gamma)
```

    ## Fitting of the distribution ' gamma ' by maximum likelihood 
    ## Parameters : 
    ##        estimate Std. Error
    ## shape 1639.8984  23.185518
    ## rate   316.3987   4.474049
    ## Loglikelihood:  6371.259   AIC:  -12738.52   BIC:  -12724.1 
    ## Correlation matrix:
    ##           shape      rate
    ## shape 1.0000000 0.9998475
    ## rate  0.9998475 1.0000000

``` r
plot(fit.gamma)
```

![](Lalonde_files/figure-gfm/fitdistr-1.png)<!-- -->

``` r
# lalonde_gamma <- fitdist(lalonde$re78 + 1, distr = "gamma", method = "mle")
# summary(lalonde_gamma)
# plot(lalonde_gamma)
```

## Matching

``` r
match_1 <- matchit(formula = treat ~ re78, 
        method = "nearest",
        data = lalonde,
        discard = "both", 
        replace = FALSE, 
        #caliper = c(),
        #std.caliper = c(),
        ratio = 1)

match_1
```

    ## A matchit object
    ##  - method: 1:1 nearest neighbor matching without replacement
    ##  - distance: Propensity score [common support]
    ##              - estimated with logistic regression
    ##  - common support: units from both groups dropped
    ##  - number of obs.: 614 (original), 360 (matched)
    ##  - target estimand: ATT
    ##  - covariates: re78

``` r
summary(match_1)
```

    ## 
    ## Call:
    ## matchit(formula = treat ~ re78, data = lalonde, method = "nearest", 
    ##     discard = "both", replace = FALSE, ratio = 1)
    ## 
    ## Summary of Balance for All Data:
    ##          Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean
    ## distance        0.3024        0.3008          0.0881     1.0875    0.0355
    ## re78         6349.1435     6984.1697         -0.0807     1.1634    0.0355
    ##          eCDF Max
    ## distance   0.0986
    ## re78       0.0986
    ## 
    ## 
    ## Summary of Balance for Matched Data:
    ##          Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean
    ## distance        0.3044        0.3043          0.0017     1.0006    0.0023
    ## re78         5501.9306     5514.3993         -0.0016     1.0008    0.0023
    ##          eCDF Max Std. Pair Dist.
    ## distance   0.0167          0.0058
    ## re78       0.0167          0.0056
    ## 
    ## Percent Balance Improvement:
    ##          Std. Mean Diff. Var. Ratio eCDF Mean eCDF Max
    ## distance            98.1       99.3      93.6     83.1
    ## re78                98.0       99.5      93.6     83.1
    ## 
    ## Sample Sizes:
    ##           Control Treated
    ## All           429     185
    ## Matched       180     180
    ## Unmatched     249       0
    ## Discarded       0       5

## Linear Modeling

#### Assumptions:

  - Linearity
  - Homoscedasticity
  - Independence
  - Normality

<http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/>

<https://data.library.virginia.edu/diagnostic-plots/>

``` r
plot(lalonde$re78)
```

![](Lalonde_files/figure-gfm/lin_mod-1.png)<!-- -->

``` r
# Pre-match data:
prematch_lm <- lm(re78 ~ treat, data = lalonde)

# Matched data:
matched_lm <- lm(re78 ~ treat, data = match.data(match_1), weights = weights)



autoplot(prematch_lm) +
  #theme_minimal() + 
  theme_ft_rc() + 
  labs (title = "Model 1 Diagnostic Plots")
```

    ## Warning: `arrange_()` is deprecated as of dplyr 0.7.0.
    ## Please use `arrange()` instead.
    ## See vignette('programming') for more help
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

![](Lalonde_files/figure-gfm/lin_mod-2.png)<!-- -->

# Generalized Linear Modeling

``` r
# Get help page:
?family


# Gaussian-distributed residuals and identity link = OLS:
glm(re78 ~ treat, data = lalonde, family=gaussian(link = "identity"))
```

    ## 
    ## Call:  glm(formula = re78 ~ treat, family = gaussian(link = "identity"), 
    ##     data = lalonde)
    ## 
    ## Coefficients:
    ## (Intercept)        treat  
    ##        6984         -635  
    ## 
    ## Degrees of Freedom: 613 Total (i.e. Null);  612 Residual
    ## Null Deviance:       3.421e+10 
    ## Residual Deviance: 3.416e+10     AIC: 12700

``` r
# Gamma default link is "inverse"
glm(re78 + 1 ~ treat, data = lalonde, family=Gamma(link = "inverse"))
```

    ## 
    ## Call:  glm(formula = re78 + 1 ~ treat, family = Gamma(link = "inverse"), 
    ##     data = lalonde)
    ## 
    ## Coefficients:
    ## (Intercept)        treat  
    ##   1.432e-04    1.432e-05  
    ## 
    ## Degrees of Freedom: 613 Total (i.e. Null);  612 Residual
    ## Null Deviance:       2724 
    ## Residual Deviance: 2722  AIC: 11140

``` r
# Using log link
glm(re78 + 1 ~ treat, data = lalonde, family=Gamma(link = "log"))
```

    ## 
    ## Call:  glm(formula = re78 + 1 ~ treat, family = Gamma(link = "log"), 
    ##     data = lalonde)
    ## 
    ## Coefficients:
    ## (Intercept)        treat  
    ##     8.85154     -0.09531  
    ## 
    ## Degrees of Freedom: 613 Total (i.e. Null);  612 Residual
    ## Null Deviance:       2724 
    ## Residual Deviance: 2722  AIC: 11140

## Uncertainty / confidence intervals

<https://moderndive.com/10-inference-for-regression.html>

## Logistic GLM

``` r
# Logistic regression is binomial with "logit" link
logistic_mod <- glm(treat ~ re74 + re75, data = lalonde, family = binomial(link="logit"))
# Output of above will be log-odds in the "estimate" column
# By exponentiating the coefficients, we can get the odds
exp(logistic_mod$coef)
```

    ## (Intercept)        re74        re75 
    ##   0.6455863   0.9998713   1.0000247

``` r
summary(logistic_mod)
```

    ## 
    ## Call:
    ## glm(formula = treat ~ re74 + re75, family = binomial(link = "logit"), 
    ##     data = lalonde)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.0694  -0.9714  -0.6520   1.3680   3.0568  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.376e-01  1.100e-01  -3.978 6.95e-05 ***
    ## re74        -1.287e-04  2.532e-05  -5.084 3.69e-07 ***
    ## re75         2.472e-05  3.960e-05   0.624    0.533    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 751.49  on 613  degrees of freedom
    ## Residual deviance: 704.15  on 611  degrees of freedom
    ## AIC: 710.15
    ## 
    ## Number of Fisher Scoring iterations: 5

``` r
confint(logistic_mod)
```

    ## Waiting for profiling to be done...

    ##                     2.5 %        97.5 %
    ## (Intercept) -6.545392e-01 -0.2229904520
    ## re74        -1.813279e-04 -0.0000818306
    ## re75        -5.496683e-05  0.0001009886
