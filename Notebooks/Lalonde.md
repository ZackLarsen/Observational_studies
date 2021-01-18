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

``` r
ggplot(lalonde, aes(x=age)) + 
  geom_histogram(binwidth=2,colour="white",fill="skyblue") +
  facet_grid(. ~ treat)
```

![](Lalonde_files/figure-gfm/eda3-3.png)<!-- -->

## Distribution Fitting

``` r
hist(rgamma(100,2,11))
```

![](Lalonde_files/figure-gfm/fitdistr-1.png)<!-- -->

``` r
hist(rnorm(100,5,.01))
```

![](Lalonde_files/figure-gfm/fitdistr-2.png)<!-- -->

``` r
x <- rgamma(10000,2,11) + rnorm(10000,5,.01)
fit.gamma <- fitdist(x, distr = "gamma", method = "mle")
summary(fit.gamma)
```

    ## Fitting of the distribution ' gamma ' by maximum likelihood 
    ## Parameters : 
    ##        estimate Std. Error
    ## shape 1672.6215  23.652321
    ## rate   322.8423   4.565953
    ## Loglikelihood:  6473.97   AIC:  -12943.94   BIC:  -12929.52 
    ## Correlation matrix:
    ##           shape      rate
    ## shape 1.0000000 0.9998505
    ## rate  0.9998505 1.0000000

``` r
plot(fit.gamma)
```

![](Lalonde_files/figure-gfm/fitdistr-3.png)<!-- -->

``` r
# lalonde_gamma <- fitdist(lalonde$re78 + 1, distr = "gamma", method = "mle")
# summary(lalonde_gamma)
# plot(lalonde_gamma)
```

## Matching

#### Nearest neighbor 1:1 match

``` r
match_1 <- matchit(formula = treat ~ age + educ + black + hispan + married + 
                   nodegree + re74 + re75, 
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
    ##  - number of obs.: 614 (original), 354 (matched)
    ##  - target estimand: ATT
    ##  - covariates: age, educ, black, hispan, married, nodegree, re74, re75

``` r
summary(match_1, subclass = TRUE, un = FALSE)
```

    ## 
    ## Call:
    ## matchit(formula = treat ~ age + educ + black + hispan + married + 
    ##     nodegree + re74 + re75, data = lalonde, method = "nearest", 
    ##     discard = "both", replace = FALSE, ratio = 1)
    ## 
    ## Summary of Balance for Matched Data:
    ##          Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean
    ## distance        0.5669        0.3750          0.8715     0.7562    0.1139
    ## age            25.4463       25.2316          0.0300     0.4336    0.0870
    ## educ           10.3220       10.5876         -0.1321     0.6076    0.0235
    ## black           0.8362        0.4915          0.9479          .    0.3446
    ## hispan          0.0621        0.2203         -0.6689          .    0.1582
    ## married         0.1977        0.2090         -0.0289          .    0.0113
    ## nodegree        0.6949        0.6384          0.1243          .    0.0565
    ## re74         2179.3904     2348.2864         -0.0346     1.3547    0.0423
    ## re75         1485.9177     1612.6659         -0.0394     1.4896    0.0491
    ##          eCDF Max Std. Pair Dist.
    ## distance   0.3955          0.8718
    ## age        0.2486          1.4087
    ## educ       0.0678          1.2392
    ## black      0.3446          0.9479
    ## hispan     0.1582          1.0512
    ## married    0.0113          0.8655
    ## nodegree   0.0565          0.8450
    ## re74       0.2542          0.7408
    ## re75       0.2034          0.7366
    ## 
    ## Sample Sizes:
    ##           Control Treated
    ## All           429     185
    ## Matched       177     177
    ## Unmatched     195       0
    ## Discarded      57       8

#### Match diagnostics

``` r
plot(summary(match_1))
```

![](Lalonde_files/figure-gfm/diagnostics-1.png)<!-- -->

``` r
love.plot(bal.tab(match_1), stars = "std")
```

![](Lalonde_files/figure-gfm/diagnostics-2.png)<!-- -->

``` r
# Examining distributional balance with plots:
bal.plot(match_1, var.name = "nodegree")
```

![](Lalonde_files/figure-gfm/diagnostics-3.png)<!-- -->

``` r
bal.plot(match_1, var.name = "distance", mirror = TRUE, type = "histogram")
```

![](Lalonde_files/figure-gfm/diagnostics-4.png)<!-- -->

``` r
plot(match_1, type = "jitter", interactive = FALSE)
```

![](Lalonde_files/figure-gfm/diagnostics-5.png)<!-- -->

``` r
plot(match_1, type = "qq", interactive = FALSE,
     which.xs = c("age", "married", "re75"))
```

![](Lalonde_files/figure-gfm/diagnostics-6.png)<!-- -->

``` r
#eCDF plot
plot(match_1, type = "ecdf", which.xs = c("educ", "married", "re75"))
```

![](Lalonde_files/figure-gfm/diagnostics-7.png)<!-- -->

## Matched data

``` r
match_1_data <- match.data(match_1)

head(match_1_data)
```

    ##   treat age educ black hispan married nodegree re74 re75       re78  distance
    ## 1     1  37   11     1      0       1        1    0    0  9930.0460 0.6387699
    ## 2     1  22    9     0      1       0        1    0    0  3595.8940 0.2246342
    ## 3     1  30   12     1      0       0        0    0    0 24909.4500 0.6782439
    ## 4     1  27   11     1      0       0        1    0    0  7506.1460 0.7763241
    ## 5     1  33    8     1      0       0        1    0    0   289.7899 0.7016387
    ## 6     1  22    9     1      0       0        1    0    0  4056.4940 0.6990699
    ##   weights subclass
    ## 1       1        1
    ## 2       1       94
    ## 3       1      105
    ## 4       1      116
    ## 5       1      127
    ## 6       1      138

#### Full matching (match 2)

From MatchIt reference:

Given the poor performance of nearest neighbor matching in this example,
we can try a different matching method or make other changes to the
matching algorithm or distance specification. Below, we’ll try full
matching, which matches every treated unit to at least one control and
every control to at least one treated unit (Hansen 2004; Stuart and
Green 2008). We’ll also try a different link (probit) for the propensity
score model.

``` r
match_2 <- matchit(formula = treat ~ age + educ + black + hispan + married + 
                   nodegree + re74 + re75, 
        method = "full",
        distance = "glm",
        link = "probit",
        data = lalonde,
        discard = "both"
        #replace = FALSE, 
        #caliper = c(),
        #std.caliper = c(),
        #ratio = 1
        )

match_2
```

    ## A matchit object
    ##  - method: Optimal full matching
    ##  - distance: Propensity score [common support]
    ##              - estimated with probit regression
    ##  - common support: units from both groups dropped
    ##  - number of obs.: 614 (original), 546 (matched)
    ##  - target estimand: ATT
    ##  - covariates: age, educ, black, hispan, married, nodegree, re74, re75

``` r
summary(match_2, subclass = TRUE, un = FALSE)
```

    ## 
    ## Call:
    ## matchit(formula = treat ~ age + educ + black + hispan + married + 
    ##     nodegree + re74 + re75, data = lalonde, method = "full", 
    ##     distance = "glm", link = "probit", discard = "both")
    ## 
    ## Summary of Balance for Matched Data:
    ##          Means Treated Means Control Std. Mean Diff. Var. Ratio eCDF Mean
    ## distance        0.5672        0.5673         -0.0003     0.9899    0.0040
    ## age            25.4463       25.8881         -0.0617     0.4301    0.0890
    ## educ           10.3220       10.3354         -0.0066     0.6197    0.0199
    ## black           0.8362        0.8316          0.0124          .    0.0045
    ## hispan          0.0621        0.0515          0.0449          .    0.0106
    ## married         0.1977        0.1647          0.0844          .    0.0331
    ## nodegree        0.6949        0.6784          0.0364          .    0.0166
    ## re74         2179.3904     2167.2675          0.0025     1.4077    0.0360
    ## re75         1485.9177     1461.8476          0.0075     1.5382    0.0405
    ##          eCDF Max Std. Pair Dist.
    ## distance   0.0565          0.0166
    ## age        0.2547          1.2411
    ## educ       0.0625          1.2022
    ## black      0.0045          0.0187
    ## hispan     0.0106          0.5753
    ## married    0.0331          0.5732
    ## nodegree   0.0166          0.9975
    ## re74       0.2331          0.7737
    ## re75       0.1848          0.8010
    ## 
    ## Sample Sizes:
    ##               Control Treated
    ## All            429.       185
    ## Matched (ESS)   60.69     177
    ## Matched        369.       177
    ## Unmatched        0.         0
    ## Discarded       60.         8

#### Match diagnostics (match 2)

``` r
plot(summary(match_2))
```

![](Lalonde_files/figure-gfm/diagnostics2-1.png)<!-- -->

``` r
love.plot(bal.tab(match_2), stars = "std")
```

![](Lalonde_files/figure-gfm/diagnostics2-2.png)<!-- -->

``` r
# Examining distributional balance with plots:
bal.plot(match_2, var.name = "nodegree")
```

![](Lalonde_files/figure-gfm/diagnostics2-3.png)<!-- -->

``` r
bal.plot(match_2, var.name = "distance", mirror = TRUE, type = "histogram")
```

![](Lalonde_files/figure-gfm/diagnostics2-4.png)<!-- -->

``` r
plot(match_2, type = "jitter", interactive = FALSE)
```

![](Lalonde_files/figure-gfm/diagnostics2-5.png)<!-- -->

``` r
plot(match_2, type = "qq", interactive = FALSE,
     which.xs = c("age", "married", "re75"))
```

![](Lalonde_files/figure-gfm/diagnostics2-6.png)<!-- -->

``` r
#eCDF plot
plot(match_2, type = "ecdf", which.xs = c("educ", "married", "re75"))
```

![](Lalonde_files/figure-gfm/diagnostics2-7.png)<!-- -->

#### Matched data (match 2)

``` r
match_2_data <- match.data(match_2)
```

## Estimating the Treatment Effect

<https://kosukeimai.github.io/MatchIt/articles/MatchIt.html>

How treatment effects are estimated depends on what form of matching was
performed. See vignette(“estimating-effects”) for information on the
variety of way to estimate effects and standard errors after each type
of matching and for several outcome types. After 1:1 matching without
replacement (i.e., the first matching specification above), we can run a
simple regression of the outcome on the treatment in the matched sample
(i.e., including the matching weights). With continuous outcomes, it is
often a good idea to also include the covariates used in the matching in
the effect estimation, as doing so can provide additional robustness to
slight imbalances remaining after the matching and can improve
precision.

Even though the 1:1 matching was not successful, we’ll demonstrate here
how to estimate a treatment effect after performing such an analysis.
First, we’ll extract the matched dataset from the matchit object using
match.data(). This dataset only contains the matched units and adds
columns for distance, weights, and subclass (described previously).

We can then estimate a treatment effect in this dataset using the
standard regression functions in R, like lm() or glm(), being sure to
include the matching weights (stored in the weights variable of the
match.data() output) in the estimation3. We recommend using
cluster-robust standard errors for most analyses, with pair membership
as the clustering variable; the lmtest and sandwich packages together
make this straightforward.

Although there are many possible ways to include covariates (i.e., not
just main effects but interactions, smoothing terms like splines, or
other nonlinear transformations), it is important not to engage in
specification search (i.e., trying many outcomes models in search of the
“best” one). Doing so can invalidate results and yield a conclusion that
fails to replicate. For this reason, we recommend only including the
same terms included in the propensity score model unless there is a
strong a priori and justifiable reason to model the outcome differently.
Second, it is important not to interpret the coefficients and tests of
the other covariates in the outcome model. These are not causal effects
and their estimates may be severely confounded. Only the treatment
effect estimate can be interpreted as causal assuming the relevant
assumptions about unconfoundedness are met. **Inappropriately
interpreting the coefficients of covariates in the outcome model is
known as the Table 2 fallacy** (Westreich and Greenland 2013). To avoid
this, in all examples that incorporate covariates in the outcome model,
we restrict the output of outcome regression models to just the
treatment coefficient.

#### Linear Modeling

#### Assumptions:

  - Linearity
  - Homoscedasticity
  - Independence
  - Normality

<http://www.sthda.com/english/articles/39-regression-model-diagnostics/161-linear-regression-assumptions-and-diagnostics-in-r-essentials/>

<https://data.library.virginia.edu/diagnostic-plots/>

#### Pre-match data model

``` r
plot(lalonde$re78)
```

![](Lalonde_files/figure-gfm/pre_match_mod-1.png)<!-- -->

``` r
# Pre-match data:
prematch_lm <- lm(re78 ~ treat, data = lalonde)

autoplot(prematch_lm) +
  theme_abyss()
```

    ## Warning: `arrange_()` is deprecated as of dplyr 0.7.0.
    ## Please use `arrange()` instead.
    ## See vignette('programming') for more help
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

![](Lalonde_files/figure-gfm/pre_match_mod-2.png)<!-- -->

``` r
  #theme_minimal() + 
  #theme_ft_rc()
```

#### Model 1 sandwich

``` r
fit1 <- lm(re78 ~ treat + age + educ + black + hispan + married + nodegree + 
             re74 + re75, 
           data = match_1_data, 
           weights = weights)

coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
```

    ## 
    ## t test of coefficients:
    ## 
    ##                Estimate  Std. Error t value Pr(>|t|)  
    ## (Intercept)  888.366052 3130.466960  0.2838  0.77675  
    ## treat       1116.038833  709.421069  1.5732  0.11660  
    ## age           -8.373257   41.839702 -0.2001  0.84150  
    ## educ         426.186450  193.915458  2.1978  0.02863 *
    ## black       -917.343709  947.193306 -0.9685  0.33348  
    ## hispan       762.385292 1197.871812  0.6364  0.52491  
    ## married     -129.340891  925.027275 -0.1398  0.88888  
    ## nodegree     273.837364 1068.170639  0.2564  0.79783  
    ## re74           0.034354    0.168952  0.2033  0.83899  
    ## re75           0.250141    0.161806  1.5459  0.12304  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#### Model 2 sandwich

``` r
fit2 <- lm(re78 ~ treat + age + educ + black + hispan + married + nodegree + 
             re74 + re75, 
           data = match_2_data, 
           weights = weights)

coeftest(fit2, vcov. = vcovCL, cluster = ~subclass)
```

    ## 
    ## t test of coefficients:
    ## 
    ##                Estimate  Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.3888e+03  3.1426e+03  1.7148 0.086968 .  
    ## treat        1.7267e+03  7.3585e+02  2.3465 0.019314 *  
    ## age         -5.2315e+01  3.6836e+01 -1.4202 0.156130    
    ## educ         1.7486e+02  1.9848e+02  0.8810 0.378710    
    ## black       -1.5640e+03  7.9872e+02 -1.9582 0.050729 .  
    ## hispan      -4.6242e+02  1.5337e+03 -0.3015 0.763145    
    ## married      1.0156e+03  1.1631e+03  0.8732 0.382971    
    ## nodegree    -1.6401e+03  1.2653e+03 -1.2962 0.195478    
    ## re74        -1.5587e-02  1.5121e-01 -0.1031 0.917939    
    ## re75         5.3991e-01  1.5017e-01  3.5953 0.000354 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Note that the “treat” coefficient in the linear regression model for the
first match is not statistically significant, but it is in the linear
regression model for the second match.

## Uncertainty / confidence intervals

#### Match 1 bootstrap

<https://kosukeimai.github.io/MatchIt/articles/estimating-effects.html#after-pair-matching-with-replacement-1>

``` r
#Block bootstrap confidence interval
# library(boot)

pair_ids <- levels(match_1_data$subclass)

est_fun <- function(pairs, i) {
  
  #Compute number of times each pair is present
  numreps <- table(pairs[i])
  
  #For each pair p, copy corresponding md row indices numreps[p] times
  ids <- unlist(lapply(pair_ids[pair_ids %in% names(numreps)],
                       function(p) rep(which(match_1_data$subclass == p), 
                                              numreps[p])))
  
  #Subset md with block bootstrapped ids
  md_boot <- match_1_data[ids,]
  
  #Effect estimation
  fit_boot <- lm(re78 ~ treat + age + educ + black + hispan + married + nodegree + 
             re74 + re75,
                 data = md_boot,
                 weights = weights)
  
  #Return the coefficient on treatment
  return(coef(fit_boot)["treat"])
}

boot_est <- boot(pair_ids, est_fun, R = 999)
boot_est
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = pair_ids, statistic = est_fun, R = 999)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original    bias    std. error
    ## t1* 1116.039 -20.16308    710.1202

``` r
boot.ci(boot_est, type = "bca")
```

    ## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
    ## Based on 999 bootstrap replicates
    ## 
    ## CALL : 
    ## boot.ci(boot.out = boot_est, type = "bca")
    ## 
    ## Intervals : 
    ## Level       BCa          
    ## 95%   (-285, 2482 )  
    ## Calculations and Intervals on Original Scale

#### Match 2 bootstrap

<https://moderndive.com/10-inference-for-regression.html> is another
helpful resource in addition to the above link from Kosuke Imai

``` r
#Block bootstrap confidence interval
# library(boot)

pair_ids <- levels(match_2_data$subclass)

est_fun <- function(pairs, i) {
  
  #Compute number of times each pair is present
  numreps <- table(pairs[i])
  
  #For each pair p, copy corresponding md row indices numreps[p] times
  ids <- unlist(lapply(pair_ids[pair_ids %in% names(numreps)],
                       function(p) rep(which(match_2_data$subclass == p), 
                                              numreps[p])))
  
  #Subset md with block bootstrapped ids
  md_boot <- match_1_data[ids,]
  
  #Effect estimation
  fit_boot <- lm(re78 ~ treat + age + educ + black + hispan + married + nodegree +
                   re74 + re75,
                 data = md_boot,
                 weights = weights)
  
  #Return the coefficient on treatment
  return(coef(fit_boot)["treat"])
}

boot_est <- boot(pair_ids, est_fun, R = 999)
boot_est
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = pair_ids, statistic = est_fun, R = 999)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original    bias    std. error
    ## t1* 1116.039 -76.97125    727.0259

``` r
boot.ci(boot_est, type = "bca")
```

    ## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
    ## Based on 999 bootstrap replicates
    ## 
    ## CALL : 
    ## boot.ci(boot.out = boot_est, type = "bca")
    ## 
    ## Intervals : 
    ## Level       BCa          
    ## 95%   (-224, 2614 )  
    ## Calculations and Intervals on Original Scale

## Generalized Linear Modeling

  - Get help page:
  - ?family

See <https://fromthebottomoftheheap.net/2016/06/07/rootograms/>

``` r
# Gaussian-distributed residuals and identity link = OLS:
gaussian_identity <- glm(re78 ~ treat + age + educ + black + hispan + married + nodegree + re74 + re75, 
                         data = match_2_data, 
                         family = gaussian(link = "identity"),
                         weights = weights)
summary(gaussian_identity)
```

    ## 
    ## Call:
    ## glm(formula = re78 ~ treat + age + educ + black + hispan + married + 
    ##     nodegree + re74 + re75, family = gaussian(link = "identity"), 
    ##     data = match_2_data, weights = weights)
    ## 
    ## Deviance Residuals: 
    ##    Min      1Q  Median      3Q     Max  
    ## -15809   -2724    -855    2249   33145  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  5.389e+03  2.220e+03   2.428  0.01552 *  
    ## treat        1.727e+03  5.236e+02   3.298  0.00104 ** 
    ## age         -5.231e+01  2.810e+01  -1.862  0.06318 .  
    ## educ         1.749e+02  1.398e+02   1.251  0.21151    
    ## black       -1.564e+03  7.893e+02  -1.982  0.04803 *  
    ## hispan      -4.624e+02  1.290e+03  -0.359  0.72004    
    ## married      1.016e+03  7.141e+02   1.422  0.15558    
    ## nodegree    -1.640e+03  7.204e+02  -2.277  0.02320 *  
    ## re74        -1.559e-02  7.060e-02  -0.221  0.82536    
    ## re75         5.399e-01  1.101e-01   4.903 1.25e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 32654842)
    ## 
    ##     Null deviance: 2.0468e+10  on 545  degrees of freedom
    ## Residual deviance: 1.7503e+10  on 536  degrees of freedom
    ## AIC: 11506
    ## 
    ## Number of Fisher Scoring iterations: 2

``` r
# Gamma default link is "inverse"
gamma_inverse <- glm(re78 + 1 ~ treat + age + educ + black + hispan + married + nodegree + re74 + re75, data = match_2_data, family = Gamma(link = "inverse"))
summary(gamma_inverse)
```

    ## 
    ## Call:
    ## glm(formula = re78 + 1 ~ treat + age + educ + black + hispan + 
    ##     married + nodegree + re74 + re75, family = Gamma(link = "inverse"), 
    ##     data = match_2_data)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -4.0995  -2.4516  -0.4159   0.5069   2.5705  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.904e-04  7.166e-05   4.053 5.81e-05 ***
    ## treat       -2.884e-05  2.023e-05  -1.425  0.15463    
    ## age          4.245e-07  9.550e-07   0.444  0.65687    
    ## educ        -9.793e-06  4.833e-06  -2.026  0.04324 *  
    ## black        3.356e-05  2.132e-05   1.574  0.11597    
    ## hispan      -2.662e-05  2.182e-05  -1.220  0.22300    
    ## married     -5.468e-06  1.836e-05  -0.298  0.76597    
    ## nodegree    -8.993e-06  2.306e-05  -0.390  0.69671    
    ## re74        -1.902e-09  1.210e-09  -1.571  0.11671    
    ## re75        -5.225e-09  1.646e-09  -3.175  0.00158 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for Gamma family taken to be 1.213309)
    ## 
    ##     Null deviance: 2478.0  on 545  degrees of freedom
    ## Residual deviance: 2437.7  on 536  degrees of freedom
    ## AIC: 9730.9
    ## 
    ## Number of Fisher Scoring iterations: 7

``` r
# Using log link
# gamma_log <- glm(re78 + 1 ~ treat + age + educ + black + hispan + married + nodegree + re74 + re75, data = match_2_data, family=Gamma(link = "log"))
# summary(gamma_log)

# # Fixing error from above due to inf values:
# match_2_data_new <- match_2_data # Duplicate data
# match_2_data_new[is.na(match_2_data_new) | match_2_data_new == "Inf"] <- NA  # Replace NaN & Inf with NA
# 
# # Add in positive constant:
# match_2_data_new$re78 <- match_2_data_new$re78 + 1
# 
# gamma_log <- glm(re78 ~ treat + age + educ + black + hispan + married + nodegree + re74 + re75, 
#                  data = match_2_data_new, 
#                  family = Gamma(link = "log"),
#                  weights = weights)
# summary(gamma_log)


# plot(rstudent(gamma_log))
# plot(influence(gamma_log)$hat)
# plot(cooks.distance(gamma_log))
```

#### Zelig

<https://docs.zeligproject.org/articles/att.html>

All models in Zelig can be estimated and results explored presented
using four simple functions:

1.  zelig to estimate the parameters,
2.  setx to set fitted values for which we want to find quantities of
    interest,
3.  sim to simulate the quantities of interest,
4.  plot to plot the simulation results.

<!-- end list -->

``` r
# https://docs.zeligproject.org/articles/zelig5_vs_zelig4.html
z5 <- zls$new()
z5$zelig(re78 + 1 ~ treat + age + educ + black + hispan + married + nodegree + re74 + re75,
         weights = match_2_data$weights,
         data = match_2_data)
```

    ## Warning: `tbl_df()` is deprecated as of dplyr 1.0.0.
    ## Please use `tibble::as_tibble()` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

    ## Warning: `group_by_()` is deprecated as of dplyr 0.7.0.
    ## Please use `group_by()` instead.
    ## See vignette('programming') for more help
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_warnings()` to see where this warning was generated.

``` r
z5$setx()
z5$sim()
z5$graph()
```

![](Lalonde_files/figure-gfm/zelig-1.png)<!-- -->

``` r
w <- match_2_data$weights
z.out <- zelig(re78 + 1 ~ treat, model = "gamma", weights = w, data = match_2_data)
```

    ## Noninteger weights were set, but the model in Zelig is only able to use integer valued weights.
    ##  A bootstrapped version of the dataset was constructed using the weights as sample probabilities.
    ## 
    ## How to cite this model in Zelig:
    ##   R Core Team. 2007.
    ##   gamma: Gamma Regression for Continuous, Positive Dependent Variables
    ##   in Christine Choirat, Christopher Gandrud, James Honaker, Kosuke Imai, Gary King, and Olivia Lau,
    ##   "Zelig: Everyone's Statistical Software," https://zeligproject.org/

``` r
summary(z.out)
```

    ## Model: 
    ## 
    ## Call:
    ## z5$zelig(formula = re78 + 1 ~ treat, data = match_2_data, weights = w)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -3.9556  -3.8325  -0.6615   0.4939   2.4171  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  2.379e-04  1.643e-05  14.481  < 2e-16
    ## treat       -9.062e-05  2.150e-05  -4.216 2.92e-05
    ## 
    ## (Dispersion parameter for Gamma family taken to be 1.693013)
    ## 
    ##     Null deviance: 2812.9  on 545  degrees of freedom
    ## Residual deviance: 2783.2  on 544  degrees of freedom
    ## AIC: 9313.1
    ## 
    ## Number of Fisher Scoring iterations: 7
    ## 
    ## Next step: Use 'setx' method

``` r
x.out <- setx(z.out)
s.out <- sim(z.out, x = x.out, treat = 1)

s.out
```

    ## 
    ##  sim x :
    ##  -----
    ## ev
    ##          mean       sd      50%     2.5%    97.5%
    ## [1,] 4850.945 271.1543 4840.306 4359.785 5402.554
    ## pv
    ##          mean       sd      50%      2.5%    97.5%
    ## [1,] 4736.924 9841.755 912.3834 0.0231313 30582.03

``` r
plot(s.out)
```

![](Lalonde_files/figure-gfm/zelig-2.png)<!-- -->

``` r
x.low <- setx(z.out, treat = 0)
x.high <- setx(z.out, treat = 1)

s.out <- sim(z.out, x = x.low, x1 = x.high)

s.out
```

    ## 
    ##  sim x :
    ##  -----
    ## ev
    ##          mean       sd    50%     2.5%    97.5%
    ## [1,] 4226.844 296.8056 4201.8 3701.698 4876.172
    ## pv
    ##          mean       sd      50%        2.5%    97.5%
    ## [1,] 4404.972 8936.878 755.6748 0.004793383 29036.68
    ## 
    ##  sim x1 :
    ##  -----
    ## ev
    ##          mean       sd     50%     2.5%    97.5%
    ## [1,] 6813.445 643.3956 6739.33 5693.468 8194.188
    ## pv
    ##          mean       sd      50%      2.5%    97.5%
    ## [1,] 6951.025 13571.82 1172.144 0.0190623 50981.34
    ## fd
    ##          mean       sd      50%     2.5%    97.5%
    ## [1,] 2586.601 726.4907 2540.744 1302.043 4105.788

``` r
plot(s.out)
```

![](Lalonde_files/figure-gfm/zelig-3.png)<!-- -->

#### Zelig experimentation

``` r
#Generatng data similar to Austin (2009) for demonstrating treatment effect estimation
gen_X <- function(n) {
  X <- matrix(rnorm(9 * n), nrow = n, ncol = 9)
  X[,5] <- as.numeric(X[,5] < .5)
  X
}

#~20% treated
gen_A <- function(X) {
  LP_A <- - 1.2 + log(2)*X[,1] - log(1.5)*X[,2] + log(2)*X[,4] - log(2.4)*X[,5] + log(2)*X[,7] - log(1.5)*X[,8]
  P_A <- plogis(LP_A)
  rbinom(nrow(X), 1, P_A)
}

# Continuous outcome
gen_Y_C <- function(A, X) {
  2*A + 2*X[,1] + 2*X[,2] + 2*X[,3] + 1*X[,4] + 2*X[,5] + 1*X[,6] + rnorm(length(A), 0, 5)
}
#Conditional:
#  MD: 2
#Marginal:
#  MD: 2

# Binary outcome
gen_Y_B <- function(A, X) {
  LP_B <- -2 + log(2.4)*A + log(2)*X[,1] + log(2)*X[,2] + log(2)*X[,3] + log(1.5)*X[,4] + log(2.4)*X[,5] + log(1.5)*X[,6]
  P_B <- plogis(LP_B)
  rbinom(length(A), 1, P_B)
}
#Conditional:
#  OR:   2.4
#  logOR: .875
#Marginal:
#  RD:    .144
#  RR:   1.54
#  logRR: .433
#  OR:   1.92
#  logOR  .655

# Survival outcome
gen_Y_S <- function(A, X) {
  LP_S <- -2 + log(2.4)*A + log(2)*X[,1] + log(2)*X[,2] + log(2)*X[,3] + log(1.5)*X[,4] + log(2.4)*X[,5] + log(1.5)*X[,6]
  sqrt(-log(runif(length(A)))*2e4*exp(-LP_S))
}
#Conditional:
#  HR:   2.4
#  logHR: .875
#Marginal:
#  HR:   1.57
#  logHR: .452

set.seed(19599)

n <- 2000
X <- gen_X(n)
A <- gen_A(X)

Y_C <- gen_Y_C(A, X)
Y_B <- gen_Y_B(A, X)
Y_S <- gen_Y_S(A, X)

d <- data.frame(A, X, Y_C, Y_B, Y_S)

head(d)
```

    ##   A         X1         X2         X3          X4 X5         X6         X7
    ## 1 0  0.1725432 -1.4282682 -0.4102538 -2.36059391  1 -1.1198630  0.6397597
    ## 2 0 -1.0958972  0.8462679  0.2455671 -0.12332771  1 -2.2686599 -1.4490669
    ## 3 0  0.1767731  0.7904544 -0.8435734  0.82365740  1 -0.2220610  0.2970925
    ## 4 0 -0.4595239  0.1725907  1.9542266 -0.62661325  1 -0.4018875 -0.8294041
    ## 5 1  0.3563142 -1.8120759  0.8134872 -0.67188804  1 -0.8297074  1.7296758
    ## 6 0 -2.4312962 -1.7983652 -1.2939700  0.04609167  1 -1.2418889 -1.1252320
    ##           X8          X9         Y_C Y_B        Y_S
    ## 1 -0.4840279 -0.59385184  0.07104125   0  278.45921
    ## 2 -0.5514441 -0.31439333  0.15618693   0  330.62919
    ## 3 -0.6966028 -0.69515666 -0.85179850   1  369.94056
    ## 4 -0.5383526  0.20729344 -2.35183911   0   91.06106
    ## 5 -0.6438651 -0.02647566  0.68057840   0  182.72605
    ## 6 -1.8658885 -0.56513308 -5.62259966   0 2563.73131

``` r
mNN <- matchit(A ~ X1 + X2 + X3 + X4 + X5 + 
                 X6 + X7 + X8 + X9, data = d)

mNN
```

    ## A matchit object
    ##  - method: 1:1 nearest neighbor matching without replacement
    ##  - distance: Propensity score
    ##              - estimated with logistic regression
    ##  - number of obs.: 2000 (original), 882 (matched)
    ##  - target estimand: ATT
    ##  - covariates: X1, X2, X3, X4, X5, X6, X7, X8, X9

``` r
md <- match.data(mNN)

head(md)
```

    ##    A         X1         X2         X3         X4 X5         X6         X7
    ## 1  0  0.1725432 -1.4282682 -0.4102538 -2.3605939  1 -1.1198630  0.6397597
    ## 3  0  0.1767731  0.7904544 -0.8435734  0.8236574  1 -0.2220610  0.2970925
    ## 5  1  0.3563142 -1.8120759  0.8134872 -0.6718880  1 -0.8297074  1.7296758
    ## 9  0  0.7807950  1.3137166  0.6580342  0.8539807  1  0.9495019 -0.5730730
    ## 10 1 -0.5651300 -0.1053473 -0.1368541  1.6232841  1 -0.5303725 -0.3341707
    ## 11 0  0.0705263 -0.1319709 -1.8881648  0.3076618  0 -1.6558365  0.8405124
    ##            X8          X9         Y_C Y_B        Y_S   distance weights
    ## 1  -0.4840279 -0.59385184  0.07104125   0  278.45921 0.08460901       1
    ## 3  -0.6966028 -0.69515666 -0.85179850   1  369.94056 0.22210416       1
    ## 5  -0.6438651 -0.02647566  0.68057840   0  182.72605 0.43290668       1
    ## 9  -0.2362267 -0.14580271 15.89771407   1   67.53084 0.15751364       1
    ## 10  0.4183507  0.46308458  1.07888291   1  113.69659 0.16697023       1
    ## 11 -0.5723312 -1.01382183 -6.55596546   0 1632.68627 0.45622858       1
    ##    subclass
    ## 1       167
    ## 3       210
    ## 5       322
    ## 9         3
    ## 10        1
    ## 11      298

``` r
#Linear model without covariates
fit1 <- lm(Y_C ~ A, data = md, weights = weights)
summary(fit1)
```

    ## 
    ## Call:
    ## lm(formula = Y_C ~ A, data = md, weights = weights)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -20.7905  -4.0539  -0.1823   3.8825  23.5382 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.7795     0.2955   6.022 2.52e-09 ***
    ## A             2.1144     0.4179   5.060 5.10e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.205 on 880 degrees of freedom
    ## Multiple R-squared:  0.02827,    Adjusted R-squared:  0.02717 
    ## F-statistic:  25.6 on 1 and 880 DF,  p-value: 5.103e-07

``` r
#Cluster-robust standard errors
coeftest(fit1, vcov. = vcovCL, cluster = ~subclass)
```

    ## 
    ## t test of coefficients:
    ## 
    ##             Estimate Std. Error t value  Pr(>|t|)    
    ## (Intercept)  1.77946    0.30255  5.8816 5.768e-09 ***
    ## A            2.11440    0.40857  5.1751 2.822e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
# data(sanction)
# zqi.out <- zelig(num ~ target + coop + mil, 
#                  model = "poisson", data = sanction, cite = FALSE)
# 
# # find the ATT where the treatement is mil = 1
# z.att <- zqi.out %>%
#              ATT(treatment = "mil", treat = 1) %>% 
#              get_qi(qi = "ATT", xvalue = "TE")
# 
# # summarize the results
# hist(z.att, 
#      main = NULL,
#      xlab ="Average Treatment Effect of mil on the Treated")

s
```

    ## function (..., k = -1, fx = FALSE, bs = "tp", m = NA, by = NA, 
    ##     xt = NULL, id = NULL, sp = NULL, pc = NULL) 
    ## {
    ##     vars <- as.list(substitute(list(...)))[-1]
    ##     d <- length(vars)
    ##     by.var <- deparse(substitute(by), backtick = TRUE, width.cutoff = 500)
    ##     if (by.var == ".") 
    ##         stop("by=. not allowed")
    ##     term <- deparse(vars[[1]], backtick = TRUE, width.cutoff = 500)
    ##     if (term[1] == ".") 
    ##         stop("s(.) not supported.")
    ##     if (d > 1) 
    ##         for (i in 2:d) {
    ##             term[i] <- deparse(vars[[i]], backtick = TRUE, width.cutoff = 500)
    ##             if (term[i] == ".") 
    ##                 stop("s(.) not yet supported.")
    ##         }
    ##     for (i in 1:d) term[i] <- attr(terms(reformulate(term[i])), 
    ##         "term.labels")
    ##     k.new <- round(k)
    ##     if (all.equal(k.new, k) != TRUE) {
    ##         warning("argument k of s() should be integer and has been rounded")
    ##     }
    ##     k <- k.new
    ##     if (length(unique(term)) != d) 
    ##         stop("Repeated variables as arguments of a smooth are not permitted")
    ##     full.call <- paste("s(", term[1], sep = "")
    ##     if (d > 1) 
    ##         for (i in 2:d) full.call <- paste(full.call, ",", term[i], 
    ##             sep = "")
    ##     label <- paste(full.call, ")", sep = "")
    ##     if (!is.null(id)) {
    ##         if (length(id) > 1) {
    ##             id <- id[1]
    ##             warning("only first element of `id' used")
    ##         }
    ##         id <- as.character(id)
    ##     }
    ##     ret <- list(term = term, bs.dim = k, fixed = fx, dim = d, 
    ##         p.order = m, by = by.var, label = label, xt = xt, id = id, 
    ##         sp = sp)
    ##     if (!is.null(pc)) {
    ##         if (length(pc) < d) 
    ##             stop("supply a value for each variable for a point constraint")
    ##         if (!is.list(pc)) 
    ##             pc <- as.list(pc)
    ##         if (is.null(names(pc))) 
    ##             names(pc) <- unlist(lapply(vars, all.vars))
    ##         ret$point.con <- pc
    ##     }
    ##     class(ret) <- paste(bs, ".smooth.spec", sep = "")
    ##     ret
    ## }
    ## <bytecode: 0x7ff213551930>
    ## <environment: namespace:mgcv>

## TWANG IPTW

<https://www.rand.org/statistics/twang/r-tutorial.html>

``` r
set.seed(1)

ps.lalonde <- ps(treat ~ age + educ + black + hispan + nodegree + 
                   married + re74 + re75,
                 data = lalonde,
                 n.trees=5000,
                 interaction.depth=2,
                 shrinkage=0.01,
                 perm.test.iters=0,
                 stop.method=c("es.mean","ks.max"),
                 estimand = "ATT",
                 verbose=FALSE)

plot(ps.lalonde)
```

![](Lalonde_files/figure-gfm/twang-1.png)<!-- -->

``` r
summary(ps.lalonde)
```

    ##             n.treat n.ctrl ess.treat  ess.ctrl    max.es    mean.es    max.ks
    ## unw             185    429       185 429.00000 1.7567745 0.56872589 0.6404460
    ## es.mean.ATT     185    429       185  22.96430 0.2177817 0.07746175 0.1223384
    ## ks.max.ATT      185    429       185  27.05472 0.2348846 0.08025994 0.1070761
    ##             max.ks.p    mean.ks iter
    ## unw               NA 0.27024507   NA
    ## es.mean.ATT       NA 0.06361021 2127
    ## ks.max.ATT        NA 0.06282432 1756

``` r
summary(
  ps.lalonde$gbm.obj,
        n.trees=ps.lalonde$desc$ks.max.ATT$n.trees,
        plot=FALSE)
```

    ##               var     rel.inf
    ## black       black 57.83094183
    ## age           age 16.51591133
    ## re74         re74 15.61359263
    ## re75         re75  3.58011684
    ## married   married  3.02141880
    ## educ         educ  2.91556075
    ## nodegree nodegree  0.44174805
    ## hispan     hispan  0.08070978

``` r
lalonde.balance <- bal.table(ps.lalonde)

lalonde.balance
```

    ## $unw
    ##             tx.mn    tx.sd    ct.mn    ct.sd std.eff.sz   stat     p    ks
    ## age        25.816    7.155   28.030   10.787     -0.309 -2.994 0.003 0.158
    ## educ       10.346    2.011   10.235    2.855      0.055  0.547 0.584 0.111
    ## black       0.843    0.365    0.203    0.403      1.757 19.371 0.000 0.640
    ## hispan      0.059    0.237    0.142    0.350     -0.349 -3.413 0.001 0.083
    ## nodegree    0.708    0.456    0.597    0.491      0.244  2.716 0.007 0.111
    ## married     0.189    0.393    0.513    0.500     -0.824 -8.607 0.000 0.324
    ## re74     2095.574 4886.620 5619.237 6788.751     -0.721 -7.254 0.000 0.447
    ## re75     1532.055 3219.251 2466.484 3291.996     -0.290 -3.282 0.001 0.288
    ##          ks.pval
    ## age        0.003
    ## educ       0.074
    ## black      0.000
    ## hispan     0.317
    ## nodegree   0.074
    ## married    0.000
    ## re74       0.000
    ## re75       0.000
    ## 
    ## $es.mean.ATT
    ##             tx.mn    tx.sd    ct.mn    ct.sd std.eff.sz   stat     p    ks
    ## age        25.816    7.155   25.802    7.279      0.002  0.015 0.988 0.122
    ## educ       10.346    2.011   10.573    2.089     -0.113 -0.706 0.480 0.099
    ## black       0.843    0.365    0.842    0.365      0.003  0.027 0.978 0.001
    ## hispan      0.059    0.237    0.042    0.202      0.072  0.804 0.421 0.017
    ## nodegree    0.708    0.456    0.609    0.489      0.218  0.967 0.334 0.099
    ## married     0.189    0.393    0.189    0.392      0.002  0.012 0.990 0.001
    ## re74     2095.574 4886.620 1556.930 3801.566      0.110  1.027 0.305 0.066
    ## re75     1532.055 3219.251 1211.575 2647.615      0.100  0.833 0.405 0.103
    ##          ks.pval
    ## age        0.892
    ## educ       0.977
    ## black      1.000
    ## hispan     1.000
    ## nodegree   0.977
    ## married    1.000
    ## re74       1.000
    ## re75       0.969
    ## 
    ## $ks.max.ATT
    ##             tx.mn    tx.sd    ct.mn    ct.sd std.eff.sz   stat     p    ks
    ## age        25.816    7.155   25.764    7.408      0.007  0.055 0.956 0.107
    ## educ       10.346    2.011   10.572    2.140     -0.113 -0.712 0.477 0.107
    ## black       0.843    0.365    0.835    0.371      0.022  0.187 0.852 0.008
    ## hispan      0.059    0.237    0.043    0.203      0.069  0.779 0.436 0.016
    ## nodegree    0.708    0.456    0.601    0.490      0.235  1.100 0.272 0.107
    ## married     0.189    0.393    0.199    0.400     -0.024 -0.169 0.866 0.010
    ## re74     2095.574 4886.620 1673.666 3944.600      0.086  0.800 0.424 0.054
    ## re75     1532.055 3219.251 1257.242 2674.922      0.085  0.722 0.471 0.094
    ##          ks.pval
    ## age        0.919
    ## educ       0.919
    ## black      1.000
    ## hispan     1.000
    ## nodegree   0.919
    ## married    1.000
    ## re74       1.000
    ## re75       0.971

``` r
plot(ps.lalonde, plots=2)
```

![](Lalonde_files/figure-gfm/twang-2.png)<!-- -->

``` r
plot(ps.lalonde, plots=3)
```

![](Lalonde_files/figure-gfm/twang-3.png)<!-- -->

``` r
plot(ps.lalonde, plots = 4)
```

![](Lalonde_files/figure-gfm/twang-4.png)<!-- -->

``` r
plot(ps.lalonde, plots = 5)
```

![](Lalonde_files/figure-gfm/twang-5.png)<!-- -->

``` r
plot(ps.lalonde, plots = 3, subset = 2)
```

![](Lalonde_files/figure-gfm/twang-6.png)<!-- -->

``` r
# A separate R package, the survey package, is useful for performing the outcomes analyses using weights. Its statistical methods account for the weights when computing standard error estimates. It is not a part of the standard R installation but installing twang should automatically install survey as well.
# 
# > library(survey)
# 
# The get.weights() function extracts the propensity score weights from a ps object. Those weights may then be used as case weights in a svydesign object. By default, it returns weights corresponding to the estimand (ATE or ATT) that was specified in the original call to ps(). If needed, the user can override the default via the optional estimand argument.

lalonde$w <- get.weights(ps.lalonde, stop.method="es.mean")

design.ps <- svydesign(ids=~1, weights=~w, data=lalonde)

glm1 <- svyglm(re78 ~ treat, design=design.ps)

summary(glm1)
```

    ## 
    ## Call:
    ## svyglm(formula = re78 ~ treat, design = design.ps)
    ## 
    ## Survey design:
    ## svydesign(ids = ~1, weights = ~w, data = lalonde)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   5616.6      884.9   6.347 4.28e-10 ***
    ## treat          732.5     1056.6   0.693    0.488    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 49804197)
    ## 
    ## Number of Fisher Scoring iterations: 2

The analysis estimates an increase in earnings of $733 for those that
participated in the

NSW compared with similarly situated people observed in the CPS. The
effect, however, does not appear to be statistically significant.

#### TWANG Doubly Robust

Some authors have recommended utilizing both propensity score adjustment
and additional covariate adjustment to minimize mean square error or to
obtain “doubly robust” estimates of the treatment effect
(Huppler-Hullsiek & Louis 2002, Bang & Robins 2005). These estimators
are consistent if either the propensity scores are estimated correctly
or the regression model is specified correctly. For example, note that
the balance table for ks.max.ATT made the two groups more similar on
nodegree, but still some differences remained, 70.8% of the treatment
group had no degree while 60.1% of the comparison group had no degree.
While linear regression is sensitive to model misspecification when the
treatment and comparison groups are dissimilar, the propensity score
weighting has made them more similar, perhaps enough so that additional
modeling with covariates can adjust for any remaining differences. In
addition to potential bias reduction, the inclusion of additional
covariates can reduce the standard error of the treatment effect if some
of the covariates are strongly related to the outcome.

``` r
glm2 <- svyglm(re78 ~ treat + nodegree, design=design.ps)

summary(glm2)
```

    ## 
    ## Call:
    ## svyglm(formula = re78 ~ treat + nodegree, design = design.ps)
    ## 
    ## Survey design:
    ## svydesign(ids = ~1, weights = ~w, data = lalonde)
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   6768.4     1471.0   4.601 5.11e-06 ***
    ## treat          920.3     1082.8   0.850    0.396    
    ## nodegree     -1891.8     1261.9  -1.499    0.134    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 49013778)
    ## 
    ## Number of Fisher Scoring iterations: 2

Adjusting for the remaining group difference in the nodegree variable
slightly increased the

estimate of the program’s effect to $920, but the difference is still
not statistically significant. We can further adjust for the other
covariates, but that too in this case has little effect on the estimated
program effect.

``` r
glm3 <- svyglm(re78 ~ treat + age + educ + black + hispan + nodegree +
                 married + re74 + re75,
               design=design.ps)

summary(glm3)
```

    ## 
    ## Call:
    ## svyglm(formula = re78 ~ treat + age + educ + black + hispan + 
    ##     nodegree + married + re74 + re75, design = design.ps)
    ## 
    ## Survey design:
    ## svydesign(ids = ~1, weights = ~w, data = lalonde)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -2.459e+03  4.289e+03  -0.573  0.56671   
    ## treat        7.585e+02  1.019e+03   0.745  0.45674   
    ## age          3.005e+00  5.558e+01   0.054  0.95691   
    ## educ         7.488e+02  2.596e+02   2.884  0.00406 **
    ## black       -7.627e+02  1.012e+03  -0.753  0.45153   
    ## hispan       6.106e+02  1.711e+03   0.357  0.72123   
    ## nodegree     5.350e+02  1.626e+03   0.329  0.74227   
    ## married      4.918e+02  1.072e+03   0.459  0.64660   
    ## re74         5.699e-02  1.801e-01   0.316  0.75176   
    ## re75         1.568e-01  1.946e-01   0.806  0.42076   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 47150852)
    ## 
    ## Number of Fisher Scoring iterations: 2

#### TWANG LM

The more traditional regression approach to estimating the program
effect would fit a linear model with a treatment indicator and linear
terms for each of the covariates.

``` r
glm4 <- lm(re78 ~ treat + age + educ + black + hispan + nodegree +
             married + re74 + re75,
           data=lalonde)

summary(glm4)
```

    ## 
    ## Call:
    ## lm(formula = re78 ~ treat + age + educ + black + hispan + nodegree + 
    ##     married + re74 + re75, data = lalonde)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -13595  -4894  -1662   3929  54570 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  6.651e+01  2.437e+03   0.027   0.9782    
    ## treat        1.548e+03  7.813e+02   1.982   0.0480 *  
    ## age          1.298e+01  3.249e+01   0.399   0.6897    
    ## educ         4.039e+02  1.589e+02   2.542   0.0113 *  
    ## black       -1.241e+03  7.688e+02  -1.614   0.1071    
    ## hispan       4.989e+02  9.419e+02   0.530   0.5966    
    ## nodegree     2.598e+02  8.474e+02   0.307   0.7593    
    ## married      4.066e+02  6.955e+02   0.585   0.5590    
    ## re74         2.964e-01  5.827e-02   5.086 4.89e-07 ***
    ## re75         2.315e-01  1.046e-01   2.213   0.0273 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6948 on 604 degrees of freedom
    ## Multiple R-squared:  0.1478, Adjusted R-squared:  0.1351 
    ## F-statistic: 11.64 on 9 and 604 DF,  p-value: < 2.2e-16

This model estimates a rather strong treatment effect, estimating a
program effect of `$`1548 with a p-value=0.048. Several variations of
this regression approach also estimate strong program effects. For
example using square root transforms on the earnings variables yields a
p- value=0.016. These estimates, however, are very sensitive to the
model structure since the treatment and control subjects differ greatly
as seen in the unweighted balance comparison ($unw) from
bal.table(ps.lalonde).
