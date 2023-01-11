Answers to exercises “Binary data: Logistic regression” part 1
================

-   [Exercise 1](#exercise-1)
    -   [1a](#1a)
    -   [1b](#1b)
    -   [1c](#1c)
    -   [1d](#1d)
-   [Exercise 2](#exercise-2)
    -   [2a](#2a)
    -   [2b](#2b)
    -   [2c](#2c)
    -   [2 d+e](#2-de)
    -   [2f](#2f)
-   [Exercise 3](#exercise-3)
    -   [3a](#3a)
    -   [3b](#3b)
    -   [3c](#3c)
    -   [3d](#3d)
    -   [3e](#3e)
-   [Exercise 4](#exercise-4)
    -   [4a](#4a)
    -   [4c](#4c)
    -   [4d](#4d)

# Exercise 1

## 1a

``` r
adress <- "https://raw.github.com/janvdbroek/Generalized-Linear-Models/master/episode.txt"
lr1 <- read.table(adress,header=TRUE)
lr1$immune <- 1*(lr1$cd4<200)
epifit.1 <- glm(episode~immune,family=binomial,data = lr1)
summary(epifit.1)
```

    ## 
    ## Call:
    ## glm(formula = episode ~ immune, family = binomial, data = lr1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8410  -0.8410  -0.3482  -0.3482   2.3804  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -2.7726     0.5951  -4.659 3.18e-06 ***
    ## immune        1.9151     0.6752   2.836  0.00456 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 90.424  on 97  degrees of freedom
    ## Residual deviance: 80.070  on 96  degrees of freedom
    ## AIC: 84.07
    ## 
    ## Number of Fisher Scoring iterations: 5

## 1b

``` r
epifit.1 <- glm(episode~log(followup),family=binomial,data = lr1)
summary(epifit.1)
```

    ## 
    ## Call:
    ## glm(formula = episode ~ log(followup), family = binomial, data = lr1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.6812  -0.6340  -0.5992  -0.5652   1.9626  
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)    -1.9931     1.0072  -1.979   0.0478 *
    ## log(followup)   0.2046     0.4551   0.450   0.6530  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 90.424  on 97  degrees of freedom
    ## Residual deviance: 90.221  on 96  degrees of freedom
    ## AIC: 94.221
    ## 
    ## Number of Fisher Scoring iterations: 4

## 1c

``` r
epifit.2 <- glm(episode~offset(log(followup)),family=binomial,data = lr1)
summary(epifit.2)
```

    ## 
    ## Call:
    ## glm(formula = episode ~ offset(log(followup)), family = binomial, 
    ##     data = lr1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9428  -0.6757  -0.5117  -0.3814   2.3356  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -3.7586     0.2731  -13.76   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 93.159  on 97  degrees of freedom
    ## Residual deviance: 93.159  on 97  degrees of freedom
    ## AIC: 95.159
    ## 
    ## Number of Fisher Scoring iterations: 4

The AIC of this model is 95.16 The AIC of the model in which
log(followup is an exposure is 94.22 So there is not much difference and
then the model with the offset is preferred since it has fewer
parameters.

## 1d

The model with only a comstant and an offset is:

log(odds)=a+log(follow-up) or log(odds/follow-up)=a

so here the odds per unit time is modelled.

# Exercise 2

## 2a

``` r
adress <- "https://raw.github.com/janvdbroek/Generalized-Linear-Models/master/lowbirth.dat"
lr2 <-read.table(adress,header=TRUE)
```

## 2b

``` r
#
fit.1 <- glm(low~age,family=binomial,data = lr2)
fit.2 <- glm(low~smoke,family=binomial,data = lr2)
fit.3 <- glm(low~ht,family=binomial,data = lr2)
fit.4 <- glm(low~1,family=binomial,data = lr2)
```

## 2c

log odds ratio if the independent variable is increased by 1

## 2 d+e

calculate the loglikelhood values one can use logLik(fit.1)

| Model | AIC   | estimate b | log-likelihhod |
|-------|-------|------------|----------------|
| age   | 235.9 | -0.05      | -115.96        |
| smoke | 233.8 | 0.70       | -114.90        |
| ht    | 234.7 | 1.2        | -115.32        |
| 1     | 236.7 |            | -117.34        |

likelihood ratio for the models compared to model with constant
(exp(l1-l0)):

| comparison | LR    |
|------------|-------|
| age vs 1   | 3.97  |
| smoke vs 1 | 11.40 |
| ht vs 1    | 7.47  |

interpretation: for instance the model with hypertension historyand a
constantant makes the data 7.54 times as probable as amodel with only a
constant.

## 2f

The model with smoke has lowest AIC although the model with ht is close.

# Exercise 3

## 3a

``` r
adress <- "https://raw.github.com/janvdbroek/Generalized-Linear-Models/master/pdd.csv"
lr3 <- read.table(adress,header=TRUE,sep=",")
```

## 3b

``` r
with(lr3,tapply(pdd,list(nop),sum))
```

    ##  0  1 
    ## 20 45

``` r
with(lr3,tapply(n,list(nop),sum))
```

    ##   0   1 
    ## 158 150

## 3c

| Nop   | pdd=0 | pdd=1 | total |
|-------|-------|-------|-------|
| nop=0 | 138   | 20    | 158   |
| nop=1 | 105   | 45    | 150   |
|       |       |       |       |
| total | 243   | 65    | 308   |

## 3d

$OR=\frac{45 \cdot 138}{20 \cdot105}=2.957$ The pdd-odds in nop centers
is about 3 times larger as in in birds not from such a center. There
seems to be some logic in that since birds are brougth to an nop center
if something is wrong with there health.

## 3e

``` r
pdd.fit <- glm(cbind(pdd,n-pdd)~nop,family=binomial,data=lr3)
summary(pdd.fit)
```

    ## 
    ## Call:
    ## glm(formula = cbind(pdd, n - pdd) ~ nop, family = binomial, data = lr3)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.8504  -0.1688   0.1095   0.2364   0.6911  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  -1.9315     0.2393  -8.073 6.87e-16 ***
    ## nop           1.0842     0.2983   3.634 0.000279 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 15.5528  on 7  degrees of freedom
    ## Residual deviance:  1.3975  on 6  degrees of freedom
    ## AIC: 33.704
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
exp(coef(pdd.fit))
```

    ## (Intercept)         nop 
    ##   0.1449275   2.9571429

# Exercise 4

## 4a

``` r
adress <- "https://raw.github.com/janvdbroek/Generalized-Linear-Models/master/dalmatian.csv"
lr4 <- read.table(adress,header=TRUE,sep=",")
```

or with the rstudio menu. \## 4b See the explanatipon in the text
section 9

## 4c

``` r
dalfit.1 <- glm(deaf~fhs,family=binomial,data=lr4)
summary(dalfit.1)
```

    ## 
    ## Call:
    ## glm(formula = deaf ~ fhs, family = binomial, data = lr4)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.4296  -0.6062  -0.5012  -0.4382   2.2741  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.58544    0.07853 -20.190  < 2e-16 ***
    ## fhs          0.41004    0.05144   7.971 1.57e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1145.0  on 1242  degrees of freedom
    ## Residual deviance: 1081.2  on 1241  degrees of freedom
    ## AIC: 1085.2
    ## 
    ## Number of Fisher Scoring iterations: 4

## 4d

``` r
dalfit.0 <- glm(deaf~1,family=binomial,data=lr4)
summary(dalfit.0)
```

    ## 
    ## Call:
    ## glm(formula = deaf ~ 1, family = binomial, data = lr4)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.6163  -0.6163  -0.6163  -0.6163   1.8733  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -1.56473    0.07499  -20.87   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1145  on 1242  degrees of freedom
    ## Residual deviance: 1145  on 1242  degrees of freedom
    ## AIC: 1147
    ## 
    ## Number of Fisher Scoring iterations: 3

| model | AIC    | log-lik | lik. ratio          |
|-------|--------|---------|---------------------|
| 1     | 1147.0 | -572.5  |                     |
| fhs   | 1085.2 | -540.4  | exp(31.9)=7.145e+13 |
