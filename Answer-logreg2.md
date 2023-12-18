Answers to exercises “Binary data: Logistic regression” part 2
================

# Exercise 1

## 1a

``` r
adress <- "https://raw.github.com/janvdbroek/Generalized-Linear-Models/master/osteochon.csv"
lr5 <- read.table(adress,header=TRUE,sep=",")
```

## 1b

``` r
osteo.fit <- glm(oc~factor(food)+factor(ground)+height,
                 family=binomial,data = lr5)
```

## 1c

``` r
drop1(osteo.fit,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## oc ~ factor(food) + factor(ground) + height
    ##                Df Deviance    AIC    LRT Pr(>Chi)   
    ## <none>              350.89 360.89                   
    ## factor(food)    2   351.06 357.06 0.1639 0.921312   
    ## factor(ground)  1   352.76 360.76 1.8636 0.172210   
    ## height          1   359.84 367.84 8.9425 0.002786 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

The variable food has highest p-value so it is left out.

## 1d

``` r
osteo2.fit <- glm(oc~factor(ground)+height,
                  family=binomial,data = lr5)
```

ground is not significant

``` r
osteo3.fit <- glm(oc~height,family=binomial,data = lr5)
drop1(osteo3.fit,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## oc ~ height
    ##        Df Deviance    AIC    LRT Pr(>Chi)   
    ## <none>      352.99 356.99                   
    ## height  1   361.41 363.41 8.4226 0.003706 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
summary(osteo3.fit)
```

    ## 
    ## Call:
    ## glm(formula = oc ~ height, family = binomial, data = lr5)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.9325  -0.5938  -0.5208  -0.4357   2.3829  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept) -17.31649    5.47881  -3.161  0.00157 **
    ## height        0.09440    0.03318   2.845  0.00444 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 361.41  on 439  degrees of freedom
    ## Residual deviance: 352.99  on 438  degrees of freedom
    ## AIC: 356.99
    ## 
    ## Number of Fisher Scoring iterations: 4

## 1e

Only height has a significant realtion with osteochondroses. The
log-odds ratio for heigth is 0.094: The difference in log-odds if height
is increased by 1. And the difference in log-odds is the log oddsratio.

## 1f

``` r
confint(osteo3.fit)
```

    ## Waiting for profiling to be done...

    ##                   2.5 %     97.5 %
    ## (Intercept) -28.2865831 -6.7577637
    ## height        0.0303029  0.1606951
    
## 1g
Those values for the parameter of height for which the likelihood ratio w.r.t. the maximum likelihood estimate is smaller than 7.

## 1h

The occurence of osteochondrosis was analysed using a logistic
regression model with food, ground and height as independent variables.
Model reduction was done with the likelihood ratio test. The final model
only contained the height variable. The log-odds ratio for heigth is
0.094: The difference in log-odds if height is increased by 1. (Off
course a description of the variables is also needed.)

# Exercise 2

## 2a

``` r
adress <- "https://raw.github.com/janvdbroek/Generalized-Linear-Models/master/episode.txt"
lr1 <- read.table(adress,header=TRUE)
lr1$immune <- 1*(lr1$cd4<200)
```

## 2b

``` r
epifit.1 <- glm(episode~immune+age+offset(log(followup)),
    family=binomial,data=lr1)
summary(epifit.1)
```

    ## 
    ## Call:
    ## glm(formula = episode ~ immune + age + offset(log(followup)), 
    ##     family = binomial, data = lr1)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.3562  -0.5972  -0.3749  -0.2474   2.7270  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -5.88782    1.26839  -4.642 3.45e-06 ***
    ## immune       1.69421    0.69639   2.433    0.015 *  
    ## age          0.02547    0.02827   0.901    0.367    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 93.159  on 97  degrees of freedom
    ## Residual deviance: 83.516  on 95  degrees of freedom
    ## AIC: 89.516
    ## 
    ## Number of Fisher Scoring iterations: 5

## 2c

The estimates are the differences in log-odds per unit time, or the
log-odds ratio’s per unit time. Or for instance: the odds per month
follow up for the low immune status group is exp(1.69421)=5.44 times as
high as for the high immune statusgroup

## 2d

``` r
drop1(epifit.1)
```

    ## Single term deletions
    ## 
    ## Model:
    ## episode ~ immune + age + offset(log(followup))
    ##        Df Deviance    AIC
    ## <none>      83.516 89.516
    ## immune  1   90.723 94.723
    ## age     1   84.341 88.341

So the model without age has smallest AIC. Immune cannot be left out.

# Exercise 3

## 3a

data is in lr5

## 3b

``` r
osteo1.fit <- glm(oc~factor(father)+factor(food)+factor(ground)
+height,family=binomial,data=lr5)
drop1(osteo1.fit,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## oc ~ factor(father) + factor(food) + factor(ground) + height
    ##                Df Deviance    AIC    LRT Pr(>Chi)   
    ## <none>              304.90 372.90                   
    ## factor(father) 29   350.89 360.89 45.994 0.023496 * 
    ## factor(food)    2   305.36 369.36  0.462 0.793692   
    ## factor(ground)  1   306.10 372.10  1.200 0.273289   
    ## height          1   314.50 380.50  9.600 0.001945 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
osteo2.fit <- glm(oc~factor(father)+factor(ground)
+height,family=binomial,data=lr5)
drop1(osteo2.fit,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## oc ~ factor(father) + factor(ground) + height
    ##                Df Deviance    AIC    LRT Pr(>Chi)   
    ## <none>              305.36 369.36                   
    ## factor(father) 29   351.06 357.06 45.696 0.025150 * 
    ## factor(ground)  1   306.67 368.67  1.310 0.252376   
    ## height          1   314.72 376.72  9.354 0.002224 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

``` r
osteo3.fit <- glm(oc~factor(father)+height,family=binomial,data=lr5)
drop1(osteo3.fit,test="Chisq")
```

    ## Single term deletions
    ## 
    ## Model:
    ## oc ~ factor(father) + height
    ##                Df Deviance    AIC    LRT Pr(>Chi)   
    ## <none>              306.67 368.67                   
    ## factor(father) 29   352.99 356.99 46.320 0.021801 * 
    ## height          1   315.80 375.80  9.128 0.002517 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

With some fathers there is a higer chance for daughters of getting oc.
This might indicate an heredibility effect. (note some of the standard
errors are huge indicating problems with the fit of this model). If a
mare at the age of 3 is relatively high then the mare grew harder which
might cause problems.

## 3e

The step function can now be used. This will give model selection based
on the AIC. The LRT test is only shown, not used.

``` r
step(osteo1.fit,test="Chisq")
```

    ## Start:  AIC=372.9
    ## oc ~ factor(father) + factor(food) + factor(ground) + height
    ## 
    ##                  Df Deviance    AIC    LRT Pr(>Chi)   
    ## - factor(father) 29   350.89 360.89 45.994 0.023496 * 
    ## - factor(food)    2   305.36 369.36  0.462 0.793692   
    ## - factor(ground)  1   306.10 372.10  1.200 0.273289   
    ## <none>                304.90 372.90                   
    ## - height          1   314.50 380.50  9.600 0.001945 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Step:  AIC=360.89
    ## oc ~ factor(food) + factor(ground) + height
    ## 
    ##                  Df Deviance    AIC    LRT Pr(>Chi)   
    ## - factor(food)    2   351.06 357.06 0.1639 0.921312   
    ## - factor(ground)  1   352.76 360.76 1.8636 0.172210   
    ## <none>                350.89 360.89                   
    ## - height          1   359.84 367.84 8.9425 0.002786 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Step:  AIC=357.06
    ## oc ~ factor(ground) + height
    ## 
    ##                  Df Deviance    AIC    LRT Pr(>Chi)   
    ## - factor(ground)  1   352.99 356.99 1.9343 0.164291   
    ## <none>                351.06 357.06                   
    ## - height          1   359.91 363.91 8.8519 0.002928 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Step:  AIC=356.99
    ## oc ~ height
    ## 
    ##          Df Deviance    AIC    LRT Pr(>Chi)   
    ## <none>        352.99 356.99                   
    ## - height  1   361.41 363.41 8.4226 0.003706 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Call:  glm(formula = oc ~ height, family = binomial, data = lr5)
    ## 
    ## Coefficients:
    ## (Intercept)       height  
    ##    -17.3165       0.0944  
    ## 
    ## Degrees of Freedom: 439 Total (i.e. Null);  438 Residual
    ## Null Deviance:       361.4 
    ## Residual Deviance: 353   AIC: 357

## 3f

Variable father is thrown out first, since this leads to the lowest AIC;
then food and ground are removed, leaving only height in the model. For
this final model the coefficients are given. An important message: model
building with LRT tests can be substantially different from model
building with AIC. So do not mix AIC with hypothesis testing.

## 3g
$\frac{L_1}{L_0}<0$ so $2log(\frac{L_1}{L_0})<2log(9)=4.3945$ and so $\alpha=0.065$ thus:

``` r
confint(glm(oc~height,family=binomial,data=lr5),level=1-0.065)
```

    ## Waiting for profiling to be done...

    ##                   2.5 %     97.5 %
    ## (Intercept) -28.2865831 -6.7577637
    ## height        0.0303029  0.1606951


# Exercise 4

# 4a

``` r
adress <- "https://raw.github.com/janvdbroek/Generalized-Linear-Models/master/lowbirth.dat"
lr2 <-read.table(adress,header=TRUE)
low.fit <- glm(low~age+lwt+factor(race)+smoke+factor(ptl)+
                 ht+ui+factor(ftv),family=binomial,data=lr2)
step(low.fit)
```

    ## Start:  AIC=220.6
    ## low ~ age + lwt + factor(race) + smoke + factor(ptl) + ht + ui + 
    ##     factor(ftv)
    ## 
    ##                Df Deviance    AIC
    ## - factor(ftv)   5   192.45 214.45
    ## - age           1   189.58 219.58
    ## <none>              188.60 220.60
    ## - smoke         1   191.20 221.20
    ## - factor(race)  2   193.23 221.23
    ## - ui            1   191.32 221.32
    ## - lwt           1   193.13 223.13
    ## - ht            1   194.72 224.72
    ## - factor(ptl)   3   202.21 228.21
    ## 
    ## Step:  AIC=214.45
    ## low ~ age + lwt + factor(race) + smoke + factor(ptl) + ht + ui
    ## 
    ##                Df Deviance    AIC
    ## - age           1   193.59 213.59
    ## <none>              192.45 214.45
    ## - ui            1   195.67 215.67
    ## - factor(race)  2   197.91 215.91
    ## - smoke         1   196.91 216.91
    ## - lwt           1   198.05 218.05
    ## - ht            1   199.64 219.64
    ## - factor(ptl)   3   203.95 219.95
    ## 
    ## Step:  AIC=213.59
    ## low ~ lwt + factor(race) + smoke + factor(ptl) + ht + ui
    ## 
    ##                Df Deviance    AIC
    ## <none>              193.59 213.59
    ## - ui            1   197.17 215.17
    ## - factor(race)  2   200.27 216.27
    ## - smoke         1   198.40 216.40
    ## - factor(ptl)   3   204.22 218.22
    ## - lwt           1   200.29 218.29
    ## - ht            1   200.94 218.94

    ## 
    ## Call:  glm(formula = low ~ lwt + factor(race) + smoke + factor(ptl) + 
    ##     ht + ui, family = binomial, data = lr2)
    ## 
    ## Coefficients:
    ##   (Intercept)            lwt  factor(race)2  factor(race)3          smoke  
    ##       0.03037       -0.01717        1.24887        0.79671        0.88537  
    ##  factor(ptl)1   factor(ptl)2   factor(ptl)3             ht             ui  
    ##       1.45787        0.27385      -14.74456        1.89821        0.89421  
    ## 
    ## Degrees of Freedom: 188 Total (i.e. Null);  179 Residual
    ## Null Deviance:       234.7 
    ## Residual Deviance: 193.6     AIC: 213.6

Only age and ftv can be left out.
