Mode Choice Models
========================================================

This document contains the commands used to estimate and check mode choice models in `R`.

First, we load the data and fit it into the `mlogit` framework. We are using Bhat and Koppelman's data from the self-instructing course in discrete choice modeling developed for the FTA.

```r
library(mlogit)
load("WorkTrips.Rdata")
# Make new id field
worktrips$IDFIELD <- paste(worktrips$HHID, worktrips$PERID, sep = "-")
# Name alternatives
alternatives <- c(" Drive Alone", " Share 2", " Share 3+", " Transit", " Bike", 
    " Walk")
worktrips$ALTNUM <- factor(worktrips$ALTNUM, labels = alternatives)

# make mlogit data frame
logitdata <- mlogit.data(worktrips, choice = "CHOSEN", alt.var = "ALTNUM", chid.var = "IDFIELD", 
    shape = "long")
```



Our first basic model contains one generic variable, `COST`, or the cost of the mode in cents.


```r
base.mnl <- mlogit(CHOSEN ~ COST, data = logitdata)
summary(base.mnl)
```

```
## 
## Call:
## mlogit(formula = CHOSEN ~ COST, data = logitdata, method = "nr", 
##     print.level = 0)
## 
## Frequencies of alternatives:
##  Drive Alone      Share 2     Share 3+      Transit         Bike 
##      0.72321      0.10280      0.03201      0.09903      0.00994 
##         Walk 
##      0.03301 
## 
## nr method
## 6 iterations, 0h:0m:3s 
## g'(-H)^-1g = 1.34E-05 
## successive function values within tolerance limits 
## 
## Coefficients :
##                        Estimate Std. Error t-value Pr(>|t|)    
##  Share 2:(intercept)  -2.575571   0.052773   -48.8   <2e-16 ***
##  Share 3+:(intercept) -4.043236   0.090506   -44.7   <2e-16 ***
##  Transit:(intercept)  -2.288322   0.057054   -40.1   <2e-16 ***
##  Bike:(intercept)     -3.940994   0.149472   -26.4   <2e-16 ***
##  Walk:(intercept)     -2.526723   0.091041   -27.8   <2e-16 ***
## COST                  -0.005033   0.000238   -21.1   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -3810
## McFadden R^2:  0.215 
## Likelihood ratio test : chisq = 2090 (p.value = <2e-16)
```


What is the likelihood of this model, and how does it compare to the special market shares or null likelihoods?

```r
# Calculate model log-likelihood
llB <- sum(log(base.mnl$fitted.values))
llB
```

```
## [1] -3814
```

```r
base.mnl$logLik
```

```
## 'log Lik.' -3814 (df=6)
```

```r

# Calculate null log-likelihood
LL0 <- 5029 * log(1/6)
LL0
```

```
## [1] -9011
```

```r
# null rhosq
r20 <- 1 - llB/LL0
r20
```

```
## [1] 0.5767
```

```r

# market shares
llC <- sum(base.mnl$freq * log(prop.table(base.mnl$freq)))
llC
```

```
## [1] -4857
```

```r
# constants rhosq
r2c <- 1 - llB/llC
r2c
```

```
## [1] 0.2147
```


From this calculation, we can see that the `mlogit` package give $\rho_C^2$ as the summary default. This could be useful information. We can also estimate models with alternative-specific variables, separating the two types with the `|` ("pipe") character:

```r
model2 <- mlogit(CHOSEN ~ COST | AGE, data = logitdata)
summary(model2)
```

```
## 
## Call:
## mlogit(formula = CHOSEN ~ COST | AGE, data = logitdata, method = "nr", 
##     print.level = 0)
## 
## Frequencies of alternatives:
##  Drive Alone      Share 2     Share 3+      Transit         Bike 
##      0.72321      0.10280      0.03201      0.09903      0.00994 
##         Walk 
##      0.03301 
## 
## nr method
## 6 iterations, 0h:0m:4s 
## g'(-H)^-1g = 7.54E-05 
## successive function values within tolerance limits 
## 
## Coefficients :
##                        Estimate Std. Error t-value Pr(>|t|)    
##  Share 2:(intercept)  -1.743882   0.177206   -9.84  < 2e-16 ***
##  Share 3+:(intercept) -3.301696   0.302263  -10.92  < 2e-16 ***
##  Transit:(intercept)  -2.105827   0.195165  -10.79  < 2e-16 ***
##  Bike:(intercept)     -1.505528   0.552158   -2.73   0.0064 ** 
##  Walk:(intercept)     -2.631592   0.296327   -8.88  < 2e-16 ***
## COST                  -0.005059   0.000239  -21.13  < 2e-16 ***
##  Share 2:AGE          -0.021828   0.004548   -4.80  1.6e-06 ***
##  Share 3+:AGE         -0.019389   0.007731   -2.51   0.0121 *  
##  Transit:AGE          -0.004677   0.004841   -0.97   0.3340    
##  Bike:AGE             -0.069746   0.016584   -4.21  2.6e-05 ***
##  Walk:AGE              0.002924   0.007183    0.41   0.6840    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -3790
## McFadden R^2:  0.22 
## Likelihood ratio test : chisq = 2130 (p.value = <2e-16)
```


Does this model significantly improve model likelihood? We can compare the two models with a *Likelihood Ratio Test*,
$$ -2[\ln(\mathcal{L}_1) - \ln(\mathcal{L}_2)]\sim \chi^2$$

```r
# LRT
lrstat <- -2 * (base.mnl$logLik - model2$logLik)
lrstat
```

```
## 'log Lik.' 47.63 (df=6)
```

```r
# find p-value
pchisq(lrstat, df = 5, lower.tail = FALSE)
```

```
## 'log Lik.' 4.228e-09 (df=6)
```

```r

# Can also use function
lrtest(base.mnl, model2)
```

```
## Likelihood ratio test
## 
## Model 1: CHOSEN ~ COST
## Model 2: CHOSEN ~ COST | AGE
##   #Df LogLik Df Chisq Pr(>Chisq)    
## 1   6  -3814                        
## 2  11  -3790  5  47.6    4.2e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


Does this dataset exhibit the IIA property? If it does not, then the MNL model assumptions are violated and we may need a nested logit model. We test this by estimating the model on a subset of alternatives, and comparing the full model coefficients to the reduced model coefficients with a Hausman-McFadden test. The H-M test statistic is
$$ (\beta_1 - \beta_0)'(\Sigma_0 - \Sigma_1)(\beta_1 - \beta_0) \sim \chi^2$$
where $\Sigma$ is the variance-covariance matrix of the parameter estimates.


```r
# estimate a model on a subset of alternatives
model2.red <- mlogit(CHOSEN ~ COST | AGE, data = logitdata, alt.subset = alternatives[1:5])

# get coefficient names that the models have in common
regressors <- names(coef(model2.red))[which(names(coef(model2.red)) %in% names(coef(model2)))]
# get relevant estimate vectors
betas0 <- coef(model2.red)[regressors]
betas1 <- coef(model2)[regressors]
# get relevant variance-covariance matrices
vcov0 <- vcov(model2.red)[regressors, regressors]
vcov1 <- vcov(model2)[regressors, regressors]

# statistic
hmfstat <- abs((betas1 - betas0) %*% MASS::ginv(vcov0 - vcov1) %*% (betas1 - 
    betas0))
hmfstat
```

```
##      [,1]
## [1,] 19.8
```

```r

pchisq(hmfstat, length(regressors), lower.tail = FALSE)
```

```
##        [,1]
## [1,] 0.0192
```

```r

# Again, can use function
hmftest(model2, model2.red)
```

```
## 
## 	Hausman-McFadden test
## 
## data:  logitdata
## chisq = 19.8, df = 9, p-value = 0.0192
## alternative hypothesis: IIA is rejected
```


We reject the null hypothesis that the dataset exhibits IIA, and conclude we should use a nested logit model. Let's group the car alternatives into a nest, and group the other alternatives into a different nest.


```r
model2.nl <- mlogit(CHOSEN ~ COST + TVTT | 0, data = logitdata, nests = list(Car = alternatives[1:3], 
    Other = alternatives[4:6]), un.nest.el = FALSE)
summary(model2.nl)
```

```
## 
## Call:
## mlogit(formula = CHOSEN ~ COST + TVTT | 0, data = logitdata, 
##     nests = list(Car = alternatives[1:3], Other = alternatives[4:6]), 
##     un.nest.el = FALSE)
## 
## Frequencies of alternatives:
##  Drive Alone      Share 2     Share 3+      Transit         Bike 
##      0.72321      0.10280      0.03201      0.09903      0.00994 
##         Walk 
##      0.03301 
## 
## bfgs method
## 11 iterations, 0h:0m:8s 
## g'(-H)^-1g =  8.38 
## last step couldn't find higher value 
## 
## Coefficients :
##           Estimate Std. Error t-value Pr(>|t|)    
## COST     -2.31e-04   3.21e-05    -7.2  6.2e-13 ***
## TVTT     -6.45e-02   1.54e-03   -41.8  < 2e-16 ***
## iv.Car    1.35e-01   3.38e-03    39.9  < 2e-16 ***
## iv.Other  7.27e-01   3.62e-02    20.1  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -4430
```


What happens if we include alternative-specific constants in this model?

```r
model3.nl <- mlogit(CHOSEN ~ COST + TVTT | 1, data = logitdata, nests = list(Car = alternatives[1:3], 
    Other = alternatives[4:6]), un.nest.el = FALSE)
summary(model3.nl)
```

```
## 
## Call:
## mlogit(formula = CHOSEN ~ COST + TVTT | 1, data = logitdata, 
##     nests = list(Car = alternatives[1:3], Other = alternatives[4:6]), 
##     un.nest.el = FALSE)
## 
## Frequencies of alternatives:
##  Drive Alone      Share 2     Share 3+      Transit         Bike 
##      0.72321      0.10280      0.03201      0.09903      0.00994 
##         Walk 
##      0.03301 
## 
## bfgs method
## 13 iterations, 0h:0m:9s 
## g'(-H)^-1g = 5.58E-08 
## gradient close to zero 
## 
## Coefficients :
##                        Estimate Std. Error t-value Pr(>|t|)    
##  Share 2:(intercept)  -3.178028   0.151680  -20.95  < 2e-16 ***
##  Share 3+:(intercept) -5.048675   0.244912  -20.61  < 2e-16 ***
##  Transit:(intercept)  -0.984988   0.083507  -11.80  < 2e-16 ***
##  Bike:(intercept)     -2.325138   0.113556  -20.48  < 2e-16 ***
##  Walk:(intercept)     -0.553336   0.113881   -4.86  1.2e-06 ***
## COST                  -0.005338   0.000246  -21.71  < 2e-16 ***
## TVTT                  -0.048142   0.002781  -17.31  < 2e-16 ***
## iv.Car                 1.429100   0.072658   19.67  < 2e-16 ***
## iv.Other               0.469573   0.048302    9.72  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Log-Likelihood: -3590
## McFadden R^2:  0.261 
## Likelihood ratio test : chisq = 2530 (p.value = <2e-16)
```

The logsum parameter `iv.Car` is greater than one, implying a violation of utility theory.
