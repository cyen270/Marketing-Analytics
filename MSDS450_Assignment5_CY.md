---
title: "MSDS 450 - Assignment 5"
author: "Calvin Yen, Vivian Jiang, Saud Khan, Nikhila Vudattu"
date: "2/24/2022"
output: 
  html_document:
    toc: true
    keep_md: true
---



## Assignment 5 - CBC Analysis for Star Technologies

### Data Preparation

Start by effects coding dummy data. 


```r
# Function to effects code an attribute matrix

effects_code <- function(attr_mat) {
  num_attr <- ncol(attr_mat)
  ef_attr_mat <- matrix(data=NA, nrow=nrow(attr_mat), ncol=1)
  for (i in 1:num_attr) {
    attr <- attr_mat[, i]
    dc_attr <- dummy(attr)
    ref_levels <- dc_attr[, 1]
    dc_attr <- dc_attr[, -1]
    if (!is.matrix(dc_attr)) {
      dc_attr <- matrix(dc_attr, ncol=1)
    }
    dc_attr[ref_levels==1, ] <- -1
    ef_attr_mat <- cbind(ef_attr_mat, dc_attr)
  }
  ef_attr_mat <- ef_attr_mat[, -1]
  colnames(ef_attr_mat) <- NULL
  return(ef_attr_mat)
}
```

Add interactions terms for brand and price. 


```r
profiles <- cbc_data[, c('screen', 'RAM', 'processor', 'price', 'brand')]
X.mat <- effects_code(profiles)

pricevec <- cbc_data$price - mean(cbc_data$price)
X.brands <- X.mat[, 9:11]
X.BrandByPrice <- X.brands * pricevec

X.matrix <- cbind(X.mat, X.BrandByPrice)


ydata <- resp.data.v3[, 4:39]
colnames(ydata)
```

```
##  [1] "DCM1_1"  "DCM1_2"  "DCM1_3"  "DCM1_4"  "DCM1_5"  "DCM1_6"  "DCM1_7" 
##  [8] "DCM1_8"  "DCM1_9"  "DCM1_10" "DCM1_11" "DCM1_12" "DCM1_13" "DCM1_14"
## [15] "DCM1_15" "DCM1_16" "DCM1_17" "DCM1_18" "DCM1_19" "DCM1_20" "DCM1_21"
## [22] "DCM1_22" "DCM1_23" "DCM1_24" "DCM1_25" "DCM1_26" "DCM1_27" "DCM1_28"
## [29] "DCM1_29" "DCM1_30" "DCM1_31" "DCM1_32" "DCM1_33" "DCM1_34" "DCM1_35"
## [36] "DCM1_36"
```

```r
ydata <- na.omit(ydata)
ydata <- as.matrix(ydata)

# Indicator of whether respondent has owned an STC product
zowner <- as.numeric(!is.na(resp.data.v3$vList3))


# Prepare data for hierarchical Bayes model
lgtdata <- NULL
for (i in 1:nrow(ydata)) {
  lgtdata[[i]] <- list(y=ydata[i,], X=X.matrix)
}
```

### Fitting the Hierarchical Bayes Model without a covariate


```r
# Fit model with first 100 respondents as a test run
lgtdata100 <- lgtdata[1:100]
mcmctest <- list(R=5000, keep=5)

Data1 <- list(p=3, lgtdata=lgtdata100)

testrun1 <- rhierMnlDP(Data=Data1, Mcmc=mcmctest)
```

```
## Z not specified
## Table of Y values pooled over all units
## ypooled
##    1    2    3 
## 1070 1040 1490 
##  
## Starting MCMC Inference for Hierarchical Logit:
##    Dirichlet Process Prior
##    3  alternatives;  14  variables in X
##    for  100  cross-sectional units
##  
##  Prior Parms: 
##   G0 ~ N(mubar,Sigma (x) Amu^-1)
##    mubar =  0
##    Sigma ~ IW(nu,nu*v*I)
##    Amu ~ uniform[ 0.01 , 10 ]
##    nu ~ uniform on log grid  [ 14.01005 , 33.08554 ]
##    v ~ uniform[ 0.1 , 4 ]
##  
##   alpha ~ (1-(alpha-alphamin)/(alphamax-alphamin))^power
##    Istarmin =  1
##    Istarmax =  10
##    alphamin =  0.10834
##    alphamax =  1.833977
##    power =  0.8
##  
##  
## MCMC Parms: 
## s= 0.636  w=  0.1  R=  5000  keep=  5  nprint=  100  maxuniq=  200  gridsize for lambda hyperparms=  20
## 
## initializing Metropolis candidate densities for  100  units ...
##   completed unit # 50
##   completed unit # 100
##  MCMC Iteration (est time to end - min) 
##  100 (0.0)
##  200 (0.0)
##  300 (0.3)
##  400 (0.2)
##  500 (0.1)
##  600 (0.1)
##  700 (0.1)
##  800 (0.2)
##  900 (0.2)
##  1000 (0.1)
##  1100 (0.1)
##  1200 (0.1)
##  1300 (0.1)
##  1400 (0.1)
##  1500 (0.1)
##  1600 (0.1)
##  1700 (0.1)
##  1800 (0.1)
##  1900 (0.1)
##  2000 (0.1)
##  2100 (0.1)
##  2200 (0.1)
##  2300 (0.1)
##  2400 (0.1)
##  2500 (0.1)
##  2600 (0.1)
##  2700 (0.1)
##  2800 (0.1)
##  2900 (0.1)
##  3000 (0.1)
##  3100 (0.1)
##  3200 (0.1)
##  3300 (0.1)
##  3400 (0.1)
##  3500 (0.1)
##  3600 (0.1)
##  3700 (0.0)
##  3800 (0.0)
##  3900 (0.0)
##  4000 (0.0)
##  4100 (0.0)
##  4200 (0.0)
##  4300 (0.0)
##  4400 (0.0)
##  4500 (0.0)
##  4600 (0.0)
##  4700 (0.0)
##  4800 (0.0)
##  4900 (0.0)
##  5000 (0.0)
##  Total Time Elapsed: 0.17
```

```r
# Get the sampled coefficient values
betadraw1 <- testrun1$betadraw
plot(density(betadraw1[1,1,901:1000]), width=1)
```

![](MSDS450_Assignment5_CY_files/figure-html/model1-1.png)<!-- -->

```r
summary(betadraw1[1,1,901:100])
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
## -0.5459  0.4866  0.8671  0.9730  1.2941  4.0840
```

```r
# Average coefficient values
apply(betadraw1[,,901:1000], 2, mean)
```

```
##  [1] -0.30691022  0.19776868  0.02999645  0.71479551  1.01944134
##  [6]  1.26587722  0.29668564 -2.55942764 -0.15675558 -0.13374023
## [11] -0.34596633 -0.05246443  0.16650887  0.10435250
```

```r
# Percentiles for coefficient values
apply(betadraw1[,,901:1000], 2, quantile, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
```

```
##           [,1]       [,2]        [,3]       [,4]       [,5]       [,6]
## 5%  -1.5757179 -1.0232880 -0.84557588 -0.3678345 -0.3777533 -0.2141896
## 25% -0.7703770 -0.3233643 -0.30995639  0.2123487  0.3019725  0.4493004
## 50% -0.2921679  0.1843226  0.03908203  0.6529599  0.9537970  1.1251567
## 75%  0.1717840  0.6978621  0.37107532  1.1617712  1.6322563  1.9929373
## 95%  0.9425831  1.5214505  0.87577247  1.9780770  2.7856559  3.1721854
##           [,7]       [,8]       [,9]       [,10]      [,11]       [,12]
## 5%  -0.5943194 -7.3672321 -1.2736336 -2.73336051 -2.7001597 -1.00738860
## 25% -0.0794646 -4.0796063 -0.5771758 -0.57527765 -0.6966739 -0.41628080
## 50%  0.2687117 -1.8909173 -0.1552093  0.03856942 -0.1340053 -0.03862374
## 75%  0.6389177 -0.6054331  0.2619322  0.58935939  0.3217016  0.32398170
## 95%  1.2967846  0.3819343  0.9659735  1.60974302  0.9544168  0.85578305
##          [,13]       [,14]
## 5%  -0.7591357 -0.88205841
## 25% -0.2115427 -0.32965417
## 50%  0.1518790  0.06656539
## 75%  0.5398521  0.48930310
## 95%  1.1150988  1.22965761
```

### Fitting the Hierarchical Bayes Model with a covariate

```r
# Fit model with first 100 respondents as a test run

zownertest <- matrix(scale(zowner[1:100], scale=FALSE), ncol=1)

Data2 <- list(p=3, lgtdata=lgtdata100, Z=zownertest)

testrun2 <- rhierMnlDP(Data=Data2, Mcmc=mcmctest)
```

```
## Table of Y values pooled over all units
## ypooled
##    1    2    3 
## 1070 1040 1490 
##  
## Starting MCMC Inference for Hierarchical Logit:
##    Dirichlet Process Prior
##    3  alternatives;  14  variables in X
##    for  100  cross-sectional units
##  
##  Prior Parms: 
##   G0 ~ N(mubar,Sigma (x) Amu^-1)
##    mubar =  0
##    Sigma ~ IW(nu,nu*v*I)
##    Amu ~ uniform[ 0.01 , 10 ]
##    nu ~ uniform on log grid  [ 14.01005 , 33.08554 ]
##    v ~ uniform[ 0.1 , 4 ]
##  
##   alpha ~ (1-(alpha-alphamin)/(alphamax-alphamin))^power
##    Istarmin =  1
##    Istarmax =  10
##    alphamin =  0.10834
##    alphamax =  1.833977
##    power =  0.8
##  
## deltabar
##  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## Ad
##       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12] [,13]
##  [1,] 0.01 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00  0.00  0.00  0.00
##  [2,] 0.00 0.01 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00  0.00  0.00  0.00
##  [3,] 0.00 0.00 0.01 0.00 0.00 0.00 0.00 0.00 0.00  0.00  0.00  0.00  0.00
##  [4,] 0.00 0.00 0.00 0.01 0.00 0.00 0.00 0.00 0.00  0.00  0.00  0.00  0.00
##  [5,] 0.00 0.00 0.00 0.00 0.01 0.00 0.00 0.00 0.00  0.00  0.00  0.00  0.00
##  [6,] 0.00 0.00 0.00 0.00 0.00 0.01 0.00 0.00 0.00  0.00  0.00  0.00  0.00
##  [7,] 0.00 0.00 0.00 0.00 0.00 0.00 0.01 0.00 0.00  0.00  0.00  0.00  0.00
##  [8,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.01 0.00  0.00  0.00  0.00  0.00
##  [9,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.01  0.00  0.00  0.00  0.00
## [10,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.01  0.00  0.00  0.00
## [11,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00  0.01  0.00  0.00
## [12,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00  0.00  0.01  0.00
## [13,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00  0.00  0.00  0.01
## [14,] 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00 0.00  0.00  0.00  0.00  0.00
##       [,14]
##  [1,]  0.00
##  [2,]  0.00
##  [3,]  0.00
##  [4,]  0.00
##  [5,]  0.00
##  [6,]  0.00
##  [7,]  0.00
##  [8,]  0.00
##  [9,]  0.00
## [10,]  0.00
## [11,]  0.00
## [12,]  0.00
## [13,]  0.00
## [14,]  0.01
##  
## MCMC Parms: 
## s= 0.636  w=  0.1  R=  5000  keep=  5  nprint=  100  maxuniq=  200  gridsize for lambda hyperparms=  20
## 
## initializing Metropolis candidate densities for  100  units ...
##   completed unit # 50
##   completed unit # 100
##  MCMC Iteration (est time to end - min) 
##  100 (0.8)
##  200 (0.4)
##  300 (0.3)
##  400 (0.2)
##  500 (0.1)
##  600 (0.2)
##  700 (0.2)
##  800 (0.2)
##  900 (0.2)
##  1000 (0.2)
##  1100 (0.2)
##  1200 (0.2)
##  1300 (0.2)
##  1400 (0.2)
##  1500 (0.2)
##  1600 (0.2)
##  1700 (0.2)
##  1800 (0.1)
##  1900 (0.1)
##  2000 (0.1)
##  2100 (0.1)
##  2200 (0.1)
##  2300 (0.1)
##  2400 (0.1)
##  2500 (0.1)
##  2600 (0.1)
##  2700 (0.1)
##  2800 (0.1)
##  2900 (0.1)
##  3000 (0.1)
##  3100 (0.1)
##  3200 (0.1)
##  3300 (0.1)
##  3400 (0.1)
##  3500 (0.1)
##  3600 (0.1)
##  3700 (0.1)
##  3800 (0.1)
##  3900 (0.0)
##  4000 (0.0)
##  4100 (0.0)
##  4200 (0.0)
##  4300 (0.0)
##  4400 (0.0)
##  4500 (0.0)
##  4600 (0.0)
##  4700 (0.0)
##  4800 (0.0)
##  4900 (0.0)
##  5000 (0.0)
##  Total Time Elapsed: 0.22
```

```r
# Get the sampled coefficient values
Deltadraw <- testrun2$Deltadraw

# Average coefficient values
apply(Deltadraw[901:1000, ], 2, mean)
```

```
##  [1] -0.297296445  0.242468480  0.104336287  0.013975590  0.767124444
##  [6]  1.100496924 -0.172233547 -1.086581224 -0.520671629  1.776120181
## [11]  0.535707746 -0.326439544  0.009221397  0.497417096
```

```r
# Percentiles for coefficient values
apply(Deltadraw[901:1000, ], 2, quantile, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
```

```
##            [,1]       [,2]        [,3]        [,4]      [,5]      [,6]
## 5%  -0.76659126 -0.1759795 -0.20260548 -0.37287773 0.3364867 0.4859344
## 25% -0.50513360  0.1046412 -0.03201358 -0.20115299 0.5392564 0.8982392
## 50% -0.28977556  0.2385985  0.10047832  0.01086489 0.7571821 1.0806998
## 75% -0.09434704  0.3786570  0.23670344  0.18146373 0.9622495 1.3123683
## 95%  0.10142480  0.6567911  0.40101103  0.49694524 1.2565955 1.6746068
##            [,7]       [,8]        [,9]    [,10]      [,11]      [,12]
## 5%  -0.58450217 -2.2371424 -0.92174360 1.167904 0.05260548 -0.8570265
## 25% -0.32490719 -1.5944686 -0.67896672 1.562839 0.32526472 -0.5066239
## 50% -0.19052673 -0.9834096 -0.51628597 1.801999 0.56578151 -0.3365674
## 75% -0.01601003 -0.6224653 -0.34661980 1.984902 0.73709756 -0.1209220
## 95%  0.26648657 -0.1411201 -0.07421229 2.288928 0.96201424  0.1274615
##          [,13]     [,14]
## 5%  -0.4167066 0.1138457
## 25% -0.1777121 0.3257516
## 50%  0.0168621 0.4476752
## 75%  0.1888152 0.6798147
## 95%  0.3620777 0.9745525
```

### Evaluating Model

Models with and without the covariate performed similarly--AUC scores hovered around a range of 0.85 to 0.86. 


```r
##### EVALUATE MODEL WITH COVARIATE #####
betameans2 <- apply(testrun2$betadraw[,,701:1000], c(1,2), mean)

# Multiply product choice sets by respondent coefficients
xbeta <- X.matrix %*% t(betameans2)
xbeta2 <- matrix(xbeta, ncol=3, byrow=TRUE)


# Find probability of selecting each choice with softmax function
expxbeta2 <- exp(xbeta2)
rsumvec <- rowSums(expxbeta2)
pchoicemat <- expxbeta2 / rsumvec
custchoice <- max.col(pchoicemat)

# Evaluate predictions - confusion matrix
ydatavec <- as.vector(t(ydata[1:100, ]))
table(custchoice, ydatavec)
```

```
##           ydatavec
## custchoice    1    2    3
##          1  911   98   40
##          2   75  848   52
##          3   84   94 1398
```

```r
# Confusion matrix
conf_mat <- data.frame(table(custchoice, ydatavec))
ggplot(conf_mat, aes(x=custchoice, y=ydatavec, fill=Freq)) +
  geom_tile() +
  scale_fill_gradient(low='white', high='blue') +
  labs(title='Model with Covariate - Confusion Matrix',
       x='Predicted Profile', 
       y='Actual Profile')
```

![](MSDS450_Assignment5_CY_files/figure-html/prediction-1.png)<!-- -->

```r
# Plot ROC curve
roctest <- roc(ydatavec, custchoice, plot=TRUE)
```

![](MSDS450_Assignment5_CY_files/figure-html/prediction-2.png)<!-- -->

```r
auc(roctest)
```

```
## Area under the curve: 0.8497
```

```r
##### EVALUATE MODEL WITHOUT COVARIATE #####
betameans1 <- apply(testrun1$betadraw[,,701:1000], c(1,2), mean)

# Multiply product choice sets by respondent coefficients
xbeta <- X.matrix %*% t(betameans1)
xbeta2 <- matrix(xbeta, ncol=3, byrow=TRUE)


# Find probability of selecting each choice with softmax function
expxbeta2 <- exp(xbeta2)
rsumvec <- rowSums(expxbeta2)
pchoicemat <- expxbeta2 / rsumvec
custchoice <- max.col(pchoicemat)

# Evaluate predictions - confusion matrix
ydatavec <- as.vector(t(ydata[1:100, ]))
table(custchoice, ydatavec)
```

```
##           ydatavec
## custchoice    1    2    3
##          1  921  107   36
##          2   69  839   51
##          3   80   94 1403
```

```r
# Confusion matrix
conf_mat <- data.frame(table(custchoice, ydatavec))
ggplot(conf_mat, aes(x=custchoice, y=ydatavec, fill=Freq)) +
  geom_tile() +
  scale_fill_gradient(low='white', high='blue') +
  labs(title='Model without Covariate - Confusion Matrix',
       x='Predicted Profile', 
       y='Actual Profile')
```

![](MSDS450_Assignment5_CY_files/figure-html/prediction-3.png)<!-- -->

```r
# Plot ROC curve
roctest <- roc(ydatavec, custchoice, plot=TRUE)
```

![](MSDS450_Assignment5_CY_files/figure-html/prediction-4.png)<!-- -->

```r
auc(roctest)
```

```
## Area under the curve: 0.8517
```

### Predicting the additional scenarios


```r
# Get average coefficient values across all respondents
betameansoverall <- apply(testrun2$betadraw[,,701:1000], 2, mean)

Xextra.matrix <- as.matrix(test_scenarios[,c("V1","V2","V3","V4","V5","V6","V7","V8","V9",
                                            "V10","V11","V12","V13","V14")])

betavec <- matrix(betameansoverall, ncol=1, byrow=TRUE)

# Derive utilities for each profile
xextrabeta <- Xextra.matrix %*% betavec
xextrabeta2 <- matrix(xextrabeta, ncol=3, byrow=TRUE)

# Derive probabilities of choosing each profile
expxbetaextra2 <- exp(xextrabeta2)
rsumvec <- rowSums(expxbetaextra2)
pchoicemat <- expxbetaextra2/rsumvec
pchoicemat
```

```
##            [,1]      [,2]       [,3]
## [1,] 0.06708651 0.7635214 0.16939211
## [2,] 0.54192679 0.4532742 0.00479898
```
