---
title: "Conjoint Analysis of Chocolate Preferences"
author: "Calvin Yen"
date: "2/12/2022"
output: 
  html_document:
    keep_md: true
---



## Exploratory Data Analysis

87 respondents participated in a conjoint study to understand consumer chocolate preferences.  Five product attributes were studied:

* kind - chocolate type (milk, walnut, delicacies, dark)
* price - low, average, high
* packing - product packaging (paperback, hardback)
* weight - light, middle, heavy
* calorie - caloric content of the chocolate (little, much)

The chocolate dataset comprises five data frames:

* clevn - vector of attribute level names
* cpref - raw vector of profile preferences for all 87 respondents (87 respondents with ratings for each of the 16 profiles)
* cprefm - matrix of profile preferences (87 respondents with ratings for each of the 16 profiles)
* cprof - matrix of profiles (16 profiles with 5 attributes each)
* csimp - matrix of simulation profiles


```r
data(chocolate, package='conjoint')

# Attribute levels
dim(clevn)
```

```
## [1] 14  1
```

```r
head(clevn)
```

```
##       levels
## 1       milk
## 2     walnut
## 3 delicaties
## 4       dark
## 5        low
## 6    average
```

```r
# Respondent profile preferences - raw
dim(cpref)
```

```
## [1] 1392    1
```

```r
head(cpref)
```

```
##   pref
## 1   14
## 2   15
## 3    5
## 4    2
## 5    1
## 6   11
```

```r
# Respondent profile preferences
dim(cprefm)
```

```
## [1] 87 16
```

```r
head(cprefm)
```

```
##   profile1 profile2 profile3 profile4 profile5 profile6 profile7 profile8
## 1       14       15        5        2        1       11        3       10
## 2       16       15        7       14        6        3       12        8
## 3        7        8        9       13       14        1       16        2
## 4        9       10       11       14       15        8        3        2
## 5        7        1        4       12       16        8       15        2
## 6       14       16       15        7        8        9       11       10
##   profile9 profile10 profile11 profile12 profile13 profile14 profile15
## 1       16        13        12         7         6         9         4
## 2        1         4        13         9         5        10        11
## 3        5         6         4        10        11         3        15
## 4        7         1         4         6         5        12        16
## 5        3         5        11        14        10         6        13
## 6       13        12         1         3         2         5         6
##   profile16
## 1         8
## 2         2
## 3        12
## 4        13
## 5         9
## 6         4
```

```r
# Product profiles
dim(cprof)
```

```
## [1] 16  5
```

```r
head(cprof)
```

```
##   kind price packing weight calorie
## 1    3     3       1      2       1
## 2    3     2       2      1       2
## 3    2     1       2      1       1
## 4    4     1       2      1       1
## 5    4     2       2      2       1
## 6    1     1       2      2       2
```

```r
# Simulated product profiles
dim(csimp)
```

```
## [1] 4 5
```

```r
head(csimp)
```

```
##   kind price packing weight calorie
## 1    4     1       2      2       1
## 2    1     3       1      1       2
## 3    2     2       1      3       1
## 4    3     1       1      1       1
```

```r
# Convert rankings to scores
cprefm <- caRankToScore(cprefm)
```

## Partworth Estimation

Partworth estimation was performed at individual, aggregate, and segment levels.  The individual-level model significantly outperformed both the aggregate and segment-level models.  

### Individual-level estimation

Partworth utilities were first estimated at an individual level.  Profiles were then predicted for each individual by taking the profile with the highest utility.  Profile selections were correctly predicted for 54 of 87 (62%) of respondents. 


```r
# Compute partworth utilities for each individual
indiv_partutilities <- caPartUtilities(y=cprefm, x=cprof, z=clevn)
head(indiv_partutilities)
```

```
##      intercept  milk walnut delicaties  dark    low average   high
## [1,]     8.417 -2.00   2.00      -6.00  6.00  0.000   0.250 -0.250
## [2,]     8.500  0.00   2.75      -0.50 -2.25  2.667  -0.333 -2.333
## [3,]     8.375  6.00  -2.00       2.00 -6.00  0.667   0.167 -0.833
## [4,]     8.792  2.00  -0.25       1.75 -3.50 -2.333  -0.333  2.667
## [5,]     8.167  1.75  -0.75       4.50 -5.50  1.333   0.583 -1.917
## [6,]     8.708  2.25   2.50      -5.25  0.50 -0.500  -0.875  1.375
##      paperback hardback  light middle  heavy little   much
## [1,]     0.000    0.000  0.333  0.083 -0.417  1.000 -1.000
## [2,]    -0.500    0.500 -2.667  1.083  1.583 -0.250  0.250
## [3,]    -0.375    0.375 -0.167 -0.042  0.208  0.375 -0.375
## [4,]     0.750   -0.750  1.167 -2.458  1.292 -0.125  0.125
## [5,]    -0.375    0.375  0.000 -1.500  1.500  0.750 -0.750
## [6,]     0.375   -0.375 -0.333 -0.333  0.667 -0.625  0.625
```

```r
# Compute total utility for each individual
indiv_totalutilities <- caTotalUtilities(y=cprefm, x=cprof)
head(indiv_totalutilities)
```

```
##        [,1]   [,2]   [,3]   [,4]   [,5]   [,6]   [,7]   [,8]   [,9]  [,10]
## [1,]  3.250  2.000 11.750 15.750 15.750  5.500 13.500  7.250  1.000  3.750
## [2,]  6.000  5.750 11.500  6.500  7.250 13.000  1.000  9.000 13.000  7.250
## [3,]  9.500 10.375  7.625  3.625  3.250 15.000  0.625 14.750 11.250 10.875
## [4,] 11.375 10.750  6.500  3.250  1.625  5.375 10.000 12.375  8.875 10.000
## [5,]  9.625 12.875  9.875  5.125  2.875  9.375 -0.375 12.375 15.125 14.375
## [6,]  4.250  2.500  9.375  7.375  7.000 10.375 11.250 10.500  3.875  2.375
##       [,11]  [,12]  [,13]  [,14]  [,15]  [,16]
## [1,]  5.500 10.000 10.750  7.750 13.000  9.500
## [2,]  4.250  8.000 10.750  7.750 10.250 14.750
## [3,] 13.375  5.625  6.500 14.875  2.500  6.250
## [4,] 14.000 10.250 11.625 10.250  5.125  4.625
## [5,]  7.625  6.875  8.125 11.625  4.375  6.125
## [6,] 12.250 11.000 12.250  9.875 10.375 11.375
```

```r
# Predict the profile chosen by each individual
indiv_profile_pred <- apply(indiv_totalutilities, 1, which.max)

# Find the profile most preferred by each individual
indiv_profile_actual <- apply(cprefm, 1, which.max)

# Find proportion of correct predictions
sum(indiv_profile_pred == indiv_profile_actual) / length(indiv_profile_actual)
```

```
## [1] 0.6206897
```

```r
# Determine attribute importances for first 10 individuals
attr_importances <- caImportance(y=cprefm[1,], cprof)
attr_importances <- data.frame(t(as.matrix(attr_importances)))
colnames(attr_importances) <- colnames(cprof)

num_profiles <- 10
for (i in 2:num_profiles) {
  attr_imp_i <- caImportance(y=cprefm[i,], cprof)
  attr_importances <- rbind(attr_importances, attr_imp_i)
}
attr_importances
```

```
##     kind price packing weight calorie
## 1  78.69  3.28    0.00   4.92   13.11
## 2  31.75 31.75    6.35  26.98    3.17
## 3  78.05  9.76    4.88   2.44    4.88
## 4  34.38 31.25    9.38  23.44    1.56
## 5  54.05 17.57    4.05  16.22    8.11
## 6  59.62 17.31    5.77   7.69    9.62
## 7  68.33 13.33    1.67  13.33    3.33
## 8  58.65 23.31    7.52   6.02    4.51
## 9  48.15 20.37   20.37   9.26    1.85
## 10 39.46 20.41    8.16  27.89    4.08
```


### Aggregate-level estimation

Partworth utilities were then estimated at an aggregate level.  In this model, all individuals are assumed to have homogeneous preferences and hence share the same utility coefficients. Using the aggregate model, we find that profile 14 is the most desirable.  Since the respondents' chocolate profile preferences are diffuse and spread across several profiles, the aggregate model does not perform well with only 15% correctness.    


```r
# Profiles most preferred by respondents
table(indiv_profile_actual)
```

```
## indiv_profile_actual
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 
##  9  2 13  1  6  6  1 11  6  8  2  2  2 13  2  3
```

```r
# Compute partworth utilities at an aggregate level
# First method - use caUtilities function
agg_partutilities1 <- caUtilities(y=cprefm, x=cprof, z=clevn)
```

```
## 
## Call:
## lm(formula = frml)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -9,8190 -3,4799 -0,4799  3,5546 11,3305 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         8,31513    0,12648  65,744  < 2e-16 ***
## factor(x$kind)1     1,08908    0,19815   5,496 4,62e-08 ***
## factor(x$kind)2     0,73276    0,19815   3,698 0,000226 ***
## factor(x$kind)3     0,92241    0,19815   4,655 3,55e-06 ***
## factor(x$price)1    0,57088    0,15254   3,743 0,000190 ***
## factor(x$price)2   -0,11877    0,17887  -0,664 0,506777    
## factor(x$packing)1  0,02874    0,11440   0,251 0,801714    
## factor(x$weight)1   0,16858    0,15254   1,105 0,269272    
## factor(x$weight)2  -0,17337    0,17887  -0,969 0,332575    
## factor(x$calorie)1  0,64655    0,11440   5,652 1,93e-08 ***
## ---
## Signif. codes:  0 '***' 0,001 '**' 0,01 '*' 0,05 '.' 0,1 ' ' 1
## 
## Residual standard error: 4,268 on 1382 degrees of freedom
## Multiple R-squared:  0,1488,	Adjusted R-squared:  0,1433 
## F-statistic: 26,85 on 9 and 1382 DF,  p-value: < 2,2e-16
```

```r
# Second method - average individual partworth utilities
agg_partutilities2 <- apply(indiv_partutilities, 2, mean)
agg_partutilities2
```

```
##    intercept         milk       walnut   delicaties         dark 
##  8.315137931  1.089080460  0.732758621  0.922413793 -2.744252874 
##          low      average         high    paperback     hardback 
##  0.570965517 -0.118689655 -0.452022989  0.028735632 -0.028735632 
##        light       middle        heavy       little         much 
##  0.168586207 -0.173367816  0.004793103  0.646551724 -0.646551724
```

```r
# Calculate estimated profile
# Dummy code the matrix of profiles
cprof_factor <- data.frame(lapply(cprof, as.factor))
cprof_dummyenc <- model.matrix(~., cprof_factor)

# Match attribute names between dummy-encoded profile matrix and utility coefficients
attr_names <- c('(Intercept)')
for (col in colnames(cprof)) {
  col_values <- unique(cprof[[col]])
  attr_name <- paste(col, sort(col_values), sep='')
  attr_names <- c(attr_names, attr_name)
}

names(agg_partutilities1) <- attr_names
agg_partutilities <- agg_partutilities1[colnames(cprof_dummyenc)]

# Multiply profile codes by utility coefficients to determine the utility of each profile
profile_utilities <- cprof_dummyenc %*% agg_partutilities
profile_utilities
```

```
##         [,1]
## 1   9.287356
## 2   8.612069
## 3  10.405172
## 4   6.928161
## 5   5.896552
## 6   9.126437
## 7   4.669540
## 8   9.965517
## 9   9.137931
## 10 10.652299
## 11  8.445402
## 12  8.479885
## 13  9.218391
## 14 10.818966
## 15  5.528736
## 16  8.827586
```

```r
# Find profile with the highest utility
best_profile <- which.max(profile_utilities)
best_profile
```

```
## [1] 14
```

```r
# Find proportion of respondents that chose the profile predicted by the model
sum(indiv_profile_actual == best_profile) / length(indiv_profile_actual)
```

```
## [1] 0.1494253
```
## Segmentation

Respondents were also segmented based on their individual-level partworth utilities. Chocolate type (e.g., milk, walnut, dark) and price were the most relevant factors in determining consumer preferences. The following segments were identified:

* Segment 1: Milk and walnut chocolate lovers.  These consumers like light chocolates and are not averse to high-calorie sweets. 
* Segment 2: Dark chocolate aficianados.  These consumers have moderate price sensitivity and prefer chocolates in medium price ranges. 
* Segment 3: Eat-any-chocolate consumers.  These consumers like chocolates of any kind with a strong preference for high-end chocolates. 
* Segment 4: Opinionated eaters.  These consumers love dark chocolate but dislike other chocolate types, with a particularly strong aversion to milk chocolate. 
* Segment 5: Delicacy connoisseurs.  These consumers strongly prefer handmade chocolate delicacies over all other types of chocolate. 


```r
# Product attribute importances
caImportance(y=cprefm, x=cprof)
```

```
## [1] 56.79 16.42  5.43 10.61 10.75
```

```r
# Try and evaluate K-Means with varying numbers of clusters
max_k <- 10
ss_ratios <- data.frame(numeric(0), numeric(0))

for (k in 2:max_k) {
  km <- kmeans(indiv_partutilities, centers=k, nstart=20)
  ss_ratio <- km$betweenss / km$totss
  ss_ratios <- rbind(ss_ratios, c(k, ss_ratio))
}

colnames(ss_ratios) <- c('num_clusters', 'ss_ratio')

# Plot SS ratios to find optimal number of clusters
ggplot(ss_ratios, aes(x=num_clusters, y=ss_ratio)) +
  geom_line(col='blue', linetype='dotted') +
  geom_point() + 
  labs(title='KMeans Clustering: BSS/TSS Ratio',
       x='Number of Clusters',
       y='BetweenSS / TotalSS')
```

![](MSDS450_Assignment4_CY_files/figure-html/clustering-1.png)<!-- -->

```r
best_k <- kmeans(indiv_partutilities, centers=5, nstart=20)

# Visualize clustering results
# Run PCA to reduce dimensions to 2D for cluster visualization
pca <- prcomp(indiv_partutilities)
summary(pca)
```

```
## Importance of components:
##                           PC1    PC2    PC3     PC4     PC5     PC6
## Standard deviation     4.4551 3.8908 3.2061 2.02640 1.69095 1.41905
## Proportion of Variance 0.3481 0.2655 0.1803 0.07201 0.05015 0.03532
## Cumulative Proportion  0.3481 0.6136 0.7938 0.86587 0.91601 0.95133
##                            PC7     PC8     PC9      PC10      PC11
## Standard deviation     1.08261 0.98351 0.79747 0.0004591 0.0004119
## Proportion of Variance 0.02055 0.01696 0.01115 0.0000000 0.0000000
## Cumulative Proportion  0.97188 0.98885 1.00000 1.0000000 1.0000000
##                             PC12      PC13      PC14      PC15
## Standard deviation     0.0002657 7.331e-16 2.112e-16 4.122e-17
## Proportion of Variance 0.0000000 0.000e+00 0.000e+00 0.000e+00
## Cumulative Proportion  1.0000000 1.000e+00 1.000e+00 1.000e+00
```

```r
pc_clusters <- data.frame(PC1=pca$x[, 'PC1'], PC2=pca$x[, 'PC2'], cluster=best_k$cluster)

ggplot(pc_clusters, aes(x=PC1, y=PC2)) +
  geom_point(aes(color=factor(cluster))) +
  labs(title='Chocolate Preference Cluster Results') +
  scale_color_brewer(palette='RdYlBu') +
  theme(axis.text=element_blank())
```

![](MSDS450_Assignment4_CY_files/figure-html/clustering-2.png)<!-- -->

## Cluster-based Model

The results of the cluster analysis were used to build a cluster-based model--respondents' chocolate preferences were predicted based on their cluster (i.e., segment). The cluster-level model outperformed the aggregate model, but does not sufficiently capture individualized preferences.  Increasing the number of clusters may raise this model's performance.  


```r
# Find utility coefficients for each segment
cluster_coef <- best_k$centers
cluster_coef
```

```
##   intercept       milk     walnut delicaties      dark       low
## 1  8.378071  1.7857143  1.3392857 -4.5000000  1.375000 0.1549286
## 2  7.952286  0.8035714  0.2321429  0.3750000 -1.410714 2.2739286
## 3  8.513889 -3.1111111 -3.2222222  2.5000000  3.833333 0.1667778
## 4  8.432292 -1.2291667  2.0208333  4.3020833 -5.093750 0.1597500
## 5  8.299692  4.4615385  0.8557692  0.4711538 -5.788462 0.3975000
##       average       high   paperback    hardback       light     middle
## 1 -0.37185714  0.2174286 -0.18750000  0.18750000  0.33335714  0.5297857
## 2  0.19357143 -2.4671429  0.16071429 -0.16071429 -0.08335714 -0.3422857
## 3  0.04177778 -0.2082222 -0.18055556  0.18055556 -0.22233333  0.1387778
## 4 -0.26212500  0.1024583  0.12500000 -0.12500000  0.11112500 -0.3888750
## 5 -0.07365385 -0.3236538  0.05769231 -0.05769231  0.40388462 -0.3701538
##         heavy    little       much
## 1 -0.86307143 0.2053571 -0.2053571
## 2  0.42557143 2.2321429 -2.2321429
## 3  0.08322222 0.5416667 -0.5416667
## 4  0.27779167 0.2239583 -0.2239583
## 5 -0.03361538 0.4567308 -0.4567308
```

```r
colnames(cluster_coef) <- attr_names
cluster_coef <- cluster_coef[, colnames(cluster_coef) %in% colnames(cprof_dummyenc)]

# Multiply profile codes by utility coefficients to determine the utility of each profile
cluster_prof_utilities <- cprof_dummyenc %*% t(cluster_coef)
cluster_prof_utilities
```

```
##            1         2         3         4         5
## 1   4.642643  7.910357 11.305222 12.796833  8.591269
## 2   3.821714  6.044643 10.472222 12.234417  8.586654
## 3  10.598500 12.446429  5.958333 10.822958 10.355885
## 4  10.634214 10.803571 13.013889  3.708375  3.711654
## 5  10.303857  8.464286 13.250000  2.786500  2.466462
## 6  10.830643  8.294643  5.347222  6.625042 12.274154
## 7   9.910500  1.919286 11.194111  3.453083  2.192231
## 8   8.946643 11.767929  5.889222  7.567708 13.168269
## 9   3.152000  8.634000 10.903111 12.822917  8.620192
## 10  4.384214 12.910714 11.319444 13.354208 10.086654
## 11 10.696214  3.812143  4.610778  7.067667 12.326846
## 12  9.286000  6.223214  4.388889 10.203167  9.086654
## 13  9.464000  8.214000  5.888889 10.932208  9.196923
## 14 10.669929 13.339286  5.708333  7.822958 14.077038
## 15  8.652000  7.169714 11.875333  3.677083  2.475962
## 16 10.009214  8.044643  4.875000 10.125042  8.783769
```

```r
# Find the profile that maximizes each segment's utility
cluster_profiles <- apply(cluster_prof_utilities, 2, which.max)
cluster_profiles
```

```
##  1  2  3  4  5 
##  6 14  5 10 14
```

```r
cprefm$cluster <- best_k$cluster
cprefm$cluster_profile <- cluster_profiles[cprefm$cluster]

sum(indiv_profile_actual == cprefm$cluster_profile) / length(indiv_profile_actual)
```

```
## [1] 0.2413793
```
