---
title: "Credit Card Customer Acquisition"
author: "Calvin Yen"
date: "3/5/2022"
output: 
  html_document:
    toc: true
    keep_md: true
  
---



### Data Cleaning


```r
data <- data.table(complete.customer.data.frame)

# Display structure of provided dataset on prospective credit card customers
str(data, list.len=10)
```

```
## Classes 'data.table' and 'data.frame':	30779 obs. of  554 variables:
##  $ ACCTNO               : chr  "AAAAPSSYY" "AAAASDQAA" "AAAASDQPY" "AAAASDQWP" ...
##  $ ZIP                  : chr  "60093" "60091" "60091" "60091" ...
##  $ ZIP4                 : chr  "3231" "" "2501" "1649" ...
##  $ LTD_SALES            : num  2160 2970 960 3513 174 ...
##  $ LTD_TRANSACTIONS     : num  10 3 4 18 2 7 1 4 2 2 ...
##  $ YTD_SALES_2009       : num  198 0 0 384 0 243 0 66 285 0 ...
##  $ YTD_TRANSACTIONS_2009: num  1 0 0 3 0 1 0 1 1 0 ...
##  $ CHANNEL_ACQUISITION  : chr  "RT" "IB" "RT" "RT" ...
##  $ BUYER_STATUS         : chr  "ACTIVE" "INACTIVE" "LAPSED" "ACTIVE" ...
##  $ ZIP9_Supercode       : chr  "600933231" "600911033" "600912501" "600911649" ...
##   [list output truncated]
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
### Evaluate dataset for missing data
# Replace blank strings with NAs
data[data == ''] <- NA

# Return columns with missing values
column_missingval <- apply(is.na(data), 2, sum)

# Flag columns that have more than 5% missing data
column_missingval <- column_missingval[column_missingval > 0.05*nrow(complete.customer.data.frame)]
head(column_missingval, 10)
```

```
##           ZIP4 READ_ALL_TYPES      BOOK_CLUB     VIDEO_CLUB      AMEX_PREM 
##           3667          10107          10107          10107          10107 
##       AMEX_REG       DEBIT_CC      DISC_PREM       DISC_REG  OTHER_PREM_CC 
##          10107          10107          10107          10107          10107
```

```r
# Remove columns with missing data
remaining_col <- setdiff(colnames(data), names(column_missingval))
data_c <- data[, .SD, .SDcols = remaining_col]
```

### Geospatial Analysis


```r
### Plot prospect data by geography

# Prepare geospatial data - convert provided latitude and longitude strings to numeric values
lat <- str_match(data_c$LAT, '(^[0-9]{3})([0-9]{6})([A-Z]{1})$')
long <- str_match(data_c$LONG, '(^[0-9]{3})([0-9]{6})([A-Z]{1})$')

lat[, 4] <- sapply(lat[, 4], function(x) if (x=='N') 1 else -1)
long[, 4] <- sapply(long[, 4], function(x) if (x=='E') 1 else -1)

lat_values <- as.numeric(paste(lat[, 2], lat[, 3], sep='.')) * as.numeric(lat[, 4])
long_values <- as.numeric(paste(long[, 2], long[, 3], sep='.')) * as.numeric(long[, 4])

# Replace latitude and longitude values in original dataset
data_c$LAT <- lat_values
data_c$LONG <- long_values

# Plot recipient locations on map
# Recipient density
center_loc <- c(lon = median(data_c$LONG)-0.1, lat = median(data_c$LAT)+0.1)
map <- qmap(location=center_loc, zoom=10)
map + 
  stat_density2d(data=data_c,
                 aes(x=LONG, y=LAT, fill=..level..), alpha=0.2, 
                 bins=40,
             geom='polygon') +
  labs(title='Distribution of Mail Campaign Recipients - Chicago Area') +
  theme(legend.position='none')
```

![](CC_Customer_Acquisition_files/figure-html/Geospatial-1.png)<!-- -->

```r
# Recipients by Experian segment
map +
  geom_point(data=data_c, 
             aes(x=LONG, y=LAT, color=INC_SCS_AMT_V4),
             alpha=0.1) +
  scale_color_gradient(low='gray', 
                       high='blue',
                       name='HH Income') +
  labs(title='Mail Campaign Recipients by Income Segment - Chicago Area') +
  theme(legend.position=c(0.88, 0.8))
```

![](CC_Customer_Acquisition_files/figure-html/Geospatial-2.png)<!-- -->

### Spending Analysis


```r
### Analyze attributes (e.g., spending, quantity, response rates) 

# Convert character columns to factor type to ease summarization
char_columns <- sapply(data_c, is.character)
char_columns <- names(char_columns[char_columns==TRUE])
data_c <- data_c %>% mutate_if(is.character, as.factor)
summary(data_c[, c(1:20)])
```

```
##        ACCTNO           ZIP          LTD_SALES      LTD_TRANSACTIONS 
##  AAAAPSSYY:    1   60091  : 3481   Min.   :     3   Min.   :  1.000  
##  AAAASDQAA:    1   60093  : 3226   1st Qu.:   225   1st Qu.:  1.000  
##  AAAASDQPY:    1   60062  : 3124   Median :   531   Median :  3.000  
##  AAAASDQWP:    1   60067  : 3086   Mean   :  1204   Mean   :  4.645  
##  AAAASHYLW:    1   60068  : 2823   3rd Qu.:  1269   3rd Qu.:  6.000  
##  AAAASYHQW:    1   (Other):15038   Max.   :122352   Max.   :194.000  
##  (Other)  :30773   NA's   :    1                                     
##  YTD_SALES_2009    YTD_TRANSACTIONS_2009 CHANNEL_ACQUISITION   BUYER_STATUS  
##  Min.   :    0.0   Min.   : 0.0000       CB: 1895            ACTIVE  :13602  
##  1st Qu.:    0.0   1st Qu.: 0.0000       IB: 4252            INACTIVE: 9161  
##  Median :    0.0   Median : 0.0000       RT:24632            LAPSED  : 8016  
##  Mean   :  224.6   Mean   : 0.8443                                           
##  3rd Qu.:  204.0   3rd Qu.: 1.0000                                           
##  Max.   :28002.0   Max.   :36.0000                                           
##                                                                              
##    ZIP9_Supercode       CRRT            DPBC             FILLER     
##  60062    :   31   C003   :  730   010    :  104   O 01YSYY :23452  
##  600933737:   19   C002   :  729   006    :  101   O 01YHYYN: 2510  
##  600692905:   16   C004   :  656   005    :   93   O 01YSCY : 2302  
##  600611235:   14   C001   :  619   016    :   86   O 01SSYY :  853  
##  600611243:   14   C024   :  615   007    :   85   O 01YBYY :  264  
##  600931003:   14   (Other):27325   (Other):30175   O 01YHCYN:  210  
##  (Other)  :30671   NA's   :  105   NA's   :  135   (Other)  : 1188  
##  INC_SCS_AMT_V4 INC_WIOUTSCS_V4 INC_WITHSCS_V4 INC_WOUTSCS_AMT_4 FIPSSTCD  
##  Min.   :  0    L      :5206    L      :5166   Min.   :  0.0     17:30779  
##  1st Qu.: 83    F      :4577    F      :4512   1st Qu.: 84.0               
##  Median :124    G      :4062    G      :4170   Median :124.0               
##  Mean   :137    E      :3882    J      :3823   Mean   :137.4               
##  3rd Qu.:192    J      :3765    E      :3728   3rd Qu.:192.0               
##  Max.   :250    H      :3377    H      :3321   Max.   :250.0               
##                 (Other):5910    (Other):6059                               
##  FIPSCNTY        TRACT           BLOCK      
##  031:22162   800500 :  891   1      :10953  
##  097: 7477   800700 :  829   2      : 8160  
##  111: 1140   804102 :  737   3      : 6708  
##              800400 :  722   4      : 3353  
##              801800 :  688   5      : 1297  
##              801100 :  670   (Other):  196  
##              (Other):26242   NA's   :  112
```

```r
par(mfrow=c(3,2))
for (col in c('LTD_SALES', 'LTD_TRANSACTIONS', 'YTD_SALES_2009', 'YTD_TRANSACTIONS_2009',
              'INC_SCS_AMT_V4', 'TOTAMT')) {
  hist_data <- data_c[[col]]
  hist_data <- hist_data[hist_data < quantile(hist_data, 0.95)]
  hist(hist_data, main=col, xlab='', col='blue')
}
```

![](CC_Customer_Acquisition_files/figure-html/EDA-1.png)<!-- -->

```r
# Summarize size, spending, and response rates for each segment
segment_breakdown <- data_c[, .(num_customers = .N), by=M_HH_LEVEL][order(-num_customers)]
kable(head(segment_breakdown, 10), align='l')
```



|M_HH_LEVEL |num_customers |
|:----------|:-------------|
|A02        |10988         |
|B03        |3546          |
|A06        |3018          |
|A01        |2707          |
|A03        |1788          |
|E03        |1773          |
|A05        |1209          |
|B06        |623           |
|B01        |585           |
|A07        |525           |

```r
segment_spending <- data_c[, .(median_spending = mean(LTD_SALES)), by=M_HH_LEVEL][order(-median_spending)]
kable(head(segment_spending, 10), align='l')
```



|M_HH_LEVEL |median_spending |
|:----------|:---------------|
|A01        |1715.0525       |
|A02        |1515.9149       |
|G01        |1443.0000       |
|C05        |1173.5294       |
|D01        |1136.7931       |
|B02        |1073.0385       |
|K03        |1066.8947       |
|E01        |1045.7391       |
|A06        |1014.4284       |
|H02        |999.7234        |

```r
segment_response <- data_c[, .(RESPONDENTS = sum(RESPONSE16),
                               CAMPAIGN16_RESP_RATE = sum(RESPONSE16) / sum(SUM_MAIL_16),
                           CAMPAIGN15_RESP_RATE = sum(RESPONSE15) / sum(SUM_MAIL_15),
                           CAMPAIGN14_RESP_RATE = sum(RESPONSE14) / sum(SUM_MAIL_14),
                           CAMPAIGN13_RESP_RATE = sum(RESPONSE13) / sum(SUM_MAIL_13)),
                           by=M_HH_LEVEL][order(-CAMPAIGN16_RESP_RATE)][RESPONDENTS > 30]
kable(head(segment_response, 10), align='l')
```



|M_HH_LEVEL |RESPONDENTS |CAMPAIGN16_RESP_RATE |CAMPAIGN15_RESP_RATE |CAMPAIGN14_RESP_RATE |CAMPAIGN13_RESP_RATE |
|:----------|:-----------|:--------------------|:--------------------|:--------------------|:--------------------|
|A01        |307         |0.2099863            |0.1323283            |0.1009723            |0.1706783            |
|A02        |1071        |0.1867806            |0.1240891            |0.0886602            |0.1716106            |
|B01        |44          |0.1712062            |0.1111111            |0.0747664            |0.1547619            |
|A06        |244         |0.1708683            |0.0982212            |0.0824222            |0.2253259            |
|A03        |137         |0.1701863            |0.0945758            |0.0887574            |0.1699670            |
|A05        |98          |0.1680961            |0.1350806            |0.0840708            |0.1966427            |
|A07        |43          |0.1666667            |0.1510417            |0.0909091            |0.1750000            |
|B06        |46          |0.1559322            |0.0954357            |0.0606061            |0.1917098            |
|B03        |256         |0.1534772            |0.0987654            |0.0864979            |0.2106179            |
|E03        |112         |0.1379310            |0.0904325            |0.0822695            |0.1300813            |

### Data Preparation


```r
missing_data <- apply(is.na(data_c), 2, sum)
missing_data <- missing_data[missing_data > 0]

data_c <- na.omit(data_c)

set.seed(1)
split <- runif(nrow(data_c), 0, 1)
split <- sapply(split, function(x) ifelse(x > 0.25, 'TRAIN', 'TEST'))

# exclude_cols <- c('ACCTNO', 'ZIP', 'ZIP9_Supercode', 'CRRT', 
#                   'DPBC', 'FILLER', 'STINCIND', 'CTINCIND',
#                   'ETHNIC_DETAIL', 'TRACT', 'BLOCK_ID', 'MCD_CCD',
#                   'LOR1', 'FIPSSTCD', 'FIPSCNTY', 'RURAL_URBAN_CODE',
#                   'CBSA_CD', 'TIMEZONE', 'LEARNING_TEST')

response_cols <- names(which(sapply(colnames(data_c), function(x) grepl('RESPONSE', x)) == TRUE))
mail_cols <- names(which(sapply(colnames(data_c), function(x) grepl('^FDSAFDSA', x)) == TRUE))
other_cols <- c('LTD_SALES', 'LTD_TRANSACTIONS',
                'YTD_SALES_2009', 'YTD_TRANSACTIONS_2009', 
                  'BUYER_STATUS',
                'INC_SCS_AMT_V4', 'LAT', 'LONG',
                'MEDIANAGE', 'P_FEMALE', 'P_MARRY', 'P_RENTER',
                'MED_INC', 'MED_HOME', 'UNIT_MORTG_1ST',
                'AVG_COMMUTETIM', 'MED_LOR', 'M_HH_LEVEL',
                'PRE2009_SALES', 'PRE2009_TRANSACTIONS')

select_cols <- c(response_cols, mail_cols, other_cols)

select_data <- data_c[, .SD, .SDcols = select_cols]

# numeric_cols <- names(which(sapply(select_data, is.numeric) == TRUE))
norm_cols <- setdiff(select_cols, response_cols)

data_p <- as.data.frame(lapply(select_data[, ..norm_cols], 
                                     function(x) if (is.numeric(x)) (x-mean(x)) / sd(x) else x))


data_p <- cbind(data_p, select_data[, ..response_cols])

dummy_enc <- dummyVars(' ~ .', data=data_p, fullRank=TRUE)
data_p <- as.data.frame(predict(dummy_enc, data_p))
```

### Model Build


```r
### Try three types of classifiers: logistic regression, random forest, and
### kmeans clustering with a separate classifier for each cluster

# Train logistic regression classifier
train <- split == 'TRAIN'

glm.fit <- glm(RESPONSE16 ~ .,
               data=data_p,
               subset=train)

glm.probs <- predict(glm.fit, 
                     newdata=data_p[!train, ],
                     type='response')

glm.pred <- ifelse(glm.probs > 0.5, 1, 0)
actual <- data_p[!train, ]$RESPONSE16
sum(glm.pred)
```

```
## [1] 407
```

```r
sum(actual)
```

```
## [1] 624
```

```r
glm_perf <- sum(glm.pred == actual) / length(actual)


# Train RF classifier
data_p <- data.table(data_p)
data_p[, RESPONSE16 := as.factor(RESPONSE16)]

xtest <- data_p[!train, ]

rf.fit <- randomForest(RESPONSE16 ~ .,
                       data=data_p[train, ],
                       ntree=200)

rf.pred <- predict(rf.fit, newdata=xtest)
rf.pred <- as.numeric(as.character(rf.pred))

rf_perf <- sum(rf.pred == actual) / length(actual)


# Train kmeans + logistic regression classifiers

upper_k <- 10
withinss <- numeric(0)

for (k in 1:upper_k) {
  km <- kmeans(data_p[train, !c('RESPONSE16')], centers=k, nstart=10)
  avg_withinss <- mean(km$withinss)
  withinss <- c(withinss, avg_withinss)
}

# Plot kmeans results
plot(x=1:upper_k, withinss, 
     type='b',
     col='blue',
     main='KMeans Results - Clustering Sum of Squares',
     xlab='Number of Clusters',
     ylab='Within Cluster Sum of Squares')
```

![](CC_Customer_Acquisition_files/figure-html/Modeling-1.png)<!-- -->

```r
best_k <- 3
best_km <- kmeans(data_p[train, !c('RESPONSE16')], centers=best_k, nstart=10)

# Visualize clusters
pc <- prcomp(data_p[train, !c('RESPONSE16')])
pc_clusters <- data.frame(PC1=pc$x[, 'PC1'], PC2=pc$x[, 'PC2'], Cluster=best_km$cluster)
ggplot(data=pc_clusters,
       aes(x=PC1, y=PC2, col=factor(Cluster))) +
  geom_point() +
  xlim(c(-10,10)) +
  ylim(c(-5,20)) +
  scale_color_brewer() +
  labs(title='Clustering Results (Principal Component Values)') +
  scale_color_brewer(palette='Paired', 'Cluster')
```

![](CC_Customer_Acquisition_files/figure-html/Modeling-2.png)<!-- -->

```r
# Predict test set clusters
xtrain = data_p[train]
xtrain$RESPONSE16 <- as.numeric(as.character(xtrain$RESPONSE16))
xtest = data_p[!train, !c('RESPONSE16')]

# Function to get minimum Euclidean distance between data point and cluster centroid
euc_dist <- function(x, centroids) {
  dists <- numeric(0)
  for (i in 1:nrow(centroids)) {
    dist <- sqrt(sum(x - centroids[i, ])^2)
    dists <- c(dists, dist)
  }
  return(which.min(dists))
}

xtest_clusters <- apply(xtest, 1, function(x) euc_dist(x, best_km$centers))
xtest$cluster <- xtest_clusters

# Build separate model for each cluster
list_models = list()
for (i in 1:best_k) {
  cluster_data <- xtrain[best_km$cluster == i]
  glm_c.fit <- glm(RESPONSE16 ~ .,
               data=cluster_data)
  list_models[[i]] <- glm_c.fit
}

# Predict mail responses based on cluster
predict_resp <- function(x, models) {
  cluster <- x['cluster']
  model <- models[[cluster]]
  obs <- data.frame(t(as.matrix(x[names(x) != 'cluster'])))
  result <- suppressWarnings(predict(model, obs))
  return(result)
}

cluster.glm.probs <- apply(xtest, 1, function(x) predict_resp(x, list_models))
cluster.glm.pred <- ifelse(cluster.glm.probs > 0.5, 1, 0)

cluster_glm_perf <- sum(cluster.glm.pred == actual) / length(actual)
```

### Model Evaluation


```r
### Compute evaluation metrics for both models - TPR, FPR, Precision, Recall, F1 score
pred_perf <- prediction(glm.pred, actual)
prec_rec <- performance(pred_perf, 'prec', 'rec')

pred_perf_rf <- prediction(rf.pred, actual)
prec_rec_rf <- performance(pred_perf_rf, 'prec', 'rec')

cluster_pred_perf <- prediction(cluster.glm.pred, actual)
prec_rec_rf_cluster <- performance(cluster_pred_perf, 'prec', 'rec')
roc_data <- performance(cluster_pred_perf, 'tpr', 'fpr')

glm_prec <- prec_rec@x.values[[1]][2]
glm_rec <- prec_rec@x.values[[1]][2]
glm_f1_score <- 2 * glm_prec * glm_rec / (glm_prec + glm_rec)

rf_prec <- prec_rec_rf@x.values[[1]][2]
rf_rec <- prec_rec_rf@y.values[[1]][2]
rf_f1_score <- 2 * rf_prec * rf_rec / (rf_prec + rf_rec)

cluster_prec <- prec_rec_rf_cluster@x.values[[1]][2]
cluster_rec <- prec_rec_rf_cluster@y.values[[1]][2]
cluster_f1_score <- 2 * cluster_prec * cluster_rec / (cluster_prec + cluster_rec)

overall_perf <- data.frame(Model=c('Logistic Regression', 'KMeans + Logistic', 'Random Forest'), 
                           Accuracy=c(glm_perf, cluster_glm_perf, rf_perf),
                           F1_Score=c(glm_f1_score, cluster_f1_score, rf_f1_score))

kable(overall_perf, align='l')
```



|Model               |Accuracy  |F1_Score  |
|:-------------------|:---------|:---------|
|Logistic Regression |0.9574382 |0.5737179 |
|KMeans + Logistic   |0.9600054 |0.7601297 |
|Random Forest       |0.9197406 |0.1465517 |

```r
plot(roc_data, main='Logistic Regression - ROC Curve',
     col='blue')
```

![](CC_Customer_Acquisition_files/figure-html/Evaluation-1.png)<!-- -->
