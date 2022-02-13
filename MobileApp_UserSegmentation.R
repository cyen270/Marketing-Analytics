###### PRELIMINARIES ######
# Load packages
library(data.table)
library(Amelia)
library(ggplot2)
library(gridExtra)
library(stats)
library(cluster)

# Load survey data
load('apphappyData.RData')


###### MISSING DATA ANALYSIS AND HANDLING ######
# Check for missing data
sum(is.na(apphappy.3.num.frame))
colSums(is.na(apphappy.3.num.frame))

summary(apphappy.3.num.frame)
missmap(apphappy.3.num.frame)

# Convert from data frame to data table
survey_results <- data.table(apphappy.3.num.frame)
str(survey_results)

# Drop columns with significant missing data
survey_results[, q5r1 := NULL]
str(survey_results)

# Drop observations with small amounts of missing data
survey_results <- survey_results[!is.na(q57) & !is.na(q12)]


###### EDA ######
# Categorize types of responses - categorical, binary, three/five-point Licker
survey_response_ranges <- sapply(survey_results[, .SD, .SDcols = !c('caseID')], function(x) {max(x) - min(x)})
unique(survey_response_ranges)
categorical_responses <- names(survey_response_ranges[survey_response_ranges >= 10 & survey_response_ranges <= 13])
binary_responses <- names(survey_response_ranges[survey_response_ranges %in% c(0,1)])
threepoint_responses <- names(survey_response_ranges[survey_response_ranges %in% c(3)])
fourpoint_responses <- names(survey_response_ranges[survey_response_ranges %in% c(4)])
fivepoint_responses <- names(survey_response_ranges[survey_response_ranges %in% c(5)])

# Plot distribution of survey responses
survey_results_reshaped <- melt(survey_results, id.vars = 'caseID')

ggplot(data=survey_results_reshaped[variable %in% categorical_responses], aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() + 
  labs(x='Survey Question', y='Response Value', title='Distribution of Responses to Demographic Questions') +
  theme(axis.text.x = element_text(angle=90), legend.position='none')


ggplot(data=survey_results_reshaped[variable %in% binary_responses], aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  labs(x='Survey Question', y='Response Value', title='Distribution of Responses to Device and App Usage Questions') +
  theme(axis.text.x = element_text(angle=90), legend.position='none')

ggplot(data=survey_results_reshaped[variable %in% fivepoint_responses[1:length(fivepoint_responses)/2]], 
       aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  labs(x='Survey Question', y='Response Value', title='Distribution of Responses to Psychographic/Behavioral Questions') +
  theme(axis.text.x = element_text(angle=90), legend.position='none')

ggplot(data=survey_results_reshaped[variable %in% fivepoint_responses[(length(fivepoint_responses)/2+1):length(fivepoint_responses)]], 
       aes(x=variable, y=value, fill=variable)) +
  geom_boxplot() +
  labs(x='Survey Question', y='Response Value', title='Distribution of Responses to Psychographic/Behavioral Questions') +
  theme(axis.text.x = element_text(angle=90), legend.position='none')


###### CLUSTERING ######
# Find optimal number of clusters using silhouette score

survey_results_dt <- survey_results[, .SD, .SDcols = !c('caseID')]
total_wss <- function(k) {
  clusters <- kmeans(survey_results_dt, k, nstart=30, iter.max=15)
  return(clusters$tot.withinss)
}

between_ss <- function(k) {
  clusters <- kmeans(survey_results_dt, k, nstart=30, iter.max=15)
  return(clusters$betweenss)
}

test_k <- 2:8
total_wss_scores <- sapply(test_k, total_wss)
total_wss_scores <- data.frame(cbind(test_k, total_wss_scores))

twss_plot <- ggplot(data = total_wss_scores, aes(x=test_k, y=total_wss_scores)) +
  geom_line(linetype='dashed', color='purple') + 
  geom_point() +
  labs(x='', y='Within-Cluster Sum of Squares')

between_ss_scores <- sapply(test_k, between_ss)
between_ss_scores <- data.frame(cbind(test_k, between_ss_scores))

bss_plot <- ggplot(data = between_ss_scores, aes(x=test_k, y=between_ss_scores)) +
  geom_line(linetype='dashed', color='blue') + 
  geom_point() +
  labs(x='Number of Clusters', y='Between-Cluster Sum of Squares')

grid.arrange(twss_plot, bss_plot, nrow=2,
             top='Clustering Performance Scores')



clusters <- kmeans(survey_results[, .SD, .SDcols = !c('caseID')], centers=4, nstart=30)

pca <- prcomp(survey_results_dt)


###### VISUALIZE CLUSTERS ######
pca_comps <- pca$x
pca_comps_2D <- pca_comps[, colnames(pca_comps) %in% c('PC1', 'PC2')]
pca_comps_2D <- cbind(pca_comps_2D, clusters$cluster)
pca_comps_2D <- data.frame(pca_comps_2D)
colnames(pca_comps_2D) <- c('PC1', 'PC2', 'Cluster')

ggplot(data=pca_comps_2D, aes(x=PC1, y=PC2, col=factor(Cluster))) +
  geom_point() +
  labs(title='Survey Respondent Clusters') +
  scale_color_brewer(palette='RdBu') +
  theme(legend.position='None')


###### SUMMARIZE CLUSTERING RESULTS ######
survey_results_dt[, Cluster := clusters$cluster]
cluster_summary <- survey_results_dt[, lapply(.SD, mean), by=Cluster]

cluster_size <- survey_results_dt[, .(count=.N, proportion=.N/nrow(survey_results_dt)), by=Cluster]
