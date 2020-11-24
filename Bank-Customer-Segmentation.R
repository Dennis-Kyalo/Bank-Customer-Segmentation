
 ## We first load the required packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(skimr)) install.packages("skimr", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(factoextra)) install.packages("factoextra", repos = "http://cran.us.r-project.org")
if(!require(cluster)) install.packages("cluster", repos = "http://cran.us.r-project.org")
if(!require(NbClust)) install.packages("NbClust", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(e1071)
library(randomForest)
library(rpart)
library(skimr)
library(knitr)
library(ggplot2)
library(factoextra)
library(cluster)
library(NbClust)
library(readxl)



# We load the data and have look at it.

url <- "https://github.com/Dennis-Kyalo/Bank-Customer-segment/raw/master/data/german_credit_data.csv"
tmp_filename <- tempfile()
download.file(url, tmp_filename)
credit_data <- read_csv(tmp_filename)
file.remove(tmp_filename)

head(credit_data)

## We can go ahead and do some data cleaning on our data.
# First we remove the first column which is simply an index set.

credit_data <- credit_data %>% select(-X) # remove the x column
skim(credit_data) # skimming the data 


## We can now proceed to check the number of unique features in each column

credit_data %>% gather(key = features, value = Value) %>%
  group_by(features) %>% summarise(Unique_features_tally = n_distinct(Value)) %>%
  arrange(Unique_features_tally)



## We can now proceed to do some exploratory data analysis on our dataset

## Let's have a look at the sex distribution

credit_data %>% dplyr::count(Sex = factor(Sex)) %>% 
  mutate(Percent = prop.table(n)) %>%
  ggplot(aes(x = Sex, y = Percent, fill = Sex, label = scales::percent(Percent))) +
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Gender Distribution") 



## Let's have a look at the age distribution

credit_data %>% ggplot(aes(x = Age, y = ..count.., label = Age)) +
  geom_histogram(binwidth = 2, colour = "black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  labs(title = "Age Distribution")

## Let's have a look at the house distribution

credit_data %>% ggplot(aes(Housing, fill = Housing)) +
  geom_histogram(stat = "count" , color = "black") +
  labs(title = "Housing Distribution")

## Let's have a look at the Housing Based on Age Distribution

credit_data %>% ggplot(aes(Age, fill = Housing)) +
  geom_histogram(binwidth = 3, color = "black")+
  facet_wrap(~ Housing, ncol = 3) +
  labs(title = "Housing Based on Age Distribution")


## Let's have a look at the Job Category Distribution

credit_data %>% mutate(Job = factor(Job)) %>% ggplot(aes(Job , fill = Job)) +
  geom_histogram(stat = "count" , color = "black") +
  labs(title = "Job Category Distribution",
       x = "Job Category")

## Let's have a look at the Job Category Based on Age Distribution

credit_data %>% mutate(Job = factor(Job)) %>% ggplot(aes(Age, fill = Job)) +
  geom_histogram(stat = "count" , binwidth = 3, color = "black") +
  facet_wrap(~ Job, ncol = 2) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  labs(title = "Job Category Based on Age Distribution",
       x = "Age")



## Let's Look at the Credit Amount vs. Age Distribution based on Gender

credit_data %>% ggplot(aes(Age, Credit.amount, color = Sex)) +
  geom_point()+
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  facet_wrap(~ Sex, ncol = 2) +
  labs(title = "Credit Amount vs. Age Distribution",
       y = "Credit Amount")


## Let's look at Duration vs. Credit Amount Distribution

credit_data %>% ggplot(aes(Credit.amount, Duration, colour = Sex)) +
  geom_point() +
  stat_smooth(formula = "y ~ x", method = "lm", se = FALSE, span = 10) +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  labs(title = "Duration vs. Credit Amount Distribution",
       x = "Credit Amount")


## Let's look at Which purpose got more loans

credit_data %>% group_by(Purpose) %>% 
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  data.frame(.) %>%
  ggplot(aes(x = reorder(Purpose, -Count),  y = Count , fill = Purpose)) +
  geom_bar(stat = "identity", colour = "black") +
  labs(x = "Purpose") +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
  labs(title = "Loan's Purpose Distribution")


## Let's look at what the loans were meant for based on gender

credit_data %>% group_by(Purpose, Sex) %>% 
  summarise(Count = n()) %>%
  arrange(Purpose ,Count) %>%
  ungroup() %>%
  data.frame(.) %>%
  ggplot(aes(x = reorder(Purpose, -Count),  y = Count , fill = Sex)) +
  geom_bar(stat = "identity", colour = "black") +
  labs(x = "Purpose") +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
  facet_wrap(~ Sex, ncol = 2) +
  labs(title = "Loan's Purpose based on Gender")


## Let's look at the Loan's Purpose Based on Age

credit_data %>% ggplot(aes(Age, fill = Sex)) +
  geom_histogram(binwidth = 4, colour = "black") +
  facet_wrap(~ Purpose, ncol = 4) +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  labs(title = "Loan's Purpose Based on Age")




## Boxplot of Credit Amount Based on Purpose 

credit_data %>% ggplot(aes(x = reorder(Purpose, -Credit.amount), Credit.amount, fill = Sex)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
  labs(title = "Credit Amount Based on Purpose Boxplot",
       x = "Purpose" ,
       y = "Credit  Amount")


## Boxplot of Duration Based on Purpose Boxplot

credit_data %>% ggplot(aes(x = reorder(Purpose, -Duration), Duration, fill = Sex)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = -30, vjust = 1, hjust = 0)) +
  labs(title = "Duration Based on Purpose Boxplot",
       x = "Purpose" ,
       y = "Duration")


## Boxplot of Credit Amount Based on Housing

credit_data %>% ggplot(aes(x = reorder(Housing, -Credit.amount), Credit.amount, fill = Sex)) +
  geom_boxplot() +
  labs(title = "Credit Amount Based on Housing Boxplot",
       x = "Housing",
       y = "Credit Amount")


## Boxplot of Credit  Amount based on Job Category

credit_data %>% ggplot(aes(as.factor(Job), Credit.amount, fill = Sex)) +
  scale_y_continuous(breaks = seq(0, 18000, by = 2500)) +
  geom_boxplot() +
  labs(title = "Credit  Amount based on Job Category Boxplot",
       x = "Job Category",
       y = "Credit  Amount")

## Boxplot of Duration Based on Job Category

credit_data %>% ggplot(aes(as.factor(Job), Duration, fill = Sex)) +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70))  +
  geom_boxplot() +
  labs(title = "Duration Based on Job Category Boxplot",
       x = "Job Category",
       y = "Duration")

## Let's look at Credit Amount vs. Age Based on Job Category

credit_data %>% mutate(Job = as.factor(Job)) %>% ggplot(aes(Age, Credit.amount, color = Job)) +
  geom_point() +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  labs(title = "Credit Amount vs. Age Based on Job Category",
       y = "Credit Amount")


## Let's check for correlation in the data

credit_data %>% select(Job, Age, Credit.amount, Duration) %>% cor(.) %>% knitr::kable() 




## Let's see how the supervised classification algorithm and unsupervised clustering algorithms work.

set.seed(1234)
credit_analysis_scaled <- credit_data %>% select(Age, Job, Credit.amount, Duration) %>% scale() %>% 
  data.frame() # Selecting the columns to be used and scaling them

## Splitting the dataset 

credit_analysis_scaled$Sex <- credit_data$Sex # Adding the Sex column after scaling the data
y <- credit_analysis_scaled$Sex
train_index <- createDataPartition(y , times = 1, p = 0.2, list = FALSE) # splitting the dataset
train_set <- credit_analysis_scaled[-train_index,]
test_set <- credit_analysis_scaled[train_index,]



## We begin our prediction using the Logistic regression. 

set.seed(1234)
train_glm <- train(Sex ~ ., method = "glm", data = train_set) # logistic regression
glm_preds <- predict(train_glm, test_set, type = "raw") # predicting the gender on the test set
glm_acc <- mean(glm_preds == test_set$Sex) # checking accuracy

accuracy <- tibble(Method = "Logistic Regression", Accuracy = glm_acc)
accuracy



## We use the K-Nearest-Neighbor.

##KNN
set.seed(1234)
tuning <- data.frame(k = seq(3, 50, 2)) # setting values of k for optimization
train_knn <- train(train_set[,-5],train_set[,5],
                   method = "knn", # knn method
                   tuneGrid = tuning)

knn_preds <- predict(train_knn, test_set) # predicting the gender on the test set
knn_acc <- mean(knn_preds == test_set$Sex) # checking accuracy

accuracy <- bind_rows(accuracy,
                      tibble(Method = "K-Nearest Neighbor", Accuracy = knn_acc))
accuracy



## We use the random forest algorithm.

library(randomForest)
set.seed(1234)
train_rf <- train(train_set[,-5],train_set[,5], 
                  method = "rf", # random forest method
                  tuneGrid = data.frame(mtry = c(1,2,3,4)))

rf_preds <- predict(train_rf, test_set) # predicting the gender on the test set
rf_acc <- mean(rf_preds == test_set$Sex)

accuracy <- bind_rows(accuracy,
                      tibble(Method = "Random Forest", Accuracy = rf_acc))
accuracy


## We now proceed to the unsupervised techniques


set.seed(1234)
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

## We use the K-Means algorithm.

set.seed(1234)
k <- kmeans(train_set[,-5], centers = 4) # setting number of clusters
kmeans_preds <- ifelse(predict_kmeans(test_set[-5], k) == 1, "female", "male")
kmeans_acc <- mean(kmeans_preds == test_set$Sex)

accuracy <- bind_rows(accuracy,
                      tibble(Method = "K-Means Clustering", Accuracy = kmeans_acc))
accuracy


## We use the Hierarchical  Clustering algorithm.

set.seed(1234)
predict_hier <- function(x, c) {
  merge <- c$merge    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(merge, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}

c <- hclust (dist(train_set[,-5]), method = "ward.D")
hierach_preds <- ifelse(predict_hier(test_set[-5], c) == 1, "female", "male")
hierach_acc <- mean(hierach_preds == test_set$Sex)

accuracy <- bind_rows(accuracy,
                      tibble(Method = "Hierarchical  Clustering", Accuracy = hierach_acc))
accuracy


## We apply the elbow method

credit_analysis <- credit_data %>%
  select(Age, Job, Credit.amount, Duration)

credit_scaled <- as.data.frame(lapply(credit_analysis, scale)) # Scale the data
fviz_nbclust(credit_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) +
  labs(subtitle = "Elbow method") ## Check Elbow


## Applying hierarchical clustering using ward.D method and setting clusters to 4

hierach_ward <- eclust(credit_scaled, "hclust", hc_method = "ward.D", k = 4) 

## We make a table on how the gender was distributed in the clusters

hierach_clusters <- hierach_ward$cluster
table(hierach_clusters, credit_data$Sex) # gender based on hierarchical cluster

## Table showing how the customers were distributed in the clusters

table(hierach_ward$cluster)

##Cluster plot diagram

clusplot(credit_analysis,
         clusters,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'))

## We create the Tree diagram i.e. Dendrogram graph 

fviz_dend(hierach_ward, show_labels = T, palette = "aaas", color_labels_by_k = TRUE, rect = TRUE) 

## Plot showing Duration vs. Credit Amount Based on Cluster

credit_analysis$Cluster <- as.factor(hierach_ward$cluster)

credit_analysis %>% 
  ggplot(aes(Credit.amount, Duration, col = Cluster)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70)) +
  scale_color_manual(breaks = c("1", "2", "3", "4"), 
                     values=c("red", "blue", "green", "darkgrey")) +
  labs(title = "Duration vs. Credit Amount Based on Cluster",
       x = "Credit Amount")

## Plot showing Credit Amount vs. Age Based on Cluster

credit_analysis %>% 
  ggplot(aes( Age, Credit.amount, col = Cluster)) +
  geom_point() +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 80, by = 10)) +
  scale_color_manual(breaks = c("1", "2", "3", "4"), 
                     values = c("red", "blue", "green", "darkgrey")) +
  labs(title = "Credit Amount vs. Age Based on Cluster",
       y = "Credit Amount",
       x = "Age")

## Table showing the mean of different attributes in each cluster 

credit_analysis$Cluster <- as.numeric(hierach_ward$cluster)

credit_analysis %>% 
  group_by(Cluster) %>%
  summarise_all(.funs = "mean") %>%
  round(.,digits = 0) %>%
  knitr::kable("pipe")



