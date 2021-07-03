# Installing libraries (if do not have)
##install.packages("tidyverse")
##install.packages("lubridate")
##install.packages("readxl")
##install.packages("skimr")
##install.packages("magrittr")
##install.packages("tidyquant")
##install.packages("tsibble")
##install.packages("feasts")
##install.packages("ggcorrplot")
##install.packages("glmnet")
##install.packages("caret")
##install.packages("rattle")

# Importing libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(skimr)
library(magrittr)
library(tidyquant)
library(tsibble)
library(feasts)
library(ggcorrplot)
library(glmnet)
library(caret)
library(rattle)

set.seed(1234)

# Import hourly solar power plan production data
df <- read_csv("production_data_with_weather.csv") %>% select(-timestamp)
head(df)

# Summary statistics of data
skim(df)

# Checking for which hours there are production
plot(df$Hour, df$Production, xlab = "Hour", ylab = "Production", main = "Hourly Production Plot")

# Filtering out from the data the hours without production
df2 <- df %>% filter(Hour >= 5 & Hour <= 19) 
head(df2, 20)

# Scatter plot, variable distribution and correlation table for target and potential independent variables
GGally::ggpairs(df %>% select(-Date, -Hour))

# As the same type of variables with different coordinates are so similar to each other in numeric manner and highly correlated, 
# we are calculating their average values to reduce them into one variable. 
df3 <- df2 %>% 
  mutate(CLOUD_LOW_LAYER_AVG = (CLOUD_LOW_LAYER_37.75_34.25 + CLOUD_LOW_LAYER_37.75_34.5 + CLOUD_LOW_LAYER_38_34.25 + CLOUD_LOW_LAYER_38_34.5) / 4,
         DSWRF_AVG = (DSWRF_37.75_34.25 + DSWRF_37.75_34.5 + DSWRF_38_34.25 + DSWRF_38_34.5) / 4,
         TEMP_AVG =  (TEMP_37.75_34.25 + TEMP_37.75_34.5 + TEMP_38_34.25 + TEMP_38_34.5) / 4) %>% 
  select(Date, Hour, Production, CLOUD_LOW_LAYER_AVG, DSWRF_AVG, TEMP_AVG)
head(df3)

# Summary statistics of reduced data
skim(df3)

# Histograms of target and independent variables
ggplot(df3, aes(x=Production)) + geom_histogram()
ggplot(df3, aes(x=TEMP_AVG)) + geom_histogram()
ggplot(df3, aes(x=DSWRF_AVG)) + geom_histogram()
ggplot(df3, aes(x=CLOUD_LOW_LAYER_AVG)) + geom_histogram()

# Boxplots of target and independent variables
par(mfrow=c(2,2))
boxplot(df3$Production, main = "Production")
boxplot(df3$TEMP_AVG, main = "Temperature")
boxplot(df3$DSWRF_AVG, main = "DSWRF")
boxplot(df3$CLOUD_LOW_LAYER_AVG, main = "Cloud Low Layer")
par(mfrow=c(1,1))

# We can observe that higher cloud low layer values causes lower frequency for high production values.
plot(df3$CLOUD_LOW_LAYER_AVG, df3$Production, xlab = "Cloud Low Layer", ylab = "Production", main = "Cloud Low Layer-Production Scatter Plot")

# We can say that DSWRF values are directly proportional to the Production values. As the DSWRF values increases, the Production values also increase. 
plot(df3$DSWRF_AVG, df3$Production, xlab = "DSWRF", ylab = "Production", main = "DSWRF-Production Scatter Plot")

# There is a relationship between DSWRF values and hours similar to the relation of Production-Hour
plot(df3$Hour, df3$DSWRF_AVG , xlab = "Hour", ylab = "DSWRF", main = "Hour-DSWRF Scatter Plot")

# As it is expected, temperature are higher at afternoon hours and lower at morning and evening hours
plot(df3$Hour, df3$TEMP_AVG, xlab = "Hour", ylab = "Temperature", main = "Hour-Temperature Scatter Plot")

# It is obvious that low temperature values correspond to low DSWRF values, while the higher DSWRF values, the slightly higher the temperature values.
plot(df3$DSWRF_AVG, df3$TEMP_AVG , xlab = "DSWRF", ylab = "Temperature", main = "DSWRF-Temperature Scatter Plot")

# Line plot of Production as a time series for each hour
df3 %>%
  ggplot(aes(x = Date, y = Production)) + 
  geom_line(aes(color = as.factor(Hour)), size = 0.9, show.legend = F) + 
  labs(title="Line plot of Production for each hour", y="Production Amount", x="Date") + 
  facet_wrap(~Hour, ncol=4) + 
  theme_tq()

# Line plot of Temperature as a time series for each hour
df3 %>%
  ggplot(aes(x = Date, y = TEMP_AVG)) + 
  geom_line(aes(color = as.factor(Hour)), size = 0.9, show.legend = F) + 
  labs(title="Line plot of Temperature for each hour", y="Temperature", x="Date") + 
  facet_wrap(~Hour, ncol=4) + 
  theme_tq()

# Line plot of DSWRF as a time series for each hour
df3 %>%
  ggplot(aes(x = Date, y = DSWRF_AVG)) + 
  geom_line(aes(color = as.factor(Hour)), size = 0.9, show.legend = F) + 
  labs(title="Line plot of DSWRF for each hour", y="DSWRF", x="Date") + 
  facet_wrap(~Hour, ncol=4) + 
  theme_tq()

# Line plot of Cloud Low Layer as a time series for each hour
df3 %>%
  ggplot(aes(x = Date, y = CLOUD_LOW_LAYER_AVG)) + 
  geom_line(aes(color = as.factor(Hour)), size = 0.9, show.legend = F) + 
  labs(title="Line plot of Cloud Low Layer for each hour", y="Cloud Low Layer", x="Date") + 
  facet_wrap(~Hour, ncol=4) + 
  theme_tq()

# Creating new variables like lagged variables
df4 <- df3 %>% 
  mutate(Production_lag24 = lag(Production, 15),
         TEMP_AVG_lag24 = lag(TEMP_AVG, 15),
         DSWRF_AVG_lag24 = lag(DSWRF_AVG, 15),
         CLOUD_LOW_LAYER_AVG_lag24 = lag(CLOUD_LOW_LAYER_AVG, 15),
         Production_lag105 = lag(Production, 105),
         TEMP_AVG_lag105 = lag(TEMP_AVG, 105),
         DSWRF_AVG_lag105 = lag(DSWRF_AVG, 105),
         CLOUD_LOW_LAYER_AVG_lag105 = lag(CLOUD_LOW_LAYER_AVG, 105)) %>% 
  filter(Date >= as_date('2019-10-16'))

# Finding the largest mean production level for the whole data to use that hour for parameter tuning and use the same parameters for other hours
df4 %>% group_by(Hour) %>%  summarise(mean_production = mean(Production)) %>% arrange(desc(mean_production))

# Splitting data into train and test sets
df4_train <- df4 %>% filter(Date < as_date('2020-12-01')) 
df4_test <- df4 %>% filter(Date >= as_date('2020-12-01') & Date <= as_date('2021-01-31'))

# Splitting train data into hourly format to have different models for every hour
df4_train_h5 <- df4_train %>% filter(Hour == 5)
df4_train_h6 <- df4_train %>% filter(Hour == 6)
df4_train_h7 <- df4_train %>% filter(Hour == 7)
df4_train_h8 <- df4_train %>% filter(Hour == 8)
df4_train_h9 <- df4_train %>% filter(Hour == 9)
df4_train_h10 <- df4_train %>% filter(Hour == 10)
df4_train_h11 <- df4_train %>% filter(Hour == 11)
df4_train_h12 <- df4_train %>% filter(Hour == 12)
df4_train_h13 <- df4_train %>% filter(Hour == 13)
df4_train_h14 <- df4_train %>% filter(Hour == 14)
df4_train_h15 <- df4_train %>% filter(Hour == 15)
df4_train_h16 <- df4_train %>% filter(Hour == 16)
df4_train_h17 <- df4_train %>% filter(Hour == 17)
df4_train_h18 <- df4_train %>% filter(Hour == 18)
df4_train_h19 <- df4_train %>% filter(Hour == 19)

# Splitting test data into hourly format to have different models for every hour
df4_test_h5 <- df4_test %>% filter(Hour == 5)
df4_test_h6 <- df4_test %>% filter(Hour == 6)
df4_test_h7 <- df4_test %>% filter(Hour == 7)
df4_test_h8 <- df4_test %>% filter(Hour == 8)
df4_test_h9 <- df4_test %>% filter(Hour == 9)
df4_test_h10 <- df4_test %>% filter(Hour == 10)
df4_test_h11 <- df4_test %>% filter(Hour == 11)
df4_test_h12 <- df4_test %>% filter(Hour == 12)
df4_test_h13 <- df4_test %>% filter(Hour == 13)
df4_test_h14 <- df4_test %>% filter(Hour == 14)
df4_test_h15 <- df4_test %>% filter(Hour == 15)
df4_test_h16 <- df4_test %>% filter(Hour == 16)
df4_test_h17 <- df4_test %>% filter(Hour == 17)
df4_test_h18 <- df4_test %>% filter(Hour == 18)
df4_test_h19 <- df4_test %>% filter(Hour == 19)

# Different models such as linear regression, decision tree, random forest, GLMNET are tried for data set of Hour 11 to make the parameter tuning on the data.
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

## Linear regression model is built. According to results, intercept parameter is tuned and set to TRUE
lin_reg_h11 <- train(Production ~ ., 
                 data = df4_train_h11 %>% select(-Date, -Hour),
                 method = "lm",
                 trControl = fitControl,
                 tuneLength = 5)
lin_reg_h11
summary(lin_reg_h11)
lin_reg_h11$finalModel

## Decision tree model is built. According to results, cp parameter is tuned and set to 0.01831669
dec_tree_h11 <- train(Production ~ ., 
                  data = df4_train_h11 %>% select(-Date, -Hour),
                  method = "rpart",
                  trControl = fitControl,
                  tuneLength = 5)
dec_tree_h11
trellis.par.set(caretTheme())
plot(dec_tree_h11)

fancyRpartPlot(dec_tree_h11$finalModel)
dec_tree_h11$finalModel

## Random forest model is built. 
## According to results, mtry parameter is tuned and set to 6, splitrule parameter is tuned and set to "variance", min.node.size parameter is tuned and set to 5
rand_forest_h11 <- train(Production ~ ., 
                       data = df4_train_h11 %>% select(-Date, -Hour),
                       method = "ranger",
                       trControl = fitControl,
                       num.trees = 50,
                       importance = "impurity")
rand_forest_h11
plot(rand_forest_h11)
rand_forest_h11$finalModel


## GLMNET model is built. According to results, alpha parameter is tuned and set to 0.1, lambda parameter is tuned and set to 0.09524498
glmnet_h11 <- train(Production ~ ., 
                         data = df4_train_h11 %>% select(-Date, -Hour),
                         method = "glmnet",
                         trControl = fitControl,
                         tuneLenght= 5)
glmnet_h11
plot(glmnet_h11)

## Model comparison
results = resamples(list(Linear_Regression = lin_reg_h11, Decision_Tree = dec_tree_h11, Random_Forest = rand_forest_h11, GLMNET = glmnet_h11))
summary(results)
bwplot(results)



# Modelling each hour with tuned parameters of all four models

## Hour 5
lin_reg_h5 <- train(Production ~ ., 
                    data = df4_train_h5 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h5 <- train(Production ~ ., 
                     data = df4_train_h5 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h5 <- train(Production ~ ., 
                        data = df4_train_h5 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h5 <- train(Production ~ ., 
                   data = df4_train_h5 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h5, Decision_Tree = dec_tree_h5, Random_Forest = rand_forest_h5, GLMNET = glmnet_h5))
summary(results)
bwplot(results)


## Hour 6
lin_reg_h6 <- train(Production ~ ., 
                    data = df4_train_h6 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h6 <- train(Production ~ ., 
                     data = df4_train_h6 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h6 <- train(Production ~ ., 
                        data = df4_train_h6 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h6 <- train(Production ~ ., 
                   data = df4_train_h6 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h6, Decision_Tree = dec_tree_h6, Random_Forest = rand_forest_h6, GLMNET = glmnet_h6))
summary(results)
bwplot(results)


## Hour 7
lin_reg_h7 <- train(Production ~ ., 
                    data = df4_train_h7 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h7 <- train(Production ~ ., 
                     data = df4_train_h7 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h7 <- train(Production ~ ., 
                        data = df4_train_h7 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h7 <- train(Production ~ ., 
                   data = df4_train_h7 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h7, Decision_Tree = dec_tree_h7, Random_Forest = rand_forest_h7, GLMNET = glmnet_h7))
summary(results)
bwplot(results)


## Hour 8
lin_reg_h8 <- train(Production ~ ., 
                    data = df4_train_h8 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h8 <- train(Production ~ ., 
                     data = df4_train_h8 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h8 <- train(Production ~ ., 
                        data = df4_train_h8 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h8 <- train(Production ~ ., 
                   data = df4_train_h8 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h8, Decision_Tree = dec_tree_h8, Random_Forest = rand_forest_h8, GLMNET = glmnet_h8))
summary(results)
bwplot(results)


## Hour 9
lin_reg_h9 <- train(Production ~ ., 
                    data = df4_train_h9 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h9 <- train(Production ~ ., 
                     data = df4_train_h9 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h9 <- train(Production ~ ., 
                        data = df4_train_h9 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h9 <- train(Production ~ ., 
                   data = df4_train_h9 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h9, Decision_Tree = dec_tree_h9, Random_Forest = rand_forest_h9, GLMNET = glmnet_h9))
summary(results)
bwplot(results)


## Hour 10
lin_reg_h10 <- train(Production ~ ., 
                    data = df4_train_h10 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h10 <- train(Production ~ ., 
                     data = df4_train_h10 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h10 <- train(Production ~ ., 
                        data = df4_train_h10 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h10 <- train(Production ~ ., 
                   data = df4_train_h10 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h10, Decision_Tree = dec_tree_h10, Random_Forest = rand_forest_h10, GLMNET = glmnet_h10))
summary(results)
bwplot(results)


## Hour 11
lin_reg_h11 <- train(Production ~ ., 
                    data = df4_train_h11 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h11 <- train(Production ~ ., 
                     data = df4_train_h11 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h11 <- train(Production ~ ., 
                        data = df4_train_h11 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h11 <- train(Production ~ ., 
                   data = df4_train_h11 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h11, Decision_Tree = dec_tree_h11, Random_Forest = rand_forest_h11, GLMNET = glmnet_h11))
summary(results)
bwplot(results)


## Hour 12
lin_reg_h12 <- train(Production ~ ., 
                    data = df4_train_h12 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h12 <- train(Production ~ ., 
                     data = df4_train_h12 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h12 <- train(Production ~ ., 
                        data = df4_train_h12 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h12 <- train(Production ~ ., 
                   data = df4_train_h12 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h12, Decision_Tree = dec_tree_h12, Random_Forest = rand_forest_h12, GLMNET = glmnet_h12))
summary(results)
bwplot(results)


## Hour 13
lin_reg_h13 <- train(Production ~ ., 
                    data = df4_train_h13 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h13 <- train(Production ~ ., 
                     data = df4_train_h13 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h13 <- train(Production ~ ., 
                        data = df4_train_h13 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h13 <- train(Production ~ ., 
                   data = df4_train_h13 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h13, Decision_Tree = dec_tree_h13, Random_Forest = rand_forest_h13, GLMNET = glmnet_h13))
summary(results)
bwplot(results)


## Hour 14
lin_reg_h14 <- train(Production ~ ., 
                    data = df4_train_h14 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h14 <- train(Production ~ ., 
                     data = df4_train_h14 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h14 <- train(Production ~ ., 
                        data = df4_train_h14 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h14 <- train(Production ~ ., 
                   data = df4_train_h14 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h14, Decision_Tree = dec_tree_h14, Random_Forest = rand_forest_h14, GLMNET = glmnet_h14))
summary(results)
bwplot(results)


## Hour 15
lin_reg_h15 <- train(Production ~ ., 
                    data = df4_train_h15 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h15 <- train(Production ~ ., 
                     data = df4_train_h15 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h15 <- train(Production ~ ., 
                        data = df4_train_h15 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h15 <- train(Production ~ ., 
                   data = df4_train_h15 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h15, Decision_Tree = dec_tree_h15, Random_Forest = rand_forest_h15, GLMNET = glmnet_h15))
summary(results)
bwplot(results)


## Hour 16
lin_reg_h16 <- train(Production ~ ., 
                    data = df4_train_h16 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h16 <- train(Production ~ ., 
                     data = df4_train_h16 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h16 <- train(Production ~ ., 
                        data = df4_train_h16 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h16 <- train(Production ~ ., 
                   data = df4_train_h16 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h16, Decision_Tree = dec_tree_h16, Random_Forest = rand_forest_h16, GLMNET = glmnet_h16))
summary(results)
bwplot(results)


## Hour 17
lin_reg_h17 <- train(Production ~ ., 
                    data = df4_train_h17 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h17 <- train(Production ~ ., 
                     data = df4_train_h17 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h17 <- train(Production ~ ., 
                        data = df4_train_h17 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h17 <- train(Production ~ ., 
                   data = df4_train_h17 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h17, Decision_Tree = dec_tree_h17, Random_Forest = rand_forest_h17, GLMNET = glmnet_h17))
summary(results)
bwplot(results)


## Hour 18
lin_reg_h18 <- train(Production ~ ., 
                    data = df4_train_h18 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h18 <- train(Production ~ ., 
                     data = df4_train_h18 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h18 <- train(Production ~ ., 
                        data = df4_train_h18 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h18 <- train(Production ~ ., 
                   data = df4_train_h18 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h18, Decision_Tree = dec_tree_h18, Random_Forest = rand_forest_h18, GLMNET = glmnet_h18))
summary(results)
bwplot(results)


## Hour 19
lin_reg_h19 <- train(Production ~ ., 
                    data = df4_train_h19 %>% select(-Date, -Hour),
                    method = "lm",
                    trControl = fitControl,
                    tuneGrid = expand.grid(intercept = T),
                    tuneLength = 5)

dec_tree_h19 <- train(Production ~ ., 
                     data = df4_train_h19 %>% select(-Date, -Hour),
                     method = "rpart",
                     trControl = fitControl,
                     tuneGrid = expand.grid(cp = 0.01831669),
                     tuneLength = 5)

rand_forest_h19 <- train(Production ~ ., 
                        data = df4_train_h19 %>% select(-Date, -Hour),
                        method = "ranger",
                        trControl = fitControl,
                        num.trees = 50,
                        tuneGrid = expand.grid(mtry = 6, splitrule = "variance", min.node.size = 5),
                        importance = "impurity")

glmnet_h19 <- train(Production ~ ., 
                   data = df4_train_h19 %>% select(-Date, -Hour),
                   method = "glmnet",
                   trControl = fitControl,
                   tuneGrid = expand.grid(alpha = 0.1, lambda = 0.09524498),
                   tuneLenght= 5)

##Model comparison
results = resamples(list(Linear_Regression = lin_reg_h19, Decision_Tree = dec_tree_h19, Random_Forest = rand_forest_h19, GLMNET = glmnet_h19))
summary(results)
bwplot(results)


# As Random Forest model is generally the best one according to the model comparison results among hourly models, 
# we choose to continue with Random Forest model to make the predictions on the test set. Below the predictions are done.
rf_h5_pred_test <- predict(rand_forest_h5$finalModel, data = df4_test_h5)
rf_h6_pred_test <- predict(rand_forest_h6$finalModel, data = df4_test_h6)
rf_h7_pred_test <- predict(rand_forest_h7$finalModel, data = df4_test_h7)
rf_h8_pred_test <- predict(rand_forest_h8$finalModel, data = df4_test_h8)
rf_h9_pred_test <- predict(rand_forest_h9$finalModel, data = df4_test_h9)
rf_h10_pred_test <- predict(rand_forest_h10$finalModel, data = df4_test_h10)
rf_h11_pred_test <- predict(rand_forest_h11$finalModel, data = df4_test_h11)
rf_h12_pred_test <- predict(rand_forest_h12$finalModel, data = df4_test_h12)
rf_h13_pred_test <- predict(rand_forest_h13$finalModel, data = df4_test_h13)
rf_h14_pred_test <- predict(rand_forest_h14$finalModel, data = df4_test_h14)
rf_h15_pred_test <- predict(rand_forest_h15$finalModel, data = df4_test_h15)
rf_h16_pred_test <- predict(rand_forest_h16$finalModel, data = df4_test_h16)
rf_h17_pred_test <- predict(rand_forest_h17$finalModel, data = df4_test_h17)
rf_h18_pred_test <- predict(rand_forest_h18$finalModel, data = df4_test_h18)
rf_h19_pred_test <- predict(rand_forest_h19$finalModel, data = df4_test_h19)

# Test set predictions are binded with test data set for each hour.
df4_test_h5_w_pred <- df4_test_h5 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h5_pred_test$predictions)
df4_test_h6_w_pred <- df4_test_h6 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h6_pred_test$predictions)
df4_test_h7_w_pred <- df4_test_h7 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h7_pred_test$predictions)
df4_test_h8_w_pred <- df4_test_h8 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h8_pred_test$predictions)
df4_test_h9_w_pred <- df4_test_h9 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h9_pred_test$predictions)
df4_test_h10_w_pred <- df4_test_h10 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h10_pred_test$predictions)
df4_test_h11_w_pred <- df4_test_h11 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h11_pred_test$predictions)
df4_test_h12_w_pred <- df4_test_h12 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h12_pred_test$predictions)
df4_test_h13_w_pred <- df4_test_h13 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h13_pred_test$predictions)
df4_test_h14_w_pred <- df4_test_h14 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h14_pred_test$predictions)
df4_test_h15_w_pred <- df4_test_h15 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h15_pred_test$predictions)
df4_test_h16_w_pred <- df4_test_h16 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h16_pred_test$predictions)
df4_test_h17_w_pred <- df4_test_h17 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h17_pred_test$predictions)
df4_test_h18_w_pred <- df4_test_h18 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h18_pred_test$predictions)
df4_test_h19_w_pred <- df4_test_h19 %>% select(Date, Hour, Production) %>% mutate(Prediction = rf_h19_pred_test$predictions)

# Test sets for each hour with predictions are row binded. Thus, we create one test set with prediction for whole data.
df4_test_w_pred <- rbind(df4_test_h5_w_pred, df4_test_h6_w_pred, df4_test_h7_w_pred, df4_test_h8_w_pred,
                         df4_test_h9_w_pred, df4_test_h10_w_pred, df4_test_h11_w_pred, df4_test_h12_w_pred, 
                         df4_test_h13_w_pred, df4_test_h14_w_pred, df4_test_h15_w_pred, df4_test_h16_w_pred,
                         df4_test_h17_w_pred, df4_test_h18_w_pred, df4_test_h19_w_pred) %>% arrange(Date, Hour)
head(df4_test_w_pred)
skim(df4_test_w_pred %>% select(Production, Prediction))

## Scatter Plot of Actual vs Predicted values for test set
plot(df4_test_w_pred$Prediction, df4_test_w_pred$Production, xlab = "Predicted", ylab = "Actual", main = "Actual vs Predicted Plot for Random Forest Model with Test Set")
abline(a=0,b=1,col='red', lty = 2)

## Histogram of residuals for test set
rf_residuals_test <- df4_test_w_pred$Production - df4_test_w_pred$Prediction
hist(rf_residuals_test, xlab = "Residuals", main = "Residuals Histogram of Random Forest Model")

## Scatter Plot of Predicted vs Residuals values for test set
plot(df4_test_w_pred$Prediction, rf_residuals_test, xlab = "Predicted", ylab = "Residuals", main = "Predicted vs Residuals Plot for Random Forest Model with Test Set")
abline(h = 0, col = "red", lty = 2)

## RMSE of test set predictions
RMSE(df4_test_w_pred$Production, df4_test_w_pred$Prediction)

