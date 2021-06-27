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

# Import electricity production and consumption data
## I have chosen the dollars for the prediction currency due to the fact that the US dollar is one of the most widely used, generally accepted and stable currencies in the world.
## Other currencies especially TRY may fluctuate greatly over the years due to irrelevant factors of this analysis topic such as inflation, interest, etc.
df <- read_csv("mcp_with_variables.csv") %>% select(-mcp_try, -mcp_euro)
head(df)

#Summary statistics of data
skim(df)

#Histogram and Box plot of mcp_dollars variable
ggplot(df, aes(x=mcp_dollars)) + geom_histogram()
boxplot(df$mcp_dollars)

#Histogram load_plan and total_prod variable
ggplot(df, aes(x=load_plan)) + geom_histogram()
ggplot(df, aes(x=total_prod)) + geom_histogram()

#Box plots of load_plan total_prod variable
boxplot(df$load_plan)
boxplot(df$total_prod)

#Seasonality plots of load_plan variable for several hours
as_tsibble(df %>% filter(hour == 0)) %>% gg_season(load_plan) + theme_tq()
as_tsibble(df %>% filter(hour == 4)) %>% gg_season(load_plan) + theme_tq()
as_tsibble(df %>% filter(hour == 8)) %>% gg_season(load_plan) + theme_tq()
as_tsibble(df %>% filter(hour == 12)) %>% gg_season(load_plan) + theme_tq()
as_tsibble(df %>% filter(hour == 16)) %>% gg_season(load_plan) + theme_tq()
as_tsibble(df %>% filter(hour == 20)) %>% gg_season(load_plan) + theme_tq()

#Four weeks MCP Price line graph for every hour to check if there is any seasonality
df %>%
  filter(date >= as_date("2019-01-07") & date <= as_date("2019-02-04")) %>% 
  ggplot(aes(x = date, y = mcp_dollars)) + 
  geom_line(aes(color = as.factor(hour)), size = 0.9, show.legend = T) + 
  labs(title="", y="MCP Price (USD)", x="Date") + 
  facet_wrap(~hour, ncol=4) + 
  theme_tq()

#Hourly MCP Price line graph for every day of a week to check if there is any seasonality
df %>%
  filter(date >= as_date("2019-01-07") & date <= as_date("2019-01-13")) %>% 
  ggplot(aes(x = hour, y = mcp_dollars)) + 
  geom_line(aes(color = as.factor(date)), size = 0.9) + 
  labs(title="", y="MCP Price (USD)", x="Hour") + 
  theme_tq()

#Four weeks Electricity Usage (load_plan) line graph for every hour to check if there is any seasonality
df %>%
  filter(date >= as_date("2019-01-07") & date <= as_date("2019-02-04")) %>% 
  ggplot(aes(x = date, y = load_plan)) + 
  geom_line(aes(color = as.factor(hour)), size = 0.9, show.legend = T) + 
  labs(title="", y="Electricity Usage", x="Date") + 
#  facet_wrap(~hour, ncol=4) + 
  theme_tq()

#Hourly Electricity Usage (load_plan) line graph for every day of a week to check if there is any seasonality
df %>%
  filter(date >= as_date("2019-01-07") & date <= as_date("2019-01-13")) %>% 
  ggplot(aes(x = hour, y = load_plan)) + 
  geom_line(aes(color = as.factor(date)), size = 0.9) + 
  labs(title="", y="Electricity Usage", x="Hour") + 
  theme_tq()

#Four weeks Total Electricity Production (total_prod) line graph for every hour to check if there is any seasonality
df %>%
  filter(date >= as_date("2019-01-07") & date <= as_date("2019-02-04")) %>% 
  ggplot(aes(x = date, y = total_prod)) + 
  geom_line(aes(color = as.factor(hour)), size = 0.9, show.legend = T) + 
  labs(title="", y="Total Electricity Production", x="Date") + 
#  facet_wrap(~hour, ncol=4) + 
  theme_tq()

#Hourly Total Electricity Production (total_prod) line graph for every day of a week to check if there is any seasonality
df %>%
  filter(date >= as_date("2019-01-07") & date <= as_date("2019-01-13")) %>% 
  ggplot(aes(x = hour, y = total_prod)) + 
  geom_line(aes(color = as.factor(date)), size = 0.9) + 
  labs(title="", y="Total Electricity Production", x="Hour") + 
  theme_tq()

#Creating new variables like lagged variables or ratio type variables
df2 <- df %>%
  mutate(renew_energy_ratio = (wind + geothermal + dam + biomass + river) / total_prod,
         cons_prod_ratio = load_plan / total_prod,
         mcp_dollars_lag168 = lag(mcp_dollars, 168),
         renew_energy_ratio_lag168 = lag(renew_energy_ratio, 168),
         cons_prod_ratio_lag168 = lag(cons_prod_ratio, 168),
         mcp_dollars_lag672 = lag(mcp_dollars, 672),
         renew_energy_ratio_lag672 = lag(renew_energy_ratio, 672),
         cons_prod_ratio_lag672 = lag(cons_prod_ratio, 672)) %>% 
  select(date, hour, mcp_dollars, mcp_dollars_lag168, renew_energy_ratio_lag168, cons_prod_ratio_lag168, 
         mcp_dollars_lag672, renew_energy_ratio_lag672, cons_prod_ratio_lag672) %>% 
  filter(date >= as_date("2015-06-29"))


##Variable distribution, scatter and histogram plots are drawn for one week with selected variables
GGally::ggpairs(df2 %>% filter(date >= as_date("2019-01-07") & date <= as_date("2019-01-13")) %>% select(-date, -hour)) + theme_tq()

##Creating train and test data sets
df2_train <- df2 %>% filter(date < as_date("2021-05-22"))
df2_test <- df2 %>% filter(date >= as_date("2021-05-22"))


fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 5)

##Linear Regression model with repeated cross validation
lin_reg <- train(mcp_dollars ~ ., 
                 data = df2_train %>% select(-date, -hour),
                 method = "lm",
                 trControl = fitControl,
                 tuneLength = 5)
summary(lin_reg)

##Decision Tree model with repeated cross validation
dec_tree <- train(mcp_dollars ~ ., 
                  data = df2_train %>% select(-date, -hour),
                  method = "rpart",
                  trControl = fitControl,
                  tuneLength = 5)
trellis.par.set(caretTheme())
plot(dec_tree) 
fancyRpartPlot(dec_tree$finalModel)

##Random Forest model with repeated cross validation
random_forest <- train(mcp_dollars ~ ., 
                      data = df2_train %>% select(-date, -hour),
                      method = "ranger",
                      trControl = fitControl,
                      num.trees = 50,
                      importance = "impurity")
plot(random_forest)
random_forest$results
random_forest$finalModel

##Model comparison
results = resamples(list(Linear_Regression = lin_reg, Decision_Tree = dec_tree, Random_Forest = random_forest))
summary(results)
bwplot(results)

##Residual Analysis
rf_predicted <- predict(random_forest, data = df2_train)
plot(rf_predicted, df2_train$mcp_dollars, xlab = "Predicted", ylab = "Actual", main = "Actual vs Predicted Plot for Random Forest Model")
abline(a=0,b=1,col='red', lty = 2)

rf_residuals <- df2_train$mcp_dollars - rf_predicted
hist(rf_residuals, xlab = "Residuals", main = "Residuals Histogram of Random Forest Model")
plot(rf_predicted, rf_residuals, xlab = "Predicted", ylab = "Residuals", main = "Predicted vs Residuals Plot for Random Forest Model")
abline(h = 0, col = "red", lty = 2)

##Variable Importance of Random Forest Model
sort(random_forest$finalModel$variable.importance, decreasing = TRUE)

##Predictions on test set
rf_predicted_test <- predict(random_forest$finalModel, data = df2_test)
plot(rf_predicted_test$predictions, df2_test$mcp_dollars, xlab = "Predicted", ylab = "Actual", main = "Actual vs Predicted Plot for Random Forest Model with Test Set")
abline(a=0,b=1,col='red', lty = 2)

rf_residuals_test <- df2_test$mcp_dollars - rf_predicted_test$predictions
hist(rf_residuals_test, xlab = "Residuals", main = "Residuals Histogram of Random Forest Model")
plot(rf_predicted_test$predictions, rf_residuals_test, xlab = "Predicted", ylab = "Residuals", main = "Predicted vs Residuals Plot for Random Forest Model with Test Set")
abline(h = 0, col = "red", lty = 2)

##RMSE of test set predictions
RMSE(df2_test$mcp_dollars, rf_predicted_test$predictions)
