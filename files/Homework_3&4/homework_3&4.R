# Installing libraries (if do not have)
##install.packages("tidyverse")
##install.packages("lubridate")
##install.packages("readxl")
##install.packages("magrittr")
##install.packages("scatterplot3d")
##install.packages("ggfortify")

# Importing libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(scatterplot3d)
library(ggfortify)
library(glmnet)

set.seed(1234)

# Import electricity consumption data
df <- read_csv("electricity_consumption_turkey.csv") %>% 
  as_tibble() %>% 
  mutate(date = dmy(date),
         hour = as.character(hour),
         hour = substring(hour, 1, 2))
head(df)


# TASK A

##Preparing dataset
df_a <- df %>% 
  mutate(forecast_lag168 = lag(consumption, 168),
         forecast_lag48 = lag(consumption, 48),
         id = row_number()) %>% 
  filter(id > 168) %>% select(-id) %>% 
  mutate(APE_lag168 = (abs(consumption-forecast_lag168) / abs(consumption)) * 100,
         APE_lag48 = (abs(consumption-forecast_lag48) / abs(consumption)) * 100)
head(df_a)

##Filtering test set
df_a_test <- df_a %>% filter(date >= as_date('2021-05-01'))

##Summary statistics of error rates
summary(df_a_test %>% select(APE_lag168, APE_lag48))

##Detailed quantiles of error rates
quantile(df_a_test$APE_lag168, probs = c(0,0.1,0.25,0.40,0.50,0.60,0.75,0.90,1))
quantile(df_a_test$APE_lag48, probs = c(0,0.1,0.25,0.40,0.50,0.60,0.75,0.90,1))

##Boxplot of error rates
boxplot(df_a_test$APE_lag168)
boxplot(df_a_test$APE_lag48)


# TASK B

##Preparing data set
df_b <- df %>% 
  mutate(lag168 = lag(consumption, 168),
         lag48 = lag(consumption, 48),
         id = row_number()) %>% 
  filter(id > 168) %>% select(-id)
head(df_b)

##Filtering train and test set
df_b_train <- df_b %>% filter(date < as_date('2020-03-01'))
df_b_test <- df_b %>% filter(date >= as_date('2020-03-01'))

##Training linear regression model
model_b <- lm(consumption ~ lag168 + lag48, data = df_b_train)
summary(model_b)

##Testing model on the test set
df_b_test_pred <- predict(model_b, newdata = df_b_test)

##Summary statistics of MAPE values for the test period
df_b_test_APE <- (abs(df_b_test$consumption-df_b_test_pred) / abs(df_b_test$consumption)) * 100
summary(df_b_test_APE)

# TASK C

## Establishing a model for each hour
model_c_h0 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "00"))
model_c_h1 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "01"))
model_c_h2 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "02"))
model_c_h3 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "03"))
model_c_h4 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "04"))
model_c_h5 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "05"))
model_c_h6 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "06"))
model_c_h7 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "07"))
model_c_h8 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "08"))
model_c_h9 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "09"))
model_c_h10 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "10"))
model_c_h11 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "11"))
model_c_h12 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "12"))
model_c_h13 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "13"))
model_c_h14 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "14"))
model_c_h15 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "15"))
model_c_h16 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "16"))
model_c_h17 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "17"))
model_c_h18 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "18"))
model_c_h19 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "19"))
model_c_h20 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "20"))
model_c_h21 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "21"))
model_c_h22 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "22"))
model_c_h23 <- lm(consumption ~ lag168 + lag48, data = df_b_train %>% filter(hour == "23"))

## Preparing test sets
df_c_test_h0 <- df_b_test %>% filter(hour == "00")
df_c_test_h1 <- df_b_test %>% filter(hour == "01")
df_c_test_h2 <- df_b_test %>% filter(hour == "02")
df_c_test_h3 <- df_b_test %>% filter(hour == "03")
df_c_test_h4 <- df_b_test %>% filter(hour == "04")
df_c_test_h5 <- df_b_test %>% filter(hour == "05")
df_c_test_h6 <- df_b_test %>% filter(hour == "06")
df_c_test_h7 <- df_b_test %>% filter(hour == "07")
df_c_test_h8 <- df_b_test %>% filter(hour == "08")
df_c_test_h9 <- df_b_test %>% filter(hour == "09")
df_c_test_h10 <- df_b_test %>% filter(hour == "10")
df_c_test_h11 <- df_b_test %>% filter(hour == "11")
df_c_test_h12 <- df_b_test %>% filter(hour == "12")
df_c_test_h13 <- df_b_test %>% filter(hour == "13")
df_c_test_h14 <- df_b_test %>% filter(hour == "14")
df_c_test_h15 <- df_b_test %>% filter(hour == "15")
df_c_test_h16 <- df_b_test %>% filter(hour == "16")
df_c_test_h17 <- df_b_test %>% filter(hour == "17")
df_c_test_h18 <- df_b_test %>% filter(hour == "18")
df_c_test_h19 <- df_b_test %>% filter(hour == "19")
df_c_test_h20 <- df_b_test %>% filter(hour == "20")
df_c_test_h21 <- df_b_test %>% filter(hour == "21")
df_c_test_h22 <- df_b_test %>% filter(hour == "22")
df_c_test_h23 <- df_b_test %>% filter(hour == "23")

## Preparing test predictions and testing models on the test sets

### Hour 0
df_c_test_pred_h0 <- predict(model_c_h0, newdata = df_c_test_h0)
summary((abs(df_c_test_h0$consumption-df_c_test_pred_h0) / abs(df_c_test_h0$consumption)) * 100)

### Hour 1
df_c_test_pred_h1 <- predict(model_c_h1, newdata = df_c_test_h1)
summary((abs(df_c_test_h1$consumption-df_c_test_pred_h1) / abs(df_c_test_h1$consumption)) * 100)

### Hour 2
df_c_test_pred_h2 <- predict(model_c_h2, newdata = df_c_test_h2)
summary((abs(df_c_test_h2$consumption-df_c_test_pred_h2) / abs(df_c_test_h2$consumption)) * 100)

### Hour 3
df_c_test_pred_h3 <- predict(model_c_h3, newdata = df_c_test_h3)
summary((abs(df_c_test_h3$consumption-df_c_test_pred_h3) / abs(df_c_test_h3$consumption)) * 100)

### Hour 4
df_c_test_pred_h4 <- predict(model_c_h4, newdata = df_c_test_h4)
summary((abs(df_c_test_h4$consumption-df_c_test_pred_h4) / abs(df_c_test_h4$consumption)) * 100)

### Hour 5
df_c_test_pred_h5 <- predict(model_c_h5, newdata = df_c_test_h5)
summary((abs(df_c_test_h5$consumption-df_c_test_pred_h5) / abs(df_c_test_h5$consumption)) * 100)

### Hour 6
df_c_test_pred_h6 <- predict(model_c_h6, newdata = df_c_test_h6)
summary((abs(df_c_test_h6$consumption-df_c_test_pred_h6) / abs(df_c_test_h6$consumption)) * 100)

### Hour 7
df_c_test_pred_h7 <- predict(model_c_h7, newdata = df_c_test_h7)
summary((abs(df_c_test_h7$consumption-df_c_test_pred_h7) / abs(df_c_test_h7$consumption)) * 100)

### Hour 8
df_c_test_pred_h8 <- predict(model_c_h8, newdata = df_c_test_h8)
summary((abs(df_c_test_h8$consumption-df_c_test_pred_h8) / abs(df_c_test_h8$consumption)) * 100)

### Hour 9
df_c_test_pred_h9 <- predict(model_c_h9, newdata = df_c_test_h9)
summary((abs(df_c_test_h9$consumption-df_c_test_pred_h9) / abs(df_c_test_h9$consumption)) * 100)

### Hour 10
df_c_test_pred_h10 <- predict(model_c_h10, newdata = df_c_test_h10)
summary((abs(df_c_test_h10$consumption-df_c_test_pred_h10) / abs(df_c_test_h10$consumption)) * 100)

### Hour 11
df_c_test_pred_h11 <- predict(model_c_h11, newdata = df_c_test_h11)
summary((abs(df_c_test_h11$consumption-df_c_test_pred_h11) / abs(df_c_test_h11$consumption)) * 100)

### Hour 12
df_c_test_pred_h12 <- predict(model_c_h12, newdata = df_c_test_h12)
summary((abs(df_c_test_h12$consumption-df_c_test_pred_h12) / abs(df_c_test_h12$consumption)) * 100)

### Hour 13
df_c_test_pred_h13 <- predict(model_c_h13, newdata = df_c_test_h13)
summary((abs(df_c_test_h13$consumption-df_c_test_pred_h13) / abs(df_c_test_h13$consumption)) * 100)

### Hour 14
df_c_test_pred_h14 <- predict(model_c_h14, newdata = df_c_test_h14)
summary((abs(df_c_test_h14$consumption-df_c_test_pred_h14) / abs(df_c_test_h14$consumption)) * 100)

### Hour 15
df_c_test_pred_h15 <- predict(model_c_h15, newdata = df_c_test_h15)
summary((abs(df_c_test_h15$consumption-df_c_test_pred_h15) / abs(df_c_test_h15$consumption)) * 100)

### Hour 16
df_c_test_pred_h16 <- predict(model_c_h16, newdata = df_c_test_h16)
summary((abs(df_c_test_h16$consumption-df_c_test_pred_h16) / abs(df_c_test_h16$consumption)) * 100)

### Hour 17
df_c_test_pred_h17 <- predict(model_c_h17, newdata = df_c_test_h17)
summary((abs(df_c_test_h17$consumption-df_c_test_pred_h17) / abs(df_c_test_h17$consumption)) * 100)

### Hour 18
df_c_test_pred_h18 <- predict(model_c_h18, newdata = df_c_test_h18)
summary((abs(df_c_test_h18$consumption-df_c_test_pred_h18) / abs(df_c_test_h18$consumption)) * 100)

### Hour 19
df_c_test_pred_h19 <- predict(model_c_h19, newdata = df_c_test_h19)
summary((abs(df_c_test_h19$consumption-df_c_test_pred_h19) / abs(df_c_test_h19$consumption)) * 100)

### Hour 20
df_c_test_pred_h20 <- predict(model_c_h20, newdata = df_c_test_h20)
summary((abs(df_c_test_h20$consumption-df_c_test_pred_h20) / abs(df_c_test_h20$consumption)) * 100)

### Hour 21
df_c_test_pred_h21 <- predict(model_c_h21, newdata = df_c_test_h21)
summary((abs(df_c_test_h21$consumption-df_c_test_pred_h21) / abs(df_c_test_h21$consumption)) * 100)

### Hour 22
df_c_test_pred_h22 <- predict(model_c_h22, newdata = df_c_test_h22)
summary((abs(df_c_test_h22$consumption-df_c_test_pred_h22) / abs(df_c_test_h22$consumption)) * 100)

### Hour 23
df_c_test_pred_h23 <- predict(model_c_h23, newdata = df_c_test_h23)
summary((abs(df_c_test_h23$consumption-df_c_test_pred_h23) / abs(df_c_test_h23$consumption)) * 100)

# TASK D

## Preparing feature matrix and target data set
df_d <- df %>% 
  mutate(lag_day7 = lag(consumption, 168),
         lag_day2 = lag(consumption, 48),
         id = row_number()) %>% 
  filter(id > 168) %>% select(-id)

df_d_matrix <- df_d %>% select(-consumption) %>% pivot_wider(names_from = hour, values_from = c(lag_day7, lag_day2))

df_d_matrix_train <- df_d_matrix %>% filter(date < as_date('2020-03-01')) %>% select(-date) %>%  as.matrix()
df_d_matrix_test <- df_d_matrix %>% filter(date >= as_date('2020-03-01')) %>% select(-date) %>% as.matrix()
head(df_d_matrix_train)

df_d_target_train <- df_d %>% select(date, hour, consumption) %>% filter(date < as_date('2020-03-01'))
df_d_target_test <- df_d %>% select(date, hour, consumption) %>% filter(date >= as_date('2020-03-01'))
head(df_d_target_train)

## Establishing lasso regression models by using feature matrix and target variables for each hour  
model_d_h0 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "00") %>% pull(consumption), type.measure = "mse")
model_d_h1 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "01") %>% pull(consumption), type.measure = "mse")
model_d_h2 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "02") %>% pull(consumption), type.measure = "mse")
model_d_h3 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "03") %>% pull(consumption), type.measure = "mse")
model_d_h4 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "04") %>% pull(consumption), type.measure = "mse")
model_d_h5 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "05") %>% pull(consumption), type.measure = "mse")
model_d_h6 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "06") %>% pull(consumption), type.measure = "mse")
model_d_h7 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "07") %>% pull(consumption), type.measure = "mse")
model_d_h8 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "08") %>% pull(consumption), type.measure = "mse")
model_d_h9 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "09") %>% pull(consumption), type.measure = "mse")
model_d_h10 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "10") %>% pull(consumption), type.measure = "mse")
model_d_h11 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "11") %>% pull(consumption), type.measure = "mse")
model_d_h12 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "12") %>% pull(consumption), type.measure = "mse")
model_d_h13 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "13") %>% pull(consumption), type.measure = "mse")
model_d_h14 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "14") %>% pull(consumption), type.measure = "mse")
model_d_h15 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "15") %>% pull(consumption), type.measure = "mse")
model_d_h16 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "16") %>% pull(consumption), type.measure = "mse")
model_d_h17 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "17") %>% pull(consumption), type.measure = "mse")
model_d_h18 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "18") %>% pull(consumption), type.measure = "mse")
model_d_h19 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "19") %>% pull(consumption), type.measure = "mse")
model_d_h20 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "20") %>% pull(consumption), type.measure = "mse")
model_d_h21 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "21") %>% pull(consumption), type.measure = "mse")
model_d_h22 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "22") %>% pull(consumption), type.measure = "mse")
model_d_h23 <- cv.glmnet(df_d_matrix_train, df_d_target_train %>% filter(hour == "23") %>% pull(consumption), type.measure = "mse")

## Preparing test set realizations
df_d_target_test_h0 <- df_d_target_test %>% filter(hour == "00") 
df_d_target_test_h1 <- df_d_target_test %>% filter(hour == "01") 
df_d_target_test_h2 <- df_d_target_test %>% filter(hour == "02") 
df_d_target_test_h3 <- df_d_target_test %>% filter(hour == "03") 
df_d_target_test_h4 <- df_d_target_test %>% filter(hour == "04") 
df_d_target_test_h5 <- df_d_target_test %>% filter(hour == "05") 
df_d_target_test_h6 <- df_d_target_test %>% filter(hour == "06") 
df_d_target_test_h7 <- df_d_target_test %>% filter(hour == "07") 
df_d_target_test_h8 <- df_d_target_test %>% filter(hour == "08") 
df_d_target_test_h9 <- df_d_target_test %>% filter(hour == "09") 
df_d_target_test_h10 <- df_d_target_test %>% filter(hour == "10") 
df_d_target_test_h11 <- df_d_target_test %>% filter(hour == "11") 
df_d_target_test_h12 <- df_d_target_test %>% filter(hour == "12") 
df_d_target_test_h13 <- df_d_target_test %>% filter(hour == "13") 
df_d_target_test_h14 <- df_d_target_test %>% filter(hour == "14") 
df_d_target_test_h15 <- df_d_target_test %>% filter(hour == "15") 
df_d_target_test_h16 <- df_d_target_test %>% filter(hour == "16") 
df_d_target_test_h17 <- df_d_target_test %>% filter(hour == "17") 
df_d_target_test_h18 <- df_d_target_test %>% filter(hour == "18") 
df_d_target_test_h19 <- df_d_target_test %>% filter(hour == "19") 
df_d_target_test_h20 <- df_d_target_test %>% filter(hour == "20") 
df_d_target_test_h21 <- df_d_target_test %>% filter(hour == "21") 
df_d_target_test_h22 <- df_d_target_test %>% filter(hour == "22") 
df_d_target_test_h23 <- df_d_target_test %>% filter(hour == "23") 

## Preparing test predictions and testing models on the test sets also stating model coefficients

### Hour 0
df_d_test_pred_h0 <- predict(model_d_h0, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h0$consumption-df_d_test_pred_h0) / abs(df_d_target_test_h0$consumption)) * 100)
coef(model_d_h0, s = "lambda.min")

### Hour 1
df_d_test_pred_h1 <- predict(model_d_h1, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h1$consumption-df_d_test_pred_h1) / abs(df_d_target_test_h1$consumption)) * 100)
coef(model_d_h1, s = "lambda.min")

### Hour 2
df_d_test_pred_h2 <- predict(model_d_h2, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h2$consumption-df_d_test_pred_h2) / abs(df_d_target_test_h2$consumption)) * 100)
coef(model_d_h2, s = "lambda.min")

### Hour 3
df_d_test_pred_h3 <- predict(model_d_h3, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h3$consumption-df_d_test_pred_h3) / abs(df_d_target_test_h3$consumption)) * 100)
coef(model_d_h3, s = "lambda.min")

### Hour 4
df_d_test_pred_h4 <- predict(model_d_h4, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h4$consumption-df_d_test_pred_h4) / abs(df_d_target_test_h4$consumption)) * 100)
coef(model_d_h4, s = "lambda.min")

### Hour 5
df_d_test_pred_h5 <- predict(model_d_h5, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h5$consumption-df_d_test_pred_h5) / abs(df_d_target_test_h5$consumption)) * 100)
coef(model_d_h5, s = "lambda.min")

### Hour 6
df_d_test_pred_h6 <- predict(model_d_h6, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h6$consumption-df_d_test_pred_h6) / abs(df_d_target_test_h6$consumption)) * 100)
coef(model_d_h6, s = "lambda.min")

### Hour 7
df_d_test_pred_h7 <- predict(model_d_h7, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h7$consumption-df_d_test_pred_h7) / abs(df_d_target_test_h7$consumption)) * 100)
coef(model_d_h7, s = "lambda.min")

### Hour 8
df_d_test_pred_h8 <- predict(model_d_h8, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h8$consumption-df_d_test_pred_h8) / abs(df_d_target_test_h8$consumption)) * 100)
coef(model_d_h8, s = "lambda.min")

### Hour 9
df_d_test_pred_h9 <- predict(model_d_h9, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h9$consumption-df_d_test_pred_h9) / abs(df_d_target_test_h9$consumption)) * 100)
coef(model_d_h9, s = "lambda.min")

### Hour 10
df_d_test_pred_h10 <- predict(model_d_h10, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h10$consumption-df_d_test_pred_h10) / abs(df_d_target_test_h10$consumption)) * 100)
coef(model_d_h10, s = "lambda.min")

### Hour 11
df_d_test_pred_h11 <- predict(model_d_h11, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h11$consumption-df_d_test_pred_h11) / abs(df_d_target_test_h11$consumption)) * 100)
coef(model_d_h11, s = "lambda.min")

### Hour 12
df_d_test_pred_h12 <- predict(model_d_h12, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h12$consumption-df_d_test_pred_h12) / abs(df_d_target_test_h12$consumption)) * 100)
coef(model_d_h12, s = "lambda.min")

### Hour 13
df_d_test_pred_h13 <- predict(model_d_h13, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h13$consumption-df_d_test_pred_h13) / abs(df_d_target_test_h13$consumption)) * 100)
coef(model_d_h13, s = "lambda.min")

### Hour 14
df_d_test_pred_h14 <- predict(model_d_h14, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h14$consumption-df_d_test_pred_h14) / abs(df_d_target_test_h14$consumption)) * 100)
coef(model_d_h14, s = "lambda.min")

### Hour 15
df_d_test_pred_h15 <- predict(model_d_h15, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h15$consumption-df_d_test_pred_h15) / abs(df_d_target_test_h15$consumption)) * 100)
coef(model_d_h15, s = "lambda.min")

### Hour 16
df_d_test_pred_h16 <- predict(model_d_h16, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h16$consumption-df_d_test_pred_h16) / abs(df_d_target_test_h16$consumption)) * 100)
coef(model_d_h16, s = "lambda.min")

### Hour 17
df_d_test_pred_h17 <- predict(model_d_h17, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h17$consumption-df_d_test_pred_h17) / abs(df_d_target_test_h17$consumption)) * 100)
coef(model_d_h17, s = "lambda.min")

### Hour 18
df_d_test_pred_h18 <- predict(model_d_h18, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h18$consumption-df_d_test_pred_h18) / abs(df_d_target_test_h18$consumption)) * 100)
coef(model_d_h18, s = "lambda.min")

### Hour 19
df_d_test_pred_h19 <- predict(model_d_h19, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h19$consumption-df_d_test_pred_h19) / abs(df_d_target_test_h19$consumption)) * 100)
coef(model_d_h19, s = "lambda.min")

### Hour 20
df_d_test_pred_h20 <- predict(model_d_h20, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h20$consumption-df_d_test_pred_h20) / abs(df_d_target_test_h20$consumption)) * 100)
coef(model_d_h20, s = "lambda.min")

### Hour 21
df_d_test_pred_h21 <- predict(model_d_h21, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h21$consumption-df_d_test_pred_h21) / abs(df_d_target_test_h21$consumption)) * 100)
coef(model_d_h21, s = "lambda.min")

### Hour 22
df_d_test_pred_h22 <- predict(model_d_h22, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h22$consumption-df_d_test_pred_h22) / abs(df_d_target_test_h22$consumption)) * 100)
coef(model_d_h22, s = "lambda.min")

### Hour 23
df_d_test_pred_h23 <- predict(model_d_h23, df_d_matrix_test, s = "lambda.min")
summary((abs(df_d_target_test_h23$consumption-df_d_test_pred_h23) / abs(df_d_target_test_h23$consumption)) * 100)
coef(model_d_h23, s = "lambda.min")






