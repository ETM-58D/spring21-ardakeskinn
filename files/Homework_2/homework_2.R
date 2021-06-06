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


# TASK 1

## Importing the csv file which show distance between the cities of Turkey
df_t1 <- read_csv("HW2_q1_data.csv") %>% as_tibble()
head(df_t1)

## Replacing NAs with zeros (0)
df_t1[is.na(df_t1)] <- 0
head(df_t1)

## Applying multidimensional scaling (MDS)
df_t1_fit <- cmdscale(df_t1, k = 2, eig = TRUE)
x_t1 <- df_t1_fit$points[, 1]
y_t1 <- df_t1_fit$points[, 2]

## Representing cities on a 2D plot using the reduced dimensions  
plot(x_t1, y_t1, pch = 19)
text(x_t1, y_t1, pos = 4, cex = 0.6, labels = colnames(df_t1))


# TASK 2

## Importing the Netflix rating data and movie titles
df_t2 <- read.table("HW2_q3_Netflix_data.dat", header=FALSE)
head(df_t2)
titles_t2 <- read_delim("HW2_q3_movie_titles.txt", delim = "/", col_names = FALSE)
head(titles_t2)

## Checking missing values
df_t2[is.na(df_t2)]
df_t2[df_t2==0]
df_t2[df_t2==0] <- NA
head(df_t2)

## Imputing missing entries with the mean of the ratings for a particular movie
df_t2 %<>% mutate_all(~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))          
head(df_t2)

## Creating euclidean distance matrix from the rating data by taking means of ratings for each movie
df_t2_dist_matrix <- dist(colMeans(df_t2), method = "euclidean", upper = TRUE)

## Applying multidimensional scaling (MDS)
df_t2_fit <- cmdscale(df_t2_dist_matrix, k = 2, eig = TRUE)
df_t2_fit
x_t2 <- df_t2_fit$points[, 1]
y_t2 <- df_t2_fit$points[, 2]

## Representing movies on a 2D plot using the reduced dimensions
plot(x_t2, y_t2, pch = 16)
text(x_t2, y_t2, pos = 4, cex = 0.5, labels = titles_t2$X1)


# TASK 3

## Importing the dataset
df_t3_x <- read.table("uWaveGestureLibrary_X_TRAIN", header=FALSE)
df_t3_y <- read.table("uWaveGestureLibrary_Y_TRAIN", header=FALSE)
df_t3_z <- read.table("uWaveGestureLibrary_Z_TRAIN", header=FALSE)

## Data manipulation by converting data to long format
df_t3_x_long <- df_t3_x %>% 
  as_tibble() %>% 
  mutate(time_series_id = row_number()) %>% 
  rename(class = V1) %>% 
  pivot_longer(cols = -c(time_series_id, class), names_to = "time_index", values_to = "X")

df_t3_y_long <- df_t3_y %>% 
  as_tibble() %>% 
  mutate(time_series_id = row_number()) %>% 
  rename(class = V1) %>% 
  pivot_longer(cols = -c(time_series_id, class), names_to = "time_index", values_to = "Y")

df_t3_z_long <- df_t3_z %>% 
  as_tibble() %>% 
  mutate(time_series_id = row_number()) %>% 
  rename(class = V1) %>% 
  pivot_longer(cols = -c(time_series_id, class), names_to = "time_index", values_to = "Z")

head(df_t3_x_long)

## Binding columns to concatenate the time series of X, Y and Z axis
df_t3_long <- bind_cols(df_t3_x_long, Y = df_t3_y_long$Y, Z = df_t3_z_long$Z) %>% mutate(class = as.factor(class))
head(df_t3_long)
summary(df_t3_long %>% select(X,Y,Z))

## Selecting one instance from each class
df_t3_class_1 <- df_t3_long %>% filter(time_series_id == 11 )
df_t3_class_2 <- df_t3_long %>% filter(time_series_id == 15 )
df_t3_class_3 <- df_t3_long %>% filter(time_series_id == 4 )
df_t3_class_4 <- df_t3_long %>% filter(time_series_id == 5 )
df_t3_class_5 <- df_t3_long %>% filter(time_series_id == 2 )
df_t3_class_6 <- df_t3_long %>% filter(time_series_id == 1 )
df_t3_class_7 <- df_t3_long %>% filter(time_series_id == 7 )
df_t3_class_8 <- df_t3_long %>% filter(time_series_id == 6 )

## Visualizing the gestures from each class
scatterplot3d(x=df_t3_class_1$X, 
              y=df_t3_class_1$Y, 
              z=df_t3_class_1$Z, 
              main = "3D Scatter Plot - Class 1",
              xlab = "X",
              ylab = "Y",
              zlab = "Z")

scatterplot3d(x=df_t3_class_2$X, 
              y=df_t3_class_2$Y, 
              z=df_t3_class_2$Z, 
              main = "3D Scatter Plot - Class 2",
              xlab = "X",
              ylab = "Y",
              zlab = "Z")

scatterplot3d(x=df_t3_class_3$X, 
              y=df_t3_class_3$Y, 
              z=df_t3_class_3$Z, 
              main = "3D Scatter Plot - Class 3",
              xlab = "X",
              ylab = "Y",
              zlab = "Z")

scatterplot3d(x=df_t3_class_4$X, 
              y=df_t3_class_4$Y, 
              z=df_t3_class_4$Z, 
              main = "3D Scatter Plot - Class 4",
              xlab = "X",
              ylab = "Y",
              zlab = "Z")

scatterplot3d(x=df_t3_class_5$X, 
              y=df_t3_class_5$Y, 
              z=df_t3_class_5$Z, 
              main = "3D Scatter Plot - Class 5",
              xlab = "X",
              ylab = "Y",
              zlab = "Z")

scatterplot3d(x=df_t3_class_6$X, 
              y=df_t3_class_6$Y, 
              z=df_t3_class_6$Z, 
              main = "3D Scatter Plot - Class 6",
              xlab = "X",
              ylab = "Y",
              zlab = "Z")

scatterplot3d(x=df_t3_class_7$X, 
              y=df_t3_class_7$Y, 
              z=df_t3_class_7$Z, 
              main = "3D Scatter Plot - Class 7",
              xlab = "X",
              ylab = "Y",
              zlab = "Z")

scatterplot3d(x=df_t3_class_8$X, 
              y=df_t3_class_8$Y, 
              z=df_t3_class_8$Z, 
              main = "3D Scatter Plot - Class 8",
              xlab = "X",
              ylab = "Y",
              zlab = "Z")


## Applying PCA
df_t3_pca <- princomp(df_t3_long %>% select(X,Y,Z), cor = T)
summary(df_t3_pca, loadings = T)

## Plotting variances of components
plot(df_t3_pca, type = "b", main = "Variances of Components")

## Visualizing each data point on a 2D scatter plot with the new coordinates (different colors for each class)
autoplot(df_t3_pca, data = df_t3_long, colour = "class")
