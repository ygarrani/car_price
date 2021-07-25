# Capstone Project : Used car price
# HarvardX : Data Science Professional Certificate

# load libraries and install its if require ####
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(data.table)) install.packages("data.table")
if(!require(corrplot)) install.packages("corrplot")
if(!require(caret)) install.packages("caret")
if(!require(xgboost)) install.packages("xgboost")

library(tidyverse)
library(data.table)
library(corrplot)
library(caret)
library(xgboost)

# load data and assemble in one file ####
unzip("cars.zip")

audi <- fread("audi.csv")
audi <- audi %>% add_column(.before = "model",
                            brand = rep("audi", times = nrow(audi)))

bmw <- fread("bmw.csv")
bmw <- bmw %>% add_column(.before = "model",
                            brand = rep("bmw", times = nrow(bmw)))

mercedes <- fread("merc.csv")
mercedes <- mercedes %>% add_column(.before = "model",
                            brand = rep("mercedes", times = nrow(mercedes)))

ford <- fread("ford.csv")
ford <- ford %>% add_column(.before = "model",
                            brand = rep("ford", times = nrow(ford)))

hyundai <- fread("hyundi.csv")
hyundai <- hyundai %>% add_column(.before = "model",
                            brand = rep("hyundai", times = nrow(hyundai)))
names(hyundai)
# change name "tax(£) to "tax" in Hyundai
hyundai <- rename(hyundai, tax = `tax(£)`)

skoda <- fread("skoda.csv")
skoda <- skoda %>% add_column(.before = "model",
                            brand = rep("skoda", times = nrow(skoda)))

toyota <- fread("toyota.csv")
toyota <- toyota %>% add_column(.before = "model",
                            brand = rep("toyota", times = nrow(toyota)))

vauxhall <- fread("vauxhall.csv")
vauxhall <- vauxhall %>% add_column(.before = "model",
                            brand = rep("vauxhall", times = nrow(vauxhall)))

vw <- fread("vw.csv")
vw <- vw %>% add_column(.before = "model",
                            brand = rep("vw", times = nrow(vw)))

cars <- rbind(audi, bmw, mercedes, ford, hyundai, skoda, toyota, vauxhall, vw)
fwrite(cars, file = "cars_price.csv")

rm(audi, bmw, ford, hyundai, mercedes, skoda, toyota, vauxhall, vw)
file.remove("audi.csv", "bmw.csv", "merc.csv", "ford.csv", "hyundi.csv",
            "skoda.csv", "toyota.csv", "vauxhall.csv", "vw.csv",
            "cclass.csv", "focus.csv", "unclean cclass.csv", "unclean focus.csv")

# initial data exploration and visualization ####
dim(cars) # 98926 rows and 10 variables

# search for duplicated rows
sum(duplicated(cars)) # 1475 rows are duplicated

# remove duplicated rows
cars <- cars %>% distinct() # 97451

# look for NAs
sapply(cars, function(i){
  sum(is.na(i))
})

str(cars)
head(cars)

# brand
cars %>% summarise(unique(brand))
# there are 9 brands

# distribution of brands
cars %>% group_by(brand) %>% summarise(n = n()) %>%
  arrange(desc(n))

# plot of brands from the most popular to the least
cars %>% group_by(brand) %>% summarise(n= n()) %>%
  ggplot(aes(x = reorder(brand, -n), y = n, fill = brand)) +
  geom_bar(stat = "identity") +
  guides(fill = "none") +
  labs(x = "")

# model ####
cars %>% summarise(unique(model)) %>% count()
# there are 186 models

model_price <- cars %>% group_by(model) %>% 
  summarise(median(price), n = n()) %>%
  arrange(n)
model_price
# there are 50 over 186 models with less than 30 vehicles per model
rm(model_price)

# plot of models
cars %>% ggplot(aes(x = model, y = price, fill = brand)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.85, size = 6))

# year ####
summary(cars$year)
# there is a max value of 2060 in variable "year" for a Ford Fiesta

# search for another non-logic value
cars %>% filter(year > 2021)

# find identical metrics
cars %>% filter(model == "Fiesta", fuelType == "Petrol", engineSize == 1.4,
                mileage >= 54807/1.2 & mileage <= 54807*1.2,
                year != 2060) %>% 
  pull(year) %>% median()
# there are 11 Ford Fiesta with roughly same metrics and median year = 2010

# replace year 2060 by the median 2010
cars$year[cars$year == 2060] <- 2010

# check the range of year now
range(cars$year)

# table of year distribution
cars %>% group_by(year) %>% summarise(n = n())
# there are 2 vehicles with year = 1970 and these models are not from this period

# replace per the median for the first one
cars %>% filter(year == 1970)
cars %>% filter(year != 1970, model == "M Class", fuelType == "Diesel",
                mileage <= 14000*1.5 & mileage >= 14000/1.5) %>%
  .$year %>% median()

cars$year[cars$year == 1970 & cars$model == "M Class"] <- 2015

# replace per the median for the second one
cars %>% filter(year != 1970, model == "Zafira", fuelType == "Petrol",
                mileage <= 37357*1.2 & mileage >= 37357/1.2,
                engineSize == 1.4, transmission == "Manual") %>%
  .$year %>% median()

cars$year[cars$year == 1970 & cars$model == "Zafira"] <- 2017

# plot for the year distribution
cars %>% group_by(year) %>% summarise(n = n()) %>%
  ggplot(aes(x = year, y = n)) +
  geom_bar(stat = "identity")

# price ####
summary(cars$price)
cars %>% ggplot(aes(x = price)) +
  geom_histogram(color = "black")
# very few cars have price > 50000

cars %>% filter(price > 50000) %>% summarise(n()) # roughly 1% of all vehicles
cars %>% filter(price < 50000) %>%
  ggplot(aes(y = price)) + geom_boxplot()

# transmission ####
table(cars$transmission)
cars %>% filter(transmission == "Other")

# plot of transmission distribution
cars %>% group_by(transmission) %>% summarise(n = n()) %>%
  ggplot(aes(x = reorder(transmission, -n), y = n, fill = transmission)) +
  geom_bar(stat = "identity") +
  guides(fill = "none") +
  labs(title = "Transmission", x = "")

# mile_age ####
summary(cars$mileage)

# plot of mileage distribution
cars %>% ggplot(aes(x = mileage)) +
  geom_area(stat = "bin")

# fuelType ####
table(cars$fuelType)

cars %>% group_by(fuelType) %>% summarise(n = n()) %>%
  ggplot(aes(x = reorder(fuelType, -n), y = n, fill = fuelType)) +
  geom_bar(stat = "identity") +
  guides(fill = "none") +
  labs(title = "Fuel type", x = "")

# tax ####
summary(cars$tax)
table(cars$tax)

# histogram of tax distribution
cars %>% ggplot(aes(x = tax)) +
  geom_histogram(binwidth = 10)

# mpg ####
summary(cars$mpg)
table(cars$mpg)

# histogram of mpg " miles per gallon"
cars %>% ggplot(aes(x = mpg)) +
  geom_histogram(binwidth = 30)

# engineSize ####
summary(cars$engineSize)
table(cars$engineSize) # 273 vehicles have engine size = 0

# the assumption is electrical vehicles have engine size = 0
cars %>% filter(fuelType == "Electric") # 2 of 6 have engine size = 0

cars %>% filter(engineSize != 0) %>%
  summarise(median(engineSize)) # the median engine size is 1.6

# replace per the median except for the 2 electrical vehicles
cars$engineSize[cars$fuelType != "Electric" & cars$engineSize == 0] <- 1.6

table(cars$engineSize)

# plot of engine size
cars %>% ggplot(aes(engineSize)) +
  geom_histogram()
# most vehicles have less than 1 or 1.6 or 2 or 2.5

# EDA ####
# search for price correlation
# with brand
cars %>% filter(price < 50000) %>%
  ggplot(aes(brand, price, color = brand)) + 
  geom_boxplot() +
  theme(legend.position = "none") +
  labs(x = "")

# with year
cars %>% ggplot(aes(year, price)) +
  geom_smooth()

cars %>%  filter(price < 50000) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, price)) + 
  geom_boxplot() + 
  ggtitle("Price per year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.85, size = 8))

# with transmission
cars %>%  filter(price < 50000) %>%
  ggplot(aes(transmission, price)) + geom_boxplot()

cars %>% filter(price < 50000) %>% 
  select(brand, price, transmission) %>% 
  ggplot(aes(x = brand, y = price, color = transmission)) +
  geom_boxplot() +
  ggtitle("Price per brand per transmission") +
  theme(axis.text.x = element_text(angle = 70, hjust = 0.85, size = 8)) +
  labs(x = "")

# with mileage
cars %>% ggplot(aes(mileage, price)) + geom_smooth()

# with fuel type
cars %>% filter(price < 50000) %>%
  ggplot(aes(fuelType, price)) + geom_boxplot()

cars %>% filter(price < 50000) %>%
  ggplot(aes(brand, price, color = fuelType)) +
  geom_boxplot() +
  ggtitle("Price per brand per fuel type") +
  theme(axis.text.x = element_text(angle = 70, hjust = 0.85, size = 8))

# with tax
cars %>% ggplot(aes(tax, price)) + geom_smooth()

# with mpg
cars %>% ggplot(aes(mpg, price)) + geom_smooth()

# with engine size
cars %>% ggplot(aes(engineSize, price)) + geom_point()
cars %>% ggplot(aes(engineSize, price)) + geom_smooth()

# add an interval column of mileage depending on quarter
cars <- cars %>% mutate(.after = "mileage", mile_interval =
                  case_when(mileage <= 7414 ~ 1,
                            mileage > 7414 & mileage <= 17444 ~ 2,
                            mileage > 17444 & mileage <= 32300 ~ 3,
                            mileage > 32300 ~ 4))

# corrplot ####
options(repr.plot.width = 10, repr.plot.height = 8)
corrplot(cars %>% select(where(is.numeric)) %>% 
           cor(), method = "number")

# data preparation ####
set.seed(2021, sample.kind = "Rounding")

# test set will be 20% of cars data
cars <- cars %>% as.data.frame()
test_index <- createDataPartition(y = cars$price, times = 1, p = 0.2,
                                list = FALSE)
train_set <- cars[-test_index, ] # 77960 rows
test_set <- cars[test_index, ] # 19491 rows

# make sure models in test set are also in training set
test_set %>% anti_join(train_set, by = "model") # 1 entries
removed <- test_set %>% anti_join(train_set, by = "model")

# remove these 4 entries from test set
test_set <- test_set %>% semi_join(train_set, by = "model")
dim(test_set) # 19490 rows

# and add its to training set
train_set <- rbind(train_set, removed)
dim(train_set) # 77961

rm(removed)

# machine learning ####
# RMSE
RMSE <- function(true_price, predicted_price){
  sqrt(mean((true_price - predicted_price)^2))
}

# first model
mu <- mean(train_set$price)
mu

# if we predicted all cars with the mean
naive_rmse <- RMSE(true_price = test_set$price, predicted_price = mu)
naive_rmse

results <- tibble(method = "Just the average", rmse = naive_rmse)
results
rm(naive_rmse)

# model effect
model_avg <- train_set %>%
  group_by(model) %>%
  summarise(b_model = mean(price - mu))

predicted_price <- test_set %>%
  left_join(model_avg, by = "model") %>%
  mutate(pred = mu + b_model) %>%
  pull(pred)

model_effect <- RMSE(test_set$price, predicted_price)
results <- results %>% add_row(method = "model_effect", 
                               rmse = model_effect)
results
rm(model_effect)

# year effect
year_avg <- train_set %>%
  left_join(model_avg, by = "model") %>%
  group_by(year) %>%
  summarise(b_year = mean(price - mu - b_model))

predicted_price <- test_set %>%
  left_join(model_avg, by = "model") %>%
  left_join(year_avg, by = "year") %>%
  mutate(pred = mu + b_model + b_year) %>%
  pull(pred)

year_effect <- RMSE(test_set$price, predicted_price)
results <- results %>% add_row(method = "year_effect", 
                               rmse = year_effect)
results
rm(year_effect)

# engine size effect
engineSize_avg <- train_set %>%
  left_join(model_avg, by = "model") %>%
  left_join(year_avg, by = "year") %>%
  group_by(engineSize) %>%
  summarise(b_engine = mean(price - mu - b_model - b_year))

predicted_price <- test_set %>%
  left_join(model_avg, by = "model") %>%
  left_join(year_avg, by = "year") %>%
  left_join(engineSize_avg, by = "engineSize") %>%
  mutate(b_engine = ifelse(!is.na(b_engine), b_engine, 0)) %>%
  mutate(pred = mu + b_model + b_year + b_engine) %>%
  pull(pred)

engineSize_effect <- RMSE(test_set$price, predicted_price)
results <- results %>% add_row(method = "engineSize_effect", 
                               rmse = engineSize_effect)
results
rm(engineSize_effect)

# mile_interval effect
mile_interval_avg <- train_set %>%
  left_join(model_avg, by = "model") %>%
  left_join(year_avg, by = "year") %>%
  left_join(engineSize_avg, by = "engineSize") %>%
  group_by(mile_interval) %>%
  summarise(b_mile_interval = mean(price - mu - b_model - b_year - b_engine))

predicted_price <- test_set %>%
  left_join(model_avg, by = "model") %>%
  left_join(year_avg, by = "year") %>%
  left_join(engineSize_avg, by = "engineSize") %>%
  mutate(b_engine = ifelse(!is.na(b_engine), b_engine, 0)) %>%
  left_join(mile_interval_avg, by = "mile_interval") %>%
  mutate(pred = mu + b_model + b_year + b_engine + b_mile_interval) %>%
  pull(pred)

mile_interval_effect <- RMSE(test_set$price, predicted_price)
results <- results %>% add_row(method = "mile_interval_effect", 
                               rmse = mile_interval_effect)
results
rm(mile_interval_effect)

# clean environment
rm(mu, model_avg, year_avg, engineSize_avg, mile_interval_avg, predicted_price)
rm(test_index, test_set, train_set)

# data processing ####
# numeric columns
cars <- cars %>% select(-c(brand, mile_interval))
numeric_vars <- cars %>%
  select(where(is.numeric)) %>% names()
numeric_vars

# categorical variables to binary ####
# transmission
cars <- cars %>%
  mutate(value = rep(1, nrow(cars))) %>%  
           spread(key = transmission, value = value, fill = 0)

# fuel type
cars <- cars %>%
  mutate(value = rep(1, nrow(cars))) %>%  
  spread(key = fuelType, value = value, fill = 0)

# model
cars <- cars %>%
  mutate(value = rep(1, nrow(cars))) %>%  
  spread(key = model, value = value, fill = 0)

# scale continuous variables
cars <- cars %>% mutate_at(numeric_vars[numeric_vars != "price"],
                                     .funs = scale)
rm(numeric_vars)

# data preparation 2 for machine learning model ####
set.seed(2021, sample.kind = "Rounding")

# test set will be 20% of cars data
test_index <- createDataPartition(y = cars$price, times = 1, p = 0.2,
                                  list = FALSE)
train_set <- cars[-test_index, ] # 77911 rows
test_set <- cars[test_index, ] # 19480 rows
rm(test_index)

# linear model with caret ####
# train the model
set.seed(2021, sample.kind = "Rounding")
train_lm <- train(price ~ ., method = "lm", data = train_set) # 3 minutes time

# predict the outcomes
lm_prediction <- predict(train_lm, newdata = test_set)

# RMSE
lm_rmse <- RMSE(test_set$price, lm_prediction)
results <- results %>% add_row(method = "lm", 
                               rmse = lm_rmse)
results
rm(train_lm, lm_prediction, lm_rmse)

# rpart model ####
# train the model
set.seed(2021, sample.kind = "Rounding")
x_train <- train_set %>% select(-price)
y_train <- train_set$price
train_rpart <- train(x_train, y_train, method = "rpart")

# predict the outcomes
rpart_prediction <- predict(train_rpart, newdata = test_set[-2])

# RMSE
rpart_rmse <- RMSE(test_set$price, rpart_prediction)
results <- results %>% add_row(method = "rpart", 
                               rmse = rpart_rmse)
results
rm(x_train, y_train, train_rpart, rpart_prediction, rpart_rmse)

# random forest ####
# train the model
set.seed(2021, sample.kind = "Rounding")
train_control <- trainControl(method = "repeatedcv", number = 5,
                              repeats = 5, p = 0.75)
grid <- expand.grid(.mtry = 1:5)

train_forest <- train(price ~ ., method = "rf", data = train_set,
                     trControl = train_control,
                     tuneGrid = grid,
                     nodesize = 10,
                     ntree = 5) # 7 minutes time

# predict the outcomes
forest_prediction <- predict(train_forest, newdata = test_set)

# RMSE
forest_rmse <- RMSE(test_set$price, forest_prediction)
results <- results %>% add_row(method = "random_forest", 
                               rmse = forest_rmse)
results
plot(forest_prediction, test_set$price)
rm(grid, train_control, train_forest, forest_prediction, forest_rmse)

# xgboost ####
# train the model
set.seed(2021, sample.kind = "Rounding")
x_train <- train_set %>% select(-price) %>%
  data.matrix()
y_train <- train_set$price

x_test <- test_set %>% select(-price) %>%
  data.matrix()
y_test <- test_set$price

train_xgb <- xgb.DMatrix(data = x_train, label = y_train)
test_xgb <- xgb.DMatrix(data = x_test, label = y_test)

watch_list <- list(train = train_xgb, test = test_xgb)

set.seed(2021, sample.kind = "Rounding")
model = xgb.train(data = train_xgb, max.depth = 8, watchlist = watch_list,
                  nrounds = 100)

final = xgboost(data = train_xgb, max.depth = 8, nrounds = 100)

# predict the outcomes
xgb_predictions <- predict(final, data.matrix(x_test))

# RMSE
xgb_rmse <- RMSE(xgb_predictions, y_test)
results <- results %>% add_row(method = "xgboost", 
                               rmse = xgb_rmse)
results
plot(xgb_predictions, y_test)

# xgboost is the most accurate, let's compare true_price ans predicted_price
compare <- tibble(true_price = test_set$price, 
                  predicted_price = round(xgb_predictions, digits = 0),
                  absolute_difference = abs(true_price - predicted_price)) %>%
  as.data.frame()
fwrite(compare, "compare.csv")

rm(test_xgb, train_xgb, xgb_predictions, xgb_rmse, y_test, y_train)









