library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(skimr)
library(DataExplorer)
library(GGally)

# Read Data

bike_train <- vroom("train.csv")
bike_train <- bike_train  |> 
  mutate(
    season = as.factor(season),
    weather = as.factor(weather),
    workingday = as.factor(workingday),
    holiday = as.factor(holiday)
  )  |> 
  select(-casual, -registered)

bike_test <- vroom("test.csv")
bike_test <- bike_test %>%
  mutate(
    season = as.factor(season),
    weather = as.factor(weather),
    workingday = as.factor(workingday),
    holiday = as.factor(holiday)
  )

View(bike_train)

# EDA

glimpse(bike_train)
skimr(bike_train)

bike_train |> 
  ggplot(aes(x=temp,
             y=count)) +
  geom_point() +
  geom_smooth(se = F)

plot_histogram(bike_train)

plot_correlation(bike_train)

## Issue of counts
plot1 <- bike_train |>
  ggplot(aes(x=weather)) +
  geom_bar()

## Distribution of data by seasons
plot2 <- bike_train |> 
  ggplot(aes(x = season, y = count)) +
  geom_boxplot()

## Issue of auto-correlation
plot3 <- bike_train |> 
  ggplot(aes(x = temp, y = atemp)) +
  geom_point() +
  geom_smooth(method = "lm")

## Distribution of Casual vs. Registered
plot4 <- bike_train |> 
  ggplot() +
  geom_histogram(aes(x = casual, fill = "casual"),
                 alpha = 0.5, bins = 30) +
  geom_histogram(aes(x = registered, fill = "registered"),
                 alpha = 0.5, bins = 30) +
  scale_fill_manual(values = c("casual" = "blue", "registered" = "red")) +
  labs(x = "count",
       y = "frequency",
       fill = "color") +
  theme_minimal()

(plot1 + plot2) / (plot3 + plot4)

# Fit Model

bike_lm <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression") |> 
  fit(formula=log(count)~ ., data=bike_train)

bike_predictions <- predict(bike_lm, new_data = bike_test)

# Write Data Set to Computer

kaggle_submission <- bike_predictions |> 
  bind_cols(bike_test) |> 
  select(datetime, .pred) |> 
  rename(count=.pred) |> 
  mutate(count=pmax(0, count)) |> 
  mutate(datetime=as.character(format(datetime)),
         count=exp(count))

vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim = ",")