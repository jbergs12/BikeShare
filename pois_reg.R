library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(skimr)
library(DataExplorer)
library(GGally)
library(poissonreg)

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

# Fit Model

bike_pois <- poisson_reg() |> 
  set_engine("glm") |> 
  set_mode("regression") |> 
  fit(formula=count~., data=bike_train)

bike_predictions <- predict(bike_pois,
                            new_data = bike_test)

# Write Data Set to Computer

pois_kaggle_submission <- bike_predictions |> 
  bind_cols(bike_test) |> 
  select(datetime, .pred) |> 
  rename(count=.pred) |>
  mutate(datetime=as.character(format(datetime)))

vroom_write(x=pois_kaggle_submission, file="./PoissonPreds.csv", delim = ",")
