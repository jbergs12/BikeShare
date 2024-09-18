library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(skimr)
library(DataExplorer)
library(GGally)

bike_train <- vroom("train.csv")
bike_train <- bike_train  |> 
  select(-casual, -registered) |> 
  mutate(count=log(count))

bike_test <- vroom("test.csv")

bike_recipe <- recipe(count~., data = bike_train) |> 
  step_mutate(
    season=factor(season,
                  levels = 1:4,
                  labels = c("Spring", "Summer", "Fall", "Winter")),
    weather=as.factor(ifelse(4,3,weather)),
    holiday=as.factor(holiday),
    workingday=as.factor(workingday),
    datetime=as.POSIXct(datetime,
                        format = "%Y-%m-%d %H:%M:%S")) |> 
  step_date(datetime, features = c("year", "month", "dow")) |>
  step_time(datetime, features = c("hour")) |>
  step_zv(all_predictors()) |> 
  step_corr(all_double_predictors(), threshold = .7) |> 
  step_dummy(all_nominal_predictors())

# prepped_recipe = prep(bike_recipe)
# bake(prepped_recipe, new_data=bike_test)

bike_lm <- linear_reg() |> 
  set_engine("lm") |> 
  set_mode("regression")

bike_workflow <- workflow() |> 
  add_recipe(bike_recipe) |> 
  add_model(bike_lm) |> 
  fit(data=bike_train)

lin_preds <- predict(bike_workflow, new_data=bike_test)


# Write Data Set to Computer

kaggle_submission <- lin_preds |> 
  bind_cols(bike_test) |> 
  select(datetime, .pred) |> 
  rename(count=.pred) |> 
  mutate(count=pmax(0, count)) |> 
  mutate(datetime=as.character(format(datetime)),
         count=exp(count))

vroom_write(x=kaggle_submission, file="./Linear_WF_Preds.csv", delim = ",")
