library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(skimr)
library(DataExplorer)
library(GGally)
library(glmnet)

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
    weather=as.factor(ifelse(weather==4, weather==3, weather)),
    holiday=as.factor(holiday),
    workingday=as.factor(workingday)) |> 
  step_date(datetime, features = "month") |>
  step_time(datetime, features = "hour", keep_original_cols = F) |>
  step_mutate(datetime_hour=as.factor(datetime_hour),
              datetime_month=as.factor(datetime_month)) |>
  step_dummy(all_nominal_predictors()) |> 
  step_normalize(all_predictors())

# prepped_recipe = prep(bike_recipe)
# bake(prepped_recipe, new_data=bike_test)

bike_lm <- linear_reg(penalty=2, mixture=.2) |> 
  set_engine("glmnet")

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

vroom_write(x=kaggle_submission, file="./Linear_Pen_Preds.csv", delim = ",")
