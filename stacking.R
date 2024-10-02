library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(skimr)
library(DataExplorer)
library(GGally)
library(glmnet)
library(rpart)
library(ranger)
library(stacks)
library(parallel)

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
    weather=as.factor(ifelse(weather==4, 3, weather)),
    holiday=as.factor(holiday),
    workingday=as.factor(workingday)) |> 
  step_date(datetime, features = c("month", "dow")) |>
  step_time(datetime, features = "hour", keep_original_cols = F) |>
  step_mutate(datetime_hour=as.factor(datetime_hour),
              datetime_month=as.factor(datetime_month),
              datetime_dow=as.factor(datetime_dow)) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_predictors())

folds <- vfold_cv(bike_train, v = 5)

untunedModel <- control_stack_grid()
tunedModel <- control_stack_resamples()

# Penalized Linear Regression

bike_preg <- linear_reg(penalty=tune(),
                        mixture=tune()) |> 
  set_engine("glmnet")

bike_preg_wf <- workflow() |> 
  add_recipe(bike_recipe) |> 
  add_model(bike_preg)

tuning_grid_preg <- grid_regular(penalty(),
                                 mixture(),
                                 levels = 5)

preg_models <- bike_preg_wf |> 
  tune_grid(resamples=folds,
            grid=tuning_grid_preg,
            metrics=metric_set(rmse),
            control=untunedModel)

# Linear Regression

bike_lin_reg <- linear_reg() |> 
  set_engine("lm")

lin_reg_wf <- workflow() |> 
  add_recipe(bike_recipe) |> 
  add_model(bike_lin_reg)

lin_reg_model <- fit_resamples(lin_reg_wf,
                               resamples=folds,
                               metrics=metric_set(rmse),
                               control=tunedModel)

# Random Forest

bike_forest <- rand_forest(mtry = 52,
                           min_n = 11,
                           trees = 300) |> 
  set_engine("ranger") |> 
  set_mode("regression")

bike_forest_wf <- workflow() |> 
  add_recipe(bike_recipe) |> 
  add_model(bike_forest)

tuning_grid_forest <- grid_regular(
  mtry(range = c(1, ncol(juice(prep(bike_recipe))))-1),
  min_n(),
  levels = 5)

rforest_model <- bike_forest_wf |> 
  fit_resamples(resamples=folds,
                grid=tuning_grid_forest,
                metrics=metric_set(rmse),
                control=tunedModel)

my_stack <- stacks() |> 
  add_candidates(rforest_model) |> 
  add_candidates(lin_reg_model) |> 
  add_candidates(preg_models)

stack_model <- my_stack |> 
  blend_predictions() |> 
  fit_members()

stack_preds <- stack_model |> predict(new_data=bike_test)

kaggle_submission <- stack_preds |> 
  bind_cols(bike_test) |> 
  select(datetime, .pred) |> 
  rename(count=.pred) |> 
  mutate(count=pmax(0, count)) |> 
  mutate(datetime=as.character(format(datetime)),
         count=exp(count))

vroom_write(x=kaggle_submission, file="./Stack_Preds.csv", delim = ",")
