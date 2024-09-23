library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(skimr)
library(DataExplorer)
library(GGally)
library(glmnet)
library(rpart)

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

bike_tree <- decision_tree(tree_depth = tune(),
                           cost_complexity = tune(),
                           min_n = tune()) |> 
  set_engine("rpart") |> 
  set_mode("regression")

bike_tree_wf <- workflow() |> 
  add_recipe(bike_recipe) |> 
  add_model(bike_tree)

grid_of_tuning_params <- grid_regular(tree_depth(),
                                      cost_complexity(),
                                      min_n(),
                                      levels = 5)

folds <- vfold_cv(bike_train, v=5)

CV_results <- bike_tree_wf |> 
  tune_grid(resamples=folds,
            grid=grid_of_tuning_params,
            metrics=metric_set(rmse))

collect_metrics(CV_results) |> 
  filter(.metric=="rmse") |> 
  ggplot(aes(x=penalty, y = mean, color = factor(mixture))) +
  geom_line()

bestTune <- CV_results |> 
  select_best(metric="rmse")

final_wf <- bike_tree_wf |> 
  finalize_workflow(bestTune) |> 
  fit(data=bike_train)

tree_preds <- final_wf |>
  predict(new_data = bike_test)

# Write Data Set to Computer

kaggle_submission <- tree_preds |> 
  bind_cols(bike_test) |> 
  select(datetime, .pred) |> 
  rename(count=.pred) |> 
  mutate(count=pmax(0, count)) |> 
  mutate(datetime=as.character(format(datetime)),
         count=exp(count))

vroom_write(x=kaggle_submission, file="./Tree_Preds.csv", delim = ",")
