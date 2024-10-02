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
library(dbarts)
source("bike_recipe.R")

bike_train <- vroom("train.csv")

bike_train <- bike_train  |> 
  select(-registered, -casual) |> 
  mutate(count=log(count))

bike_test <- vroom("test.csv")

bike_rec <- bike_recipe(bike_train)

bike_bart <- parsnip::bart(trees = 500) |> 
  set_engine("dbarts") |> 
  set_mode("regression")

final_wf <- workflow() |> 
  add_recipe(bike_rec) |> 
  add_model(bike_bart) |>
  fit(data=bike_train)

bart_preds <- final_wf |>
  predict(new_data = bike_test)

# Write Data Set to Computer

kaggle_submission <- bart_preds |> 
  bind_cols(bike_test) |> 
  select(datetime, .pred) |> 
  rename(count=.pred) |> 
  mutate(count=pmax(0, count)) |> 
  mutate(datetime=as.character(format(datetime)),
         count=exp(count))

vroom_write(x=kaggle_submission, file="./Bart_Preds.csv", delim = ",")
