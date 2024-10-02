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

bike_casual <- bike_train  |> 
  mutate(count=log(casual+.01)) |> 
  select(-registered, -casual)

bike_registered <- bike_train |> 
  mutate(count=log(registered+.01)) |> 
  select(-casual, -registered)

bike_test <- vroom("test.csv")

bike_rec_cas <- bike_recipe(bike_casual)

bike_rec_reg <- bike_recipe(bike_registered)

bike_bart <- parsnip::bart(trees = 500) |> 
  set_engine("dbarts") |> 
  set_mode("regression")

casual_wf <- workflow() |> 
  add_recipe(bike_rec_cas) |> 
  add_model(bike_bart) |>
  fit(data=bike_casual)

registered_wf <- workflow() |> 
  add_recipe(bike_rec_reg) |> 
  add_model(bike_bart) |> 
  fit(data=bike_registered)

bart_preds_cas <- casual_wf |>
  predict(new_data = bike_test)

bart_preds_reg <- registered_wf |> 
  predict(new_data = bike_test)

# Write Data Set to Computer

bart_preds <- (exp(bart_preds_cas[,1])-.01) + (exp(bart_preds_reg[,1])-.01)

kaggle_submission <- bart_preds |> 
  bind_cols(bike_test) |> 
  select(datetime, .pred) |> 
  rename(count=.pred) |> 
  mutate(count=pmax(0, count)) |> 
  mutate(datetime=as.character(format(datetime)))

vroom_write(x=kaggle_submission, file="./Bart_Preds_Sep.csv", delim = ",")
