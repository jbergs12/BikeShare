bike_recipe <- function(traindata){
  recipe(count~., data = traindata) |> 
    step_mutate(
      season=factor(season,
                    levels = 1:4,
                    labels = c("Spring", "Summer", "Fall", "Winter")),
      weather=as.factor(ifelse(weather==4, 3, weather)),
      holiday=as.factor(holiday),
      workingday=as.factor(workingday)) |> 
    step_date(datetime, features = c("month", "dow", "year")) |>
    step_time(datetime, features = "hour", keep_original_cols = F) |>
    step_mutate(datetime_hour=as.factor(datetime_hour),
                datetime_month=as.factor(datetime_month),
                datetime_dow=as.factor(datetime_dow),
                datetime_year=as.factor(datetime_year)) |>
    step_dummy(all_nominal_predictors()) |>
    step_normalize(all_predictors())
}



run_cv <- function(folds, grid, metric=rmse){
  cl <- makePSOCKcluster(8)
  doParallel::registerDoParallel(cl)
  cvStart <- Sys.time()
  CV_results <- bike_bart_wf |>
    tune_grid(resamples=folds,
              grid=grid,
              metrics=metric_set(metric))
  
  print("CV time: ")
  Sys.time()-cvStart
  stopCluster(cl)
  return(CV_results)
}