library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(skimr)
library(DataExplorer)
library(GGally)

bike_train <- vroom("train.csv")
View(bike_train)

glimpse(bike_train)
skimr(bike_train)

bike_train |> 
  ggplot(aes(x=temp,
             y=count)) +
  geom_point() +
  geom_smooth(se = F)

bike_train |> 
  ggpairs()

plot1 <- bike_train |>
  ggplot(aes(x=weather)) +
  geom_bar()
