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

plot_histogram(bike_train)

plot_correlation(bike_train)

# Issue of counts
plot1 <- bike_train |>
  ggplot(aes(x=weather)) +
  geom_bar()

# Distribution of data by seasons
plot2 <- bike_train |> 
  ggplot(aes(x = as.factor(season), y = count)) +
  geom_boxplot() +
  labs(x = "season")

# Issue of auto-correlation
plot3 <- bike_train |> 
  ggplot(aes(x = temp, y = atemp)) +
  geom_point() +
  geom_smooth(method = "lm")

# Distribution of Casual vs. Registered
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
