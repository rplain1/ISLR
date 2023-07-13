library(tidyverse)
library(tidymodels)

df <- penguins


df_pca <- recipe(~., data = df) |> 
  step_impute_mean(all_numeric()) |> 
  step_impute_mode(sex) |> 
  step_normalize(all_numeric()) |> 
  step_dummy(c(species, sex, island), one_hot = TRUE) |> 
  step_pca(all_numeric(), num_comp = 4) |> 
  prep() |> 
  bake(df)

k <- kmeans(df_pca, 3) |> 
  augment(df_pca)

df$cluster <- k$.cluster


df |> 
  ggplot(aes(bill_length_mm, bill_depth_mm)) +
  geom_point(aes(color = cluster))


df |> 
  ggplot(aes(as.factor(species), body_mass_g)) +
  geom_jitter(aes(color = cluster))

df
k

df <- penguins

df_svd <- recipe(~., data = df) |> 
  step_impute_mean(all_numeric()) |> 
  step_impute_mode(sex) |> 
  #step_normalize(all_numeric()) |> 
  step_dummy(c(species, sex, island), one_hot = TRUE) |> 
  step_normalize(all_numeric()) |> 
  prep() |> 
  bake(df)

df_svd |> 
  svd() |> 
  tidy(matrix='d') |> 
  ggplot(aes(PC, cumulative)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = c(0:12)) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_grey(base_size = 16) +
  theme(
    panel.grid.minor = element_blank()
  )



















