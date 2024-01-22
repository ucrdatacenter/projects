library(tidyverse)
library(tidymodels)
library(glmnet)
library(MASS)

data("Boston")
d <- Boston |> as_tibble()
set.seed(2)
s <- d |> initial_split(prop = 3/5)
s1 <- training(s)
s2 <- testing(s)
r <- s1 |> vfold_cv(v = 5)

f1 <- crim ~ .
f2 <- recipe(crim ~ ., data = s1) |> step_mutate(rad = as_factor(rad)) |> step_dummy(rad)
m <- linear_reg()
m2 <- linear_reg(penalty = tune(), engine = "glmnet")
w <- workflow() |> add_formula(f) |> add_model(m)
w2 <- workflow() |> add_recipe(f2) |> add_model(m2)
g1 <- grid_regular(penalty(), levels = 10)

t <- w2 |> tune_grid(grid = g1, resamples = r)
w2 <- w2 |> finalize_workflow(t |> select_best("rmse"))

w2 |> fit_resamples(r) |> collect_metrics()
w2 |> fit(s1) |> augment(s2) |> rmse(crim, .pred)
w2 |> last_fit(s) |> collect_metrics()

w2 |> fit(s1) |> pull_workflow_fit() |> tidy()

# ggplot(d, aes(x = rad, y = crim)) + geom_smooth(method = "lm") + geom_point()
