library(tidyverse)
library(tidymodels)
library(vip)

## special thanks to Julia Silge at RStudio
## for a good tutorial on using tidymodels
## https://juliasilge.com/blog/sf-trees-random-tuning/  

tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

### modify data
tbi_age$age_group <- as.factor(tbi_age$age_group)
levels(tbi_age$age_group) <- c("0-17","0-4","5-14","15-24","25-34","35-44","45-54","55-64","65-74","75+","Total")


#### reshape tbi_age data for classification by injury_mechanism ####

tbi_age$rate_est_rnd <- round(tbi_age$rate_est,0)

#newdata <- tbi_age[,!names(tbi_age)=="rate_est"]
filterdata <- tbi_age[which(tbi_age$rate_est_rnd>0),]
filterdata <- filterdata[complete.cases(filterdata),]

rateestrnd <- filterdata$rate_est_rnd

newdata <- data.frame(
  age_group=rep(filterdata$age_group,rateestrnd)
  ,type=rep(filterdata$type,rateestrnd)
  ,injury_mechanism=rep(filterdata$injury_mechanism,rateestrnd)
  ,number_est=rep(filterdata$number_est,rateestrnd)
)

#### devel and valid data

set.seed(123)
trees_split <- initial_split(newdata, strata = injury_mechanism)
trees_train <- training(trees_split)
trees_test <- testing(trees_split)

#### train model 
tree_rec <- recipe(injury_mechanism ~ ., data=newdata)

tree_prep <- prep(tree_rec)
juiced <- juice(tree_prep)

tune_spec <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")


tune_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(tune_spec)

set.seed(234)
trees_folds <- vfold_cv(trees_train)


doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = 20
)

tune_res


#### tune AUC
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "AUC")


### grid tune parameters
rf_grid <- grid_regular(
  mtry(range = c(2, 3)),
  min_n(range = c(5, 20)),
  levels = 5
)

rf_grid

set.seed(456)
regular_res <- tune_grid(
  tune_wf,
  resamples = trees_folds,
  grid = rf_grid
)

regular_res

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "AUC")

### select best
best_auc <- select_best(regular_res, "roc_auc")

final_rf <- finalize_model(
  tune_spec,
  best_auc
)

final_rf


### variable importance
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(injury_mechanism ~ .,
      data = juice(tree_prep)
  ) %>%
  vip(geom = "point")

### final workflow fit
final_wf <- workflow() %>%
  add_recipe(tree_rec) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(trees_split)

final_res %>%
  collect_metrics()


pred <- final_res %>% collect_predictions()


### confusion matrix
conf_mat(pred, injury_mechanism, .pred_class)


pred %>%
  conf_mat(injury_mechanism, .pred_class) %>%
  pluck(1) %>%
  as_tibble() %>%
  ggplot(aes(Prediction, Truth, alpha = n)) +
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "white", alpha = 1, size = 4) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))