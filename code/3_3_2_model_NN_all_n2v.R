#install.packages("remotes")
#remotes::install_github("rstudio/tensorflow")
#library(tensorflow)
#install_tensorflow(version = "2.0.0b1", method = "conda", envname = "r-reticulate")
#library(keras)

source("code/utils.R")
theme_set(theme_light())

df_train <- 
  read_rds("data/df_train_all_n2v.rds")

df_test_final <-  
  read_rds("data/df_test_all_n2v.rds")

df_tr <- 
  df_train %>%
  group_by(match) %>% 
  ungroup() %>% 
  arrange(as.integer(pair_id)) 

df_tr <-
  df_tr %>% 
  mutate(match = ifelse(match == "match", 1, 0)) %>%
  select(contains("metric"), match) %>% 
  mutate_if(is.logical, as.integer) %>% 
  mutate_if(is.factor, as.integer) %>% 
  mutate_if(is.integer, as.double) %>%
  mutate_if(is.character, as.logical) %>% 
  mutate_if(is.logical, as.integer)

tr_means <- map_dbl(df_tr, ~mean(.x, na.rm = T))
tr_sds <- map_dbl(df_tr, ~sd(.x, na.rm = T))


set.seed(13)
model <- build_nn(df_tr, 35)
save_model_hdf5(model, "result/model_NN_2_all_n2v.h5")


t1 <- 0.996
t2 <- 0.0003
#t1 <- 0.96
#t2 <- 0.0008
save(tr_means, tr_sds, t1, t2, file = "result/model_NN_2_all_n2v.RData")

model <- load_model_hdf5("result/model_NN_2_all_n2v.h5")

df_ts <- df_test_final %>% 
  group_by(match) %>% 
  ungroup() %>% 
  arrange(as.integer(pair_id))

df_ts <-
  df_ts %>% 
  mutate(match = ifelse(match == "match", 1, 0)) %>%
  select(contains("metric"), match) %>% 
  mutate_if(is.logical, as.integer) %>% 
  mutate_if(is.factor, as.integer) %>% 
  mutate_if(is.integer, as.double) %>%
  mutate_if(is.character, as.logical) %>% 
  mutate_if(is.logical, as.integer)

df_ts <- 
  pmap_df(list(a = df_ts, b = tr_means, c = tr_sds), function(a, b, c){
    (a -b)/c
  }) %>% 
  mutate_all(fill_na_0) %>% 
  mutate(match = ifelse(match == min(match), 0, 1))

ts_x <- 
  df_ts %>% 
  select(-match) %>% 
  as.matrix()

ts_y <- 
  df_ts$match

df_review <- model$predict(ts_x)[,1]

df_review_sub <- 
  df_test_final %>% 
  mutate(prob = df_review) %>% 
  filter(prob > t2 & prob < t1) %>% 
  arrange(abs(0.5 - prob))

df_review_sub %>% 
  write_csv("result/model_NN_2_all_n2v_review.csv")

x <- 
  df_test_final %>% 
  mutate(prob = df_review) %>% 
  filter(!(prob > t2 & prob < t1)) %>% 
  select(prob, match) %>% 
  mutate(pred = ifelse(prob > 0.5, "match", "unmatch"))


c_metric <- MLmetrics::ConfusionMatrix(x$pred %>% as.character(), x$match %>% as.character())
c_metric

TP <- c_metric[1]
FP <- c_metric[2]
FN <- c_metric[3]
TN <- c_metric[4]

P <- TP / (TP + FP)
R <- TP / (TP + FN)
F1 <- 2 * P * R / (P + R)

c_metric
F1

