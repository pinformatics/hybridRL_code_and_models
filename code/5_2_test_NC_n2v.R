source("./code/utils.R")
theme_set(theme_light())

df_test_final <- read_rds(("./data/NC_test_all_n2v_2.rds"))

#################################################
#               model_RF_2_all_N2V
################################################

model <- read_rds("./result/model_RF_2_all_n2v.rds")  


t1 <- 0.949
t2 <- 0.103
#t1 <- 0.88
#t2 <- 0.15

df_collection = tibble(model_rf_full = list(model),
                       df_ts = list(df_test_final))

df_review <- df_collection %>% 
  mutate(pred_probs_ts = map2(model_rf_full,
                              df_ts,
                              ~predict(.x, .y, na.action = na.pass, type = "prob")$match))



df_review_sub <- 
  df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  filter(prob > t2 & prob < t1) %>% 
  arrange(abs(0.5 - prob))

#df_review_sub %>% 
#  write_csv("result/NC_RF_1_all_review.csv")


df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  ggplot(aes(prob)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = c(t1, t2))



x <- 
  df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  filter(!(prob > t2 & prob < t1)) %>% 
  select(prob, match) %>% 
  mutate(pred = ifelse(prob > 0.5, "match", "unmatch"))


c_metric <- MLmetrics::ConfusionMatrix(x$pred %>% as.character(), x$match %>% as.character())

TP <- c_metric[1]
FP <- c_metric[2]
FN <- c_metric[3]
TN <- c_metric[4]

P <- TP / (TP + FP)
R <- TP / (TP + FN)
F1 <- 2 * P * R / (P + R)

c_metric
F1


#################################################
#               model_SVM_linear_2_all_N2V
################################################

model <- read_rds("./result/model_svmLinear_2_all_n2v.rds")  

#t1 <- 0.974
#t2 <- 0.085

t1 <- 0.95
t2 <- 0.1

df_collection = tibble(model_svm_full = list(model),
                       df_ts = list(df_test_final))

df_review <- df_collection %>% 
  mutate(pred_probs_ts = map2(model_svm_full,
                              df_ts,
                              ~predict(.x, .y, na.action = na.pass, type = "prob")$match))



df_review_sub <- 
  df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  filter(prob > t2 & prob < t1) %>% 
  arrange(abs(0.5 - prob))

#df_review_sub %>% 
#  write_csv("result/NC_SVM_linear_1_all_review.csv")


df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  ggplot(aes(prob)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = c(t1, t2))



x <- 
  df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  filter(!(prob > t2 & prob < t1)) %>% 
  select(prob, match) %>% 
  mutate(pred = ifelse(prob > 0.5, "match", "unmatch"))


c_metric <- MLmetrics::ConfusionMatrix(x$pred %>% as.character(), x$match %>% as.character())

TP <- c_metric[1]
FP <- c_metric[2]
FN <- c_metric[3]
TN <- c_metric[4]

P <- TP / (TP + FP)
R <- TP / (TP + FN)
F1 <- 2 * P * R / (P + R)

c_metric
F1




#################################################
#               model_SVM_radial_2_all_N2V
################################################

model <- read_rds("./result/model_svmRadial_2_all_n2v.rds")  

t1 <- 0.976
t2 <- 0.233

t1 <- 0.97
t2 <- 0.25

df_collection = tibble(model_svm_full = list(model),
                       df_ts = list(df_test_final))

df_review <- df_collection %>% 
  mutate(pred_probs_ts = map2(model_svm_full,
                              df_ts,
                              ~predict(.x, .y, na.action = na.pass, type = "prob")$match))



df_review_sub <- 
  df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  filter(prob > t2 & prob < t1) %>% 
  arrange(abs(0.5 - prob))

#df_review_sub %>% 
#  write_csv("result/NC_SVM_linear_1_all_review.csv")


df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  ggplot(aes(prob)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = c(t1, t2))



x <- 
  df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  filter(!(prob > t2 & prob < t1)) %>% 
  select(prob, match) %>% 
  mutate(pred = ifelse(prob > 0.5, "match", "unmatch"))


c_metric <- MLmetrics::ConfusionMatrix(x$pred %>% as.character(), x$match %>% as.character())

TP <- c_metric[1]
FP <- c_metric[2]
FN <- c_metric[3]
TN <- c_metric[4]

P <- TP / (TP + FP)
R <- TP / (TP + FN)
F1 <- 2 * P * R / (P + R)

c_metric
F1


#################################################
#               model_NN_2_all_N2V
################################################
load("result/model_NN_2_all_n2v.RData")
model <- load_model_hdf5("result/model_NN_2_all_n2v.h5")

t1 <- 0.95
t2 <- 0.008
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

df_test_final %>% 
  mutate(prob = df_review) %>% 
  ggplot(aes(prob)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(breaks = seq(0, 1, 0.05)) +
  geom_vline(xintercept = c(t1, t2))

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
