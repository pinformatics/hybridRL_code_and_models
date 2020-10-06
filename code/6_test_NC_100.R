source("./code/utils.R")
theme_set(theme_light())

df_test_final <- read_rds(("./data/all_NC_test.rds"))

df_test_final$metric_ffreq_a <- data.matrix(df_test_final$metric_ffreq_a)
df_test_final$metric_ffreq_b <- data.matrix(df_test_final$metric_ffreq_b)
df_test_final$metric_lfreq_a <- data.matrix(df_test_final$metric_lfreq_a)
df_test_final$metric_lfreq_b <- data.matrix(df_test_final$metric_lfreq_b)

#################################################
#               model_NN_1_all
################################################
load("result/model_NN_1_all.RData")
model <- load_model_hdf5("result/model_NN_1_all.h5")

result_all <- data.frame(f1=0, review_size = 0)
for (i in seq(100)){
  print(i)
  
  # match <- 
  #   match %>%
  #   sample_n(600)
  # 
  # unmatch <-
  #   unmatch %>%
  #   sample_n(9400)
  # 
  # df_pairs <- 
  #   unmatch %>%
  #   bind_rows(match) %>% 
  #   sample_n((nrow(.)))
  
  df_pairs <- df_test_final %>% sample_n(10000)
  df_ts <- df_pairs %>% 
    group_by(match) %>% 
    ungroup() %>% 
    arrange(as.integer(pair_id))
  
  df_ts <-
    df_ts %>% 
    select(contains("metric"), match) %>% 
    mutate_if(is.logical, as.integer) %>% 
    mutate_if(is.factor, as.integer) %>% 
    mutate_if(is.integer, as.double)
  
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
    df_pairs %>% 
    mutate(prob = df_review) %>% 
    filter(prob > t2 & prob < t1) %>% 
    arrange(abs(0.5 - prob))
  
  df_pairs %>% 
    mutate(prob = df_review) %>% 
    ggplot(aes(prob)) +
    geom_histogram(bins = 100) +
    scale_x_continuous(breaks = seq(0, 1, 0.05)) +
    geom_vline(xintercept = c(t1, t2))
  
  x <- 
    df_pairs %>% 
    mutate(prob = df_review) %>% 
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
  
  result_all[i,] = c(F1, nrow(df_review_sub))
}

colMeans(result_all)
a <- as.matrix(result_all)
sd(a[,1])
sd(a[,2])






#################################################
#               model_RF_1_all
################################################
df_pairs <- df_test_final
model <- read_rds("./result/model_RF_1_all.rds")  

t1 <- 0.949
t2 <- 0.103
#t1 <- 0.78
#t2 <- 0.32

result_all <- data.frame(f1=0, review_size = 0)
for (i in seq(100)){
  print(i)
  
  df_test_final <- df_pairs %>% sample_n(10000)
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
  
  result_all[i,] = c(F1, nrow(df_review_sub))
}

colMeans(result_all)
a <- as.matrix(result_all)
sd(a[,1])
sd(a[,2])



#################################################
#               model_SVM_linear_1_all
################################################
df_pairs <- df_test_final
model <- read_rds("./result/model_svmLinear_1_all.rds")  

t1 <- 0.974
t2 <- 0.085

result_all <- data.frame(f1=0, review_size = 0)
for (i in seq(100)){
  print(i)
  
  df_test_final <- df_pairs %>% sample_n(10000)
  
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
  
  result_all[i,] = c(F1, nrow(df_review_sub))
}

colMeans(result_all)
a <- as.matrix(result_all)
sd(a[,1])
sd(a[,2])







#################################################
#               model_SVM_radial_1_all
################################################
df_pairs <- df_test_final

model <- read_rds("./result/model_svmRadial_1_all.rds")  

t1 <- 0.976
t2 <- 0.233

result_all <- data.frame(f1=0, review_size = 0)
for (i in seq(100)){
  print(i)
  
  df_test_final <- df_pairs %>% sample_n(10000)
  
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
  
  result_all[i,] = c(F1, nrow(df_review_sub))
}

colMeans(result_all)
a <- as.matrix(result_all)
sd(a[,1])
sd(a[,2])



