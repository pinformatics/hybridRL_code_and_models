source("code/utils.R")
theme_set(theme_light())

df_train <- 
  read_rds("data/df_train_all_n2v.rds")

df_test_final <-  
  read_rds("data/df_test_all_n2v.rds")

set.seed(5)

df_tr <- 
  df_train %>% 
  group_by(match) %>% 
  sample_frac(0.9) %>% 
  ungroup() %>% 
  arrange(as.integer(pair_id))

df_valid <- 
  df_train %>% 
  anti_join(df_tr, by = "pair_id")

train_control <- 
  trainControl(method = "cv", 
               number = 10,
               verboseIter = T,
               savePredictions = TRUE,
               classProbs = TRUE)

set.seed(13)
model <-
  train(
    match ~ ., 
    data = df_tr %>% select(contains("metric"), match),
    trControl = train_control,
    preProcess = c("medianImpute"),
    #method = "svmRadial",
    method = "svmLinear",
    na.action = na.pass)
print(model)


model %>% 
  write_rds("result/model_svmLinear_2_all_n2v.rds")

df_collection = tibble(model_svm_full = list(model),
                       df_va = list(df_valid),
                       df_ts = list(df_test_final))
ppv <- 1
npv <- 1

(df_review <- 
    df_collection %>% 
    mutate(
      pred_probs_va = map2(model_svm_full, df_va, ~predict(.x, .y, na.action = na.pass, type = "prob")$match),
      pred_probs_ts = map2(model_svm_full, df_ts, ~predict(.x, .y, na.action = na.pass, type = "prob")$match),
      preds_va = map2(model_svm_full, df_va, ~predict(.x, .y, na.action = na.pass)),
      preds_ts = map2(model_svm_full, df_ts, ~predict(.x, .y, na.action = na.pass)),
      f1_va = map2_dbl(preds_va, df_va, ~F1_Score(.x, .y$match, positive = "match")),
      f1_ts = map2_dbl(preds_ts, df_ts, ~F1_Score(.x, .y$match, positive = "match")),
      df_grades_va = map2(pred_probs_va, df_va, function(probs, df_val){
        # browser()
        df_grades <- 
          tibble(
            probs = probs,
            actuals = df_val$match
          ) %>% 
          mutate(
            preds = ifelse(probs >= 0.5, "match", "unmatch"),
            grade = preds == actuals
          )
      }),
      threshold_1 = map_dbl(df_grades_va, function(df_grade){
        # browser()
        sub <- 
          tibble(thresholds = seq(0.5, 0.999, 0.001)) %>% 
          mutate(accuracy = map_dbl(thresholds, function(t){
            df_grade %>% 
              filter(probs >= t) %>% 
              pull(grade) %>% 
              mean()
          })) %>% 
          filter(accuracy >= ppv) %>% 
          pull(thresholds) 
        
        ifelse(length(sub) == 0, NA, min(sub))
      }),
      threshold_2 = map_dbl(df_grades_va, function(df_grade){
        sub <- 
          tibble(thresholds = seq(0.001, 0.499, 0.001)) %>% 
          mutate(accuracy = map_dbl(thresholds, function(t){
            df_grade %>% 
              filter(probs <= t) %>% 
              pull(grade) %>% 
              mean()
          })) %>% 
          filter(accuracy >= npv) %>% 
          pull(thresholds)
        ifelse(length(sub) == 0, NA, max(sub))
      }),
      review_percent = pmap_dbl(list(p = pred_probs_ts, t1 = threshold_1, t2 = threshold_2),
                                function(p, t1, t2){
                                  # browser()
                                  # mean(!((p >= t1) | (p <= t2)) )
                                  if(is.na(t1) & is.na(t2)){
                                    1
                                  } else if(is.na(t1)){
                                    mean(p > t2)
                                  } else if(is.na(t2)){
                                    mean(p < t1)
                                  } else {
                                    mean((p < t1) & (p > t2))
                                  }
                                })
    )) 

t1 <- 
  df_review %>% 
  select(contains("thresh")) %>% 
  pull(threshold_1)

t2 <- 
  df_review %>% 
  select(contains("thresh")) %>% 
  pull(threshold_2)


df_review_sub <- 
  df_test_final %>% 
  mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
  filter(prob > t2 & prob < t1) %>% 
  arrange(abs(0.5 - prob))

df_review_sub %>% 
  write_csv("result/model_svmLinear_2_all_n2v_review.csv")


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

