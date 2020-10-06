if(!"pacman" %in% installed.packages()){
  install.packages("pacman")
}


pacman::p_load(tidyverse, stringdist, phonics, glue, caret, lubridate,
               rebus, fs, ModelMetrics, MLmetrics, caretEnsemble,
               PGRdup, pROC, scico, RColorBrewer, ggthemes, plotly, keras
               ,doParallel, pushoverr, janitor, tensorflow)



#rlErrorGeneratoR, LaCroixColoR
# source("data/credentials.R")


# preprocessing ------------------------------------------------------

extract_major_token <- function(x){
  str_split(x, " ") %>% 
    map_chr(function(s){
      # print(s)
      if(any(is.na(s))){
        NA
      } else if(str_length(s[1]) >= 3){
        s[1]
      } else{
        longest_token <- s[str_length(s) == max(str_length(s))]
        sample(longest_token, 1)
      }
    })
}

# preprocess_data <- function(df, year_a = T){
#   
#   df  %>% 
#     select(starts_with("id"), 
#            fname = first_name, lname = last_name, 
#            birth_age, gender_code, race_code, 
#            voter_reg_num, name_suffix) %>% 
#     mutate(birth_age = birth_age %>% as.integer(),
#            name_suffix = if_else(is.na(name_suffix), "", name_suffix),
#            lname = glue("{lname} {name_suffix}") %>% str_trim(),
#            # fname_longest = extract_major_token(fname),
#            # lname_longest = extract_major_token(lname),
#            year_a = year_a,
#            birth_year = ifelse(year_a, 2013 - birth_age, 2017 - birth_age)) %>% 
#     select(-name_suffix, -year_a) %>% 
#     add_count(fname) %>%
#     rename(ffreq = n) %>% 
#     add_count(lname) %>% 
#     rename(lfreq = n) %>% 
#     mutate(ffreq = scale(ffreq),
#            lfreq = scale(lfreq))
# }

preprocess_data <- function(df){
  df %>% 
    rename(pair_id = id,
           id_a = lid_a,
           id_b = lid_b) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    mutate_at(vars(contains("dob")), as_date) %>% 
    mutate_at(vars(contains("g_")), ~{
      case_when(
        .x == "0" ~ "a",
        .x == "2" ~ "m",
        .x == "3" ~ "f",
        .x == "4" ~ "b",
      )
    }) %>%   
    mutate(
      match = 
        ifelse(match == "1", "match", "unmatch") %>% 
        factor(levels = c("unmatch", "match"))
    )
}



# views -----------------------------------------------------------------

vectors_to_pairs <- function(df, View = F){
  # id_var <- enquo(id_var)
  
  if(!"pair_id" %in% colnames(df)){
    df <-
      df %>% 
      mutate(pair_id = row_number())
  }
  
  df_a <-
    df %>%
    select(-contains("_b"))
  
  colnames(df_a) <- str_replace(colnames(df_a), "_a", "")
  
  df_b <-
    df %>%
    select(-contains("_a"))
  
  colnames(df_b) <- str_replace(colnames(df_b), "_b", "")
  
  df <- 
    bind_rows(df_a, df_b) %>%
    arrange(pair_id) %>% 
    select(pair_id, 
           fname, lname, gender_code, race_code, birth_year, 
           match, everything())
  
  if(View){
    View(df)
  } else {
    df
  }
  
}


pairs_to_vectors <- function(df){
  
  df_a <- 
    df %>% 
    group_by(pair_id) %>% 
    slice(1)%>% 
    ungroup()
  
  names(df_a) <- 
    str_c(names(df_a), "_a") %>% 
    str_replace("pair_id_a", "pair_id") %>% 
    str_replace("match_a", "match")
  
  df_b <- 
    df %>% 
    group_by(pair_id) %>% 
    slice(2) %>% 
    ungroup()
  
  names(df_b) <- 
    str_c(names(df_b), "_b") %>% 
    str_replace("pair_id_b", "pair_id") %>% 
    str_replace("match_b", "match")
  
  df_a %>% 
    left_join(df_b) %>% 
    select(pair_id, 
           id_a, fname_a, lname_a, birth_year_a, contains("bplstr_a"), 
           gender_code_a, race_code_a,
           id_b, fname_b, lname_b, birth_year_b, contains("bplstr_b"), 
           gender_code_b, race_code_b, 
           everything())
}

attach_dbs_to_ids <- function(df_id){
  df_id %>% 
    left_join(df_a_mod, by = "id_a") %>% 
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b"))
}

attach_dbs_to_ids_dfs <- function(df_id, df_a_mod, df_b_mod){
  df_id %>% 
    left_join(df_a_mod, by = "id_a") %>% 
    left_join(df_b_mod, by = "id_b", suffix = c("_a", "_b"))
}




# feature_extraction ------------------------------------------------------
string_dist_norm <- function(x, y, ...){
  stringdist(x, y, ...)/map2_int(str_length(x),str_length(y), ~max(c(.x,.y)))
}

standardize_stringdist <- function(x, y, dist = "osa"){
  stringdist(x, y, dist)/map2_int(str_length(x), str_length(y), ~max(c(.x,.y)))
}

extract_initals <- function(x){
  x %>% str_split(" ") %>% map_chr(~str_c(str_sub(.x,1,1), collapse = ""))
}

summarise_all_string_metrics <- function(x, y, col_name, 
                                         methods = c("dl", "jw", "soundex", 
                                                     "lcs", "qgram", "cosine"),
                                         p = 0.1, q = 2){
  
  dists <- map_dbl(methods, ~stringdist(x, y, method = .x, p = p, q = q))
  tibble(metric = str_c("metric_", col_name, "_", methods), dists = dists) %>% 
    spread(metric, dists)
}





# helpers -----------------------------------------------------------------

household_potential_match <- function(household_id, df_a, df_b){
  inner_join(x = df_a %>% mutate(cmn = 1),
             y = df_b %>% mutate(cmn = 1),
             by = c("cmn"),
             suffix = c("_a", "_b")) %>% 
    select(-cmn)
}

sample_ns <- function(..., seed = 1){
  set.seed(seed)
  sample_n(...)
}

extract_one_to_one <- function(df, field){
  field <- enquo(field)
  df %>% 
    add_count(!!field) %>% 
    filter(n == 1) %>% 
    select(-n)
}


read_data <- function(path){
  df_var <- 
    path %>% 
    path_file() %>%  
    str_replace(or(literal(".csv"), literal(".rds")), "")
  
  if(path %>% str_detect("csv")){
    assign(df_var, read_csv(path), parent.frame())
  } else {
    assign(df_var, read_rds(path), parent.frame())
  }
  
}

print.data.frame <- function(df, ...){
  df %>% as_tibble() %>% print(...)
}


# predict -----------------------------------------------------------------

predict_links_raw <- function(model, df_pair_vector){
  
  df_preds <- 
    predict(model, df_pair_vector , type = "prob", na.action = na.pass) %>% 
    as.tibble()
  
  names(df_preds) <- c("unmatch_prob", "match_prob")
  
  # df_preds_aug <- 
  df_preds %>% 
    mutate(pair_id = df_pair_vector$pair_id,
           conf = abs(match_prob - 0.5)*2,
           match_pred = as.integer(match_prob >= 0.5)) %>% 
    left_join(df_pair_vector, by = "pair_id")
}

links_1to1 <- function(df_preds_aug){
  
  df_pred_1to1 <- 
    df_preds_aug %>% 
    filter(match_pred == 1) %>% 
    select(id_a, id_b) %>% 
    add_count(id_a) %>% 
    add_count(id_b) %>% 
    filter(n == 1, nn == 1) %>% 
    mutate(one_one = TRUE) %>% 
    select(id_a, id_b,one_one)
  
  # df_preds_aug <- 
  df_preds_aug %>% 
    left_join(df_pred_1to1) %>% 
    mutate(match_pred_one_to_one = ifelse(!is.na(one_one), 1, 0)) %>% 
    view_as_pairs() %>% 
    arrange(conf, pair_id) %>% 
    select(pair_id, fname, lname, gender_code, race_code, birth_year, 
           match_prob, conf, match_pred, everything())
  
}

# postprocessing ----------------------------------------------------------

resolve_linkage <- function(df_aug){
  df_match <- 
    df_aug %>% 
    filter(match_pred == 1)
  
  df_aug %>% 
    left_join(df_match %>% 
                find_best_links_aggregated() %>% 
                mutate(match_pred_resolved = 1)) %>% 
    mutate(match_pred_resolved = ifelse(is.na(match_pred_resolved), 0, match_pred_resolved))
}



add_count_name <- function(df, var, var_name, scale = F){
  var_name <- quo_name(var_name)
  var <- enquo(var)
  
  df <- 
    df %>% 
    add_count(!!var)
  
  if(scale){
    df <- 
      df %>% 
      mutate(n = scale(n))
  }
  
  df %>% 
    rename((!!var_name) := n)
  
}

find_best_links_aggregated <- function(df_match){
  df_match_counts <-
    df_match %>%
    select(id_a, id_b, match_prob) %>%
    add_count_name(id_a, "n_a") %>%
    add_count_name(id_b, "n_b")
  
  df_1to1 <-
    df_match_counts %>%
    filter(n_a == 1, n_b == 1) %>% 
    select(id_a, id_b)
  
  # df_prob_1 <- 
  #   df_match_counts %>% 
  #   filter(match_prob == 1) %>% 
  
  
  df_match_counts %>% 
    filter(n_a > 1 | n_b > 1) %>%
    find_best_links(id_a) %>% 
    bind_rows(df_match_counts %>% 
                find_best_links(id_b)) %>% 
    distinct() %>% 
    add_count(id_a) %>% 
    add_count(id_b) %>% 
    filter(n  == 1, nn == 1) %>% 
    select(id_a, id_b) %>% 
    bind_rows(df_1to1)
  
}

find_best_links <- function(df_match, id){
  id = enquo(id)
  
  df_match %>% 
    filter(n_a > 1 | n_b > 1) %>% 
    group_by(!!id) %>% 
    arrange(desc(match_prob), .by_group = T) %>% 
    mutate(odds = match_prob - match_prob[2],
           match_pred_mod = ifelse(odds > 0.4, 1, 0)) %>% 
    select(odds, id_a, id_b, match_pred_mod, everything()) %>% 
    filter(match_pred_mod == 1) %>% 
    ungroup() %>% 
    select(id_a, id_b)
}




# pipeline ----------------------------------------------------------------

link_datasets <- function(df_a, df_b, model){
  
  df_blocked <- 
    multipass_blocking(df_a, df_b) %>% 
    distinct()
  
  df_pair_vector <- 
    df_blocked %>% 
    add_feature_vector()
  
  df_blocked <- 
    df_pair_vector %>% 
    select(id_a, id_b) %>% 
    left_join(df_blocked)
  
  df_preds_aug <- 
    predict_links_raw(model, df_pair_vector)
  
  # predict_links_1to1 <- 
  pf_preds_aug %>% 
    resolve_linkage()
  
}













# ml diagnostics ----------------------------------------------------------

evaluate_model <- function(model, df_test, ...){
  calculate_metrics(predict_model(model, df_test), df_test, ...)
}

predict_model <- function(model, df_test){
  df_preds <- 
    predict(model, df_test, type = "prob", na.action = na.pass) %>% 
    as.tibble()
  
  names_preds <- names(df_preds)
  
  if(length(names_preds) == 1){
    tibble(match_prob = df_preds %>% pull(1))
  } else {
    names(df_preds) <- names_preds %>% str_c("_prob")
    df_preds
  }
  
}

calculate_metrics <- function(df_preds, df_test, 
                              metric = precision,
                              k_range = seq(0.5, 1, .001),
                              value = 0.99,
                              lowest = T,
                              plot_roc = T){
  df_preds_aug <- 
    df_preds %>% 
    mutate(pair_id = df_test$pair_id,
           conf = abs(match_prob - 0.5)*2,
           match_pred = ifelse(match_prob >= 0.5, "match", "unmatch") %>% 
             factor(levels = c("match", "unmatch"))) %>% 
    left_join(df_test, by = "pair_id") %>%
    arrange(conf, pair_id) %>% 
    select(pair_id, 
           fname_a, fname_b, lname_a, lname_b, birth_year_a, birth_year_b, 
           gender_code_a, gender_code_b, race_code_a, race_code_b,
           match_pred, match, conf, match_prob, everything())
  
  
  df_preds_aug_pairs <- 
    df_preds_aug %>% 
    vectors_to_pairs()%>% 
    select(pair_id, 
           fname, lname, gender_code, race_code, birth_year,
           match_prob, conf, match_pred, match, everything())
  
  results <- list()
  confidence <- list()
  
  confidence$most_confident_mistakes <- 
    df_preds_aug_pairs %>% 
    filter(match != match_pred) %>% 
    arrange(desc(conf))
  
  confidence$least_confident_mistakes <- 
    df_preds_aug_pairs %>% 
    filter(match != match_pred) %>% 
    arrange(conf)
  
  confidence$most_confident_right <- 
    df_preds_aug_pairs %>% 
    filter(match == match_pred) %>% 
    arrange(desc(conf))
  
  confidence$least_confident_right <- 
    df_preds_aug_pairs %>% 
    filter(match == match_pred) %>% 
    arrange(conf)
  
  confidence$most_confident_decisions <- 
    df_preds_aug_pairs %>% 
    arrange(desc(conf))
  
  confidence$least_confident_decisions <- 
    df_preds_aug_pairs %>% 
    arrange(conf)
  
  results$confidence <- confidence
  results$data <- list(
    df_preds = df_preds,
    df_preds_aug = df_preds_aug,
    df_preds_aug_pairs = df_preds_aug_pairs
  )
  
  metrics <- list()
  
  match <- ifelse(as.character(df_preds_aug$match) == "match", 1, 0) 
  match_pred <- ifelse(as.character(df_preds_aug$match_pred) == "match", 1, 0)
  pred_prob <- df_preds_aug$match_prob
  
  threshold_precision_k <- 
    calc_threshold_for_metric_value(df_preds_aug$match,
                                    pred_prob,
                                    metric = metric,
                                    value = value,
                                    k_range = k_range,
                                    lowest = lowest)
  match_pred_k <- 
    as.integer(df_preds_aug$match_prob >= threshold_precision_k)
  
  metrics$precision_k <- precision(match, match_pred_k)
  metrics$recall_k <- recall(match, match_pred_k)
  metrics$specificity_k <- specificity(match, match_pred_k)
  metrics$accuracy_k <- Accuracy(match, match_pred_k)
  metrics$f1_k <- f1Score(match, match_pred_k)
  
  metrics$accuracy <- Accuracy(match, match_pred)
  metrics$auc <- auc(roc(match, match_pred))
  metrics$precision <- precision(match, match_pred)
  metrics$sensitivity <- recall(match, match_pred)
  metrics$specificity <- specificity(match, match_pred)
  metrics$npv <- npv(match, match_pred)
  metrics$f1 <- f1Score(match, match_pred)
  metrics$error <- ce(match, match_pred)
  metrics$brier <- brier(match, pred_prob)
  metrics$brier_sqrt <- sqrt(metrics$brier)
  metrics$recall <- recall(match, match_pred)
  metrics$auc <- auc(match, pred_prob)
  metrics$gini <- Gini(match, pred_prob)
  metrics$n_wrong <- nrow(results$confidence$most_confident_mistakes)/2
  
  metrics$df_metric_table <-
    metrics %>% 
    enframe() %>% 
    rename(metric = name) %>% 
    mutate(value = map_dbl(value, 1)) 
  # %>% 
  # spread(name, value)
  
  thresholds <- seq(0, 1.05, 0.05)
  metrics$df_roc <- 
    tibble(
      thresholds = thresholds,
      sensitivity = map_dbl(thresholds, ~recall(match, (pred_prob >= .x) %>% as.integer())),
      specificity = map_dbl(thresholds, ~specificity(match, (pred_prob >= .x) %>% as.integer()))
    ) %>% 
    mutate(fpr = 1-specificity)
  
  metrics$roc_curve <- 
    ggplot(metrics$df_roc, aes(fpr, sensitivity))+
    geom_line(size = 1, alpha = 0.7) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
    labs(title= "ROC curve", 
         x = "False Positive Rate (1-Specificity)", 
         y = "True Positive Rate (Sensitivity)")
  
  if(plot_roc) plot(metrics$roc_curve)
  
  metrics$confusion_matrix <- ConfusionMatrix(match, match_pred)
  metrics$f_scores <- structure(FBeta_Score(match, match_pred, positive = 1, beta = 1:10), names = 1:10) 
  
  results$metrics <- metrics
  
  class(results) <- "rl_results"
  
  results
}

explain_metrics <- function() rstudioapi::viewer("https://en.wikipedia.org/wiki/Precision_and_recall#Definition_(classification_context)") 


print.rl_results <- function(results){
  walk2(results$confidence, names(results$confidence), 
        function(data, desc){
          cat(paste0("\n", desc, "\n"))
          print(data %>% select(1:10), n = 6)
        })
  
  cat("\n")
  print(results$metrics$confusion_matrix)
  
  cat(paste0("\nmetrics\n"))
  print(results$metrics$df_metric_table, n = 6)
}


extract_model_component <- function(df_model_results, model_select, component){
  if(is.character(model_select)){
    component <- 
      df_model_results %>% 
      filter(model_name == model_select) %>% 
      pull(component)
  } else{
    component <- 
      df_model_results %>% 
      slice(model_select) %>% 
      pull(component)
  }
  
  if(is.list(component)){
    component[[1]]
  }
}


calc_threshold_for_metric_value <- 
  function(y_true, 
           y_prob, 
           metric = precision, 
           value = 0.99, 
           k_range = seq(0.5, 1, 0.001), 
           lowest = TRUE){
    # browser()
    
    y_true <- y_true == "match"  
    
    df_values_thresholds <- 
      tibble(k_range = k_range,
             values =
               map_dbl(k_range, function(x){
                 metric(y_true, y_prob >= x)
               })) 
    
    if(lowest){
      df_filtered <- 
        df_values_thresholds %>% 
        filter(values >= value) %>% 
        arrange(k_range) %>% 
        slice(1) 
      
      
      
    } else {
      df_filtered <- 
        df_values_thresholds %>% 
        filter(values <= value) %>% 
        arrange(desc(k_range)) %>% 
        slice(1) 
    }
    
    message(glue("Metric has achieved {df_filtered$values} \\
                 at threshold {df_filtered$k_range}"))
    
    df_filtered$k_range
  }


plot_roc_all <- function(df_evaluated, model_col = model, results_col = results){
  model_col <- enquo(model_col)
  results_col <- enquo(results_col)
  p <- df_evaluated %>% 
    mutate(df_roc = !!results_col %>% map("metrics") %>% map("df_roc")) %>% 
    select(!!model_col, df_roc) %>% 
    unnest() %>% 
    ggplot(aes(fpr, 
               sensitivity, 
               color = !!model_col, 
               group = !!model_col,
               text = glue("threshold: {thresholds}
                           fpr: {round(fpr, 3)}
                           sensitivity: {round(sensitivity, 3)} 
                           specificity: {round(specificity, 3)}")))+
    geom_line(size = 1, alpha = 0.7) +
    geom_point(size = 0.5, 
               alpha = 0.5) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
    labs(title= "ROC curve", 
         x = "False Positive Rate (1-Specificity)", 
         y = "True Positive Rate (Sensitivity)") +
    theme_light() +
    scale_color_continuous(low = "gray13", high = "red")
  
  plotly::ggplotly(p, tooltip="text")
}




filter_na_rows <- function(df){
  df %>% 
    filter_all(any_vars(is.na(.)))
}

fill_na_0 <- function(x, repl = 0){
  x[is.na(x)] <- repl
  x
}
####################################
#             NN
#####################################
build_model <- function(inp_len) {
  
  model <- 
    keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu",
                input_shape = inp_len) %>%
    # kernel_regularizer = regularizer_l2(0.0001)
    layer_batch_normalization() %>% 
    layer_dropout(rate = 0.001) %>%
    layer_dense(units = 64, 
                activation = "relu") %>%
    layer_batch_normalization() %>% 
    layer_dense(units = 1, activation = "sigmoid")
  
  model %>% compile(
    loss = "binary_crossentropy",
    optimizer = optimizer_rmsprop(),
    metrics = list("accuracy")
  )
  
  model
}


build_nn <- function(df_tr, input_len, epochs=50){
  df_tr <-
    df_tr %>% 
    select(contains("metric"), match) %>% 
    mutate_if(is.logical, as.integer) %>% 
    mutate_if(is.factor, as.integer) %>% 
    mutate_if(is.integer, as.double)  
  
  tr_means <- map_dbl(df_tr, ~mean(.x, na.rm = T))
  tr_sds <- map_dbl(df_tr, ~sd(.x, na.rm = T))
  
  df_tr <- 
    pmap_df(list(a = df_tr, b = tr_means, c = tr_sds), function(a, b, c){
      (a -b)/c
    }) %>% 
    mutate_all(fill_na_0) %>% 
    mutate(match = ifelse(match == min(match), 0, 1))
  
  tr_x <- 
    df_tr %>% 
    select(-match) %>% 
    as.matrix()
  
  tr_y <- 
    df_tr$match
  
  model <- build_model(inp_len = input_len)
  model_name <- glue("result/model_NN_1_all.h5")
  
  history <- 
    model %>% 
    fit(
      tr_x,
      tr_y,
      epochs = epochs,
      validation_split = 0.1,
      verbose = 1,
      callbacks = list(
        callback_model_checkpoint(model_name,
                                  verbose = T,
                                  save_best_only = T,
                                  monitor = "val_acc"),
        callback_reduce_lr_on_plateau(patience=10, 
                                      verbose = T,
                                      factor = 0.8)
      )
    )
  
  model

}


