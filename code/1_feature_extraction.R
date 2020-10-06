source("./code/utils.R")
theme_set(theme_light())

df_train_raw <- 
  read_rds("./raw/tuning.rds") %>% 
  preprocess_data()

df_test_raw <- 
  read_rds("./raw/test.rds") %>% 
  preprocess_data()


df_freqs_tot <- 
  df_train_raw %>%
  select(ends_with("_a")) %>%
  rename_all(~{.x %>% str_replace("_a", "")}) %>%
  bind_rows(
    df_train_raw %>%
      select(ends_with("_b")) %>%
      rename_all(~{.x %>% str_replace("_b", "")})
  ) %>% 
  distinct()

df_ffreq_lookup <- 
  tibble(fname = df_freqs_tot$fname) %>% 
  count(fname) %>% 
  rename(ffreq_raw = n) %>% 
  mutate(ffreq = scale(ffreq_raw))

df_lfreq_lookup <- 
  tibble(lname = df_freqs_tot$lname) %>% 
  count(lname) %>% 
  rename(lfreq_raw = n) %>% 
  mutate(lfreq = scale(lfreq_raw))

add_freq <- function(df){
  
  df %>% 
    left_join(df_ffreq_lookup,
              by = c("fname_a" = "fname")) %>% 
    left_join(df_ffreq_lookup,
              by = c("fname_a" = "fname"), suffix = c("_a", "_b")) %>% 
    left_join(df_lfreq_lookup,
              by = c("lname_a" = "lname")) %>% 
    left_join(df_lfreq_lookup,
              by = c("lname_a" = "lname"), suffix = c("_a", "_b")) %>% 
    mutate_at(vars(contains("freq")), 
              ~ifelse(is.na(.x), median(.x, na.rm = T), .x))
  
}


add_feature_vector <- function(df){
  # browser()
  
  df %>%
    add_freq() %>% 
    mutate(
      #dob
      birth_year_a = as.double(year(dob_a)),
      birth_year_b = as.double(year(dob_b)),
      birth_day_a = as.double(day(dob_a)),
      birth_day_b = as.double(day(dob_b)),
      birth_month_a = as.double(month(dob_a)),
      birth_month_b = as.double(month(dob_b)),
      year_diff = as.integer(birth_year_a) - as.integer(birth_year_b),
      metric_dob_dist = stringdist(dob_a, dob_b, "dl"),
      metric_year_dist = stringdist(birth_year_a, birth_year_b, "dl"),
      metric_month_dist = stringdist(birth_month_a, birth_month_b, "dl"),
      metric_day_dist = stringdist(birth_day_a, birth_day_b, "dl"),
      metric_dm_swaps = 
        ((!is.na(dob_a)) & (!is.na(dob_b))) &
        (birth_month_a == birth_day_b) & 
        (birth_month_b == birth_day_a),
      metric_age_a = as.double(ymd("2019-01-01") - dob_a)/365,
      metric_age_b = as.double(ymd("2019-01-01") - dob_b)/365,
      
      # fname
      fname_metrics = map2(fname_a, 
                           fname_b, 
                           summarise_all_string_metrics, 
                           col_name = "fname",
                           methods = c("dl", "jw", "soundex", "lcs", "qgram")),
      str_length_fname_a = str_length(fname_a),
      str_length_fname_b = str_length(fname_b),
      metric_fname_len_max = map2_int(str_length_fname_a, str_length_fname_b, ~max(c(.x,.y))),
      metric_fname_len_min = map2_int(str_length_fname_a, str_length_fname_b, ~min(c(.x,.y))),
      metric_ffreq_a = ffreq_a,
      metric_ffreq_b = ffreq_b,
      # metric_ffreq_mean = map2_dbl(ffreq_a, ffreq_b, ~mean(c(.x,.y))),
      # metric_ffreq_diff = abs(ffreq_a - ffreq_b),
      
      # lname
      lname_metrics = map2(lname_a, 
                           lname_b, 
                           summarise_all_string_metrics, 
                           col_name = "lname",
                           methods = c("dl", "jw", "soundex")),
      str_length_lname_a = str_length(lname_a),
      str_length_lname_b = str_length(lname_b),
      metric_lname_len_max = map2_int(str_length_lname_a, str_length_lname_b, ~max(c(.x,.y))),
      metric_lname_len_min = map2_int(str_length_lname_a, str_length_lname_b, ~min(c(.x,.y))),
      metric_lfreq_a = lfreq_a,
      metric_lfreq_b = lfreq_b,
      # metric_lfreq_mean = map2_dbl(lfreq_a, lfreq_b, ~mean(c(.x,.y))),
      
      # mname
      mname_present = (mname_a != "") & (mname_b != ""),
      mname_same = mname_a == mname_b,
      
      
      # name swaps
      metric_name_swaps = 
        ((!is.na(fname_a)) & (!is.na(fname_b)) & 
           (!is.na(lname_a)) & (!is.na(lname_b))) &
        (lname_a == fname_b) & 
        (lname_b == fname_a),
      
      # gender
      gender_code = map2_chr(g_a, g_b, function(x, y){
        str_c(sort(c(x, y)), collapse = "")
      }),
      metric_gender_code_ff = gender_code  %>% str_count("f"),
      metric_gender_code_mm = gender_code  %>% str_count("m"),
      
      # marriage
      metric_potential_marriage =
        (!is.na(metric_age_a) | !is.na(metric_age_b)) &
        ((metric_age_a >= 20) | (metric_age_b >= 20)) &
        (!is.na(fname_a) & !is.na(fname_b)) &
        (fname_a == fname_b) &
        (!is.na(lname_a) & !is.na(lname_b)) &
        (lname_a != lname_b) &
        (g_a == "f" | g_b == "f"),
      
      # ssn
      ssn_metrics = map2(ssn_a, 
                         ssn_b, 
                         summarise_all_string_metrics, 
                         col_name = "ssn",
                         methods = c("dl", "lcs"),
                         q = 3),
      
      # address
      address_metrics = map2(
        address_a, 
        address_b, 
        summarise_all_string_metrics, 
        col_name = "address",
        methods = c("dl", "lcs"),
        q = 3),
      
      #phone
      phone_metrics = map2(
        phone_a, 
        phone_b, 
        summarise_all_string_metrics, 
        col_name = "phone",
        methods = c("dl", "lcs"),
        q = 3)
      
    ) %>%
    unnest() %>%
    select(pair_id, 
           fname_a, fname_b,
           # mname_a, mname_b,
           lname_a, lname_b,
           starts_with("dob"), 
           starts_with("gender_code"), 
           starts_with("race_code"),
           starts_with("metric"),
           everything()) %>%
    as.data.frame()
}

df_train <- 
  df_train_raw %>% 
  add_feature_vector() 

df_test_final <- 
  df_test_raw %>% 
  add_feature_vector() 


df_train %>% 
  write_rds("./data/df_train_all.rds")

df_test_final %>% 
  write_rds("./data/df_test_all.rds")

df_train %>% write_csv("./name2vec/train.csv")
df_test_final %>% write_csv("./name2vec/test.csv")
