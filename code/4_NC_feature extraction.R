source("./code/utils.R")
theme_set(theme_light())

df <- read.csv("raw/NC_pairs_all.csv") %>%
  rename( 
    g_a = gender_code_a, 
    g_b = gender_code_b, 
    ssn_a = voter_reg_num_a, 
    ssn_b = voter_reg_num_b) %>% 
  mutate_if(is.character, str_to_lower) %>%
  mutate(match = match %>% factor(levels = c("unmatch", "match")))


# df doesn't have dob. we add dob (based on the patients' age) from another dataset
df_dob <-
  read_csv("raw/dob.csv") %>%
  rename(bday = DOB)

ssn <- as.data.frame(unique(c(df$ssn_a, df$ssn_b)))
names(ssn)[1] <- "ssn_a"

df_dob <- df_dob[sample(nrow(df_dob), length(ssn)), ]
df_dob <- cbind(ssn, df_dob) 

df <- df %>% 
  left_join(df_dob, by= "ssn_a")

year(df$bday) <- df$birth_year_a

df <- df %>%
  rename(dob_a = bday)

index <- df$dob_day == 29 & df$dob_mnth == 2 
df[index, "dob_a"] <- df[index, "dob_a"] - 1

df <- df %>%
  select(pair_id, match, fname_a, fname_b, 
         lname_a, lname_b, dob_a, 
         address_a, g_a, id_a, mname_a, phone_a, ssn_a, 
         address_b, g_b, id_b, mname_b, phone_b, ssn_b,
         ffreq_a, lfreq_a, ffreq_b, lfreq_b, birth_year_b)

names(df_dob)[1] <- "ssn_b"

df <- df %>% 
  left_join(df_dob, by= "ssn_b")

year(df$bday) <- df$birth_year_b

df <- df %>%
  rename(dob_b = bday)

index <- df$dob_day == 29 & df$dob_mnth == 2 
df[index, "dob_b"] <- df[index, "dob_b"] - 1

df <- df %>%
  select(pair_id, match, fname_a, fname_b, 
         lname_a, lname_b, dob_a, dob_b,
         address_a, g_a, id_a, mname_a, phone_a, ssn_a, 
         address_b, g_b, id_b, mname_b, phone_b, ssn_b,
         ffreq_a, lfreq_a, ffreq_b, lfreq_b)

add_feature_vector <- function(df){
  # browser()
  
  df %>%
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

NC_test <- df %>% 
  mutate_if(is.integer, as.character) %>% 
  mutate_at(vars(contains("phone")), as.character)

NC_test[is.na(NC_test)] <- ""

NC_test <- NC_test %>%
  add_feature_vector() 

NC_test %>% 
  write_rds("./data/all_NC_test.rds")
NC_test %>% write_csv("./name2vec/all_NC_test.csv")
