library(tidyverse)
df_train_n2v <- read.csv("./name2vec/train_n2v.csv", row.names = 1)

names(df_train_n2v)[76] <- "metric_fname_n2v"
names(df_train_n2v)[77] <- "metric_lname_n2v"
a <- df_train_n2v %>% 
  select(fname_a, fname_b, metric_fname_n2v, 
         lname_a, lname_b, metric_lname_n2v)
df_train_n2v %>% write_rds("./data/df_train_all_n2v.rds")

df_test_n2v <- read.csv("./name2vec/test_n2v.csv", row.names = 1)
names(df_test_n2v)[76] <- "metric_fname_n2v"
names(df_test_n2v)[77] <- "metric_lname_n2v"
b <- df_test_n2v %>% 
  select(fname_a, fname_b, metric_fname_n2v, 
         lname_a, lname_b, metric_lname_n2v)
df_test_n2v %>% write_rds("./data/df_test_all_n2v.rds")


#################################
#         binary n2v
#################################
df_train_n2v$metric_fname_n2v <- as.numeric(df_train_n2v$metric_fname_n2v < 0.4)
df_train_n2v$metric_lname_n2v <- as.numeric(df_train_n2v$metric_lname_n2v < 0.4)
df_train_n2v %>% write_rds("./data/df_train_all_binary_n2v.rds")

df_test_n2v$metric_fname_n2v <- as.numeric(df_test_n2v$metric_fname_n2v < 0.4)
df_test_n2v$metric_lname_n2v <- as.numeric(df_test_n2v$metric_lname_n2v < 0.4)
df_test_n2v %>% write_rds("./data/df_test_all_binary_n2v.rds")




####################################
#           for NC data
####################################
nc_test_n2v <- read.csv("./name2vec/all_NC_test_n2v.csv", row.names = 1)
names(nc_test_n2v)[72] <- "metric_fname_n2v"
names(nc_test_n2v)[73] <- "metric_lname_n2v"
b <- nc_test_n2v %>% 
  select(fname_a, fname_b, metric_fname_n2v, 
         lname_a, lname_b, metric_lname_n2v)
nc_test_n2v %>% write_rds("./data/all_NC_test_n2v.rds")
