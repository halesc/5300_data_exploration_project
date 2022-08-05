# 00_data_prep.R
# Chris Hales
# 2022-08-01

# Libraries
library(purrr)
library(vtable)
library(tidyverse)
library(lubridate)

# Get list of files beginning w/ trends_up_to from dir: data_raw.
file_vec = list.files(path = "./data_raw", pattern = "^[trends_up_to]", 
                      full.names = TRUE)
# Read in and consolidate the data.
trends_df = map_df(file_vec, read_csv)
View(trends_df)
vtable(trends_df)

# Check for data discrepancies.
check_na = trends_df %>% filter(is.na(index))
View(check_na)
# Some issues with NA being populated into fields (32,951). Possible parsing issue.
check_null = trends_df %>% filter(is.null(index))
View(check_null)
# No null data for index.

# Could give NA a mean or something but for simplicity's sake will just drop.
trends_df = trends_df %>% filter(!is.na(index))
check_na = trends_df %>% filter(is.na(index))
View(check_na)

# Convert monthorweek to a datetime variable at the week and month level.
trends_df = trends_df %>% mutate(week = ymd(str_sub(monthorweek, end = 10)),
                                 month = ym(str_sub(monthorweek, end = 7)))
View(trends_df)
# Standardize the index variable within school name and keyword.
trends_df = trends_df %>% group_by(schname, keyword) %>% 
  mutate(index_std = (index - mean(index)) / sd(index))
View(trends_df)
# Index of 100 = NA or just outlier?

# Sanity check for aggregation.
trends_df %>% filter(week == '2013-03-31' & schname == 'young harris college') %>% 
  group_by(schname, keyword, week)
# Index_std mean within keyword for young harris college is 0.175.
trends_df %>% filter(week == '2013-03-31' & schname == 'young harris college') %>% 
  group_by(schname, week) %>% summarize(mean(index_std))
# Checks out.

# Create new DF mean index_std by week grouped by school name and keyword
trends_schnameKeyword_by_week_df = trends_df %>% group_by(schname, week) %>% 
  summarize(index_std = mean(index_std))
View(trends_schnameKeyword_by_week_df)

# Read in the Scorecard and linking data
scorecard_df = read_csv('./data_raw/Most+Recent+Cohorts+(Scorecard+Elements).csv')
id_name_link_df = read_csv('./data_raw/id_name_link.csv')
View(id_name_link_df)

# Change the scorecard_df's headers to lowercase
names(scorecard_df) <- tolower(names(scorecard_df))

# Remove duplicate schools
id_name_link_df = id_name_link_df %>% group_by(schname) %>% mutate(n = n()) %>% 
  filter(n == 1)
vtable(id_name_link_df)

# Inner join id_name_link_df to trends_schnameKeyword_by_month_df on schname.
# Then, inner join to the Scorecard data on unitid and opeid.
processed_df = trends_schnameKeyword_by_week_df %>% 
  inner_join(id_name_link_df, by = "schname") %>% 
  inner_join(scorecard_df, by = c("unitid", "opeid"))
vtable(processed_df)

# Outwrite to file.
write_csv(processed_df, "./data_processed/weekly_trends_with_school_scorecards.csv")
