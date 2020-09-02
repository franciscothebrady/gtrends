# install.packages(c("gtrendsR"))
library(gtrendsR)
library(lubridate)
library(dplyr)
library(readr)
library(purrr)
#Seattle code geo = c("US-WA-819")
#Set proxy to be able to start download


search_terms <-
  c("refinance",
    "realtor",
    "zillow",
    "homes for sale",
    "forbearance",
    "open houses",
    "mortgage help")

# build date vectors
current_date <- today()
start_date <- as.Date("2018-11-01")
months_sequence <- seq.Date(from = start_date, to = current_date, by = "months")
i <- length(months_sequence)
i <- NULL
interval_vector <- c() # to store
for(i in 1:length(months_sequence)){
  # define end date (90 days)
  start_date <- months_sequence[i]
  end_date <- months_sequence[i] + months(3) - days(1)
  # end date check 
  end_date <- if_else(end_date > current_date, current_date, end_date) 
  
  interval_vector[[i]] <- ifelse(difftime(end_date, start_date) > 60, paste0(start_date, " ", end_date), NA)
  
  }


interval_vector <- interval_vector[which(!is.na(interval_vector))]
interval_vector
# strsplit(interval_vector, "\\s+")
# if the number of days in each interval is less that 60 drop it

# interval_vector[length(interval_vector)]

# pull search terms ----
trends_t <- list()
#i <- 18
i <- NULL
for (i in 1:length(search_terms)) {
  print(search_terms[i])
  temp <- list()
  # for each date in interval 
  #j <- 1
  j <- NULL
  for(j in 1:length(interval_vector)){
    paste0("search term: ", search_terms[i], " for ", interval_vector[j])
    temp <- gtrends(c(search_terms[i]), 
                    geo = c("US"), 
                    time = interval_vector[j], 
                    gprop="web", hl="en-US",
                    low_search_volume = FALSE, onlyInterest = TRUE)#$interest_over_time
    # take the log of first differences 
    temp <- bind_rows(temp) %>%
      # take the log of the first diff 
      mutate(date = as.Date(date),
             lfdiff = log(hits / lag(hits)),
             lfdiff = ifelse(lfdiff %in% c(Inf, -Inf), 0, lfdiff)) %>%
      # then drop anything not in the last month we want 
      filter(month(date) == month(strsplit(interval_vector[j], "\\s+")[[1]][2])) %>%
      mutate(year = year(strsplit(interval_vector[j], "\\s+")[[1]][2]))
    # then bind it to the list 
    trends_t <- bind_rows(temp, trends_t) 
    # then hang for second so papa google doesnt get tired
    Sys.sleep(sample(0:10, replace = TRUE))
  }
  # write_csv(trends_t$interest_over_time, paste0(search_terms[i], 
  #                                              gsub(pattern = " ",
  #                                                   replacement = "_",
  #                                                   x = interval_vector[j]), 
  #                                              ".csv"))
}
# combine them all ---- 

all_series <- trends_t %>%
  select(year, date, hits, lfdiff, keyword, time) %>%
  arrange(keyword, time, date) %>%
  group_by(time) %>% 
  mutate(id = group_indices()) %>% ungroup

trendlist <- all_series %>%
  group_by(keyword) %>%
  group_split(keep = TRUE) 
  
keywords <- c("forbearance", "homes_for_sale", "mortgage_help",
              "open_houses", "realtor", "refinance", "zillow")
names(trendlist) <- keywords
# before running this part split into 2019 and 2020
# year_list <- 2019:2020

## ugh i'm annoyed i have to do it this way 
# 2019 ----
# forbearance
tm <- trendlist$forbearance %>% 
  filter(year == 2019) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
  # determine scaling factor to rebase levels to Jan 1 = 100 
  scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>% write_csv("new_forbearance_2019.csv")

# homes for sale 
tm <- trendlist$homes_for_sale %>% 
  filter(year == 2019) %>%  
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
  # determine scaling factor to rebase levels to Jan 1 = 100 
  scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>% write_csv("new_homes_for_sale_2019.csv")

# mortgage help
tm <- trendlist$mortgage_help %>% 
  filter(year == 2019) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>% write_csv("new_mortgage_help_2019.csv")

# open houses
tm <- trendlist$open_houses %>% 
  filter(year == 2019) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_open_houses_2019.csv")

# realtor
tm <- trendlist$realtor %>% 
  filter(year == 2019) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_realtor_2019.csv")

# refinance
tm <- trendlist$refinance %>% 
  filter(year == 2019) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_refinance_2019.csv")

# zillow
tm <- trendlist$zillow %>% 
  filter(year == 2019) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_zillow_2019.csv")

# 2020 ---- 
# forbearance
tm <- trendlist$forbearance %>% 
  filter(year == 2020) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_forbearance_2020.csv")

# homes for sale 
tm <- trendlist$homes_for_sale %>% 
  filter(year == 2020) %>%  
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_homes_for_sale_2020.csv")

# mortgage help
tm <- trendlist$mortgage_help %>% 
  filter(year == 2020) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_mortgage_help_2020.csv")

# open houses
tm <- trendlist$open_houses %>% 
  filter(year == 2020) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_open_houses_2020.csv")

# realtor
tm <- trendlist$realtor %>% 
  filter(year == 2020) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_realtor_2020.csv")

# refinance
tm <- trendlist$refinance %>% 
  filter(year == 2020) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_refinance_2020.csv")

# zillow
tm <- trendlist$zillow %>% 
  filter(year == 2020) %>%
  # drop first lfdiff
  mutate(lfdiff = ifelse(date == min(date), 0, lfdiff)) %>%
  # make cumulative sum of lfdfiff 
  mutate(cs_lfdiff = cumsum(lfdiff)) %>%
  select(date, hits, pct_chg = lfdiff, cum_chg = cs_lfdiff) %>%
  # replace first lfdiff with 0
  mutate(pct_chg = replace(pct_chg, row_number() == 1, 0)) %>%
  # create levels
  mutate(levels = hits[1]) %>%
  # replace levels with cum pct growth 
  mutate(levels = hits*(1+cum_chg)) 
# determine scaling factor to rebase levels to Jan 1 = 100 
scale <- 100 / tm$hits[1]
tm %>% 
  mutate(levels_rebased = levels * scale) %>%
  write_csv("new_zillow_2020.csv")


