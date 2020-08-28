## Looking at coverage in the tennis match data

library(tidyverse)
library(knitr)

## MATCH LEVEL DATA - womens 
df_matches_w <- read.csv("tennis_MatchChartingProject/charting-w-matches.csv")

nrow(df_matches_w) #2577

# Date field 
df_matches_w %>% 
  mutate(yr = substr(Date,1,4), 
         date = parse_date(Date, format = "%Y%m%d")) -> df_matches_w

# By tournament  
df_matches_w %>% 
  group_by(Tournament) %>% 
  summarise(n_matches=n()) %>% 
  arrange(desc(n_matches)) %>% kable()

# Major tourneys
majors <- c("Australian Open", "Wimbledon", "US Open", "Roland Garros") 

# All majors
df_matches_w %>% 
  filter(Tournament %in% majors, yr >= 1995) %>% 
  group_by(Tournament, yr) %>% 
  summarise(n=n()) %>% 
  spread(Tournament, n, fill = 0) %>%
  mutate(Total = rowSums(.[2:5])) %>% 
  kable()

# Finals only 
df_matches_w %>% 
  filter(Tournament %in% majors, yr >= 1995, Round %in% c("F")) %>% 
  group_by(Tournament, yr) %>% 
  summarise(n=n()) %>% 
  spread(Tournament, n, fill = 0) %>%
  mutate(Total = rowSums(.[2:5])) %>% 
  kable()

# Semi finals 
df_matches_w %>% 
  filter(Tournament %in% majors, yr >= 1995, Round %in% c("SF")) %>% 
  group_by(Tournament, yr) %>% 
  summarise(n=n()) %>% 
  spread(Tournament, n, fill = 0) %>%
  mutate(Total = rowSums(.[2:5])) %>% 
  kable()

## MATCH LEVEL DATA - mens 
df_matches_m <- read.csv("tennis_MatchChartingProject/charting-m-matches.csv", quote = "")

nrow(df_matches_m) #2577

# Date field 
df_matches_m %>% 
  mutate(yr = substr(Date,1,4), 
         date = parse_date(Date, format = "%Y%m%d")) -> df_matches_m


# By tournament  
df_matches_m %>% 
  group_by(Tournament) %>% 
  summarise(n_matches=n()) %>% 
  arrange(desc(n_matches)) %>% kable()

# All majors
df_matches_m %>% 
  filter(Tournament %in% majors, yr >= 1995) %>% 
  group_by(Tournament, yr) %>% 
  summarise(n=n()) %>% 
  spread(Tournament, n, fill = 0) %>%
  mutate(Total = rowSums(.[2:5])) %>% 
  kable()

# Finals only 
df_matches_m %>% 
  filter(Tournament %in% majors, yr >= 1995, Round %in% c("F")) %>% 
  group_by(Tournament, yr) %>% 
  summarise(n=n()) %>% 
  spread(Tournament, n, fill = 0) %>%
  mutate(Total = rowSums(.[2:5])) %>% 
  kable()

# Semi finals 
df_matches_m %>% 
  filter(Tournament %in% majors, yr >= 1995, Round %in% c("SF")) %>% 
  group_by(Tournament, yr) %>% 
  summarise(n=n()) %>% 
  spread(Tournament, n, fill = 0) %>%
  mutate(Total = rowSums(.[2:5])) %>% 
  kable()

# Restricted dataset
df_matches_m %>% 
  filter(Tournament %in% majors, yr >= 1995, yr <= 2018, Round %in% c("F", "SF")) -> df_restricted_m

# Summarize coverage again 
df_restricted_m %>% 
  group_by(Tournament, yr) %>% 
  summarise(n=n()) %>% 
  spread(Tournament, n, fill = 0) %>%
  mutate(Total = rowSums(.[2:5])) %>% 
  kable()

df_restricted_m %>% 
  group_by(Tournament) %>% 
  summarise(n=n()) %>% 
  kable()

# Create post and treatment dummies 
adopt_date <- as.Date("2006-09-01") # Effective adopt date 

df_restricted_m %>% 
  mutate(post = ifelse(date >= adopt_date,1,0), 
         treat= ifelse(Tournament != 'Roland Garros',1,0)) -> df_restricted_m

# Alternative way to code adoption 
#is_hawkeye = ifelse(Tournament!='Roland Garros',ifelse((Tournament %in% c('Wimbledon','Australian Open') & yr >= 2007) | (Tournament == 'US Open' & yr >= 2006),1,0),0),