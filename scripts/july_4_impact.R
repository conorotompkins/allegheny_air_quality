library(tidyverse)
library(lubridate)

theme_set(theme_bw())

df <- read_csv("data/allegheny_aq.csv") %>% 
  mutate(datetime = ymd(datetime),
         year = year(datetime),
         month = month(datetime, label = TRUE),
         mday = mday(datetime),
         yday = yday(datetime))

glimpse(df)

df_july4 <- df %>% 
  mutate(year = year(datetime),
    month = month(datetime, label = TRUE),
         mday = mday(datetime)) %>% 
  select(datetime, year, month, mday) %>% 
  distinct() %>% 
  filter(month == "Jul",
         mday == 4) %>% 
  rename(july4 = datetime) %>% 
  select(year, july4)
df_july4


df_analysis <- df %>% 
  filter(stat == "Avg",
         site %in% c("Flag Plaza", "Glassport High Street", "Liberty 2", "Lincoln")) %>% 
  select(datetime, year, yday, month, mday, stat, site,
         bcstat, h2s,
         no, no2, nox, noy, noydif,
         out_rh, out_t, ozone, ozone2,
         pm10, pm10b, pm25, pm25b, pm25t, so2) %>% 
  select(datetime:site, bcstat:so2) %>% 
  tidyr::pivot_longer(-c(datetime, year, yday, month, mday, stat, site), names_to = "measure", values_to = "value") %>% 
  arrange(site, measure, datetime)
  
df_analysis

df_analysis %>% 
  ggplot(aes(datetime, value, color = site)) +
  geom_line() +
  facet_grid(measure~site) +
  geom_vline(xintercept = as.integer(df_july4$july4), alpha = .3)

df_analysis <- df_analysis %>% 
  left_join(df_july4) %>% 
  mutate(timeline = datetime - july4)

df_analysis %>% 
  #mutate(yday = yday(datetime)) %>% 
  ggplot(aes(yday, as.numeric(timeline))) +
  geom_point() +
  facet_wrap(~year)

df_analysis %>% 
  ggplot(aes(timeline, value, color = measure)) +
  geom_point(alpha = .1) +
  geom_smooth() +
  geom_vline(xintercept = 0) +
  facet_grid(measure~site, scale = "free_y")

df %>% 
  select(datetime, yday, site, stat, no2) %>% 
  filter(!is.na(no2)) %>% 
  filter(stat == "Avg") %>% 
  ggplot(aes(yday, no2, color = site)) +
  geom_point(alpha = .1) +
  geom_smooth()
  

