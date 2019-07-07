library(tidyverse)
library(lubridate)
library(hrbrthemes)

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
  #filter(stat == "Avg",
  #       site %in% c("Flag Plaza", "Glassport High Street", "Liberty 2", "Lincoln")) %>% 
  select(datetime, year, yday, month, mday, stat, site,
         #bcstat, h2s,
         no, no2, nox, noy, noydif,
         #out_rh, out_t, ozone, ozone2,
         pm10, pm10b, pm25, `pm25(2)`, pm25b, pm25t) %>% 
  select(datetime:site, no2:pm25t) %>% 
  tidyr::pivot_longer(-c(datetime, year, yday, month, mday, stat, site), names_to = "measure", values_to = "value") %>% 
  arrange(site, measure, datetime)

df_analysis <- df_analysis %>% 
  left_join(df_july4) %>% 
  mutate(timeline = datetime - july4)
  
df_analysis

df_analysis %>% 
  filter(!is.na(value),
         stat %in% c("Avg", "Max")) %>% 
  ggplot(aes(yday, value, color = site)) +
  geom_point(alpha = .1) +
  facet_grid(stat~measure, scales = "free_y") +
  labs(x = "Day of year",
       y = "Value") +
  scale_color_discrete("Site")
  #geom_vline(xintercept = as.integer(df_july4$july4), alpha = .3)

df_analysis %>% 
  #mutate(yday = yday(datetime)) %>% 
  ggplot(aes(yday, as.numeric(timeline))) +
  geom_point() +
  facet_wrap(~year)

df_analysis %>% 
  group_by(measure) %>% 
  mutate(value = scale(value)) %>% 
  filter(stat %in% c("Avg", "Max")) %>% 
  ggplot(aes(yday, value, color = site)) +
  #geom_point(alpha = .8, size = .3) +
  geom_smooth() +
  geom_vline(xintercept = 0) +
  facet_grid(stat~measure, scale = "free_y", margins = FALSE) +
  labs(x = "Day of year",
       y = "Scaled value") +
  scale_color_discrete("Site") +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

df_analysis %>% 
  group_by(measure) %>% 
  mutate(value = scale(value)) %>% 
  filter(stat %in% c("Avg", "Max")) %>% 
  ggplot(aes(timeline, value, color = measure)) +
  #geom_point(alpha = .3) +
  geom_smooth() +
  coord_cartesian(xlim = c(-30, 30))


df_analysis %>% 
  select(timeline, site, stat, measure, value) %>% 
  filter(!is.na(value)) %>% 
  filter(stat %in% c("Max", "Avg"),
         measure == "pm25") %>% 
  ggplot(aes(timeline, value, color = site)) +
  geom_point(alpha = .4) +
  geom_smooth() +
  geom_vline(xintercept = 0) +
  facet_grid(~stat) +
  labs(x = "Days since July 4",
       y = "Value") +
  scale_color_discrete("Site") +
  theme_ipsum()
  

