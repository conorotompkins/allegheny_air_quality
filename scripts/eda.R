#devtools::install_github("tidyverse/tidyr")

library(tidyverse)
library(lubridate)

theme_set(theme_bw())

df <- read_csv("data/allegheny_aq.csv") %>% 
  mutate(datetime = ymd(datetime))

glimpse(df)

df %>% 
  count(site, sort = TRUE)

df %>% 
  count(stat)

df %>% 
  count(stat, no2, sort = TRUE)

#not working
df %>% 
  select(starts_with("pm")) %>% 
  pivot_longer(names_to = "measure", values_to = "value")
  count()

df %>% 
  select(site, datetime, stat,
         bcstat, h2s, 
         no, no2, nox, noy, noydif,
         out_rh, out_t, ozone, ozone2,
         contains("pm"),
         #pm10, pm10b, pm25, pm25b, pm25t, 
         so2#,
         #uvpm, rainfall, solarrad,
         #rwd, rws, sigtheta, sonicwd, sonicws, int_t
         ) %>% 
  select(site:stat, bcstat:so2) %>% 
  tidyr::pivot_longer(-c(site, datetime, stat), names_to = "measure", values_to = "value") %>% 
  filter(!is.na(value)) %>% 
  count(site, stat, measure, sort = TRUE) %>% 
  mutate(stat = as.factor(stat),
         measure = as.factor(measure)) %>% 
  #filter(measure %in% c("pm25", "pm10")) %>% 
  #filter(measure == "no2") %>% 
  ggplot(aes(stat, site, fill = n)) +
  geom_tile(color = "black") +
  scale_fill_viridis_c() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  facet_wrap(~measure, nrow = 1) +
  theme(panel.grid = element_blank())

df %>% 
  ggplot(aes(h2s)) +
  geom_density()

df %>% 
  ggplot(aes(datetime, h2s, color = site)) +
  geom_smooth()

df %>% 
  ggplot(aes(datetime, noy, color = site))+
  geom_smooth()

df_graph <- df %>% 
  filter(stat == "Avg") %>%
  select(site, datetime, 
         bcstat, h2s, 
         no, no2, nox, noy, noydif,
         out_rh, out_t, ozone, ozone2,
         pm10, pm10b, pm25, pm25b, pm25t, so2,
         uvpm, rainfall, solarrad,
         rwd, rws, sigtheta, sonicwd, sonicws, int_t) %>% 
  select(site:datetime, bcstat:solarrad) %>% 
  complete(site, datetime) %>%
  tidyr::pivot_longer(-c(site, datetime), names_to = "measure", values_to = "value") %>% 
  arrange(site, measure, datetime) %>%
  group_by(measure) %>% 
  mutate(value = scale(value))
df_graph

df_graph %>%
  ggplot(aes(datetime, measure, fill = value)) +
  geom_tile() +
  facet_wrap(~site) +
  scale_fill_viridis_c() +
  scale_x_date(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))
    