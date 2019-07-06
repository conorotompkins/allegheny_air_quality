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
    