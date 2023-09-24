library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(ggridges)
a <- read_csv2("data/armor.csv") %>%
  mutate(
    type = factor(type, type, ordered = TRUE),
    totalhp = 100 + hp
  )
d <- read_csv2("data/weapstats.csv")

stats <- crossing(d,a) %>%
  mutate(
    stk = ceiling(totalhp/dmg),
    ttk = 60*(stk-1)/rpm
  )

all_lines <- stats %>%
  ggplot(aes(y = ttk, x = totalhp,
  color = class, group = weapon)) +
  geom_point(size = 3) + geom_line()

avg_lines <- stats %>%
  group_by(class, totalhp) %>%
  summarise(ttk = mean(ttk)) %>%
    ggplot(aes(y = ttk, x = totalhp,
  color = class, group = class)) +
  geom_point(size = 3) + geom_line()

all_ridge <- bind_rows(
  stats,
  stats %>%
  filter(class == "CAR") %>%
  slice(rep(1:n(), each = 3))
) %>% ggplot +
geom_density_ridges(aes(x = ttk, y = type,
  group = interaction(type, class), color = class),
  alpha = 0.01, size = 2, scale = 1)

ggsave("out/all_lines.png",  all_lines)
ggsave("out/avg_lines.png",  avg_lines)
ggsave("out/all_ridge.png",  all_ridge)