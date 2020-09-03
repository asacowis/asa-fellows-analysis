## Figures
library(tidyverse)

n_industry <- read_csv("data/nominees_industry.csv",
                       col_types = cols(
                         year = col_double(),
                         academe = col_double(),
                         industry = col_double(),
                         government = col_double()
                       )
)
n_industry <- n_industry %>%
  select(year, Academia = academe, `Business/Industry` = industry, Government = government) %>%
  pivot_longer(-year, names_to = "industry", values_to = "count")

fellow_industry <- read_csv("data/new_fellows_industry.csv",
                            col_types = cols(
                              year = col_double(),
                              academe = col_double(),
                              industry = col_double(),
                              government = col_double()
                            )
)
fellow_industry <- fellow_industry %>%
  select(year, Academia = academe, `Business/Industry` = industry, Government = government) %>%
  pivot_longer(-year, names_to = "industry", values_to = "count") 

fellow_industry %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  ggplot(aes(x = year, y = pct, color = industry)) +
  geom_point() + 
  geom_line() +
  scale_y_continuous("Percent", limits = c(0, 1), labels = scales::percent) + 
  scale_color_manual(name = "Employment sector",
                     values = c("#3B528BFF", "#27AD81FF", "#FDE725FF")) +
  scale_x_continuous("Year", breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        legend.title = element_blank()) 

ggsave("figures/fig-1.png")

n_industry %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  ggplot(aes(x = year, y = pct, color = industry)) +
  geom_point() + 
  geom_line() +
  scale_y_continuous("Percent", limits = c(0, 1), labels = scales::percent) + 
  scale_color_manual(name = "Employment sector",
                     values = c("#3B528BFF", "#27AD81FF", "#FDE725FF")) +
  scale_x_continuous("Year", breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        legend.title = element_blank()) 

ggsave("figures/fig-2.png", width = 7, height = 3)

left_join(n_industry, fellow_industry, by = c("year", "industry")) %>%
  mutate(pct = count.y / count.x) %>%
  ggplot(aes(x = year, y = pct, group = industry, color = industry)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous("Percent", labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous("Year", breaks = c(2004, 2008, 2012, 2016, 2020)) +
  scale_color_viridis_d(name = "", begin = 0.25) +
  theme_minimal() 

ggsave("figures/fig-3.png", width = 7, height = 3)



fellow_gender <- read_csv("data/new_fellows_gender.csv",
                          col_names = c("year", "Female", "Male"),
                          col_types = cols(
                            year = col_double(),
                            Female = col_double(),
                            Male = col_double()
                          ))
n_gender <- read_csv("data/nominees_gender.csv",
                     col_names = c("year", "Female", "Male"),
                     col_types = cols(
                       year = col_double(),
                       Female = col_double(),
                       Male = col_double()
                     ))

n_gender <- n_gender %>%
  pivot_longer(-year, names_to = "gender", values_to = "count")

fellow_gender <- fellow_gender %>%
  pivot_longer(-year, names_to = "gender", values_to = "count")

fellow_gender %>%
  filter(year > 2003) %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  ggplot(aes(x = year, y = pct, color = gender)) +
  geom_point() + 
  geom_line() +
  scale_y_continuous("Percent", limits = c(0, 1), labels = scales::percent) + 
  scale_color_manual(values = c("orange", "cornflower blue")) +
  scale_x_continuous("Year", breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        legend.title = element_blank()) 

ggsave("figures/fig-4.png", width = 7, height = 3)

n_gender %>%
  filter(year > 2003) %>%
  group_by(year) %>%
  mutate(pct = count/sum(count)) %>%
  ggplot(aes(x = year, y = pct, color = gender)) +
  geom_point() + 
  geom_line() +
  scale_y_continuous("Percent", limits = c(0, 1), labels = scales::percent) + 
  scale_color_manual(values = c("orange", "cornflower blue")) +
  scale_x_continuous("Year", breaks = c(2004, 2008, 2012, 2016, 2020)) +
  theme_minimal() +
  theme(axis.ticks = element_line(),
        legend.title = element_blank()) 

ggsave("figures/fig-5.png", width = 7, height = 3)

n_gender %>%
  left_join(fellow_gender, by = c("year", "gender")) %>%
  filter(year > 2003) %>%
  mutate(pct = count.y / count.x) %>%
  ggplot(aes(x = year, y = pct, group = gender, color = gender)) + 
  geom_point() + 
  geom_line() +
  scale_y_continuous("Percent", labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous("Year", breaks = c(2004, 2008, 2012, 2016, 2020)) +
  scale_color_manual("", values = c("orange", "cornflower blue")) +
  theme_minimal() 

ggsave("figures/fig-6.png", width = 7, height = 3)
