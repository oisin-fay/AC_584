install.packages("tidyverse")
install.packages("plotly")

library(tidyverse)
library(plotly)

unicef_metadata <- read_csv("unicef_metadata.csv")
unicef_indicator_1 <- read_csv("unicef_indicator_1.csv")
unicef_indicator_2 <- read_csv("unicef_indicator_2.csv")
Continents <- read_csv("Continents.csv")

# exercise 1
data_join <- full_join(unicef_metadata, unicef_indicator_1)
data_join <- full_join(unicef_metadata, unicef_indicator_1, by = join_by(country, year == time_period))
data_join <- full_join(unicef_metadata, unicef_indicator_1, by = c("country", "year" = "time_period"))

# exercise 2
data_join <- full_join(unicef_metadata, unicef_indicator_2, by = c("country", "year" = "time_period"))

# exercise 3
data_join <- full_join(unicef_metadata, Continents, by = c("country"))

# final data object
data_join <- unicef_metadata %>%
  full_join(unicef_indicator_1, by = c("country", "year" = "time_period")) %>% 
  full_join(unicef_indicator_2, by = c("country", "year" = "time_period")) %>%
  full_join(Continents, by = c("country"))

map_world <- map_data("world")

# map 1
map_data_join <- full_join(unicef_indicator_2, map_world, by = c("country" = "region"))

ggplot(map_data_join) +
  aes(x = long, y = lat, group = group, fill = obs_value) +
  geom_polygon(color = "black", linewidth = 0.2) +
  scale_fill_gradient(name = "refugees per 1000", low = "lightblue", high = "darkblue") +
  labs(title = "Refugee Concentration 2022", x = "longitude", y = "latitude") +
  theme_light() +
  theme(text = element_text(family = "serif"))
  
# timeseries 1

timeseries_plot_1 <- data_join %>%
  ggplot() +
  aes(year, LifeExp , group = country, colour = continent) +
  geom_line() +
  labs( title = "Life Expectancy over time", x = "Year", y = "Life expectancy") +
  scale_color_discrete( name = "continent") +
  theme_light() +
  theme(text = element_text(family = "serif"))

ggplotly(timeseries_plot_1)

# scatter plot 1

ggplot(data_join) +
  aes(GDP, LifeExp, color = continent, size = Population) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ year) +
  scale_x_continuous(limits = c(0,50000), breaks = c(20000,40000), labels = scales::unit_format(unit = "k", scale = 0.001)) +
  labs(x = "GDP per capita", y = "Life expectancy", title = "Evolution of the relationship between GDP per capita and life expectancy") +
  guides (color = "none", size = "none") +
  theme_light() +
  theme(text = element_text(family = "serif"))

# bar chart 1

data_join %>% 
  group_by(continent, year) %>%
  summarise(m_obs_value.y = mean(obs_value.y, na.rm = TRUE)) %>%
  ggplot() +
  aes(reorder(continent, m_obs_value.y), m_obs_value.y, fill = continent) +
  geom_col() +
  labs(x = "", y = "Average refugees per 1000", fill = "continent", title = "Most refugees hosted by continent") +
  theme_light() +
  theme(text = element_text(family = "serif")) +
  scale_fill_manual(values = c("red", "purple", "pink", "blue", "turquoise", "white"))

