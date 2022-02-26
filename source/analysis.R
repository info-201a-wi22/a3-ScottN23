library(tidyverse)
library(scales)
library(reshape2)
library(leaflet)
library(maps)
library(mapproj)

# Imports data from Vera Institute into variable
incarceration_data <- read.csv(url("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"))

# Data Wrangling for trends over time chart of jail population of specific race divided by total ages 15-64 in that race group
per_capita_jail_race_data <- incarceration_data %>%
  filter(year >= 1990) %>%
  group_by(year) %>%
  summarize(AAPI = sum(aapi_jail_pop, na.rm = TRUE) / sum(aapi_pop_15to64, na.rm = TRUE), Black = sum(black_jail_pop, na.rm = TRUE) / sum(black_pop_15to64, na.rm = TRUE), Latinx = sum(latinx_jail_pop, na.rm = TRUE) / sum(latinx_pop_15to64, na.rm = TRUE), Native = sum(native_jail_pop, na.rm = TRUE) / sum(native_pop_15to64, na.rm = TRUE), White = sum(white_jail_pop, na.rm = TRUE) / sum(white_pop_15to64, na.rm = TRUE))

per_capita_jail_race_data <- melt(per_capita_jail_race_data, id = "year")

# Graphs the trend over time chart
per_capita_jail_race <- ggplot(per_capita_jail_race_data, aes(x = year, y = value, color = variable)) +
  geom_line() +
  scale_colour_discrete(name = "Race") +
  scale_x_continuous(breaks = seq(1990, 2016, 5)) +
  labs(x = "Year", y = "Jail Population per Capita (Ages 15-64)", title = "Jail Population per Capita Over Time (Ages 15-64)")
per_capita_jail_race

# Data Wrangling for variable comparison chart of White and Black jail population in 2018
prop_white_black_jailed_2018_data <- incarceration_data %>%
  filter(year == 2018) %>%
  group_by(year) %>%
  summarize(White = sum(white_jail_pop, na.rm = TRUE), Black = sum(black_jail_pop, na.rm = TRUE))

prop_white_black_jailed_2018_data <- melt(prop_white_black_jailed_2018_data, id = "year")

# Graphs the variable comparison pie chart
prop_white_black_jailed_2018 <- ggplot(prop_white_black_jailed_2018_data, aes(x = "", y = value, fill = variable)) + 
  geom_col(color = "black") +
  geom_text(aes(label = value),
            position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Jail Population")) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Comparison of White vs Black Population in Jail (2018)")
prop_white_black_jailed_2018

# Create Map for black, white, and overall jail population within the year of 2018
incarceration_data_map <- incarceration_data %>%
  filter(year == 2018)

county_shapes <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

map_data <- county_shapes %>%
  left_join(incarceration_data_map, by = "fips")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )

black_incarceration_map <- ggplot(map_data) + 
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = black_jail_pop),
    color = "grey", size = 0.2
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$black_jail_pop)), na.value = "white", low = "steelblue", high = "steelblue4") +
  blank_theme +
  ggtitle("Black Jailing Population in United States") +
  guides(fill = guide_legend(title = "Black Jail Population"))
black_incarceration_map

white_incarceration_map <- ggplot(map_data) + 
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = white_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$white_jail_pop)), na.value = "white", low = "steelblue", high = "steelblue4") +
  blank_theme +
  ggtitle("White Jailing Population in United States") +
  guides(fill = guide_legend(title = "White Jail Population"))
white_incarceration_map

total_incarceration_map <- ggplot(map_data) + 
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = total_jail_pop),
    color = "gray", size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(limits = c(0, max(map_data$total_jail_pop)), na.value = "white", low = "steelblue", high = "steelblue4") +
  blank_theme +
  ggtitle("Total Jail Population in United States") +
  guides(fill = guide_legend(title = "Jail Population"))
total_incarceration_map

# Summary Variable Calculations

# Black, White, and total jail population in 2018
black_jail_2018 <- incarceration_data %>%
  filter(year == 2018) %>%
  summarize(black_jail_2018 = sum(black_jail_pop, na.rm = TRUE)) %>%
  pull(black_jail_2018)

white_jail_2018 <- incarceration_data %>%
  filter(year == 2018) %>%
  summarize(white_jail_2018 = sum(white_jail_pop, na.rm = TRUE)) %>%
  pull(white_jail_2018)
  
total_jail_2018 <- incarceration_data %>%
  filter(year == 2018) %>%
  summarize(total_jail_2018 = sum(total_jail_pop, na.rm = TRUE)) %>%
  pull(total_jail_2018)

# Proportion of Black and White compared to total in 2018
prop_blacK_jail_2018 <- black_jail_2018 / total_jail_2018 * 100
  
prop_white_jail_2018 <- white_jail_2018 / total_jail_2018 * 100

# Capita of black and white in jail from ages 15-64 within respective races in 2018
capita_black_jail <- incarceration_data %>%
  filter(year == 2018) %>%
  summarize(capita_black_jail = black_jail_2018 / sum(black_pop_15to64, na.rm = TRUE) * 100) %>%
  pull(capita_black_jail)

capita_white_jail <- incarceration_data %>%
  filter(year == 2018) %>%
  summarize(capita_white_jail = white_jail_2018 / sum(white_pop_15to64, na.rm = TRUE) * 100) %>%
  pull(capita_white_jail)

# Highest jailed state in 2018
highest_jailed_state_2018 <- incarceration_data %>%
  filter(year == 2018) %>%
  group_by(state) %>%
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(state)

# Highest jailed county in 2018
highest_jail_county_2018 <- incarceration_data %>%
  filter(year == 2018) %>%
  group_by(county_name) %>%
  summarize(total_jail_pop = sum(total_jail_pop, na.rm = TRUE)) %>%
  filter(total_jail_pop == max(total_jail_pop, na.rm = TRUE)) %>%
  pull(county_name)
