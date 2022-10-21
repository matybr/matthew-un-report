library(tidyverse)
getwd()

# load gapminder_data
gapminder_data <- read_csv("data/gapminder_data.csv") 

# summarize data
summarize(gapminder_data, averageLifeExp = mean(lifeExp))

#using pipe

gapminder_data %>%
  summarize(averagelifeexp = mean(lifeExp))

# assigning object
gapminder_data_summarize <- gapminder_data %>%
  summarize(averagelifeexp = mean(lifeExp))
gapminder_data_summarize

# filter data using filter() function
gapminder_data %>%
  filter(year == 2007) %>%
  summarize(averagelifeexp = mean(lifeExp))

# trying on my own finding earliest year in dataset
gapminder_data %>%
  summarize(minyear = min(year))

# finding avggdp for earliest year
gapminder_data %>%
  filter(year == 1952) %>%
  summarize(avggdp = mean(gdpPercap))

# grouping data

gapminder_data %>%
  group_by(year) %>%
  summarise(average = mean(lifeExp))

gapminder_data %>%
  group_by(continent) %>%
  summarise(averagelifeexp = mean(lifeExp), minlifeexp = min(lifeExp))

# add new col w/ mutate()

gapminder_data %>%
  mutate(gdp = pop * gdpPercap, 
         pop_in_millions = pop/1000000) # does not change orig. table because not object assigned

# subset col or change order w/ select()

gapminder_data %>%
  select(pop, year)

# dropping cols from dataset
gapminder_data %>%
  select(-pop)

# isolating cols of interest
gapminder_data %>%
  select(continent, country)

#reordering cols without removing
gapminder_data %>%
  select(gdpPercap, everything())

# moving b/w long and wide data w/ pivot_wider() & pivot_longer()

gapminder_data %>%
  select(country, continent, year, lifeExp) %>%
  pivot_wider(names_from = year, values_from = lifeExp)

# dataset for analysis

gapminder_data_2007 <-
  gapminder_data %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)

view(gapminder_data_2007)  




# data cleaning

read_csv("data/co2-un-data.csv", skip = 1)

read_csv("data/co2-un-data.csv", skip = 2,
         col_names = c("region","country","year","series", 
                       "value", "footnotes", "source"))

read_csv("data/co2-un-data.csv", skip = 1) %>%
  rename(country = ...2)

# makes all column headers lower case
read_csv("data/co2-un-data.csv", skip =1 ) %>%
  rename_all(tolower)

co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
                                col_names = c("region","country","year","series", 
                                              "value", "footnotes", "source"))


# practicing select() country, year, series, value
co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" =
                         "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" =
                           "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  # number of observations per year
  count(year)

# filter for 2005 only
co2_emissions_clean_2005 <- co2_emissions_dirty %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" =
                           "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" =
                           "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year)

# joining data frames

df <- inner_join(gapminder_data_2007, co2_emissions_clean_2005)
view(df)

# pulls out observations that are not shared by both datasets
anti_join(gapminder_data_2007, co2_emissions_clean_2005, 
          by = "country")

co2_emissions_clean_2005 <- read_csv("data/co2-un-data.csv",
                                     skip = 2,
                                     col_names = c("region", "country", "year",
                                                   "series", "value", "footnotes", 
                                                   "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, 
                         "Emissions (thousand metric tons of carbon dioxide)" =
                           "total_emissions", 
                         "Emissions per capita (metric tons of carbon dioxide)" =
                           "per_capita_emissions")) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  filter(year == 2005) %>%
  select(-year) %>%
  mutate(country = recode(country, 
                          "Bolivia (Plurin. State of)" = "Bolivia",
                          "United States of America" = "United States",
                          "Venezuela (Boliv. Rep. of)" = "Venezuela"))

anti_join(gapminder_data_2007, co2_emissions_clean_2005, by = "country")  

# fixing naming discrepencies
gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country,
                          "Puerto Rico" = "United States"))

anti_join(gapminder_data_2007, co2_emissions_clean_2005, by = "country")  

#fixing puerto ricos hanging variables
gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country,"Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarise(lifeExp = sum(lifeExp * pop) / sum(pop),
            gdpPercap = sum(gdpPercap * pop) / sum(pop),
            pop = sum(pop))

inner_join(gapminder_data_2007, co2_emissions_clean_2005, by = "country")

