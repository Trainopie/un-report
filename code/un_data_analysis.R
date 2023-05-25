library(tidyverse)

#Read in data
gapminder_data <- read_csv("data/gapminder_data.csv")

#What is the mean life expectancy?
#summariza()

summarize(gapminder_data, averageLifeExp = mean(lifeExp))

#use piper reader (ctrl+shift+M) to make things run smoother in one dataset
gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

#establish a new object
gapminder_data_summarized <- gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

#What is the mean population in the gapminder dataset?
gapminder_data %>% 
  summarize(averagePopulation = mean(pop))

#What is the mean population and the mean lifeexpectancy?
gapminder_data %>%
  summarize(averageLifeExp = mean(lifeExp),
            meanPop=mean(pop))

#What is the mean life expectancy for the most recent year?
#filter()
#max() find the maximum value in the column
gapminder_data %>% 
  summarize(maxYear = max(year))
gapminder_data %>% 
  filter(year == 2007) %>% 
  summarize(meanLifeExp=mean(lifeExp))

#instead of guess the maxyear, just assume the maxyear
gapminder_data %>% 
  filter(year == max(year)) %>% 
  summarize(meanLifeExp=mean(lifeExp))

#what is the mean gdp per capital for the first/earliest year?

gapminder_data %>% 
  filter(year == min(year)) %>% 
  summarize(meanGdpCapital=mean(gdpPercap))

# > < !=(not equal to)

#What is the mean life expectancy for each year?
#group_by()
gapminder_data %>% 
  group_by(year) %>% 
  summarize(meanLifeExp= mean(lifeExp))
#What is the mean life expectanct for each continent?
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp= mean(lifeExp))

#What is the mean life expectancy and mean GDP for each continent?
gapminder_data %>% 
  group_by(continent) %>% 
  summarize(meanLifeExp= mean(lifeExp), meanGDP=mean(gdpPercap))

#What is the GDP (not per capita)?
#mutate() adds a new column attached to the original data
gapminder_data %>% 
  mutate(gdp = gdpPercap * pop)
#Make a new column for population in millions
gapminder_data %>% 
  mutate(pop_in_million = pop/1000000)
#overwrite/generate new data in the file
gapminder_data_popmil <- gapminder_data %>% 
  mutate(gdp = gdpPercap * pop, pop_in_million = pop/1000000)

#select(): chooses a subset of columns from a dataset
gapminder_data %>% 
  select(year, pop)

#select all but the continent column
gapminder_data %>% 
  select(-continent)

#create a tibble with only country, continent, year and lifeExp
gapminder_data %>% 
  select(country, year, continent, lifeExp)

gapminder_data %>% 
  select(-pop, -gdpPercap)


#select helper function: starts_with(), ends_with(), contains()
gapminder_data %>% 
  select(year, starts_with("c"))

#Vectors
#c()
my_vec <- c("dog", "cat", "horse")
num_vec <- c(1,2,3,4)
proof <- gapminder_data %>% 
  pull(year)

# %in% filter ids in c("id1", "id2", "id3")

#reshaping functions
#pivot_longer() and pivot_wider()
gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)
#pivot_wider, but populate values with gdpPercap

gapminder_data %>% 
  select(country, continent, year, gdpPercap) %>% 
  pivot_wider(names_from = year, values_from = gdpPercap)

#pivot_longer
gapminder_data %>% 
  pivot_longer(cols = c(pop, lifeExp,gdpPercap),
               names_to = "measurement_type",
               values_to = "measurement")

#Is there a relationship between GDP and CO2 emissions?

#assign the result to gapminder_data_2007
#filter for year == 2007 & continent Americas
#remove the year and continent columns
gapminder_data_2007 <- gapminder_data %>% 
  filter(year==2007, continent=="Americas") %>% 
  select(-year, -continent)

#read in the CO2 data
#skip the first row
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2, 
         col_names = c("region","country", "year", "series", "value", 
                       "footnotes", "source"))
co2_emissions <- co2_emissions_dirty %>% 
  select(country,year, series,value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from = series,
              values_from = value) %>% 
  filter(year==2005) %>% 
  select(-year)

#inner_join
inner_join(gapminder_data_2007, co2_emissions, by="country")




