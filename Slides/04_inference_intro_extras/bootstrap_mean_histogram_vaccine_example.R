# covid vaccine per capita
# data source: https://ourworldindata.org/grapher/covid-vaccinations-vs-gdp-per-capita
library(tidyverse)
library(lubridate)
vaccine_data_orig <- read_csv("covid-vaccinations-vs-gdp-per-capita.csv")

covid_vaccines <- covid_vaccines_orig %>%
  select(country = Entity,
         date = Day,
         vaccines_per_hundred = total_vaccinations_per_hundred,
         gdp_per_capita = `GDP per capita, PPP (constant 2011 international $)`,
         continent = Continent) 
total_covid_vaccines <- covid_vaccines %>%
  group_by(country) %>%
  filter(vaccines_per_hundred == max(vaccines_per_hundred)) %>%
  ungroup()

set.seed(238479)
map_df(1:100, function(i) {
  mean_doses <- total_covid_vaccines %>%
    # bootstrap samples
    sample_n(nrow(total_covid_vaccines), replace = TRUE) %>%
    drop_na(vaccines_per_hundred) %>%
    summarise(mean_vaccine_doses_per_hundred = mean(vaccines_per_hundred)) %>%
    pull(mean_vaccine_doses_per_hundred)
  data.frame(mean_doses = mean_doses)
}) %>%
  ggplot() +
  geom_histogram(aes(x = mean_doses), color = "white",
                 binwidth = 1) +
  theme_classic()
