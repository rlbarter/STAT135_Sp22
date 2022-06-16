library(tidyverse)
library(gapminder)
gapminder

# the population variable looks very not normal
gapminder %>% 
  ggplot() + 
  geom_histogram(aes(x = pop))

gapminder %>%
  ggplot() + 
  geom_qq(aes(sample = pop)) +
  geom_qq_line(aes(sample = pop))


# the logarithm population variable looks a lot more normal
gapminder %>% 
  ggplot() + 
  geom_histogram(aes(x = log(pop)))

gapminder %>%
  ggplot() + 
  geom_qq(aes(sample = log(pop))) +
  geom_qq_line(aes(sample = log(pop)))

