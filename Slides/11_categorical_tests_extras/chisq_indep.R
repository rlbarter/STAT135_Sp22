# chisq test for homogeneity
library(tidyverse)
color_pref <- data.frame(
  personality = c("extroverted", "introverted"),
  blue = c(5, 10),
  red = c(20, 5),
  yellow = c(5, 5)) 
color_pref


color_pref_long <- color_pref %>%
  pivot_longer(cols = c("blue", "red", "yellow")) %>%
  rename(color = name, count = value)
color_pref_long

# compute the row and column sums
color_pref_long <- color_pref_long %>%
  group_by(personality) %>%
  mutate(rowsum = sum(count)) %>%
  ungroup() %>%
  group_by(color) %>%
  mutate(colsum = sum(count)) %>%
  ungroup()
color_pref_long

total <- sum(color_pref_long$count)
total  

# add expected value column
color_pref_long <- color_pref_long %>%
  mutate(expected = rowsum * colsum / total)
color_pref_long

# compute the test statistic
color_pref_long <- color_pref_long %>%
  mutate(X2_terms = (count - expected)^2 / expected)
X2 <- color_pref_long %>%
  summarise(X2 = sum(X2_terms)) %>%
  pull(X2)
X2

# p-value 
I <- 2
J <- 3
df <- (I - 1) * (J - 1)
df
1 - pchisq(X2, df)



# or you can just use chisq.test!
chisq.test(color_pref[, c("blue", "red", "yellow")])
