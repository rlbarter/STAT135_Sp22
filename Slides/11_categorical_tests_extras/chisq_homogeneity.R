# chisq test for homogeneity
library(tidyverse)
austin <- data.frame(
  word = c("a", "an", "this", "that", "with", "without"),
  sense = c(147, 25, 32, 94, 59, 18),
  emma = c(186, 26, 39, 105, 74, 10),
  sanditon_1 = c(101, 11, 15, 37, 28, 10),
  sanditon_2 = c(83, 29, 15, 22, 43, 4)) 



austin_long <- austin %>%
  pivot_longer(cols = c("sense", "emma", "sanditon_1", "sanditon_2")) %>%
  rename(book = name, count = value)
austin_long

# compute the row and column sums
austin_long <- austin_long %>%
  group_by(word) %>%
  mutate(rowsum = sum(count)) %>%
  ungroup() %>%
  group_by(book) %>%
  mutate(colsum = sum(count)) %>%
  ungroup()
austin_long

total <- sum(austin_long$count)
total  

# add expected value column
austin_long <- austin_long %>%
  mutate(expected = rowsum * colsum / total)
austin_long

# compute the test statistic
austin_long <- austin_long %>%
  mutate(X2_terms = (count - expected)^2 / expected)
X2 <- austin_long %>%
  summarise(X2 = sum(X2_terms)) %>%
  pull(X2)
X2

# p-value 
I <- 6
J <- 4
df <- (I - 1) * (J - 1)
df
1 - pchisq(X2, df)



# or you can just use chisq.test!
chisq.test(austin[, c("sense", "emma", "sanditon_1", "sanditon_2")])

chisq.test(austin[, c("sense", "emma", "sanditon_1")])
