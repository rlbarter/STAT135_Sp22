# chi-squared goodness of fit test
library(tidyverse)

######### Left-handed/Right-handed example

data <- matrix(c(43, 9, 44, 4), ncol = 2, byrow = TRUE)
data

?phyper
# phyper(k, n.1, n.2, n1.)
# phyper(k, 1st col sum, second col sum, first row sum)
1 - phyper(47.48, 87, 13, 52) + phyper(43, 87, 13, 52)
# but hypergeom takes integer values
1 - phyper(47, 87, 13, 52) + phyper(43, 87, 13, 52)
fisher.test(data)





############################ Anxiety example ##################################

data <- matrix(c(12, 5, 4, 9), ncol = 2, byrow = TRUE)


# p-value (param values are just read from table)
# phyper(k, n.1, n.2, n1.)
1 - phyper(11, 16, 14, 17) + phyper(6.14, 16, 14, 17)
1 - phyper(11, 16, 14, 17) + phyper(6, 16, 14, 17)


# much easier: just give the data to fisher.test

fisher.test(data)
fisher.test(t(data))
