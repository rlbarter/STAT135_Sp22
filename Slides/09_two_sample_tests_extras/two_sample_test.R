## Pizza hypothesis test

x <- c(20.4, 24.2, 15.4, 21.4, 20.2, 18.5, 21.5)
y <- c(20.2, 16.9, 18.4, 17.3, 20.5)
n <- 10
# H0: mu1 = mu2
# H1: mu1 != mu2

#-----------------------------------------------------------------------------#
################### Two-sample t-test (unequal variance) ######################
#-----------------------------------------------------------------------------#


# compute all the things we need:
n <- length(x)
m <- length(y)
xbar <- mean(x)
xbar
ybar <- mean(y)
ybar
s1 <- sd(x)
s1
s2 <- sd(y)
s2


df <- (s1^2/n + s2^2/m)^2 / (s1^4 / (n^2 * (n-1)) + s2^4 / (m^2 * (m-1)))
df

t <- (xbar - ybar) / (sqrt(s1^2/n + s2^2 / m))
t

pval <- 2 * (1 - pt(t, df))
pval

# Compare with variance unequal (Welch's) test
t.test(x, y, alternative = "two.sided")



#-----------------------------------------------------------------------------#
################### Two-sample t-test (pooled variance) #######################
#-----------------------------------------------------------------------------#

# compute all the things we need:
n <- length(x)
m <- length(y)
xbar <- mean(x)
xbar
ybar <- mean(y)
ybar
s1 <- sd(x)
s1
s2 <- sd(y)
s2

# assume equal variances, so we will used the pooled variance
sp <- sqrt(((n-1)*s1^2 + (m-1)*s2^2) / (n + m - 2))
sp

# pooled test statistic
t <- (xbar - ybar) / (sp * sqrt(1/n + 1/m))
t
df <- n + m - 2
df


# p-value
pvalue <- 2*(1 - pt(t, df))
pvalue 

# Check answer with t.test (set variance equal):
t.test(x, y, alternative = "two.sided", var.equal = TRUE)



#-----------------------------------------------------------------------------#
##################### Mann-whitney non-parametric test ########################
#-----------------------------------------------------------------------------#

n <- length(x)
m <- length(y)

# combine the two datasets into one (smaller dataset first)
z <- c(x, y)
z
# compute the ranks
ranks <- rank(z)
ranks


r1 <- sum(ranks[1:n])
r1
r2 <- sum(ranks[(n + 1):(n + m)])
r2

u1 <- n * m  + n * (n + 1) / 2 - r1
u1
u2 <- n * m  + m * (m + 1) / 2 - r2
u2

u <- min(u1, u2)
u

# Mann-whitney non-parametric test
wilcox.test(x, y, alternative = "two.sided", exact = FALSE)
wilcox.test(y, x, alternative = "two.sided", exact = FALSE)
# similar p-value to the parametric t-test

