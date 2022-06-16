## Paired blood pressure hypothesis tests

x <- c(125, 132, 138, 120, 125, 127, 136, 139, 131, 132, 135, 136, 128, 127, 130)
y <- c(118, 134, 130, 124, 105, 130, 130, 132, 123, 128, 126, 140, 135, 126, 132)
d <- x - y
n <- length(d)
#-----------------------------------------------------------------------------#
######################## paired two-sample t-test #############################
#-----------------------------------------------------------------------------#

# H0: mu_d = 0
# H1: mu_d > 0

# compute all the things we need:

dbar <- mean(d)
dbar
s_d <- sd(d)
s_d

# test statistic
t <- dbar / (s_d / sqrt(n))
t
df <- n - 1
df


# p-value
1 - pt(t, df)

# Check answer with t.test (set paired = TRUE):
t.test(x, y, alternative = "greater", paired = TRUE)





#-----------------------------------------------------------------------------#
########################## Sign test ##########################################
#-----------------------------------------------------------------------------#

# compute the signs of the differences
signs <- sign(d)
signs
# compute the number of positive differences
w <- sum(signs == 1)
w

# compute the p-value using the pbinom function
pval <- 1 - pbinom((w-1), n, 0.5)
pval

# using an inbuilt test
binom.test(w, n, alternative = "greater")






#-----------------------------------------------------------------------------#
####################### Wilcoxon signed rank test #############################
#-----------------------------------------------------------------------------#
n <- length(d)
signs <- sign(d)
# compute ranks of absolute differences
r <- rank(abs(d))
r
# compute signed ranks
signed_r <- signs * r
signed_r

# compute test statistic
w <- sum(signed_r[signed_r > 0])
w
# compute expected value of test statistic under H0
w_expected_val <- n * (n + 1) / 4
w_expected_val
# compute variance of test statistic under H0
w_var <- n * (n + 1) * (2*n + 1) / 24
w_var

z <- (w - w_expected_val) / sqrt(w_var)
z

pval <- 1 - pnorm(z)
pval 

wilcox.test(x, y, paired = TRUE, 
            alternative = "greater", 
            exact = FALSE,
            correct = FALSE)

