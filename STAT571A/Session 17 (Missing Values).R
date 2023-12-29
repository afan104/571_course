library(mice)
rm(list = ls())
set.seed(571)

# Suppose we want to fit to this synthetic data: B(1,2,1,1/2)
B0 = 1
B1 = 2
B2 = 1/2
eps = rnorm(100, sd = 0.7)

# full data
x1 <- seq(0.01,1,.01)
x2 <- sample(c(rep(0,50),rep(1,50)))
y <- B0 + B1*x1 + B2*x2 + eps
df <- data.frame(x1,x2,y)

# create df with some x1/x2 removed
na_indices1 <- sample.int(100,30)
na_indices2 <- sample.int(100,30)

df_miss <- df
df_miss$x1[na_indices1] <- NA
df_miss$x2[na_indices2] <- NA

# plot data and indicate missing values
colors = c(1,2)
alpha = c(1,0.5,0)

na_table <- table(factor(c(na_indices1,na_indices2), levels = 1:100))

cols <- colors[as.factor(x2)]
alphas <- alpha[as.factor(na_table)]

plot(y ~ x1, data = df, 
     col = alpha(cols,alphas),
     pch = 16
     )
legend("bottomright", pch = 1, col = colors,
       legend = levels(as.factor(x2)), title = "x2",
       bty = 'n')

legend("topleft", pch = 15, col = alpha(1,alpha),
       bty = 'n', legend = c("complete", "partial", "missing"),
       border = 'black')
########################################################################################################
# Workflow: 
# 1. sample from FCS-based imputation distribution
#### From imputation distribution, pick out 5 options for each missing datapoint
imp <-mice(df_miss, m = 5)

# You can plot it out: 
# 0 shows the full data without any imputations
# 1-5 show the distribution with the imputed values
# Notice: only x1 and x2 have red in their plots. y did not have any missing values
# so it does not have any imputed values in the 1-5 reps.
stripplot(imp)

# 2. Fit model to each complete set of values (m=5 in this case), then pool the results
fit_mi <- with(imp, lm(y~x1+x2)) # applies lm() function to each of the imputations
pool_mi <- pool(fit_mi) # and then you can pool them

# Can see the average lm() summary from the pooled_mi
summary_mi <- summary(pool_mi)

summary_mi # imputation method
summary(lm(y~x1+x2, data = df_miss)) # deletion method (not using imputation)

# comparing the two summaries in the two lines above, you see that the imputation method
# takes in more data points which actually *decreases* the standard error compared to the deletion method
# since the deletion method has less data points.

# To get imputation method CI: We just do x +/- z_(1-a)/2 * se as a normal distr. CI
CI <- data.frame('lwr' = summary_mi$estimate + qnorm(0.0125) * summary_mi$std.error,
                 'upr' = summary_mi$estimate + qnorm(0.975) * summary_mi$std.error)
CI # smaller (slightly) and center/fit is closer to true value

# Compare this with the deletion method CI
confint(lm(y~x1+x2, data = df_miss)) # larger and center/fit is further from true values