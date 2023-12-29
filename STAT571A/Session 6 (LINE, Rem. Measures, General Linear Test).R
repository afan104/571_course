rm(list = ls())

# The goal of diagnostics is to identify potential outliers.
set.seed(2023)
n <- 100

# IMPORTANT: For traditional linear regression, we consider the predictor X to be fixed.
# There are NO DISTRIBUTIONAL ASSUMPTIONS about the predictor variable. 
# To show that it really doesn't matter the distribution of the predictor X,
# We mix a couple normal distributions and toss in 3 more values.
X <- c(rnorm(n - 3, mean = c(-2,3), sd = c(1,2)), 11, 12, 13)

# Plot these predictors. Recall, it really doesn't matter the distribution for this.
# For nice visualization, set break points as 20. You are dividing/breaking the data 20 times to place into bins 
hist(X, breaks = 20)

# Now let's define Y. This looks linear B_0 = 0, B_1 = 2, epsilon term has a sd of 2
Y <- 0 + 2 * X + rnorm(n, sd = 2)
hist(Y, breaks = 20) 


# Plot the relationship of X against Y. Note the 3 values that are very far from the mean. 
# These values have high leverage.
plot(x=X,y=Y)


# Let's fit it with slr and look at the diagnostics.
# You can plot diagnostic plots from the fitted object directly with plot(fit)
# The diagnostic plots reveal that there are no issues. Cook's distance shows potential outliers 
# from a couple points with high leverage, but there is no violation of normality even with these present.
fit <- lm(Y~X)
confint(fit)
layout(matrix(1:4, 2,2))
par(mar = c(4,4,2,1))
plot(fit, 1:4)

###################################################################################################################################
# 4 most important diagnostics for simple linear regression are mneumonitized as LINE: 
# Linearity, Independence of residuals, Normally distributed residuals, Equal variance

# We can investigate the Linearity assumption in 3 standard ways: 
# QQ plots, residuals v. predictor variable, residuals v. fitted values

# For all of the above, there is a mechanical way of plotting by extracting the
# residuals data, and there is direct way of plotting using the fitted object.
# Both will be shown. But, using the fitted object method gives more information 
# which is better.

# We return to the trees data
trees <- read.csv("./Datasets/Tree_Equity_Scores_Tucson_noNA.csv")
fit_trees <- lm(HeatSeverity ~ PCTTreeCover, data = trees)

# Let's look at QQ plots first: QQ plots are normal probability plots. The more residuals stray from the line, the less
# likely those values are normally distributed. 

# For visualization purposes, we can look at an ideal QQ plot where normality is definitely true
# To plot a QQ plot, we can use qqnorm() on the residuals, and plot the normal line using
# qqline() on the residuals
layout(matrix(1:3, 1, 3))
idealized_residuals <- rnorm(nrow(trees))
qqnorm(idealized_residuals, main = "Idealized")
qqline(idealized_residuals, lty = 2)

# We repeat this with the actual trees data. Here, we find possibly a heavy lower tail
# This mean we may be systematically underestimating for lower predictor values
qqnorm(fit_trees$residuals, main = 'Heat Severity')
qqline(fit_trees$residuals, lty = 2)

# Alternative method through fitted object.
# This method also identifies the predictor values of possible outliers at the edges.
plot(fit_trees, 2, main = "Heat Severity")

# Diagnostics for assessing Linearity, Normally distributed Residuals, Equal variance 
# can be done with Residuals v. Fitted plot and Residuals v. Predictor plot.
layout(matrix(1:3, 1, 3))

# residual v. predictor plot
# Residuals appear normally distributed with equal variance across predictor levels
# Also appears linear
plot(trees$PCTPoverty, fit_trees$residuals, main = "Heat Severity",
     ylab = "residual", xlab = "predictor value")

# residual v. fitted plot
# While the residuals appear linear for a range of fitted values, 
# and the residuals appear normally distributed,
# There appears to possibly be lower variance for smaller fitted values
plot(fit_trees$fitted.values, fit_trees$residuals, main = "Heat Severity",
     ylab = "residual", xlab = "fitted value")

# Alternative method through accessing object
# Gives more information with nice trend line
plot(fit_trees,1)

# Biggest concern: Diagnostics for Independence of residuals w.r.t. time or space
# Difficult to visualize. Can possibly visualize against temporal/spatial indices

dev.off()

################################################################################################
# Sample residuals ei have nonconstant variance
# Residuals in the near the mean have the highest sampling variance

################################################################################################
# General linear tests
# These are used when there are multiple observations for each level of X. 
# The general linear test drops the linearity assumption and allows each value X_i to have
# its own mean mu_i
# Note: Simple SLR model as a special case of the general linear test

# Let's simulate predictor X and the related Y
X <- sample(1:12, size = 1e2, replace = T)
Y <- 0 + 0.5 * X + rnorm(1e2)
plot(Y~X)

# Now we fit the full model (general linear test) and the simple SLR
# The general linear test groups each level of X 
fit_full <- lm(Y~as.factor(X))
fit_lm <- lm(Y~X)

# We see how the general linear test treats the data
# It finds the mean of each level and no longer treats Y as linearly increasing
# Rather, it treats each level of X independently and as having it's own mean.
# Much more complicated, but SLR is the specific case where all of the means fall on a line
segments(x0 = 1:12 - 0.4, x1 = 1:12 + 0.4,
         y0 = predict(fit_full,
                      newdata = data.frame(X = 1:12)),
         lwd = 3, col = "darkred")

# This is the SLR fit. It assumes that the X's are fixed and Y is increasing linearly. 
# Special case of general linear test.
abline(fit_lm, lwd = 2, col = "darkgreen")


# Now let's compare to see if one is better than the other
# Nope. Very similar as F score is pretty high. There is indication of a linear trend.
anova(fit_lm, fit_full)

# Now let's try with nonlinear data
# The general linear test can deal well with nonlinear trends
# Since it looks at each level of X as independent. 
# We use quadratic simulated data
X <- sample(1:12, size = 1e2, replace = T)
Y <- 0.2 * (X - 5)^2 + rnorm(1e2)

# plot
plot(X, Y)

# Again, fit with both general linear test and simple SLR
fit_full <- lm(Y ~ as.factor(X))
fit_lm <- lm(Y ~ X)

# Let's see how they do visually in a plot
# Clearly the general linear test will perform better
segments(x0 = 1:12 - 0.4, x1 = 1:12 + 0.4,
         y0 = predict(fit_full,
                      newdata = data.frame(X = 1:12)),
         lwd = 3, col = "darkred")
abline(fit_lm, lwd = 2, col = "darkgreen")

# And you see this comparison formally with ANOVA
anova(fit_full, fit_lm)

########################################################################################
# Let's try the general linear test with the trees data
# We can artificially create repeated X values by binning similar values
# and fitting with as.factor()

# For the trees data, we can bin by rounding down X values 
fit_full <- lm(HeatSeverity ~ as.factor(floor(PCTTreeCover)), data = trees)

fit_trees <- lm(HeatSeverity ~ floor(PCTTreeCover), data = trees)

# Visualize both test fits on a plot
# Both look decent and not too different from each other
plot(HeatSeverity ~ PCTTreeCover, data = trees)
segments(x0 = floor(trees$PCTTreeCover) - 0.4,
         x1 = floor(trees$PCTTreeCover) + 0.4,
         y0 = predict(fit_full),
         col = "darkred", lwd = 3)
abline(fit_trees, lwd = 2, col = "darkgreen")

# And agrees with formal comparison with ANOVA
anova(fit_full, fit_trees)

# When applied to clearly nonlinear data, the benefits are apparent.
X <- sample(1:12, size = 1e2, replace = T) # repeats sampling integers 1-12 only
epsilon <- rnorm(1e2) # normally distributed epsilon; default mu = 0, sd = 1
Y <- 0.2 * (X - 5)^2  + epsilon
plot(X,Y)
fit_full <- lm(Y ~ as.factor(X))
fit_lm <- lm(Y ~ X)
segments(x0 = 1:12 - 0.4, x1 = 1:12 + 0.4,
         y0 = predict(fit_full, 
                      newdata = data.frame(X = 1:12)),
         lwd = 3, col = "darkred")

abline(fit_lm, lwd = 2, col = "darkgreen")

# And agrees with ANOVA
anova(fit_lm, fit_full)