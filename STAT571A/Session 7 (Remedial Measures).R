
rm(list = ls())

# Exponential relationship with normally distributed errors

X = rnorm(125, mean = 0, sd = 1)
B0 = -.1
B1 = .8
eps = rnorm(X, sd = 1)
Y <- exp(B0 + B1*X) + eps
plot(X,Y)
curve(exp(B0+B1*x), from = min(X), to = max(X), add = TRUE)

# Exponential relationship with exponentially distributed errors

X = rnorm(125, mean = 0, sd = 1)
B0 = -.1
B1 = .8
eps = rnorm(X, sd = 1)
Y <- exp(B0 + B1*X + eps)
plot(X,Y)
curve(exp(B0+B1*x), from = min(X), to = max(X), add = TRUE)

# Log transform to get linear relationship with normally distributed errors (same as previous)
X = rnorm(125, mean = 0, sd = 1)
B0 = -.1
B1 = .8
eps = rnorm(X, sd = 1)
log_Y <- B0 + B1*X + eps
plot(X,log_Y)
curve(B0+B1*x, from = min(X), to = max(X), add = TRUE)

#################################################################################

# When variance is nonnormal, can apply log transform to stabilize variance
# The trade-off is heavier tails on the QQ 
trees <- read.csv("./Datasets/Tree_Equity_Scores_Tucson_noNA.csv")
layout(matrix(1:4, 2, 2))
par(mar = c(4,4,2,1))
names(trees)
fit_trees <- lm(HeatSeverity ~ PCTTreeCover,
                data = trees)
fit_log <- lm(log(HeatSeverity) ~ PCTTreeCover,
              data = trees)
plot(fit_trees, 1:2)
plot(fit_log, 1:2) # the residuals look less heteroskedastic
                  # QQ looks worse now

# Take out outlier (outliers: 370, 375, 308)
trees[c(370, 375, 308), c("HeatSeverity", "PCTTreeCover", "RAWTotalPop", "PopDensity")] # remove 370, 308 for low pop
layout(matrix(1:4, 2, 2))
par(mar = c(4,4,2,1))
fit_trees <- lm(HeatSeverity ~ PCTTreeCover,
                data = trees[-c(370, 308),])
fit_log <- lm(log(HeatSeverity) ~ PCTTreeCover,
              data = trees[-c(370,308),])
plot(fit_trees, 1:2)
plot(fit_log, 1:2) # still doesn't help much with QQ plot

# Box-Cox transformation
library(MASS)
bc <- boxcox(fit_trees)
bc$x[which.max(bc$y)] # to choose value with maximum likelihood

fit_bc <- lm(HeatSeverity^0.79 ~ PCTTreeCover,
             data = trees[-c(370, 308)])
layout(matrix(1:4, 2, 2))
par(mar = c(4,4,2,1))
plot(fit_trees, 1:2)
plot(fit_bc, 1:2) # doesn't help much overall

dev.off()

# Methods shown in book not very helpful here, but we can also try using omitted variables, and weighted least squares
# It seems weighted least squares would be most helpful (weights errors differently)