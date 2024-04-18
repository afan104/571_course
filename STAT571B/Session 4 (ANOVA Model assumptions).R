# Model assumptions for ANOVA
setwd("~//571_course//STAT571B")
etching <- read.csv(".//Datasets//etching.csv")
etching_aov <- aov(rate ~ as.factor(power), data = etching)

# 1. Normal residuals 
qqnorm(resid(etching_aov))
qqline(resid(etching_aov))

# 2. Constant variance
plot(etching_aov, which = 1)

# 3. Pairwise independent (all groups); usually use both bartlett and levene test
# Bartlett test: requires normality assumption (more power)
# Levene test: no assumptions (less power)
bartlett.test(rate ~ as.factor(power), data = etching)

library(car)
leveneTest(rate ~ as.factor(power), data = etching)

# Remedial measures for 3.
### PART 1: Transform response 
# Usually plot log(y_bar_i) against log(S_i) the stds of each group to determine best transform
# y* = y^(1-slope)

sds <- aggregate(rate ~ power, data = etching, FUN = sd)
means <- aggregate(rate ~ power, data = etching, FUN = mean)

# you more or less see a visual correction with the plotting
# fit to linear regression i guess to find slope
plot(log(means$rate), log(sds$rate), xlab = "log-mean", ylab = "log-SD")

### PART 2: Do non-parametric inference (relaxes assumptions due to less power)
# Kruskal-Wallis (no normality assumption; uses ranks instead of raw obs)

kruskal.test(rate ~ as.factor(power), data = etching)