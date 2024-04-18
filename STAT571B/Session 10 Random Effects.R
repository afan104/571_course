# Random effects: Rather than fixed effects, random effects are considered RV coming from a distribution. 
# e.g. block 1 does not have a fixed effect of 1, it now has an effect coming from N(0, 3). Usually mean is 0.

# We use the following to determine predictors p-values under random effect assumptions
# 1. lme4::lmer() and confint() or  **tests the predictors**
# 2. lmerTest::lmer() and anova(lmer()) to get the p-value **tests the predictors**
# 3. lmerTest::ranova() to test whether the random effects are significant **tests whether random/fixed effect**
######################################################################################################################################################

# New model: yij = mu + tau_i + B_j + eps_ij where B_j ~ N(0, sigma_b^2), eps_ij ~ N(0, sigma^2)
#                fixed  fixed  random random

tools::package_dependencies("Matrix", which = "LinkingTo", reverse = TRUE)[[1L]]
install.packages("lme4", type = "source")
library(lme4)

vascular <- read.csv("./Datasets/vascular.csv")
head(vascular)

# lmer() computes the random effects, but the authors didn't include p-value
# this is bc getting the degrees of freedom for mixed models is complicated...
vascular_RE <- lmer(flicks ~ as.factor(pressure) + (1 | batch), data = vascular)
anova(vascular_RE)

# instead we can find confidence intervals to substitute p-values
confint(vascular_RE)
###                               2.5 %     97.5 %
### .sig01                    1.136322  5.5401925   # This is the sigma_b from random effect variance 
### .sigma                    1.840242  3.5643700   # This is the sigma from √sigma^2
### (Intercept)              89.726101 95.9072331
### as.factor(pressure)8700  -4.085262  1.8185951
### as.factor(pressure)8900  -6.851928 -0.9480715
### as.factor(pressure)9100 -10.001928 -4.0980715


# Alternatively, we can get the p-values using lmerTest to get degrees of freedom
# We can also try bootstrapping (not shown here)
library(lmerTest)
vascular_RE2 <- lmer(flicks ~ as.factor(pressure) + (1 | batch), data = vascular) # The (1 |) signifies a random intercept
anova(vascular_RE2)

# ranova() can be used to see if the random effects are significant
ranova(vascular_RE2)

