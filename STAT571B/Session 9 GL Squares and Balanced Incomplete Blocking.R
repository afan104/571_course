# Going over 
# 1. Graeco Latin Squares (expanding to 3-blocks) and 
# 2. Balanced Incomplete Block Designs which lose their orthogonality
# so ORDER MATTERS!!! Need to put treatment last to account for blocks beforehand.
# 3. Type 1 & 3 ANOVA: 1 (standard/default) and 3 (treats every variable as LAST VARIABLE)
# i.e. pvalues for Type 3 reflect the amount of var explained after every other var has been accounted for
# summary(aov) or anova(aov) produce the same results for type 1 ANOVA
############################################################################################################
# Graeco Latin Square Design - adding a 3rd blocking variable
### Latin squares: 2 blocking variables

# Graeco-Latin squares maintain orthogonality like Latin squares do:
# SST = SS_treatment + SS_block1 + SS_block2 + SS_block3 + SSE


# How does adding 3 blocking variable change the analysis?
### 1. SS stay the same -> due to orthogonality of predictors
### 2. F stat and p-value change -> different number of residual df; F distr changes
rocket <- read.csv("./Datasets/rocket.csv")
head(rocket)

rocket_aov <- aov(burn - 25 ~ propellant + as.factor(batch) +
                       as.factor(operator), data = rocket)

rocket_aov_GL <- aov(burn - 25 ~ propellant + as.factor(batch) +
                       as.factor(operator) + assembly, data = rocket)
summary(rocket_aov)
summary(rocket_aov_GL)

############################################################################################################

# Balanced incomplete block designs
# Description: Because these are incomplete, the residuals do not break down orthogonally.
# Now, the order in which you set the predictors matters: the first one has the best chance 
# at explaining variation and the later predictors explain additional variation)
catalyst <- read.csv("./Datasets/catalyst.csv")
head(catalyst)

summary(aov(time ~ as.factor(batch) + as.factor(treatment), data = catalyst)) # THIS ONE
summary(aov(time ~ as.factor(treatment) + as.factor(batch), data = catalyst))

# one at a time
# Notice the mean squared errors are the same: 
summary(aov(time ~ as.factor(batch), data = catalyst))
summary(aov(time ~ as.factor(treatment), data = catalyst))

# We want to use the first summary(aov) model since we want to look at the "effect of the treatment"
# "while accounting for batch". Therefore, we look at the pvalue for treatment after batch has first been considered.
# without doing it in this order, we would just be running a 1-variable anova (same MSE)
catalyst_aov <- aov(time ~ as.factor(batch) + as.factor(treatment), data = catalyst)
wrong_catalyst_aov <- aov(time ~ as.factor(treatment) + as.factor(batch), data = catalyst) # wrong order for comparing with correct one

layout(matrix(1:2, 1, 2)); par(mar= c(2,2,2,1))
catalyst_HSD <- TukeyHSD(catalyst_aov, which = "as.factor(treatment)")
wrong_catalyst_HSD <- TukeyHSD(wrong_catalyst_aov, which = "as.factor(treatment)")

plot(catalyst_HSD)
plot(wrong_catalyst_HSD)

############################################################################################################

anova(catalyst_aov) # Type 1
#summary(catalyst_aov) # same thing as above

car::Anova(catalyst_aov, type = "III") # Type 3

