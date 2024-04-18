# Topic 1: Interaction effects: treat same as linear regression representation even though we are using factor levels now

# Topic 2: Factorial v One at a Time design:
# We prefer factorial design for 3 main reasons: orthogonality, efficiency, and ability to detect interaction terms

# Topic 3: As usual, we do a model assumptions check at the end: 
# 1. homoskedastic residuals
# 2. normal residuals
# 3. additive or is there evidence of interaction terms? (Tukey Test of Additivity)

# Topic 4: Hierarchy of coefficients
########################################################################################################################################################################

# Many methods to coding out the factor levels:


# ONE WAY: code -1 for low and 1 for high
# e.g. A=-1, B=-1 means bottom left corner of the square/low in A and B with a value of 20.
dat <- data.frame(A = c(-1, 1, -1, 1), B = c(-1, -1, 1, 1), y = c(20, 40, 30, 52))
dat$AB <- dat$A * dat$B
dat$AB
## [1] 1 -1 -1 1
coef(aov(y ~ A + B + AB, data = dat))
## (Intercept) A B AB
## 35.5 10.5 5.5 0.5


# ALTERNATIVELY (DEFAULT IN R): can code 0 for low 1 for high
dat <- data.frame(A = c(0, 1, 0, 1), B = c(0, 0, 1, 1), y = c(20, 40, 30, 52))
dat$AB <- dat$A * dat$B
dat$AB
## [1] 0 0 0 1
coef(aov(y ~ A + B + AB, data = dat))
## (Intercept) A B AB
## 20 20 10 2

########################################################################################################################

# Factorial design v. One-factor-at-a-time design
# Factorial design: includes all combinations of each level
# OAAT design: Have a baseline and change one factor at a time: e.g. A-B- (baseline), A-B+, A+B-
#              Missing some combinations.

# Both have the smae number of total observations


dat_Ftrl <- data.frame(A = rep(c("Low", "High", "Low", "High"), 3),  # 4*3 = 12
                       B = rep(c("Low", "Low", "High", "High"), 3))

dat_OaaT <- data.frame(A = rep(c("Low", "Low", "High"), 4),         # 3*4 = 12
                       B = rep(c("High", "Low", "Low"), 4))

# create data
set.seed(2024)
tau <- c(Low = -1, High = 1)
theta <- c(Low = 0, High = 1.5)
sigma <- 1
epsilon <- rnorm(12, sd = sigma)
dat_Ftrl$y <- tau[dat_Ftrl$A] + theta[dat_Ftrl$B] + epsilon
dat_OaaT$y <- tau[dat_OaaT$A] + theta[dat_OaaT$B] + epsilon
head(dat_OaaT)

# factorial design: orthogonality (order doesn't matter)
anova(aov(y ~ A + B, dat = dat_Ftrl)) # order doesn't matter
anova(aov(y ~ B + A, dat = dat_Ftrl))

# One at a time design: missing orthogonality (order matters)
anova(aov(y ~ A + B, dat = dat_OaaT)) # order does matter
anova(aov(y ~ B + A, dat = dat_OaaT))

# Simulate this 1000 times. We see that the effect size of factorial design is higher.
# We see lower p-values for both A and B in the factorial design
p_values <- sapply(1:1000, function(rep){
  epsilon <- rnorm(12, sd = sigma)
  dat_Ftrl$y <- tau[dat_Ftrl$A] + theta[dat_Ftrl$B] + epsilon
  dat_OaaT$y <- tau[dat_OaaT$A] + theta[dat_OaaT$B] + epsilon
  cbind(Ftrl = anova(aov(y ~ A + B, dat = dat_Ftrl))[c("A", "B"), "Pr(>F)"],
        OaaT = c(anova(aov(y ~ B + A, dat = dat_OaaT))["A", "Pr(>F)"],
                 anova(aov(y ~ A + B, dat = dat_OaaT))["B", "Pr(>F)"]))
})
mean_p_values <- matrix(rowMeans(p_values), 2, 2)
colnames(mean_p_values) <- c("Ftrl", "Oaat")
rownames(mean_p_values) <- c("A", "B")
mean_p_values
## Ftrl Oaat
## A 0.02671223 0.05991287
## B 0.08409549 0.14590490


# Greatest disadvantage of One at a time: cannot detect interactions

# create interaction term
tau
## Low High
## -1 1
theta
## Low High
## 0.0 1.5
tau_theta <- c("HighHigh" = 2)

# perform analysis
dat_Ftrl$y <- tau[dat_Ftrl$A] + theta[dat_Ftrl$B] +
  tau_theta * (dat_Ftrl$A == "High" & dat_Ftrl$B == "High") + epsilon
dat_OaaT$y <- tau[dat_OaaT$A] + theta[dat_OaaT$B] +
  tau_theta * (dat_OaaT$A == "High" & dat_OaaT$B == "High") + epsilon
Ftrl_aov <- aov(y ~ A * B, dat = dat_Ftrl)
OaaT_aov <- aov(y ~ A * B, dat = dat_OaaT)
coef(Ftrl_aov)
## (Intercept) ALow BLow ALow:BLow
## 4.543002 -4.458085 -3.329762 2.549909
coef(OaaT_aov)
## (Intercept) ALow BLow
## 2.695740 -2.150143 -1.588633


predictions <- expand.grid(A = c("Low", "High"), B = c("Low", "High"))
predictions$Ftrl <- predict(Ftrl_aov, predictions)
predictions$OaaT <- predict(OaaT_aov, predictions)
## Warning in predict.lm(OaaT_aov, predictions): prediction from rank-deficient
## fit; attr(*, "non-estim") has doubtful cases
predictions$true <- tau[predictions$A] + theta[predictions$B] +
  tau_theta * (predictions$A == "High" & predictions$B == "High")

# The warning shows that it's not doing a great fit (this is bc it's not accounting for interactions that exist!)

###################################################################################################

# Battery example

battery <- read.csv("./Datasets/battery.csv")
battery_aov <- aov(life ~ as.factor(material) * as.factor(temp), data = battery)
anova(battery_aov)

# Tukey's HSD: we can group levels within a factor using Tukey HSD
# e.g. let's look at factor A at factor B = 70 degrees.
# It appears in rows 2 and 3 that level 2 and level 3 are significantly positively different from level 1.
# Thus, we can probably group (1) & (2 and 3).
TukeyHSD(battery_aov, which = "as.factor(material):as.factor(temp)")

## Tukey multiple comparisons of means
## 95% family-wise confidence level
##
## $`as.factor(material):as.factor(temp)`
##                 diff     lwr         upr     p adj
## 3:125-3:15    -58.50 -120.323184 3.323184 0.0742711
## 2:70-1:70     62.50 0.676816 124.323184 0.0460388
## 3:70-1:70     88.50 26.676816 150.323184 0.0014173
## 1:125-1:70    0.25 -61.573184 62.073184 1.0000000
## 2:125-1:70    -7.75 -69.573184 54.073184 0.9999614
## 3:125-1:70    28.25 -33.573184 90.073184 0.8281938
## 3:70-2:70     26.00 -35.823184 87.823184 0.8822881
## 1:125-2:70    -62.25 -124.073184 -0.426816 0.0474675

# Model assumptions checking: looks great
layout(matrix(1:6, 2, 3)); par(mar = c(4, 4, 2.5, 1.5))
plot(battery_aov)
plot(battery$material, rstudent(battery_aov))
plot(battery$temp, rstudent(battery_aov))

# to visualize interacting
par(mar = c(4, 4, 0.5, 0.5))
interaction_plot(battery_aov, lwd = 2, cex = 1.5, ylim = range(battery$life))
points(life ~ jitter(material, 0.5), data = battery, pch = (1:3)[as.factor(temp)],
       col = RColorBrewer::brewer.pal(3, "Dark2")[as.factor(temp)])
###############################################################################################
### Tukey's Test of (Non-) Additivity
# The proper test for checking for interaction term(s)
# Adding an extra term into the anova: 
######extra term:  q = y_i^2 
######test: anova(y ~ x1 + x2 + q, data) ; when p-value for q is lower than alpha, there is an interaction.
impurity <- data.frame(pressure = rep(c(25, 30, 35, 40, 45), 3),
                       temp = rep(c(100, 125, 150), rep(5, 3)),
                       impurity = c(5, 4, 6, 3, 5,
                                    3, 1, 4, 2, 3,
                                    1, 1, 3, 1, 2))
impurity_add <- aov(impurity ~ as.factor(pressure) + as.factor(temp), data = impurity)
impurity$q <- fitted(impurity_add)^2
impurity_tukey_add <- aov(impurity ~ as.factor(pressure) + as.factor(temp) + q,
                          data = impurity)
anova(impurity_tukey_add)
# Analysis of Variance Table
# 
# Response: impurity
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# as.factor(pressure)  4 11.6000  2.9000 10.6759 0.0042006 ** 
#   as.factor(temp)      2 23.3333 11.6667 42.9491 0.0001174 ***
#   q                    1  0.0985  0.0985  0.3627 0.5660026      ### THIS MEANS NO INTERACTION
# Residuals            7  1.9015  0.2716                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

################################################################################################################
# Power Analysis

power_sim <- function(n, sigma, design, formula, coef, alpha = 0.05, nsim = 2000){
  # n: sample size
  # sigma: standard dev
  # design: ?
  # formula: formula for anova test
  # coef: coefficient values of regression
  dat <- data.frame(lapply(design, rep, times = n))
  X <- model.matrix(formula, data = dat)
  N <- nrow(X)
  if(ncol(X) != length(coef)) stop("Mismatch between formula and coef length.")
  expected <- X %*% coef
  ## one fit to extract names of factors based on formula
  dat$y <- expected + rnorm(N, sd = sigma)
  effect_names <- rownames(anova(aov(update(formula, y ~ .), data = dat)))
  power <- rowMeans(sapply(1:nsim, function(sim){
    dat$y <- expected + rnorm(N, sd = sigma)
    AOV <- anova(aov(update(formula, y ~ .), data = dat))
    AOV[1:(nrow(AOV) - 1), "Pr(>F)"] < alpha
  }))
  names(power) <- effect_names[1:(length(effect_names) - 1)]
  power
}

a <- b <- 3 # 2 factors, 3 levels each
sigma <- 25
alpha <- 0.05
design <- expand.grid(A = as.factor(1:a), B = as.factor(1:b)) # expand.grid creates a complete factorial 
                                                            # design (all combinations of factors appear)
design
#   A B
# 1 1 1
# 2 2 1
# 3 3 1
# 4 1 2
# 5 2 2
# 6 3 2
# 7 1 3
# 8 2 3
# 9 3 3
formula <- ~ A * B
X <- model.matrix(formula, data = design)
head(X, 4)
# (Intercept) A2 A3 B2 B3 A2:B2 A3:B2 A2:B3 A3:B3
# 1           1  0  0  0  0     0     0     0     0
# 2           1  1  0  0  0     0     0     0     0
# 3           1  0  1  0  0     0     0     0     0
# 4           1  0  0  1  0     0     0     0     0

coef <- rep(0, 1 + (a - 1) + (b - 1) + (a - 1)*(b - 1)) # how many coefficients? 1 for intercept,
                                                        # a-1 for a factor, a-1 for b factor, 
                                                        # and (a-1)(b-1) for possible interactors.
names(coef) <- colnames(X)
coef[2:3] <- c(20, 40) # we set two main effects for the coefficients
                       # We are basically asking: "if these are the two main effects,
                       # how much power do we have to detect that these two are different from 0?
coef
## (Intercept) A2 A3 B2 B3 A2:B2
## 0 20 40 0 0 0
## A3:B2 A2:B3 A3:B3
## 0 0 0
sapply(c("2" = 2, "3" = 3, "4" = 4), power_sim, sigma = sigma, design = design,
       formula = formula, coef = coef, nsim = 2000)
## 2 3 4
## A 0.5370 0.7975 0.9235
## B 0.0455 0.0535 0.0475
## A:B 0.0495 0.0505 0.0435

#######################################################################################################
# Hierarchy of coefficients
soda <- read.csv("./STAT571B/Datasets/soda.csv")
soda$carbonation <- as.factor(soda$carbonation)
soda$pressure <- as.factor(soda$pressure)
soda$speed <- as.factor(soda$speed)

soda_aov <- aov(deviation ~ carbonation * pressure * speed, data = soda)
summary(soda_aov)
##                           Df Sum Sq Mean Sq F value   Pr(>F)
## carbonation                2 252.75  126.38 178.412 1.19e-09 ***
## pressure                   1  45.38   45.38  64.059 3.74e-06 ***
## speed                      1  22.04   22.04  31.118  0.00012 ***
## carbonation:pressure       2   5.25    2.63  3.706   0.05581 .
## carbonation:speed          2   0.58    0.29  0.412   0.67149
## pressure:speed             1   1.04    1.04  1.471   0.24859
## carbonation:pressure:speed 2   1.08    0.54  0.765   0.48687
## Residuals 12 8.50 0.71
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

soda_aov2 <- aov(deviation ~ carbonation * pressure + carbonation * speed +
                   pressure * speed, data = soda)
summary(soda_aov2)
##                      Df Sum Sq Mean Sq F value   Pr(>F)
## carbonation           2 252.75  126.38 184.617 8.68e-11 ***
## pressure              1  45.38   45.38  66.287 1.12e-06 ***
## speed                 1  22.04   22.04  32.200 5.74e-05 ***
## carbonation:pressure  2  5.25     2.63   3.835    0.047 *
## carbonation:speed     2  0.58     0.29   0.426    0.661
## pressure:speed        1  1.04     1.04   1.522    0.238
## Residuals            14  9.58     0.68
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

soda_aov3 <- aov(deviation ~ carbonation * pressure + pressure * speed, data = soda)
summary(soda_aov3)
##                     Df Sum Sq Mean Sq F value   Pr(>F)
## carbonation          2 252.75  126.38 198.885 5.00e-12 ***
## pressure             1  45.38   45.38  71.410 2.71e-07 ***
## speed                1  22.04   22.04  34.689 2.28e-05 ***
## carbonation:pressure 2   5.25    2.63   4.131   0.0358 *
## pressure:speed       1   1.04    1.04   1.639   0.2187
## Residuals           16  10.17    0.64
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

soda_aov4 <- aov(deviation ~ carbonation * pressure + speed, data = soda)
summary(soda_aov4)
##                       Df Sum Sq Mean Sq F value   Pr(>F)
## carbonation            2 252.75  126.38 191.677 2.18e-12 ***
## pressure               1  45.38   45.38  68.822 2.22e-07 ***
## speed                  1  22.04   22.04  33.431 2.21e-05 ***
## carbonation:pressure   2   5.25    2.63   3.981   0.0382 *
## Residuals             17  11.21    0.66
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1



# test for interactions
summary(lm(soda_aov4)) # shows likely interactions

# visualize difference in predictions
par(mar = c(4, 4, 1, 1))
plot(fitted(soda_aov), fitted(soda_aov4))
abline(0, 1, lty = 2, col = "gray") # with and without interaction terms seem to produce similar predictions

# visualize interactions
interaction.plot(soda$carbonation,
                 soda$pressure,
                 soda$deviation, fun = mean)
