setwd("C:/Users/amyfa/OneDrive - University of Arizona/Desktop/STAT571A")

rm(list = ls())
ripa_2021 <- read.csv("./Datasets/ripa_summer_2021.csv")
names(ripa_2021)
dim(ripa_2021) # There are 70k observations

# We take a random small subset for clearer visualization
set.seed(1234)
subset <- sample(1:nrow(ripa_2021), 3e2)
ripa_sub <- ripa_2021[subset,]

# Look at the trends across different predictors with SLR
# for the response variable that measures "stop duration"
# We notice evidence of nonlinearity and address with a log transform.
# This transform helps with the perceived_age predictor, but only
# somewhat with the exp_years predictor
layout(matrix(1:4,2,2))
par(mar = c(4,4,1,1))
plot(stopduration ~ exp_years,
     data = ripa_sub)
plot(log(stopduration) ~ exp_years,
     data = ripa_sub)
plot(stopduration ~ perceived_age,
     data = ripa_sub)
plot(log(stopduration) ~ perceived_age,
     data = ripa_sub)

# We can code formulas for linear regression in multiple ways:
# lm(Y ~ ., data) regresses on all variables and intercept
# lm(Y ~ . -x2, data) all variables except x2
# lm(Y ~ . -1, data) removes the intercept (default is included)
# lm(ripa_formula, data) can also store formulas as variable
ripa_formula <- log(stopduration) ~ exp_years + isschool + isstudent +
  perceived_limited_english + perceived_age +
  perceived_gender + perceived_lgbt

# let's see how R treats categorical/binary coefficients
# We can see that it uses dummy coding. One factor level is 
# considered the "reference" and the coefficients for the other
# levels are just added on. 
# i.e. genders = female, male, trans boy, trans girl
# Then, the coeffiecents are: reference b0 (female), b0 + b1 (boy)
# b0 + b2 (trans boy), b0 + b3 (trans girl)
X <- model.matrix(ripa_formula, data = ripa_2021)
X[1,]
# We see the reference is the intercept, so the first observation is a female
# Notice the reference category for perceivedlgbt is "No" since "yes" is coded with 1.

# Now we try to fit it
fit_multi <- lm(ripa_formula, data = ripa_2021)
summary(fit_multi)
##############################################################################################################
# This part looks at ANOVA (extra sums of squares): Type 1 and Type 3 Testing
# Type 1 testing: When you have an order for increasingly likely/impactful predictors
# Type 2 testing: Not sure the order


# T-testing with summary()
# For categorical variables, there is no way to compile the p-values of all the
# different levels of a single categorical predictor into 1 p-value 
# and decide whether to keep it. THIS IS WHY WE USE EXTRA SUMS OF (Type 1 or 3 shown next).
summary(fit_multi)


# Type 1 (ANOVA): Order matters! 
# Notice these identical models have two different p-vals
# This is b/c we test the model with 1) just b1 and then 2) b2 given b1
anova(lm(log(stopduration) ~ exp_years + isschool, data = ripa_2021))
anova(lm(log(stopduration) ~ isschool + exp_years, data = ripa_2021))

# Type 3 (ANOVA): No order difference
# This is b/c we test the model with 1) b1 given b2 2) b2 given b1
car::Anova(lm(log(stopduration) ~ exp_years + isschool, data = ripa_2021), type = "III")

###############################################################################################################
# 1) Standardizing across all Xi predictors to make Xi values comparable. 
# Scaling by 1/sqrt(n-1) to change allow X'X = rxx
Xstar <- scale(X[, -1]) /
  sqrt(nrow(ripa_2021) - 1)
Ystar <- scale(log(ripa_2021$stopduration)) /
  sqrt(nrow(ripa_2021) - 1)
fit_std_book <- lm(Ystar ~ Xstar)
coef(fit_std_book)

# 2) Standardizing each coefficient Bi but leaving Y_i alone. 
X_alt <- scale(X[, -1])
fit_std <- lm(log(stopduration) ~ X_alt,
              data = ripa_2021)
coef(fit_std)

# Difference between 1) and 2）is a scale of s_Y (ignore the intercept term)
sd(log(ripa_2021$stopduration))
coef(fit_std) / coef(fit_std_book)
