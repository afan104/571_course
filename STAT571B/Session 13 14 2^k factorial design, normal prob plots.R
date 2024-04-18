# Discuss factorial design
# 1-0 coding v. 1 -1 coding (aka orthogonal coding)
# Note: for y_i = Beta_0 + Beta_A*x_iA + Beta_B*x_iB + Beta_AB*x_i*x_iB + epsilon
#       x_iA = 1_{A=+}, x_iB=1_{A=+} are indicator functions and not related to each other
#       x_iA and x_iB are NOT necessarily equal (two indicator functions)
# Single Replicate Design (when limited materials only allow 1 replicate)
# -> look at the normal prob plot of the coefficients (QQ plot of the coeffs)

################################################################################################
# 1-0 encoding
# Average effects for models with interaction terms

chemical <- read.csv("./STAT571B/Datasets/chemical.csv", stringsAsFactors = T)
head(chemical)
chemical_aov <- aov(yield ~ reactant * catalyst, data = chemical)
summary(lm(chemical_aov))
model.matrix( ~ reactant * catalyst, dat = chemical)

# average formula: average effect for A = Beta_A + 1/2 * Beta_AB
coef(chemical_aov) %*% c(0, 1, 0, 0.5)
## 8.333333
coef(chemical_aov) %*% c(0, 0, 1, 0.5)
## -5
coef(chemical_aov) %*% c(0, 0, 0, 0.5)
## 1.666667

# These values are equivalent to the additive model coeffs
coef(aov(yield ~ reactant + catalyst, data = chemical))
## (Intercept) reactant+ catalyst+
## 25.833333 8.333333 -5.000000
coef(aov(yield ~ reactant, data = chemical))
## (Intercept) reactant+
## 23.333333 8.333333
coef(aov(yield ~ catalyst, data = chemical))
## (Intercept) catalyst+
## 30 -5

# The model matrix for 1-0 encoding
levels <- factor(c("-", "+"))
df <- expand.grid(A = levels,
                  B = levels,
                  C = levels)

model.matrix(~ A * B * C, data = df)
################################################################################################
# 1 -1 encoding (aka contrast encoding)
# pros: orthogonal
# Given: for y_i = Beta_0 + Beta_A*x_iA + Beta_B*x_iB + Beta_AB*x_i*x_iB + epsilon
# Then: the average main effect for A = 2*Beta_A; B = 2*Beta_B; AB = 2*Beta_AB; 
#       extends to higher factorial designs (e.g. average main effect for ABC = 2*Beta_ABC)

# We will start doing contrast encoding
chemical <- read.csv("./STAT571B/Datasets/chemical.csv", stringsAsFactors = T) 

# Look at the model matrix
model.matrix(~ A * B * C, data = df,
             contrasts.arg = list(A = "contr.helmert",
                                  B = "contr.helmert",
                                  C = "contr.helmert"))

# use orthogonal encoding on the anova test
chemical_aov_orth <- aov(yield ~ reactant * catalyst, data = chemical,
                         contrasts = list(reactant = "contr.helmert",
                                          catalyst = "contr.helmert"))
# average effects (these match previous 1-0 coded average effects and no interaction model effects)
# Notice how average effects are MUCH easier to calculate for 1 -1 encoding when there are higher and higher levels of factors
# 1-0 average effects include all terms for average effects of interaction terms (AB requires A,B,AB; ABC requires A,B,AB,AC,ABC; etc.)
summary(lm(chemical_aov_orth))
coef(chemical_aov_orth) * 2

# visualize summary of anova
anova(chemical_aov_orth)
####################################################################################################################
# single replicate design
# since n=1, there is no residual error (perfect fit)
# with MSE=0, F statistic is infinite and the null distribution is not defined (denominator df=0)
# R will run but with a warning

# gather data and determine effects
chem2 <- read.csv("./STAT571B/Datasets/chemical2.csv")

# anova generates warning because only a single replicate 
chem2_aov <- aov(filtration ~ A * B * C * D, data = chem2)
anova(chem2_aov)

# orthogonal coding to measure average effects
chem2_aov_orth <-
  aov(filtration ~ A * B * C * D,
      data = chem2,
      contrasts = list(A = "contr.helmert",
                       B = "contr.helmert",
                       C = "contr.helmert",
                       D = "contr.helmert"))
# coef() only includes significant coefficients
coefs <- coef(chem2_aov_orth) 

# QQ plots of the coefficients
qqvalues <- qqnorm(coefs[-1] * 2, datax = T) # [-1] to remove the intercept term
qqline(coefs[-1] * 2, datax = T)
text(qqvalues$x, qqvalues$y, labels = names(qqvalues$x)) # these should match (can't do on R markdown)
sort(coefs) # these should match (can do when working on R markdown)

qqnorm(abs(coefs[-1] * 2), datax = T) # half normal QQ plot

# Method 1 to estimating error:
# "Adding a replicate" by removing a factor
# Here, B is not significant, so we remove B which allows us to have an extra replicate
chem2_aov_ACD <- aov(filtration ~ A * C * D, data = chem2)

# Now, error estimation is possible
anova(chem2_aov_ACD)

# Method 2 to estimating error:
# Only considering lower-order interaction effects

# Example: only considering 2-way interactions
chem2_aov_lower <- aov(filtration ~ A * B + A * C + A * D + B * C + B * D + C * D,
                       data = chem2)
anova(chem2_aov_lower)

# Example: only considering 3-way and lower interactions
chem2_aov_lower2 <- aov(filtration ~ A + B + C + A*B*C + A*B*D + A*C*D + B*C*D ,
                       data = chem2)
anova(chem2_aov_lower2)

# DIAGNOSTICS (look fine except for possible heteroskedasticity => use response transform)
layout(matrix(1:5, 1, 5)); par(mar = c(4, 4, 1.5, 1))
plot(chem2_aov_ACD, which = 1:2); plot(rstudent(chem2_aov_ACD) ~ as.factor(chem2$A))
plot(rstudent(chem2_aov_ACD) ~ as.factor(chem2$C))
plot(rstudent(chem2_aov_ACD) ~ as.factor(chem2$D))
