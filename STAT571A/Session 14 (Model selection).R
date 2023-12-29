setwd("~/amyfan/OneDrive - University of Arizona/Desktop/STAT571A")
ripa_2021 <- read.csv("../Datasets/ripa_summer_2021.csv")
ripa_formula <- log(stopduration) ~ exp_years + isschool + isstudent +
  perceived_limited_english + perceived_age +
  perceived_gender + perceived_lgbt

# model selection; picks the 2 best models (nbest) for each size (nvmax=9)
library(leaps)
best_subset <-
  regsubsets(ripa_formula, data = ripa_2021, nvmax = 9,
             nbest = 2, method = "exhaustive")
summary_best <- summary(best_subset)
plot(best_subset, scale = 'bic')

head(signif(sort(summary_best$bic), 4)) # usually a bic difference of 2 is considered significant

# can start 1.backwards/2.forwards/3.specific and moving both directions
# backwards: scope() sets lower limit
stepwise_bw <- step(lm(ripa_formula, data = ripa_2021), k = log(nrow(ripa_2021)))
sort(names(stepwise_bw$coefficients)) # sort doesn't really do anything; useful for results comparisons

# forwards: scope() sets upper limit 
stepwise_fw <- step(lm(log(stopduration) ~ 1, data = ripa_2021), k = log(nrow(ripa_2021)),
                    scope = ripa_formula, direction = "forward")
sort(names(stepwise_fw$coefficients))

#both: scope() sets both bounds; specific place determined by formula
stepwise_both <- step(lm(log(stopduration) ~ exp_years, data = ripa_2021),
                      k = log(nrow(ripa_2021)),
                      scope = list(upper = ripa_formula,
                                   lower = log(stopduration) ~ 1), direction = "both")
sort(names(stepwise_both$coefficients))

############################################################################################
# 2. Simulate normally distributed data with true linear relationship B0=B1=B2=1
X1 <- rnorm(50,-1,1)
X2 <- rnorm(50,1,1)
eps <- rnorm(50,0,.05)
B0<-1
B1<-1
B2<-1
Y <- B0 + B1 * X1 + B2 * X2 + eps
df <- data.frame(y = Y, X1 = X1, X2 = X2)
plot(df)
confint(lm(y~X1+X2, data = df))
stepwise_optimum <- step(lm(y ~ 1, data = df), k = log(nrow(df)),
                         scope = list(upper = y ~ X1 + X2, lower = y ~ 1),
                         direction = "forward")
# supposed to be failing where the "best" result is Y~1. Asking prof why this example failed.

# Can manually check the AIC of each model for comparisons
extractAIC(lm(y ~ 1, data = df), k = log(nrow(df)))
## [1]  1.00000 20.43026
extractAIC(lm(y ~ X1, data = df), k = log(nrow(df)))
## [1]  2.000000 -2.185465
extractAIC(lm(y ~ X2, data = df), k = log(nrow(df)))
## [1]  2.000000 -3.238735
extractAIC(lm(y ~ X1 + X2, data = df), k = log(nrow(df)))
## [1]    3.0000 -282.4025
extractAIC(stepwise_optimum, k = log(nrow(df)))
## [1]    3.0000 -282.4025
#######################################################################################################################################
# 3. Multicollinearity: "Correlation" is a term used for 2 R.V. predictors. 
# In this case, it's fixed predictors, so it's really "linear dependence"

library(corrplot)
X <- model.matrix(ripa_formula, data = ripa_2021)[, -1]
corrplot(cor(X), type = "lower", tl.cex = 0.65)

# We can see manually that removing either related predictors greatly increases the region of the confidence interval
ripa_formula
## log(stopduration) ~ exp_years + isschool + isstudent + perceived_limited_english +
## perceived_age + perceived_gender + perceived_lgbt
fit_ripa <- lm(ripa_formula, data = ripa_2021)
confint(fit_ripa, parm = c("isschool", "isstudent"))
## 2.5 % 97.5 %
## isschool -1.7838347 0.3270911
## isstudent 0.6982556 3.0586152
formula_school <- stopduration ~ exp_years + isschool + perceived_limited_english +
  perceived_age + perceived_gender + perceived_lgbt
confint(lm(formula_school, data = ripa_2021), parm = "isschool")
## 2.5 % 97.5 %
## isschool 4.860187 56.38179
formula_student <- stopduration ~ exp_years + isstudent + perceived_limited_english +
  perceived_age + perceived_gender + perceived_lgbt
confint(lm(formula_student, data = ripa_2021), parm = "isstudent")
## 2.5 % 97.5 %
## isstudent 13.79375 71.4021

# We can also see through vif()
library(car)
vif(fit_ripa)

# How to tackle: Reparameterization      -> generally not good to randomly remove one of the variables
# Create a new variable to "orthogonalize" the predictors
cor(ripa_2021$isschool, ripa_2021$isstudent)
## [1] 0.8944081
ripa_2021$isguest <- ripa_2021$isschool * (ripa_2021$isstudent == 0)
cor(ripa_2021$isschool, ripa_2021$isguest)
## [1] 0.4471754 This greatly reduces the dependence/correlation
formula_transform <- stopduration ~ exp_years + isschool + isguest +
  perceived_limited_english + perceived_age + perceived_gender + perceived_lgbt
fit_transform <- lm(formula_transform, data = ripa_2021)
confint(fit_transform)

#############################################################################################################################################
# 4. Ridge Regression: To visualize the best c that minimizes the estimators

X <- model.matrix(ripa_formula, data = ripa_2021)[, -1]
X_scl <- scale(X) ## scale predictors

## grid of penalty terms
cs <- (nrow(X) - 1) * exp(seq(log(1e-2), log(20), l = 3e1))
get_rr <- function(Y, X, c){ ## function to obtain RR fit
  solve(t(X) %*% X + c * diag(ncol(X))) %*% t(X) %*% Y
}
betas <- sapply(cs, function(c){ ## apply function to each c
  get_rr(Y = log(ripa_2021$stopduration), X = X_scl, c = c)
})
beta_cols <- RColorBrewer::brewer.pal(ncol(X), "Set1")
par(mar = c(4, 4.3, 1, 0.1))
matplot(cs / nrow(X), t(betas), type = "l", lty = 1, lwd = 2, col = beta_cols,
        log = "x", xlab = "c/(n - 1)", ylab = expression(betaˆR))
legend(x = 0.75e-2, y = -0.05, lty = 1, lwd = 2, col = beta_cols,
       legend = colnames(X), bty = "n", cex = 0.7)

# another way to center and scale?
X_scl <- scale(X)
X_h <- data.frame("exp_years" = 0:35, "isschool" = 0, "isstudent" = 0,
                  "preceived_limited_english" = 0, "perceived_age" = 25,
                  "perceived_genderMale" = 0,
                  "perceived_genderTransgender man/boy" = 0,
                  "perceived_genderTransgender woman/girl" = 0,
                  "perceived_lgbtYes" = 0)
X_h_scl <- t((t(X_h) - attr(X_scl, "scaled:center")) / attr(X_scl, "scaled:scale"))
X_h_scl2 <- scale(X_h, center = attr(X_scl, "scaled:center"),
                  scale = attr(X_scl, "scaled:scale"))
identical(as.numeric(X_h_scl), as.numeric(X_h_scl2))
## [1] TRUE