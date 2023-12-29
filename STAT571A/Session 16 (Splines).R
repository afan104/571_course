rm(list = ls())
# load in libraries and data
library(mgcv,nmle)
trees <- read.csv("../Datasets/Tree_Equity_Scores_Tucson_noNA.csv")
trees_subset <- trees[-which(trees$PCTSenior == 1 | trees$PCTSenior==0),]  # remove 1's and 0's for logit
trees_subset <- trees_subset[-which(trees_subset$PCTSenior == min(trees_subset$PCTSenior)),] # remove random low value

# functions
logit <- function(x) log( x / (1 - x) )
expit <- function(x) exp(x)/(1+exp(x))

pred_WH_GAM <- function(object.gam, newdata, level=0.95) {
  n <- nrow(object.gam$model)
  p <- object.gam$rank
  fit <- predict(object.gam, newdata,
                 se.fit = T)
  alpha <- 1 - level
  W_alpha <- sqrt(p * qf(level, p, n - p))
  ME <- fit$se.fit * W_alpha
  upr <- fit$fit + ME
  lwr <- fit$fit - ME
  return(cbind(fit = fit$fit, lwr, upr))
}

####################################################################################################################
# fitting GAM for a nonlinear predictor.
# To account for nonlinear predictors, we must fit a GAM
fit_gam <- gam(logit(PCTSenior/100) ~ 
                 s(PCTChildren, k=7),
               data = trees_subset)

# plot predictions using WH ci
newdata <- data.frame(PCTChildren=0:max(trees_subset$PCTChildren))
plot(logit(PCTSenior/100) ~ PCTChildren, data = trees_subset)
matlines(x=newdata, y=pred_WH_GAM(fit_gam, newdata), lty = c(1, 2, 2), col = c(1,2,2), lwd = c(2, 1, 1))

####################################################################################################################
# Let's try fitting with multiple predictors: One nonlinear predictor + linear predictor
# We account for the nonlinear predictor with the GAM and the linear predictor can simply be added on (as normal mlr)
fit_gam2 <- gam(logit(PCTSenior/100) ~ s(PCTChildren, k = 7) + HeatSeverity,
                data = trees_subset)

summary(fit_gam2)

# The summary output splits the parametric (linear) coefficients and the nonparametric (nonlinear) coefficients
# The linear coefficients are shown like usual.
# The nonlinear coefficients are shown at the bottom. 
# Instead of showing you all 7 basis functions, its wrapped up into one overall description of the predictor.

# edf: effective degrees of freedom - shows you how much penalty is being placed on smoothing the function
###### In this case, edf = 3.85 is much lower than 7 (the max it can be in this case), so there is a strong smoothing effect
###### and we would only need 4 bases functions.

####################################################################################################################
# Let's try 2 nonlinear predictors now
fit_gam3 <- gam(logit(PCTSenior/100) ~ s(PCTChildren, k = 7) + 
                  s(HeatSeverity, k = 7),
                data = trees_subset)

summary(fit_gam3)
# Very likely nonlinear effects for both HeatSeverity and PCTChildren.
# edf values: 3.6 and 2.6, respectively.

####################################################################################################################
# Let's now try a * very * parameterized model: k = 50 bases functions
fit_gam4 <- gam(logit(PCTSenior/100) ~ s(PCTChildren, k = 50) + 
                  s(HeatSeverity, k = 50),
                data = trees_subset)

summary(fit_gam4)

# the smoothing does pretty well for the HeatSeverity works pretty well.
# However, the PCTChildren predictor did not get penalized very well
# edf values: 38.9 and 4.0 for PCT Children and HeatSeverity, respectively

# plot it
plot(logit(PCTSenior/100) ~ PCTChildren,
     data = trees_subset,
     xlab = "percent children")
newdata <- data.frame(PCTChildren = 0:max(trees_subset$PCTChildren),
                      HeatSeverity = mean(trees_subset$HeatSeverity))
matlines(newdata$PCTChildren, pred_WH_GAM(fit_gam4, newdata), lty = c(1, 2, 2), col = c(1,2,2), lwd = c(2, 1, 1))
