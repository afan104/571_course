rm(list = ls())
# 1. Create WH for MLR: 
# Method - Do single-pointwise ci -> rescale to get WH ci
# Y_hat_h +/- t(1-a/2;n-p)*s(Y_hat_h) vs. Y_hat_h +/- W*s(Y_hat_h) where W = sqrt(p*F(1-a;p;n-p))
# The resulting scale factor from PW to WH is: W/t(1-a;n-p)

pred_WH_MLR <- function(object, newdata, level = 0.95){
  n <- nrow(object$model) ## extract n from model object
  p <- object$rank
  
  # PW ci
  fit <- predict(object, newdata, interval = 'confidence') 
  ME_confidence <- fit[,'fit'] - fit[,'lwr'] 
  
  # rescale to WH ci
  alpha <- 1-level
  t_alpha <- qt(1-alpha/2, df = n-p)
  W_alpha <- sqrt(p * qf(level, n-p, p))
  
  ME <- ME_confidence*W_alpha/t_alpha
  
  upr <- fit[,'fit'] + ME
  lwr <- fit[,'fit'] - ME
  return(cbind(fit[,'fit'], lwr, upr))
}

# Polynomial regression; use poly() (gives orth. predictors) instead of sum of individuals predictors
trees <- read.csv("../Datasets/Tree_Equity_Scores_Tucson_noNA.csv")
fit_quad <- lm(PCTSenior ~ poly(HeatSeverity, 2),
               data = trees)
graph_confidence <- function(object){
  HS_grid <- seq(0, 10, l = 1e2) # x "grid" of HeatSeverity values
  newdata <- data.frame(HeatSeverity = HS_grid) 
  pred_quad <- pred_WH_MLR(object, newdata = newdata)
  
  plot(PCTSenior ~ HeatSeverity, data = trees)
  matlines(HS_grid, pred_quad, lty = c(1, 2, 2), col = 2, lwd = c(2, 1, 1))
}

graph_confidence(fit_quad)

# Higher degree poly regressions; difference - at edges!
dev.off()
par(mfrow=c(2,3))
for (i in c(0:4,10)) {
  if (i == 0){
    fit <- lm(PCTSenior ~ HeatSeverity,
                   data = trees)
  }
  else{
    fit <- lm(PCTSenior ~ poly(HeatSeverity, i),
                   data = trees)
  }
  
  graph_confidence(fit)
}
################################################################################################
# 2. Higher Order Visualization (3+ terms): colors (col), point size (cex)
dev.off()
breaks <- quantile(trees$PCTChildren, probs = seq(0,1,.25)) # break data into equal chunks
breaks
PCTChildren_cut <- cut(trees$PCTChildren,breaks) # converts value to a level (one of the chunks)
head(PCTChildren_cut)
as.numeric(head(PCTChildren_cut)) #returns level to numeric

# col: color
plot(PCTSenior ~ HeatSeverity, data = trees, pch = 16, col = PCTChildren_cut)
legend("topright", pch = 16, col = 1:4, legend = levels(PCTChildren_cut), title = "PCTChildren")

colors <- hcl.colors(4, alpha = .7, palette = "viridis") # with other colors
plot(PCTSenior ~ HeatSeverity, data = trees, pch = 16, col = colors[PCTChildren_cut])
legend("topright", pch = 16, col = colors, legend = levels(PCTChildren_cut), title = "PCTChildren")

# cex: point size
PCTChildren_scl <- #rescale: ( Xi - min(Xi) ) / ( max(Xi) - min(Xi) )
  (trees$PCTChildren - min(trees$PCTChildren)) /
  diff(range(trees$PCTChildren))

range(PCTChildren_scl) # so that range of points becomes [0,1] so it's positive

plot(PCTSenior ~ HeatSeverity, data = trees,
     cex = 3 * PCTChildren_scl + 0.5) # rescale to make visualization look good

# both cex and col doubly emphasizes the gradient of PCT quantiles, but not super necessary
# on both gradients (color and size) apply to SAME predictor, so it doesn't give any more new info
plot(PCTSenior ~ HeatSeverity, data = trees,
     pch = 16, col = colors[PCTChildren_cut],
     cex = 3 * PCTChildren_scl + 0.5)
legend("topright", pch = 16, col = colors,
       legend = levels(PCTChildren_cut),
       title = "PCTChildren")
############################################################################################################################
# 3. Interactions Among Continuous Predictors
# use * or : to indicate interaction: lm(Y ~ X1*X2, data = data), lm(Y ~ X1:X2, data = data)
# : just interaction, * all individual predictors and interaction lm(Y ~ X1+X2+X1:X2)

fit_inter <- lm(PCTSenior ~ HeatSeverity * PCTChildren, data = trees)
summary(fit_inter) # is significant, the slopes for different PCTChildren bins differ

# diagnostics
dev.off()
par(mfrow = c(2,3))
plot(trees$PCTChildren, fit_inter$residuals, col = colors[PCTChildren_cut]) # resid v. pred
plot(trees$HeatSeverity, fit_inter$residuals)                       # resid v. other pred
plot(fit_inter)
# Main Concerns: heteroskedasticity in PCTChildren predictor, nonnormality (heavy QQ tails), high leverage at 344 (Cook's)
##############################################################################################################################
# 4. a. Logit Transformation
logit <- function(x) log(x/(1-x))
trees[340, c(3, 7, 8, 11:14, 16, 17)] # look at the values for point 344
trees_subset <- trees[trees$PCTSenior > 0 & trees$NAME != "University of Arizona", ] # take subset without 0 values for logit
fit_logit <- lm(I(logit(PCTSenior/100)) ~ HeatSeverity * PCTChildren, data = trees_subset) # logit maps (0,1) to R

par(mfrow = c(2,3))
plot(trees_subset$PCTChildren, fit_logit$residuals, col = colors[PCTChildren_cut]) # resid v. pred
plot(trees_subset$HeatSeverity, fit_logit$residuals)                       # resid v. other pred
plot(fit_logit)
# Improved concerns: heteroskedasticity better, nonnormality better in upper tail, no influential points
# New concern more noticeable: trend in residuals v fitted suggests missing variable in model
summary(fit_logit) # This time, interaction term is not significant. Removing is fine, but not removing is also fine 
                  #(estimator will be close to 0 since so many points to provide accurate relationship ~0)

# plot regression
dev.off()
HS_grid <- seq(0, 10, l = 1e2) # create HeatSeverity grid (cts from 1-10)
PC_grid <- quantile(trees_subset$PCTChildren, probs = seq(0.125, 0.875, 0.25)) # create PCTChildren grid (4 values indicating diff quantiles)
newdata <- expand.grid(HeatSeverity = HS_grid, PCTChildren = PC_grid) # expands a matrix; dim = 100 x 2 (expands length 4 PCT_grid vector to length 100 to match grid_HS)
pred_logit <- matrix(predict(fit_logit, newdata), ncol = length(PC_grid)) # matrix(): now each column refers to a different PCTChildren quantile each row is a different Heat Severity
expit <- function(x) 1 / (1 + exp(-x)) # function: inverse of logit to remap onto original scale
plot(PCTSenior ~ HeatSeverity,
     data = trees_subset, pch = 16,
     col = colors[PCTChildren_cut])
matlines(HS_grid, 100 * expit(pred_logit), # remap onto original scale
         lty = 1, lwd = 2, col = colors)
legend("topright", pch = 16, col = colors,
       legend = levels(PCTChildren_cut),
       title = "PCTChildren")

# 4. b. ADD NEW VARIABLE: PCTPoverty, since residuals v. fitted had trend suggesting missing var
plot(I(logit(PCTSenior/100)) ~ PCTPoverty,
     data = trees_subset, pch = 16,
     col = colors[PCTChildren_cut],
     ylab = "logit(PCTSenior)")
legend("topright", pch = 16, col = colors,
       legend = levels(PCTChildren_cut),
       title = "PCTChildren")
# Appears to be PCTPoverty, so we can update previous model with this term and test for significance
fit_logit2 <- lm(I(logit(PCTSenior/100)) ~ HeatSeverity + PCTChildren * PCTPoverty,
                 data = trees_subset)
summary(fit_logit2) # appears significant!

# check diagnostics
par(mfrow = c(2,3))
plot(trees_subset$PCTChildren, fit_logit2$residuals, col = colors[PCTChildren_cut]) # resid v. pred
plot(trees_subset$HeatSeverity, fit_logit2$residuals)                       # resid v. other pred
plot(fit_logit2)
# improvements: residuals v fitted relationship is gone

# plot regression: original and logit
PP_grid <- 0:100
newdata <- expand.grid(HeatSeverity = median(trees_subset$HeatSeverity),
                       PCTPoverty = PP_grid, PCTChildren = PC_grid)
pred_logit2 <- matrix(predict(fit_logit2, newdata), ncol = length(PC_grid))

plot(PCTSenior ~ PCTPoverty,                
     data = trees_subset, pch = 16,            # original
     col = colors[PCTChildren_cut])
matlines(PP_grid, 100 * expit(pred_logit2), 
         lty = 1, lwd = 2, col = colors)
legend("topright", pch = 16, col = colors,
       legend = levels(PCTChildren_cut),
       title = "PCTChildren")


plot(I(logit(PCTSenior/100)) ~ PCTPoverty,                
     data = trees_subset, pch = 16,            # logit
     col = colors[PCTChildren_cut])
matlines(PP_grid, pred_logit2, 
         lty = 1, lwd = 2, col = colors)
legend("topright", pch = 16, col = colors,
       legend = levels(PCTChildren_cut),
       title = "PCTChildren")
####################################################################################################################################
# 5. Interactions with categorical predictors
ripa_2021 <- read.csv("./Datasets/ripa_summer_2021.csv")
ripa_2021$perceived_gender <- as.factor(ripa_2021$perceived_gender)
plot(log(stopduration) ~ jitter(as.numeric(perceived_gender), 2),      # jitter spread out overlapping data horizontally; the 2nd argument is for the thickness
     data = ripa_2021[order(ripa_2021$isschool), ], pch = 16,               # order() in increasing order so that isschool = 1 (the lesser factor level) is ranked last (shows up during jitter)
     col = 1 + isschool, cex = 1 + isschool, xaxt = 'n', xlab = "Perceived Gender") #xaxt just hides "h" the x axis title; plot changes slightly due to jitter() but locations are just shuffled horizontally
axis(1, 1:4, levels(ripa_2021$perceived_gender))                              # col: intercept is black, other factors are different (just one other factor level in isschool)
legend("topright", pch = 16, pt.cex = 1:2, col = 1:2, legend = c("Off campus", "On campus"))

# 5. a. Let's start with purely additive model - additive means no interaction terms
# More common parameterization: 0 factor level wrapped into intercept
summary(lm(log(stopduration) ~ perceived_gender, data = ripa_2021))

# Less common: remove intercept; each effect becomes the "pure" effect rather than an "addition" to the default 0 factor level
summary(lm(log(stopduration) ~ perceived_gender - 1, data = ripa_2021))

# 5. b. Add in interaction terms
fit_qual <- lm(log(stopduration) ~ perceived_gender * isschool, data = ripa_2021) # (4-1) x (2-1) = 3 interaction terms
summary(fit_qual)

# 5. c. Two ways to predict Y: 
# 1) just to add up the estimator values
coef(fit_qual)['(Intercept)'] + coef(fit_qual)['perceived_genderMale'] +
  coef(fit_qual)['isschool'] + coef(fit_qual)['perceived_genderMale:isschool']
# 2) use predict function
predict(fit_qual, newdata = data.frame(isschool = 1, perceived_gender = 'Male'))

# warning: when there are no observations in a level, can try remedying by collapsing the category (e.g. transgender girl/boy to transgender)
predict(fit_qual, newdata = data.frame(isschool = 1,
                                       perceived_gender = 'Transgender woman/girl'))
####################################################################################################
# 6. Interactions "categorical:continuous"
fit_exp_school <- lm(log(stopduration) ~ exp_years * isschool, data = ripa_2021)
summary(fit_exp_school)

# Conclusion: see below graph
# intercept term - intercept of on-campus
# exp_years term - slope of on-campus
# isschool term - intercept of off-campus when added to intercept term (i.e. a difference in base effects)
# interaction term -  slope of off-campus when added to exp_years term (i.e. a difference in slope effects)

newdata <-data.frame(exp_years = rep(0:35,2), isschool = c(rep(0,36),rep(1,36)))

newdata$predicted[1:36]<-predict(fit_exp_school, newdata = newdata[1:36,])
newdata$predicted[37:72]<-predict(fit_exp_school, newdata = newdata[37:72,])

newdata$isschool <-factor(newdata$isschool)

library(ggplot2)
ggplot(aes(x=exp_years, y=predicted, col=isschool), data=newdata) +
  geom_line(data = newdata[1:36,]) +
  geom_line(data = newdata[37:72,])



