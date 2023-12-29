rm(list=ls())

# read file
trees_data <-read.csv("./Datasets/Tree_Equity_Scores_Tucson.csv")

# remove row indices where population is 0
zero_pop <- which(trees_data$RAWTotalPop == 0)
na_pop <- which(is.na(trees_data$RAWTotalPop))

trees_narm <- trees_data[-c(zero_pop, na_pop),]

# write it into a new file for use in future sessions
write.csv(trees_narm, file = './Datasets/Tree_Equity_Scores_Tucson_noNA.csv')

# plot percentage of people not working (senior, children, nonworking adults)
hist(rowSums(trees_narm[, c("PCTSenior", "PCTChildren", "PCTNonWorkingAge")]),
     xlab = 'sum', main = "histogram of senior + children + 'nonworking'", breaks = 100)

# see how heat severity relates to percent elderly
plot(HeatSeverity ~ PCTSenior, data = trees_narm, col = "gray")
fit_slr <- lm(HeatSeverity ~ PCTSenior, data = trees_narm)
pred_slr <- predict(fit_slr, newdata = data.frame(PCTSenior = seq(0,100,1)),
                    interval = 'confidence')
matlines(pred_slr, lty = c(1,2,2), col = 1, lwd = 2)

# using a diff trendline to measure uncertainty
# Working - Hotelling procedure: will learn later
# Measures overall trend rather than specific predictor values

# A new confidence interval for trying to predict for a new observation:
pred_new <- predict(fit_slr, newdata = data.frame(PCTSenior = seq(0,100,1)),
                    interval = 'prediction')
matlines(pred_new, lty = c(0,2,2), col = 3, lwd = 2)

# summarize fit
summary(fit_slr)

# 95% CI for regression coeffs
coef(fit_slr)
confint(fit_slr)
slope <-coef(fit_slr)[2][[1]]

10*slope

# Diagnostic plots reveal potential violation of assumptions
layout(matrix(1:4, 2, 2))
par(mar = c(4, 4, 2, 1))
# Check Linearity assumption: residuals v fitted and Normal Q-Q
?plot.lm()
plot(fit_slr, which = c(1, 2)) # we see systematic overpredicting at tails, underpredicting at center; suggests curvy shape over line
                               # we see heavy tails on QQ to suggest this is not normal
# PCTSenior 
plot(trees_narm$PCTSenior, 
     fit_slr$residuals, 
     xlab = "PCTSenior",
     ylab = "Residuals")
# PCTTreeCover underestimates as percentage increases
plot(trees_narm$PCTSenior, 
     fit_slr$residuals, 
     xlab = "PCTTreeCover",
     ylab = "Residuals")

# To address nonlinearity, let's try a more flexible relationship (2nd degree poly)
fit_slr2 <- lm(HeatSeverity ~ poly(PCTSenior, 2), data = trees_narm)
par(mar = c(4,4,1,1))
plot(HeatSeverity ~ PCTSenior, data = trees_narm)
pred_slr2 <- predict(fit_slr2, newdata = data.frame(PCTSenior = seq(0, 100, 1)),
                     interval = "confidence")
matlines(pred_slr2, lty = c(1, 2, 2), col = "black", lwd = 2)

#########################################################
# What about all those other variables?
sub_vars <- c("PCTMinority", "PCTUnemployment", "PCTPoverty", "PopDensity",
              "PCTSenior", "PCTChildren", "PCTNonWorkingAge",
              "HeatSeverity", "PCTTreeCover")
# create df with multiple vars and do mlr fit
trees_MVR <- trees_narm[, sub_vars]
head(trees_MVR)
fit_full <- lm(HeatSeverity ~ ., data = trees_MVR)
summary(fit_full)
# consult diagnostics for linearity assumption
layout(matrix(1:2, 1, 2))
plot(fit_full, which = 1:2) # linear residuals
                            # QQ a little sus, but usually this is ok if residuals are fine.

# Multicollinearity
X_full <- model.matrix(fit_full)
corrplot :: corrplot(cor(X_full[,-1]),
                     type = 'lower')
car::vif(fit_full)
# removing these unnecessary predictors
fit_noWA <- lm(HeatSeverity ~ . - PCTNonWorkingAge,
              data = trees_MVR)
X_noWA <- model.matrix(fit_noWA)
corrplot::corrplot(cor(X_noWA[, -1]),
                   type = "lower")
car::vif(fit_noWA)

# Now compare the change in regression coefficient for PCTSenior
coef(fit_full)
coef(fit_noWA) # much stronger relationship between PCTSenior and HeatSeverity

# Interactions: will go over in last section of course
# we notice that PCTPoverty has a neg coeff, but appears to have pos relationship when plotted
plot(HeatSeverity ~ PCTPoverty, data = trees_MVR)
# what if PCTMinority depends on PCTPoverty?
fit_inter <- lm(HeatSeverity ~ PCTSenior + PCTChildren + PCTUnemployment + PopDensity +
                  PCTTreeCover + PCTMinority * PCTPoverty,
                data = trees_MVR)
summary(fit_inter) #Now PCTPoverty has a + correlation to HeatSeverity
 
