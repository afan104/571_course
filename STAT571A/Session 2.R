rm(list=ls())

Toluca <- read.csv("./Datasets/CH01TA01.csv")
head(Toluca)
fit_ls <- lm(Y~X, data = Toluca)
# Property: sum of residuals equals 0
sum(fit_ls$residuals)
# Property: line passes through x_bar and y_bar
x_bar <- mean(Toluca$X)
new <- data.frame(X = c(x_bar))
ls_pred <- predict(fit_ls, newdata = new)

y_bar <- mean(Toluca$Y)
c(y_bar, ls_pred)

?plot
plot(Y ~ X, data = Toluca )
pred_slr <- predict(fit_ls, newdata = data.frame(X = seq(0,120,1)))
?matlines
matlines(pred_slr, lty = 1, col = 1, lwd = 2)

# Minimizing Q with alternative metric: Q = sum of absolute value of residuals
# Benefits: More robust than minimizing SSE since squaring increases the cost of outliers exponentially.
abs_fit_ls <- rq(Y ~ X, tau = 0.5, data = Toluca)
abs_pred_slr <- predict(abs_fit_ls, newdata = data.frame(X = seq(0,120,1)))
matlines(abs_pred_slr, lty = 1, col = 2, lwd = 2)
# Does not admit a closed-form expression for estimators like L2 norm does (B0 and B1 closed form expressions obtained by first deriv.)
# Nor does it have the Gauss-Markov theorem result: not necc. pass through x_bar, y_bar


###########################################################################################
# Trees data
trees_data <-read.csv(".\\Datasets\\Tree_Equity_Scores_Tucson_noNA.csv")
fit_trees <- lm(HeatSeverity ~ PCTTreeCover, data = trees_data)
coef(fit_trees)

# Now try with normalized x var
trees_data$PCTTreeCover_std <- scale(trees_data$PCTTreeCover) # normalizes (centers and divides by sd)
fit_std <- lm(HeatSeverity ~ PCTTreeCover_std, data = trees_data)
coef(fit_std)
# What is the expected heat severity for tracts with the mean amount of tree cover?
stdx_bar = 0
coef(fit_std)[1] # the intercept

x_bar <-mean(trees_data$PCTTreeCover)
coef(fit_trees)[[1]] + x_bar*coef(fit_trees)[[2]]
# By how much is heat severity expected to decrease when the perecntage of tree cover increases by 1 sd?
coef(fit_std)[2] # slope


