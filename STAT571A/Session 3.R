rm(list = ls())

# Common distributions in R
# 1. Density function: pdf
mu <- 0.5; sigma <- 0.9
dnorm(x = 1, mean = mu, sd = sigma) # HEIGHT of the density function

# 2. Probability function (CDF)
pnorm(q = -1, mean = mu, sd = sigma) # AREA of density function -1 sd away or lower

# 3. Quantile function 
q = qnorm(p = 0.04779035, mean = mu, sd = sigma) # HORIZONTAL AXIS VALUE (RV value); 
                                            # reverse of pnorm = quantile associated with this p value

# 4. random sampling function
rnorm(n = 3, mean = mu, sd = sigma) # RANDOM POINT from distr.

# visualization of CDF
Y <- seq(-3,3,by = 0.01)
Fy <- dnorm(Y, mean = mu, sd = sigma)
plot(Y,Fy, type = "l")
polygon(c(Y[Y<=q],q), 
        c(Fy[Y<=q],0), 
        col = 'grey')
dev.off()

##########################################################################################
# trees data
trees <- read.csv("./Datasets/Tree_Equity_Scores_Tucson.csv")
plot(HeatSeverity ~ PCTTreeCover, data = trees)

# create object containing lm fit
fit_trees <- lm(HeatSeverity ~ PCTTreeCover, data = trees)
fit_trees$coefficients
head(fit_trees$residuals)
length(fit_trees$residuals) #462
mean(fit_trees$residuals) # basically 0

summary(fit_trees) # notice 460 dof (n - 2)

# inference
confint(object = fit_trees, level = 0.94) 
predict(object = fit_trees, newdata = data.frame(PCTTreeCover = 20))
predict(object = fit_trees, newdata = data.frame(PCTTreeCover = 20),
        interval = "prediction") # default 95% but can be customized

# qq plot
plot(fit_trees, 2) # suggests that higher deviation on edges (heavy tails suggests)

# We are in danger of over-confident inference: 
confint(fit_trees)

# Bootstrap confidence intervals do not depend on normality assumption
library(boot)
bootfit <- boot(trees, statistic = ?? , R = 500) # we should come back to this
boot.ci(boot_fit, index = 2,
        conf - 0.95, type = "perc")
# the output should be somewhat similar: generally QQ plot is fine; 
# doesn't really matter and inference still pretty good even if nonnormal residuals

