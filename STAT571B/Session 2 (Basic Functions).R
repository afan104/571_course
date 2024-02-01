# basic functions

# sample() to draw randomly from a vector
order <- sample(x=1:20, size = 20, replace = FALSE)

# as.factor() makes explicit for variables that could be mistaken as continuous
etching <- read.csv("./STAT571B/Datasets/etching.csv") # not available online
plot(as.factor(etching$power)) # given etching dataset


# anova() can be used for comparing behavior between different factor levels
etching_aov <- aov(rate ~ as.factor(power), data = etching)
summary(etching_aov)

# anova() can also be used to fit linear models and see if they are significant
fit_lm <- lm(rate ~ as.factor(power), data = etching)
summary(fit_lm)