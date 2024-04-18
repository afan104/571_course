# Randomized Complete Block Design (RCBD) 1 block 1 treatment -> Latin Squares 2 block 1 treatment
# interaction plots

setwd("~//571_course//STAT571B")

vascular <- read.csv("./Datasets/vascular.csv")
head(vascular)

# RCBD: This design assigns all different pressure levels (Blocks) to each batch (Treatment)
### batch pressure flicks
### 1     8500   90.3
### 1     8700   92.5
### 1     8900   85.5
### 1     9100   82.5
### 2     8500   89.2
### 2     8700   89.5
vascular_aov <- aov(flicks ~ as.factor(pressure) + as.factor(batch), data = vascular)

vascular_HSD <- TukeyHSD(vascular_aov,
                         which = "as.factor(pressure)")


# Given that lower rates of flicks are better (x axis values), 
# we can see that the following 4 groups do not appear to significantly be better:
# 8700-8500, 8900-8500, 8900-8700, 9100-8900 since they overlap 0 at 95% confidence
# i.e. it would NOT be good to have these 4 be separate groups on their own.
plot(vascular_HSD)

########################################################################################################
# Generalized to more than 1 treatment: Latin Squares (sudoku-style concept)
# Orthogonality is preserved in Latin squares (i.e. don't have any information about treatment or block A if given block b: could be from any group for both treatment and block a)
rocket <- read.csv(".//Datasets//rocket.csv")
head(rocket, 4)
rocket_aov <- aov(burn ~ propellant + as.factor(batch) + as.factor(operator),
                  data = rocket)
summary(rocket_aov)

# order of aov() doesn't change anything due to orthogonality
summary(aov(burn ~ as.factor(batch) + as.factor(operator) + propellant,
            data = rocket))

### model assumptions pretty much same:
### QQ (normality), constant variance (homoskedasticity), independence of errors and additivity of predictors/factors
# 1. QQ
plot(rocket_aov, which = 2)

# 2. Homoskedasticity
plot(rocket_aov, which = 5)
plot(rocket$batch, resid(rocket_aov))
plot(rocket$operator, resid(rocket_aov))

# 3. Independence of errors and additivity: Looking for parallel plots
### using interaction.plot() switching order of blocks
layout(matrix(1:4, 2,2)); par(mar = c(4, 4, 1, 1))
interaction.plot(vascular$pressure, vascular$batch, vascular$flicks, type = "b",
                 pch = 1:6, col = RColorBrewer::brewer.pal(6, "Dark2"))

interaction.plot(vascular$batch, vascular$pressure, vascular$flicks, type = "b",
                 pch = 1:6, col = RColorBrewer::brewer.pal(6, "Dark2"))

### using interaction_plot() based on SAS; takes the fitted effects based on the additive model
### also switching order of blocks; this function can take up to 3 blocks in a model (will display 2, average 3rd) but cannot take more
### i.e. can take 4 predictors max (1 treatment + 3 blocks)
source('interaction_plot.R')
interaction_plot(aov(flicks ~ as.factor(pressure) + as.factor(batch), data = vascular))
interaction_plot(aov(flicks ~ as.factor(batch) + as.factor(pressure), data = vascular))
