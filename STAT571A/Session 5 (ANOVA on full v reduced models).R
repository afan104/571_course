rm(list = ls())

trees<- read.csv("../Datasets/Tree_Equity_Scores_Tucson_noNA.csv")
fit_trees <- lm(HeatSeverity ~ PCTTreeCover, data = trees)
summary(fit_trees) # t value: -13.93

# Test and see if SLR model (Full) is better than the model containing b0 only (Reduced)
anova(fit_trees) # F value: 194.08
# F score equal to t score squared
-13.93**2 # t^2: 194.05

# Test and see if Cubic fit is better than linear fit (SLR)
fit_cube <- lm(HeatSeverity ~ poly(PCTTreeCover, 3), data = trees) 
anova(fit_cube, fit_trees)
