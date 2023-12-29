rm(list = ls())

trees <- read.csv("./Datasets/Tree_Equity_Scores_Tucson_noNA.csv")

# Need to wrap transformations to predictors with I() to "inhibit" misinterpretations of arithmetic symbols 
# in the formula; This paramaeterization spans the linear space
?I()
fit_quad1 <- lm(HeatSeverity ~ PCTTreeCover + I(PCTTreeCover^2), data = trees)
X_quad1 <- model.matrix(fit_quad1)
head(X_quad1)

# Alternative reparameterization: poly(predictor, n) creates predicts that span the n-polynomial space
fit_quad2 <- lm(HeatSeverity ~ poly(PCTTreeCover, 2), trees)
X_quad2 <- model.matrix(fit_quad2)

# Nice mathematical properties for poly() transforms
t(X_quad1) %*% X_quad1 # normal-looking
t(X_quad2) %*% X_quad2 # nice diagonal matrix (orthogonal)

