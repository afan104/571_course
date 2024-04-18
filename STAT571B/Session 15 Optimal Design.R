# Determining optimal design with different criteria
# A-optimality: minimizing the average sample variance (sample var proportional to diag(XtX_inv) )
# D-optimality: minimizing the volume of the joint confidence interval (proportional to det(XtX_inv) )

####################################################################################################
# Design a 2^k factorial design
levels <- factor(c("-","+"))
df <- expand.grid(A=levels,
                  B=levels,
                  C=levels)
df <- rbind(df, df)

# Design a design that is "slightly modified" (in row 4)
df_mod <- df
df_mod[4,1] <- df_mod[4,2] <- "-"
head(df_mod,4) # now all negative condition

formula <- ~ A + B + C
X <- model.matrix(formula, data = df)
X_mod <- model.matrix(formula, data = df_mod)

####################################################################################################
# A-optimality: minimizing average sample variance
# sample variance proportional to diagonals of XtX_inv
XtX_inv <- solve(t(X) %*% X)
XtX_inv_mod <- solve(t(X_mod) %*% X_mod)

mean(diag(XtX_inv))
## [1] 0.25
mean(diag(XtX_inv_mod)) # modified design is more A-optimal in 0-1 encoding
## [1] 0.24375

# BUT! It depends on encoding type

# Orthogonal encoding
X_eff <- model.matrix(formula, data = df, contrasts.arg = list(A = contr.helmert,
                                                               B = contr.helmert,
                                                               C = contr.helmert))
X_eff_mod <- model.matrix(formula, data = df_mod, contrasts.arg = list(A = contr.helmert,
                                                                       B = contr.helmert,
                                                                       C = contr.helmert))
XtX_inv_eff <- solve(t(X_eff) %*% X_eff)
XtX_inv_eff_mod <- solve(t(X_eff_mod) %*% X_eff_mod)

mean(diag(XtX_inv_eff)) # 2^k factorial design is A-optimal with orthogonal encoding
## [1] 0.0625
mean(diag(XtX_inv_eff_mod))
## [1] 0.06458333

####################################################################################################
# D-Optimality
# Minimizing the volume of the joint confidence interval (joint Margin of Error)

# Volume of joint confidence region proportional to det(XtX_inv)^(0.5)
det(XtX_inv) # 2^k factorial design is D-optimal
## [1] 0.0009765625
det(XtX_inv_mod)
## [1] 0.001041667