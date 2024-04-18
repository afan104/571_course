# In part 1:
# You can block without creating replicates. As long as you keep them independent of 
# any one factor (to the best of your ability. However, there will always be some confounding
# since you were unable to fit the entire design into one block).

# For a 2^k factorial design split over 2^p blocks, there are 2^k + 2^p - 1) unknowns

# Since you are fitting the blocks all into one design, you end up needing to confounding a total
# of 2^p-1 variables. i.e. the number of confounding variables for blocking within one design is 2^p - 1.

# In part 2:
# Going over how to pick which variables to confound (and if applicable which ones end up being confounded implicitly)

# In part 3:
# How to simulate the power of different confounding methods
####################################################################################
# 4 observations and 5 unknowns 
# (confounding since not enough degrees of freedom to fit 5 unknowns)
# Fitting with 2 blocks
levels <- c("-", "+")
df <- expand.grid(A = levels, B = levels)
df$block <- factor(c(1, 2, 2, 1))
X <- model.matrix(~ A * B + block, data = df,
                  contrasts.arg = list(A = contr.helmert,
                                       B = contr.helmert,
                                       block = contr.helmert))
# one way to confound is shown below: confounding with AB interaction
# there are 3C2 ways in this scenario of 5 unknowns, 4 obs, 2 blocks
#    -> 2 blocks and 4 observations. then there are (2+1) choose 2 ways. 
#       pretend one block is fixed (has 2 values in each block). 
#       then there are 3 positions around it for the other 2 values in the other block to fill in.
X
##    (Intercept) A1 B1 block1 A1:B1
## 1          1   -1 -1 -1       1
## 2          1    1 -1  1      -1
## 3          1   -1  1  1      -1
## 4          1    1  1 -1       1

# we can see that the factors are not completely indep of one another
cor(X[, -1])
##       A1 B1 block1 A1:B1
## A1     1  0      0     0
## B1     0  1      0     0
## block1 0  0      1    -1
## A1:B1  0  0     -1     1


############################################################################################
### Same thing shown with 0-1 encoding
levels <- c("-", "+")
df <- expand.grid(A = levels, B = levels)
df$block <- factor(c(1, 2, 2, 1))
X01 <- model.matrix(~ A * B + block, data = df)
cor(X01[, -1])
##               A+        B+    block2       A+:B+
## A+     1.0000000 0.0000000 0.0000000   0.5773503
## B+     0.0000000 1.0000000 0.0000000   0.5773503
## block2 0.0000000 0.0000000 1.0000000  -0.5773503
## A+:B+  0.5773503 0.5773503 -0.5773503  1.0000000

# same -1 dependence seen as -1 1 encoding
# recall average effect of B = Beta_AB - .5*Beta_A - .5*Beta_B
cor(X01[, 'A+:B+'] - 0.5 * X01[, 'A+'] - 0.5 * X01[, 'B+'], X01[, 'block2'])
## [1] -1

# eigen values are not all nonzero. One missing eigenvalue indicates lack of independence
eigen(cor(X01[, -1]))$values
## [1] 2.000000e+00 1.000000e+00 1.000000e+00 2.220446e-15
############################################################################################

# General note: 
# Can pick confounder but the last one will be defined implicity by the choise of the first 2
# formula = mod 2 of the exponents of the product

# Formula example - 
# Factors A, B, C, AB, BC, AC, ABC
# 3 confounding variables, you pick two as AB, BC
# Last confounding variable = (AB)(BC) = AC

############################################################################################

# Another note:
# some choices of confoudnign are better than others
# for the optimal choice for each number of factors, 
# refer to a table of "Suggested blocking arrangements for the 2^k factorial design"
# pg 316 in the book, pg 332/757 in the pdf copy of the book

############################################################################################
# which has the most power? i.e. which one produces lower p values on average
# simulation given that only one variable has an effect size (in this case equal to 2)
df_alt <- expand.grid(A = levels, B = levels, C = levels)
df_alt$block4 <- factor(c(1, 4, 2, 3, 3, 2, 4, 1))
df_alt$block3 <- factor(c(1, 1, 2, 3, 3, 2, 2, 1))
X3 <- model.matrix(~ A * B * C + block3, data = df_alt)
X4 <- model.matrix(~ A * B * C + block4, data = df_alt)
beta <- c(0, 2, 0, 0, 0, 0, 0, 0, 0, 0)
names(beta) <- colnames(X3)
p_values <- rowMeans(sapply(1:500, function(rep){
  y <- X3 %*% beta + rnorm(nrow(X3))
  c("3 Blocks" = anova(aov(y ~ A + B + C + block3, data = df_alt))['A', 'Pr(>F)'],
    "4 Blocks" = anova(aov(y ~ A + B + C + block4, data = df_alt))['A', 'Pr(>F)'])
}))
p_values
## 3 Blocks 4 Blocks
## 0.1262140 0.1949921



