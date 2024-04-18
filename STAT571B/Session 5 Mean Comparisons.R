# Comparing Means 
### 3 Basic methods: Fisher's (no pairwise correction), Bonferroni (pairwise but conservative), Tukey's HSD (pairwise)
### Scheffe's method good for experimentwise error (pairwise) for **all contrasts** (most controlling)

# Fisher's Least Significant Difference (not great; basically just individaul T tests)
# NO CONTROLLING FOR EXPERIMENTWISE ERROR (i.e. family-wise)
t.test(rate ~ power, data = etching[etching$power %in% c(160, 180), ], conf.level = 0.99)
t.test(rate ~ power, data = etching[etching$power %in% c(160, 200), ], conf.level = 0.99)
t.test(rate ~ power, data = etching[etching$power %in% c(180, 200), ], conf.level = 0.99)
t.test(rate ~ power, data = etching[etching$power %in% c(180, 220), ], conf.level = 0.99)
t.test(rate ~ power, data = etching[etching$power %in% c(200, 220), ], conf.level = 0.99)

# Bonferroni (controls for experimentwise error; divide by number pairs: nCr(no. groups,2) )

# Tukey's HSD (Best so far)
# controls experimentwise error and less conservative than bonferroni
etching_hsd <- TukeyHSD(etching_aov,
                        conf.level = 0.99)
etching_hsd


######################################################################################
# Contrasts: Scheffe's method
### More general comparing linear combinations of group means
### e.g. mu1+mu2-mu3-m4; then c = (1,1,-1,-1) and called contrasts
### You compare the S_C sample value to the |C| threshold; S_C > |C| is significant
c <- c(1, -1, 0, 0)
means <- aggregate(rate ~ power, data = etching, FUN = mean)
ybars <- means$rate
C <- sum(c * ybars) # use the contrasts in the sum sum_i_to_n=4
S_C <- sqrt(sigma(etching_aov)^2 * sum(c^2 / 5))
abs(C) 
## [1] 36.2
S_C * sqrt(3 * qf(1 - 0.01, 3, 16))
## [1] 46.03492

