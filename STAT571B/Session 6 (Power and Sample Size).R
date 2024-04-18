# Power calculations
# power.anova.test() - At least 1 needs to be null to calculate what would be required for that value: 
# groups, n, between.var, power, within.var, sig.level

n <- 3
mus <- c(mu1 = 575, mu2 = 600, mu3 = 650, mu4 = 675)
alpha <- 0.01
sigma <- 25
power.anova.test(groups = 4, n = 4, between.var = var(mus),
                   within.var = sigma^2, sig.level = .01) # power is .9621
