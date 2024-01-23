# y_bar is N(mu, sigma^2/n) distributed
plot_y_bar <- function(m,n) {
  ybars <- sapply(1:m, function(rep){
    mean(rnorm(n, mean=2, sd = 3))
  })
  
  hist(ybars, probability = T, breaks = 'fd')
  lines(seq(0, 4, l=1e2),
        dnorm(seq(0, 4, l=1e2),
              mean=2, sd = 3 / sqrt(n)))  
}

m=200
n=50
par(mfrow = c(1,3))
plot_y_bar(m,n)

# m -> infinity; graph fills in more and distribution approaches N(mu, sigma^2/n)

m = 10000
n = 50
plot_y_bar(m,n)

# n -> infinity; variance -> 0 and distribution approaches pinpointing at 2
 
m = 200
n = 100000
plot_y_bar(m,n)

####################################################################################################
cement <- data.frame(modified = c(16.85, 16.40, 17.21, 16.35, 16.52,
                                  17.04, 16.96, 17.15, 16.59, 16.57),
                     unmodified = c(16.62, 16.75, 17.37, 17.12, 16.98,
                                    16.87, 17.34, 17.02, 17.08, 17.27))
# mean and variance
colMeans(cement)
sapply(cement, var)

# t test to compare unmodified v modified means
t.test(cement$modified, cement$unmodified, var.equal = F)

# See if variances of two populations are equal?
Fstat <- var(cement$modified)/ var(cement$unmodified)
Fstat # 1.63
1 - pf(Fstat, 9 , 9) # pvalue: .024
qf(1-0.05, 9, 9) # threshold for rejection at .05: 3.18

# Conclusion: we can assume equal var