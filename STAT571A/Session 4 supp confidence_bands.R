## Working-Hotelling Band function
pred_WH <- function(object, newdata, level = 0.95){
  fit <- predict(object, newdata) ## Yhat
  MSE <- summary(object)$sigma^2
  n <- nrow(object$model) ## extract df from model object
  W <- sqrt(2 * qf(level, 2, n - 2))
  X_obs <- object$model[, attr(object$terms, "term.labels")] ## extract X
  X <- newdata[, attr(object$terms, "term.labels")]
  ME <- W * sqrt(MSE * (1 / n + (X - mean(X_obs))^2 / 
                          sum((X_obs - mean(X_obs))^2)))
  upr <- fit + ME
  lwr <- fit - ME
  return(cbind(fit, lwr, upr))
}
## Simulation study
set.seed(2023)
n <- 1e2
X_grid <- seq(0, 1, l = 1e2)
X <- sort(runif(n))
sigma <- 0.1
beta0 <- 1
beta1 <- 0.5
EY <- beta0 + X_grid * beta1
n_reps <- 1e3
bands <- list(CI = array(NA, dim = c(100, 3, n_reps), 
                         dimnames = list(NULL, c('fit', 'lwr', 'upr'), NULL)), 
              WH = array(NA, dim = c(100, 3, n_reps), 
                         dimnames = list(NULL, c('fit', 'lwr', 'upr'), NULL)))
errors <- data.frame(CI = rep(NA, n_reps),
                     WH = rep(NA, n_reps))
## Simulate data + compute bands
for(rep in 1:n_reps){
  epsilon <- rnorm(n, sd = sigma)
  Y <- beta0 + beta1 * X + epsilon
  fit <- lm(Y ~ X)
  bands$CI[, , rep] <- predict(fit, newdata = data.frame(X = X_grid),
                               interval = "confidence")
  bands$WH[, , rep] <- pred_WH(fit, newdata = data.frame(X = X_grid))
}
## Plot bands using semi-transparent curves
layout(matrix(1:2, 1, 2))
par(mar = c(4, 4, 2, 1))
plot(Y ~ X, main = "pointwise CI")
for(rep in 1:n_reps){
  matlines(X_grid, bands$CI[, 2:3, rep], lty = 1, 
           col = adjustcolor("darkgray", 0.02))
  errors$CI[rep] <- (sum(EY < bands$CI[, 'lwr', rep]) + 
                       sum(EY > bands$CI[, 'upr', rep])) > 0
}
text(0.75, 1, labels = paste("error rate =", mean(errors$CI)))
lines(EY ~ X_grid, lwd = 2)
plot(Y ~ X, main = "Working-Hotelling")
for(rep in 1:n_reps){
  matlines(X_grid, bands$WH[, 2:3, rep], lty = 1, 
           col = adjustcolor("darkred", 0.02))
  errors$WH[rep] <- (sum(EY < bands$WH[, 'lwr', rep]) + 
                       sum(EY > bands$WH[, 'upr', rep])) > 0
}
text(0.75, 1, labels = paste("error rate =", mean(errors$WH)))
lines(EY ~ X_grid, lwd = 2)