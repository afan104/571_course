# 1. Fit the model: using glm() instead of lm()
# Danger: lm() will work but it will produce a * terrible * fit 
#         since it will be fitting linear regr. instead of logistic regr. 
library(ISLR2) # we will be using 'Default' dataset

fit_glm <- glm(default ~ student + balance + income, data = Default,
               family = binomial(link = "logit"))

head(Default)
head(fit_glm$y) # automatically converts it to factor levels

# coeff interpretations
coef(fit_glm) # interpretation: because its a log odds "-" means decreases likelihood, "+" means increases likelihood

# Confidence intervals using confint()
confint(fit_glm, level = 0.95)
##################################################################################################################################
# 2. predictions
# 2a. Manually converting to probabilities expit(logit(p)) -> p
out <- predict(fit_glm, newdata = data.frame(student = "Yes", balance = 1900, income = 3000))
out
### -0.6073607
### this negative value indicates that they are unlikely to default (negative value for log odds means -> not likely)

exp(out) / ( 1 + exp(out))
### 0.3526615 
### expit(logit(p)) = p -> this is the actual probability

# 2b. automatically get out p using type = 'response' argument
predict(fit_glm, newdata = data.frame(student = "Yes", balance = 1900, income = 3000),
        type = 'response')
### 0.3526615 
### type = 'response' argument converts to probability automatically
###################################################################################################################################
# 3. plotting
balance_seq <- seq(0,3000,length.out=1e2)
p_curve_balance <- predict(fit_glm,type="response",
                           newdata=data.frame(student= 'Yes',
                                              balance = balance_seq,
                                              income=30000))
p_curve_balance_no <- predict(fit_glm,type="response",
                           newdata=data.frame(student= 'No',
                                              balance = balance_seq,
                                              income=30000))

plot(Default$balance,
     Default$default == 'Yes', # converts to 0 or 1
     xlab = "balance", ylab = "",
     main = "Probability of default",
     xlim = c(0,3000))

lines(balance_seq, p_curve_balance, lwd = 2, col=2)
lines(balance_seq, p_curve_balance_no, lwd = 2, col=2, lty = 2)
###################################################################################################################################
# 4. plotting with confidence band
plot(Default$balance,
     Default$default == 'Yes', # converts to 0 or 1
     xlab = "balance", ylab = "",
     main = "Probability of default",
     xlim = c(0,3000))


# importantly, to compute the confidence interval, we do all operations
# fit + z_(1-a)/2 * se **inside/with** the logit function, and THEN back transform it
# using the invlogit function (i.e. expit function)

# to get se, set se.fit=T in predict()
p_curve_se <- predict(fit_glm, se.fit = T,
                      newdata=data.frame(student= 'Yes',
                      balance = balance_seq,
                      income=30000))
#invlogit() aka expit()
inv_logit <- function(x) exp(x)/(1+exp(x))

# add lines:
lines(balance_seq, inv_logit(p_curve_se$fit), lwd = 2, col=2)
lines(balance_seq, inv_logit(p_curve_se$fit +
                               qnorm(0.025) * 
                               p_curve_se$se.fit),
      lwd = 2, col=2, lty=2)
lines(balance_seq, inv_logit(p_curve_se$fit +
                               qnorm(0.975) * 
                               p_curve_se$se.fit),
      lwd = 2, col=2, lty=2)
######################################################################################
######################################################################################
# 5. LETS REPEAT WITH A BIKESHARE DATASET:
## Now, extend these logistic regression ideas from a binary response to 
## generalized linear model responses 
######################################################################################
######################################################################################
# Generalize binary logistic regression into a class of variables that 
# do not match normality of residuals assumption. Another example is 
# Bikers which are DISCRETE (no normally distributed errors) and
# can be modeled with Poisson distribution

head(Bikeshare) # Note: response variable is number of bikers (integer) which can be Pois()

# Link functions: transformt the domain of the variable of interest into a -infinity to infinity domain
# For poisson, lambda is [0,infinity) --> link fn --> (-infinity,infinity)
# and the link function can therefore be log(lambda) which is most commonly used for Pois()
# inv link function is exp().
mod.pois <- glm(
  bikers ~ mnth + hr + workingday +
    temp + weathersit,
  data = Bikeshare, family = poisson)
summary(mod.pois)

params = c("workingday", "temp", "weathersitcloudy/misty",
           "weathersitlight rain/snow", "weathersitheavy rain/snow")

confint(mod.pois, parm = params)

# Let's compare the different effects of each predictor at different levels
## Note: inverse link function is exp()

## compare working and nonworking: small shift indicates low importance of workingday
exp(predict(mod.pois, newdata = data.frame(mnth = "Nov", hr = "8", workingday = 0:1,
                                           temp = 0.5, weathersit = "clear")))  

## compare temperature: big shift indicates importance of temperature
exp(predict(mod.pois, newdata = data.frame(mnth = "Nov", hr = "8", workingday = 1,
                                           temp = c(0.25, 0.5, 0.75), weathersit = "clear")))  

## compare weather situation: big shift indicates importance of weather situation
exp(predict(mod.pois, newdata = data.frame(mnth = "Nov", hr = "8", workingday = 1,
                                           temp = 0.5, weathersit = c("clear", "light rain/snow",
                                                                      "heavy rain/snow"))))
## etc. etc.


# plot fit
par(mar = c(4,4,0.2,0.2))
plot(bikers ~ hr, data = Bikeshare[Bikeshare$mnth =="Nov",])
pred_grid <- predict(mod.pois,
                     newdata = data.frame(mnth = "Nov",
                                          hr = as.factor(0:23), workingday = 1,
                                          temp = mean(Bikeshare$temp),
                                          weathersit = "clear"))
points(exp(pred_grid), col = "darkblue", lwd = 2)

# plot CI
pred_grid_se <- predict(mod.pois, se.fit = T,
                        newdata = data.frame(mnth = "Nov",
                                             hr = as.factor(0:23), workingday = 1,
                                             temp = mean(Bikeshare$temp),
                                             weathersit = "clear"))
plot(bikers ~ hr, data = Bikeshare[Bikeshare$mnth =="Nov",])
arrows(1:24,
       y0 = exp(pred_grid_se$fit + 
                  pred_grid_se$se.fit * qnorm(.025)),
       y1 = exp(pred_grid_se$fit + 
                  pred_grid_se$se.fit * qnorm(.975)),
       col = "darkblue", lwd = 2,
       angle = 90, length = 0.04, code = 3)

######################################################################################
# 6. Convert General Linear Model with GAM into a continuous model
######################################################################################
sapply(Bikeshare[, c('hr', 'mnth')], class) # we see that hr and mnth are factors
head(Bikeshare[, c('hr', 'mnth')], 3)  # more detail

# let's create a new dataset that converts these two variables into numeric (instead of factors)
Bikeshare_num <- Bikeshare 
Bikeshare_num$hr <- as.numeric(Bikeshare$hr)
Bikeshare_num$mnth <- as.numeric(Bikeshare$mnth)

# And run the gam on it
library(mgcv)
fit_gam_bikes <- gam(bikers ~ s(hr) + s(mnth) + workingday + s(temp) + weathersit,
                     data = Bikeshare_num, family = poisson)
summary(fit_gam_bikes)

# And overlay it onto our previous discretized general linear model plot
plot(bikers ~ hr, data = Bikeshare[Bikeshare$mnth =="Nov",])
pred_grid <- predict(mod.pois,
                     newdata = data.frame(mnth = "Nov",
                                          hr = as.factor(0:23), workingday = 1,
                                          temp = mean(Bikeshare$temp),
                                          weathersit = "clear"))
points(exp(pred_grid), col = "darkblue", lwd = 2)

hrs_grid <- seq(1,24,by = 0.25)

pred_gam <- predict(fit_gam_bikes,
                    newdata = data.frame(mnth = 11,
                                         hr = hrs_grid, workingday = 1,
                                         temp = mean(Bikeshare$temp),
                                         weathersit = "clear"))
lines(hrs_grid, exp(pred_gam))