# Techniques
## 571a: Dr. Henry Scharf
SLR, MLR, GLM, confidence intervals (new/existing obs), confidence bands (Working-Hotelling procedure) for SLR/MLR, (LINE) assumptions, remedial measures to (LINE) violations, bootstrapping for nonnormality, nonlinear transformations on predictors/response (poly(), boxcox(), log()), interactions terms, regularization with LASSO/Ridge using glmnet, family-wise inference, Full v. Reduced models, logistic regression, ggplot (plotting all of the above in session 13), splines, missing data (mice package)

### Session 1: Overview of techniques (most are used)
In this session, we will take a look at the relationship between two variables heat severity and percent elderly in a simple single-variable relationship. With this simple linear regression model, we will look at new predictions and confidence intervals for these predictions. In addition, confidence bands for regression lines and confidence intervals for the estimated coefficients b0 and b1.

We will then investigate the validity of the linear model by looking for violations of normality in the residuals and Q-Q plot. Residuals help us notice a likely nonlinear relationship which prompts us to transform the x into a second-degree polynomial which fits the data much better.

We then investigate the relationship between heat severity and multiple predictors in multiple linear regression. Again, we check for violations of normality. Residuals look fine, but QQ has heavy tails. However, in most cases, the QQ plot does not matter much if residuals look normal, so we disregard this.

As with all multiple linear regressions, we must check for collinearity between the multiple predictors. We perform a vif() to test for collinearity and remove the collinear variable with the highest vif score. Then, we rerun the model.

Finally, after removing collinear predictor(s), we check to see if there are interaction terms. We see an indication of this since one of the predictors (PCTPoverty) has a negative coefficient, but the plotted values indicate a positive relationship due to a positive-looking slope. By rerunning the model with an interaction term PCTPoverty*PCTMinority, we see that the negative coefficient becomes positive. The minority variable was likely dependent on poverty.

### Session 2: Properties of linear regression
In this session, we will look at some properties of the simple linear regression model. When the optimization parameter Q is equal to the sum of squared residuals, the model exhibits nice properties:
1. sum of residuals is 0
2. the line passes through x_bar and y_bar
The optimization metric Q can be other things as well, such as the sum of the absolute value of residuals. When using this metric, the model no longer has the same nice properties:
it does not follow the Gauss-Markov theorem, and it does not pass through x_bar and y_bar. One nice property is that it is more robust to outliers since the residuals are not squared. Squaring residuals exponentially increases the cost of outliers for the sum of squares Q metric.

Finally, we quickly go over the fact that linearly transforming the data does not change the predictions given by both models. Additionally, with centering and scaling, the intercept becomes meaningful as it indicates the mean response.

### Session 3: boostrapping with boot() to address nonnormality
In this session, we will look at common distributions in R: the pdf function dnorm() which indicates the height of the pdf distribution at some value, the cdf function pnorm() which indicates the LH area of the pdf distribution at some value, the quantile function qnorm() which indicates the value associated with the probability of the cdf function. qnorm() is the reverse of pnorm() and takes the value produced by pnorm(). the random sampling function rnorm() which produces random point(s) from the pdf distribution. There is also a quick visual of these different outputs using the polygon() function

Then, we look at trees data again and create a simple linear regression model with HeatSeverity ~ PCTTreeCover. We create a confidence interval for our coefficients b0 and b1, as well as for a new observation. We look at the QQ plots and find nonnormality, so we decide that we may be overconfident in our confidence intervals. So, we recreate them by bootstrapping the data to create bootstrapped confidence intervals. These do not require the assumption of nonnormality. The results demonstrate very similar confidence intervals as the non bootstrapped confidence intervals. The conclusion we can take from this is that generally nonnormal QQ plots are alright and can be more or less disregarded. However, if you take them seriously, bootstrap your data to create your confidence intervals.

### Session 4: inference (betas/response) and Working-Hotelling bands
In this session, we investigate the confidence intervals of different estimators, namely, b0, b1, Yj_hat (observed data), and Ynew_hat (new data). We note that most of these are t-distributed since they depend on sample-estimated variance MSE. Ynew_hat has an additional MSE term added to account for the uncertainty of a new observation, so it is not t distributed. We compute the confidence intervals for these four estimators with the formulas and compare them to the output generated by the R functions confint() for coefficients and predict() for data predictions

Finally, we look at confidence bands of the regression line as a whole. There is a function that generates the Working-Hotelling confidence bands and compares to a pointwise-created confidence band. The comparison shows that you cannot create an accurate confidence band through pointwise methods. Most of the code is sourced from another file, but the function is explicitly shown at the bottom of this file.

*supplemental file includes the function used to create confidence bands

### Session 5: ANOVA full v. reduced
We will test ANOVA on Full vs. Reduced models. When taking in only one model, anova() will compare to the b0 only model. putting in two models to anova() compares the two models.

### Session 6: LINE and remedial measures; GLM
In this session, we will explore diagnostics (LINE assumptions) and take remedial measures. We will also explore the General Linear Test which allows us to drop the linearity assumption.

LINE assumptions: linearity, independent residuals, normally distr. residuals, equal var. 3 of LINE can be checked straightforwardly. Harder to diagnose independent residuals. Independence of residuals: if the coefficient value looks different from the plot, plot the residuals against another index (predictor, time, space) not in the model. If there is a trend, then the residuals are dependent on this predictor, and the predictor should be included in the model to fix this issue. The diagnostics plots are: QQ plots, residual v. predictor, residual v. fitted. These plots can be generated with the residual values from the fitted object (e.g. fitted$residuals) or with plot(fitted, c(1,2,3,4)) to get all of them along with Cook's leverage.

The General Linear Test can be done with as.factor(xvar) since it assumes multiple observations on each level of X and takes the mean for each level. For continuous data, this can artificially be done by binning into integer groups with floor(). GLT is particularly helpful when the data is nonlinear.

### Session 7: Log and Box-Cox transforms (remedial measures ct'd)
### Session 8 & 10: categorical MLR and Extra Sums of Squares
This session introduces MLR and dummy coding for categorical variables. Then, it looks at ANOVA testing (extra sums of squares) to compare different models. Type 1 and Type 3 extra sum of squares testing are considered here. The purpose of extra SS is to decide whether categorical variables should be included. (There is no way to combine the p-values from each factor level into a single p-value for the categorical predictor with the summary() method)

Further, ANOVA is used as an overall method of comparison between models, but ANOVA Extra Sum of Squares is a specialized version that focuses on the importance of "specific" variables and allows you to look at them with a hyper-focused lens.

Finally, we look at standardized MLR models. Two methods: 
1. Standardizing across all Xi predictors to make Xi values comparable
2. Standardizing each coefficient Bi by its respective sample mean and sample sd
Overall, the difference for each bi from method 1 vs. method 2 is a scale of the sample sd This part was removed accidentally.

### Session 9: nonlinear MLR with poly()
Here, we explain how to use nonlinear transformations. Most important is the use of poly(). It is advised since using "raw" coding does not allow you to see significant predictors if not all of the predictors are significant. Using poly() creates orthogonal poly predictors, so the predictors' coefficients are jointly independent. Meaning, higher order polynomial predictor estimates and p-vals indicate their effect "on top of" the lower poly predictor coefficients. i.e. it allows you to see if higher-order coefficient effects are more effective than lower ones.

### Session 11, 12, 13 (pt 1): Working-Hotelling Confidence Surface for MLR, base R plotting techniques for 3 predictors (3rd variable: col, cex), interaction terms
1. WH ci for MLR and example
2. Visualization of higher order regression - colors (col), point sizes (cex) (Bin values into quantiles and color by each quantile for third predictor)
3. Interactions "continuous:continuous"
4. Logit Transformation (0,1) to R
5. Interactions "categorical:categorical"
6. Interaction "categorical:continuous"

### Session 13 (pt 2): Visualization with ggplot and base R
GGPLOT SYNTAX: 
ggplot(main dataset for points) + 
  geom_point(aes(x,y,col=variable)) + 
  geom_line(aes(x,y,group=,col=), data=fitted)) + 
  geom_ribbon(aes(x,ymin,ymax,group,col), data=predictions) + 
  scale_color_viridis_b() + scale_fill_viridis_b(alpha)

BASE R: 
1. bin continuous data
2. create colors vector
3. a) plot points
3. b) confidence bands and regression line

### Session 14: 
1. Looking at model selection using fwd/bwd/both methods for adding or subtracting predictors in model
2. Explore the limitations in scoring methods which can be "shortsighted" by accepting local minima over global minima
3. Multicollinearity
4. Ridge Regression

### Session 16: Splines
Non-parametric Regression (splines) We will use the mgcv library to fit the data with splines. In particular, we will be fitting a generalized additive model (GAM) and using the WH-ci to create confidence bands.

We can combine nonlinear and linear predictors (just add linear ones onto the GAMs), and the results can be interpreted with the summary() function where linear and nonlinear coefficients are split into 2 groups.

We can also create a *very* parameter-heavy model. Sometimes the penalty works well, other times it works very poorly and overfits. Plotting helps us notice overfitting behavior. In these cases, just give more reasonable starting parameters.

### Session 17: Missing Values
Linear regression is not as good at dealing with missing values. Handling them is not built into the method. Thus, the proper way to treat missing data: 
1. Determine an imputation distribution
2. Pick from it several times to fit several models. 
3. Average the coefficients.

We will use the mice package here. It's got clean documentation and workflow description: https://amices.org/mice/

Mice package workflow: 
1. use the mice() function to create 5+ replicates (e.g. m=5) of the data where the imputation has occurred on the missing values 
2. perform linear regression on all imputed replicates of the data and pool the data 
3. CI: because we have imputed, the se and confidence intervals are actually a little smaller for multiple imputation method compared to the deletion method. This is because we have a bit more precise estimates from imputing.

### Session 18 & 19: Logistic Regression
Here, we are implementing logistic regression. We must use the glm() function from ISLR2 library. The output is log odds, but we can convert it back to probabilities with the expit function -> expit(logit(p)) = p
1. glm() function and logit coeff interpretation
2. predictions convert back using expit() or predict(type='response',...) to skip that step.
3. beautiful plot curve
4. plotting with confidence band
5. Explore all 1-4 within the scope of general linear models
6. Can fit discrete variables into a continuous form using GAM. Revisit general linear model from # 5. and use GAM instead to allow predictors to be continuous
