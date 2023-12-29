#GGPLOT:
### ggplot(main dataset for points) +
###  geom_point(aes(x,y,col=variable)) +                            RAW POINTS colored by variable
###  geom_line(aes(x,y,group=,col=), data=fitted)) +                FITTED LINE grouped and colored by same variable
###  geom_ribbon(aes(x,ymin,ymax,group,col), data=predictions) +    CONFIDENCE BAND grouped and colored by variable   
###  scale_color_viridis_b() + scale_fill_viridis)b(alpha)          CONFIDENCE BAND COLORING _color_ bins the cts data >  _fill_ maps it with alpha transparency, guide removes the legend for the conf. band 
trees <- read.csv("./Datasets/Tree_Equity_Scores_Tucson_noNA.csv")
trees_subset <- trees[trees$PCTSenior > 0 & trees$NAME != "University of Arizona", ] # take subset without 0 values for logit
pred_WH_MLR <- function(object, newdata, level = 0.95){
  n <- nrow(object$model) ## extract n from model object
  p <- object$rank
  
  # PW ci
  fit <- predict(object, newdata, interval = 'confidence') 
  ME_confidence <- fit[,'fit'] - fit[,'lwr'] 
  
  # rescale to WH ci
  alpha <- 1-level
  t_alpha <- qt(1-alpha/2, df = n-p)
  W_alpha <- sqrt(p * qf(level, n-p, p))
  
  ME <- ME_confidence*W_alpha/t_alpha
  
  upr <- fit[,'fit'] + ME
  lwr <- fit[,'fit'] - ME
  return(cbind(fit[,'fit'], lwr, upr))
}

PP_grid <- 0:100
PC_grid <- quantile(trees_subset$PCTChildren, probs = seq(0.125, 0.875, 0.25)) # create PCTChildren grid (4 values indicating diff quantiles)
newdata <- expand.grid(HeatSeverity = median(trees_subset$HeatSeverity),
                       PCTPoverty = PP_grid, PCTChildren = PC_grid)
logit <- function(x) log(x/(1-x))
expit <- function(x) 1 / (1 + exp(-x)) # function: inverse of logit to remap onto original scale
fit_logit2 <- lm(I(logit(PCTSenior/100)) ~ HeatSeverity + PCTChildren * PCTPoverty,
                 data = trees_subset)
#############################################################################################
predictions <- as.data.frame(cbind(newdata, pred_WH_MLR(fit_logit2, newdata = newdata)))
colnames(predictions)[4] <- "fit"

library(ggplot2)
ggplot(data = trees_subset) +
  geom_point(aes(x = PCTPoverty, y = PCTSenior, col = PCTChildren)) +
  geom_line(aes(x = PCTPoverty, y = expit(fit) * 100, group = PCTChildren, col = PCTChildren), data = predictions) +
  geom_ribbon(aes(x = PCTPoverty, ymin = expit(lwr) * 100, ymax = expit(upr) * 100,
                  group = PCTChildren, fill = PCTChildren), data = predictions) +
  scale_color_viridis_b() + scale_fill_viridis_b(alpha = 0.2, guide = "none")

# base R
# 1. cut continuous data
breaks <- quantile(trees$PCTChildren, probs = seq(0,1,.25)) # break data into equal chunks
PCTChildren_cut <- cut(trees$PCTChildren,breaks) # converts value to a level (one of the chunks)

# 2. create colors vector
colors <- hcl.colors(4, alpha = .7, palette = "viridis") # with other colors

# 3a. plot points
plot(PCTSenior ~ PCTPoverty, data = trees_subset, pch = 16, col = colors[PCTChildren_cut])

# 3b. confidence bands and regression line
for(PC in PC_grid){
  lines(PP_grid, 100 * expit(predictions[newdata$PCTChildren == PC, 'fit']),
        col = colors[which(PC == PC_grid)], lwd = 2)
  polygon(x = c(PP_grid, rev(PP_grid)),
          y = 100 * expit(c(predictions[newdata$PCTChildren == PC, 'lwr'],
                            rev(predictions[newdata$PCTChildren == PC, 'upr']))),
          col = adjustcolor(colors[which(PC == PC_grid)], 0.3), border = NA)
}
legend("topright", pch = 16, col = colors,
       legend = levels(PCTChildren_cut), title = "PCTChildren")

