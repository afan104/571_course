interaction_plot <- function(aov, ...){
  n_treatments <- length(aov$xlevels[[1]])
  n_groups <- length(aov$xlevels[[2]])
  treatment <- aov$model[, 2]; group <- aov$model[, 3]; response <- aov$model[, 1]
  x_names <- names(aov$xlevels)
  x_names <- gsub(pattern = "as.factor\\(", replacement = "", x_names)
  x_names <- gsub(pattern = "\\)", replacement = "", x_names)
  group_cols <- RColorBrewer::brewer.pal(n_groups, name = "Dark2")
  interaction.plot(treatment, group, response, type = "p", 
                   xlab = x_names[1], ylab = names(aov$model)[1],
                   col = group_cols, legend = T, pch = 1:n_groups, ...)
  if(length(aov$xlevels) == 2){
    for(i in 2:n_treatments){
      y0data <- data.frame(aov$xlevels[[1]][i - 1], aov$xlevels[[2]])
      y1data <- data.frame(aov$xlevels[[1]][i], aov$xlevels[[2]])
      names(y0data) <- names(y1data) <- x_names
      segments(x0 = rep(i - 1, n_groups), 
               x1 = rep(i, n_groups),
               y0 = predict(aov, newdata = y0data),
               y1 = predict(aov, newdata = y1data),
               col = group_cols, lty = 1:n_groups)
    }
  } else if(length(aov$xlevels) == 3){
    for(i in 2:n_treatments){
      y0data <- expand.grid(aov$xlevels[[1]][i - 1], aov$xlevels[[2]], aov$xlevels[[3]])
      y1data <- expand.grid(aov$xlevels[[1]][i], aov$xlevels[[2]], aov$xlevels[[3]])
      names(y0data) <- names(y1data) <- x_names
      y0data$pred <- predict(aov, newdata = y0data)
      y1data$pred <- predict(aov, newdata = y1data)
      segments(x0 = rep(i - 1, n_groups), 
               x1 = rep(i, n_groups),
               y0 = aggregate(y0data$pred, by = list(y0data[[3]]), mean)$x,
               y1 = aggregate(y1data$pred, by = list(y1data[[3]]), mean)$x,
               col = group_cols, lty = 1:n_groups)
    }
  } else {stop("Functionality for more than 3 predictors not available.")}
}