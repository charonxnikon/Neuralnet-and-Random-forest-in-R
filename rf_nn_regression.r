library(zoo)
library(neuralnet)
library(randomForest)

data <- read.csv("VAR5.csv", header=TRUE, sep = ';')
data <- data[ ,3:6]

r <- apply(data, 2, function(z) rollapply(z, 2, function(x) x[2]/x[1] - 1))


calc_rmse <- function(n)
{
  nn <- neuralnet(ROSNEFT ~ ., 
                  data = r,
                  err.fct = 'sse', 
                  hidden = n)
  pred <- predict(nn, newdata = r[ ,2:4], type = 'response')
  return( sqrt( mean( (r[ ,1] - pred)^2 ) ) )
}

set.seed(0)
err <- sapply(1:30, calc_rmse)
plot(err, type = 'b', lwd = 2, xlab = 'Number of Neurons in the Hidden Layer',
     ylab = 'RMSE')
best_nhidden <- which.min(err)

nn <- neuralnet(ROSNEFT ~ ., 
                data = r,
                err.fct = 'sse', 
                hidden = best_nhidden,
                rep = 2)
plot(nn, rep = 'best')
nn_pred <- predict(nn, newdata = r[ ,2:4], type = 'response')
plot(r[ ,1], type = 'l', lwd = 2, xlab = 'Time', ylab = 'ROSNEFT Return', 
     main = 'Neural Network Prediction')
lines(nn_pred, col = 'red', lwd = 2)

set.seed(0)
rf <- randomForest(ROSNEFT ~ .,
                   data = r,
                   ntree = 500)
prf <- plot(rf)
best_ntree <- which.min(prf)
abline(h = prf[best_ntree], col = 'blue')

best_rf <- randomForest(ROSNEFT ~ .,
                        data = r,
                        ntree = best_ntree)
rf_pred <- predict(best_rf, newdata = r[ ,2:4])
plot(r[ ,1], type = 'l', lwd = 2, xlab = 'Time', ylab = 'ROSNEFT Return', 
     main = 'Random Forest Prediction')
lines(rf_pred, col = 'red', lwd = 2)

lm <- lm(ROSNEFT ~ ., data = as.data.frame(r))
lm_pred <- predict(lm, newdata = as.data.frame(r[ ,2:4]))
plot(r[ ,1], type = 'l', lwd = 2, xlab = 'Time', ylab = 'ROSNEFT Return', 
     main = 'Linear Model Prediction')
lines(lm_pred, col = 'red', lwd = 2)

get_prices <- function(start, r)
{
  price <- rep(NA, length(r) + 1)
  price[1] <- start
  for(i in 2:length(price))
    price[i] <- (r[i-1] + 1)*price[i-1]
  return(price)
}

plot_price <- function(orig, pred, name)
{
  price <- get_prices(orig[1], pred)
  rmse <- sqrt( mean( (orig - price)^2 ) )
  plot(orig, type = 'l', lwd = 2, xlab = 'Time', ylab = 'ROSNEFT',
       main = paste(name, 'RMSE', round(rmse, 2)))
  lines(price, lwd = 2, col = 'red')
}

plot_price(data[ ,1], nn_pred, 'Neural Network')
plot_price(data[ ,1], rf_pred, 'Random Forest')
plot_price(data[ ,1], lm_pred, 'Linear Model')
