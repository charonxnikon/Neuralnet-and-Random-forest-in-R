set.seed(5)

# Load data
data <- read.csv("train_var_15.csv",header=T)
data$f <- as.factor(data$f)

data_test <- read.csv("test_var_15.csv",header=T)

# Shuffle data
data_shuffled <- data[sample(nrow(data)),]

# Training the neural net
library(neuralnet)
nnDa <- neuralnet(f~x1+x2+x3,data=data, err.fct="sse",rep=2,hidden=c(2,2))
predict <- predict(nnDa,data_test)

# Converting predictions to correct format
library(ramify)

wtf <- unname(argmax(data.frame(predict)[1,]))
res <- c(1:200)

for (i in c(1:200))
{
  wtf <- unname(argmax(data.frame(predict)[i,]))
  if (wtf == 1)
    res[i]= "A"
  if (wtf == 2)
    res[i]= "B"
  if (wtf == 3)
    res[i]= "C"
  if (wtf == 4)
    res[i]= "D"

}
write.csv(res, "res.csv", row.names=FALSE)
