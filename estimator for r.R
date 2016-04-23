############################
# Estimator for rth record #
############################

path = "E:/RDir/Record Values/Record Value Positions"
setwd("E:/RDir/Record Values/Record Value Positions")
recordValuePosition <- matrix(nrow=0, ncol=10)
file.names <- dir(path, pattern=".txt")
for(i in 1:length(file.names)) {
  file <- read.table(file.names[i])
  file <- as.matrix(file)
  colnames(file) <- NULL
  recordValuePosition <- rbind(recordValuePosition, file) 
}

# Using mean
for(v in 2:10) {
  sum <- 1
  n <- nrow(recordValuePosition)
  for(i in 1:n) {
    sum <- sum * (log(recordValuePosition[i,v])^(1/n))
  }
  print(sum+1)
}

# Estimator for median
for(r in 2:10) {
  print(paste("Estimated:", exp(r-1), "  Actual:", median(recordValuePosition[1:46099,r])))
}

r <- 6
names(sort(-table(recordValuePosition[1:46099,r])))[1] #MODE
exp(exp(log(r-1)-(1/r))) #Apparently NOT THE MODE

# Using T2=sum(i=1 to n)(log(log(x)))
r <- 6
sum <- 0
for(i in 1:n) {
  sum <- sum + log(log(recordValuePosition[i,r])) 
}
sum <- 1/sum
sum <- sum^2

# Reasonable statistics for estimating the population median
# are M1 = the sample median, and the maximum likelihood
# estimator for M2 = exp(exp(muhat)). 
