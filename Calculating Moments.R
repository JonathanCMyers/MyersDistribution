#######################
# Calculating Moments #
#######################

path = "E:/RDir/Record Values/Record Value Positions"
setwd("E:/RDir/Record Values/Record Value Positions")
recordValuePosition <- matrix(nrow=0, ncol=10)
file.names <- dir(path, pattern=".txt")

# Bind all of the record value position data together
for(i in 1:length(file.names)) {
  file <- read.table(file.names[i])
  file <- as.matrix(file)
  colnames(file) <- NULL
  recordValuePosition <- rbind(recordValuePosition, file) 
}

mean(recordValuePosition[1:46099,6])
r <- 8
exp(mean(log(recordValuePosition[1:46099,r])))
#mean(recordValuePosition[1:46099,r])
exp(exp(log(r-1) + (1/2)*(1/r)))


#2 mu + 2 sigma^2

r <- 6
exp(1)*pnorm((log(r-1)/((1/r)^.5)))
