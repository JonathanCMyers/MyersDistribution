#################################################################
# Date Created:         11/6/2015                               #
#                                                               #
# Date Last Modified:   11/6/2015 -                             #
#                       12/2/2015 - Added Description           #
#                                                               #
# Purpose:              Simulating from Myers Distribution      #
#################################################################

fx <- function(x, r) { 
  rconst <- (r/(2*pi))^.5 
  expon <- log(log(x)/(r-1))*(r/2) 
  val <- (1/(x*log(x)))*rconst*((r-1)/(log(x)))^expon 
  return(val) 
} 

r <- 6
alpha0.1UB <- 0
x <- 2
counter <- 0
while(alpha0.1UB < .9) {
  alpha0.1UB <- alpha0.1UB + fx(x, r)
  x <- x + 1
  counter <- counter + 1
  if(counter > 1000) {
    print(x)
    counter <- 0
  }
}
# x = 410436
# 99% of the data is below 410,436
# x = 4616
# 90% of the data is below 4,616

r <- 10
pointsPerX <- 2000
counter <- 0
freqlength <- 10000
ub <- max(fx(2:200,r))
print(ub)
freq <- numeric(freqlength) # rep(0,4616)
for(i in 2:freqlength) { # was 2:4616
  for(j in 1:pointsPerX) {
    c <- runif(1, 0, ub*1.05)
    if(c < fx(i, r)) {
      freq[i] <- freq[i] + 1
    }
  }
  counter <- counter + 1
  if(counter > 1000) {
    counter <- 0
    print(i)
  }
}
pb2 <- recordValuePosition[1:46099,r]
plot(freq)
plot(pb2[pb2 < freqlength])
hist(pb2[pb2 < freqlength], breaks=100)

