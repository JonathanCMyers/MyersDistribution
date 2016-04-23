#################################################################
# Date Created:         12/2/2015                               #
# Date Last Modified:   12/2/2015                               #
# Purpose:  Used to generate graphs of the data                 #
#################################################################

#install.packages("ggplot2")
library(ggplot2)
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

col1 <- data.frame(val=log(recordValuePosition[1:46099,1]))
col2 <- data.frame(val=log(recordValuePosition[1:46099,2]))
col3 <- data.frame(val=log(recordValuePosition[1:46099,3]))
col4 <- data.frame(val=log(recordValuePosition[1:46099,4]))
col5 <- data.frame(val=log(recordValuePosition[1:46099,5]))
col6 <- data.frame(val=log(recordValuePosition[1:46099,6]))
col7 <- data.frame(val=log(recordValuePosition[1:46099,7]))
col8 <- data.frame(val=log(recordValuePosition[1:46099,8]))
col9 <- data.frame(val=log(recordValuePosition[1:46099,9]))
col10 <- data.frame(val=log(recordValuePosition[1:46099,10]))

lev <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")

col1$position <- factor("1", levels=lev)
col2$position <- factor("2", levels=lev)
col3$position <- factor("3", levels=lev)
col4$position <- factor("4", levels=lev)
col5$position <- factor("5", levels=lev)
col6$position <- factor("6", levels=lev)
col7$position <- factor("7", levels=lev)
col8$position <- factor("8", levels=lev)
col9$position <- factor("9", levels=lev)
col10$position <- factor("10", levels=lev)

colCombined <- rbind(col1, col2, col3, col4, col5, col6, col7, col8, col9, col10)
combinedGraph <- ggplot(colCombined, aes(val, fill=position)) + geom_density(alpha=0.35)
combinedGraph + coord_cartesian(xlim=c(0, 12), ylim=c(0, .8))

latterCombined <- rbind(col4, col5, col6, col7)
latterGraph <- ggplot(latterCombined, aes(val, fill=position)) + geom_density(alpha=0.35)
latterGraph

latterCombined <- rbind(col6, col7, col8, col9, col10)
latterGraph <- ggplot(latterCombined, aes(val, fill=position)) + geom_density(alpha=0.35)
latterGraph

#######################
# Log 3rd Record Time #
#######################
rx <- log(recordValuePosition[1:46099,3])
rn <- rlnorm(n=1000000, meanlog=log(3-1), sdlog=.48) 

# Plots in ggplot
drx <- data.frame(val=rx)
drn <- data.frame(val=rn)

fact <- c("Data", "LogNormal")

drx$type <- factor("Data", levels=fact)
drn$type <- factor("LogNormal", levels=fact)

dCombined <- rbind(drx, drn)
combinedGraph <- ggplot(dCombined, aes(val, fill=type)) + geom_density(alpha=0.34) + ggtitle("Record 3")
combinedGraph

#######################
# Log 4th Record Time #
#######################
rx <- log(recordValuePosition[1:46099,4])
rn <- rlnorm(n=1000000, meanlog=log(4-1), sdlog=.447) 

# Plots in ggplot
drx <- data.frame(val=rx)
drn <- data.frame(val=rn)

fact <- c("Data", "LogNormal")

drx$type <- factor("Data", levels=fact)
drn$type <- factor("LogNormal", levels=fact)

dCombined <- rbind(drx, drn)
combinedGraph <- ggplot(dCombined, aes(val, fill=type)) + geom_density(alpha=0.34) + ggtitle("Record 4")
combinedGraph

#######################
# Log 5th Record Time #
#######################
rx <- log(recordValuePosition[1:46099,5])
rn <- rlnorm(n=1000000, meanlog=log(5-1), sdlog=.44) 

# Plots in ggplot
drx <- data.frame(val=rx)
drn <- data.frame(val=rn)

fact <- c("Data", "LogNormal")

drx$type <- factor("Data", levels=fact)
drn$type <- factor("LogNormal", levels=fact)

dCombined <- rbind(drx, drn)
combinedGraph <- ggplot(dCombined, aes(val, fill=type)) + geom_density(alpha=0.34) + ggtitle("Record 5")
combinedGraph

#######################
# Log 6th Record Time #
#######################
rx <- log(recordValuePosition[1:46099,6])
rn <- rlnorm(n=1000000, meanlog=log(6-1), sdlog=.4) 

# Plots in ggplot
drx <- data.frame(val=rx)
drn <- data.frame(val=rn)

fact <- c("Data", "LogNormal")

drx$type <- factor("Data", levels=fact)
drn$type <- factor("LogNormal", levels=fact)

dCombined <- rbind(drx, drn)
combinedGraph <- ggplot(dCombined, aes(val, fill=type)) + geom_density(alpha=0.34) + ggtitle("Record 6")
combinedGraph

#######################
# Log 7th Record Time #
#######################
rx <- log(recordValuePosition[1:46099,7])
rn <- rlnorm(n=1000000, meanlog=log(7-1), sdlog=.38) 

# Plots in ggplot
drx <- data.frame(val=rx)
drn <- data.frame(val=rn)

fact <- c("Data", "LogNormal")

drx$type <- factor("Data", levels=fact)
drn$type <- factor("LogNormal", levels=fact)

dCombined <- rbind(drx, drn)
combinedGraph <- ggplot(dCombined, aes(val, fill=type)) + geom_density(alpha=0.34) + ggtitle("Record 7")
combinedGraph

#######################
# Log 8th Record Time #
#######################
rx <- log(recordValuePosition[1:46099,8])
rn <- rlnorm(n=1000000, meanlog=log(8-1), sdlog=.349)

# Plots in ggplot
drx <- data.frame(val=rx)
drn <- data.frame(val=rn)

fact <- c("Data", "LogNormal")

drx$type <- factor("Data", levels=fact)
drn$type <- factor("LogNormal", levels=fact)

dCombined <- rbind(drx, drn)
combinedGraph <- ggplot(dCombined, aes(val, fill=type)) + geom_density(alpha=0.34) + ggtitle("Record 8")
combinedGraph

#######################
# Log 9th Record Time #
#######################
rx <- log(recordValuePosition[1:46099,9])
rn <- rlnorm(n=1000000, meanlog=2.07944, sdlog=.334)

# Plots in ggplot
drx <- data.frame(val=rx)
drn <- data.frame(val=rn)

fact <- c("Data", "LogNormal")

drx$type <- factor("Data", levels=fact)
drn$type <- factor("LogNormal", levels=fact)

dCombined <- rbind(drx, drn)
combinedGraph <- ggplot(dCombined, aes(val, fill=type)) + geom_density(alpha=0.34) + ggtitle("Record 9")
combinedGraph

########################
# Log 10th Record Time #
########################
rx <- log(recordValuePosition[1:46099,10])
rn <- rlnorm(n=1000000, meanlog=log(10-1), sdlog=.318) #.305
hist(rx, breaks=300)
hist(rn, breaks=300)

# Plots in ggplot
drx <- data.frame(val=rx)
drn <- data.frame(val=rn)

fact <- c("Data", "LogNormal")

drx$type <- factor("Data", levels=fact)
drn$type <- factor("LogNormal", levels=fact)

dCombined <- rbind(drx, drn)
combinedGraph <- ggplot(dCombined, aes(val, fill=type)) + geom_density(alpha=0.34) + ggtitle("Record 10")
combinedGraph


# Predicting log of distribution for record times at r=20
r <- 20
rn <- rlnorm(n=1000000, meanlog=log(r-1), sdlog=(1/r)^.5) 
fact <- c("Data", "LogNormal")
drn <- data.frame(val=rn)
drn$type <- factor("LogNormal", levels=fact)

combinedGraph <- ggplot(drn, aes(val, fill=type)) + geom_density(alpha=0.55) + ggtitle("Record 20 Prediction")
combinedGraph
exp(mean(rn))
