#################################################################
# Date Created:         10/25/2015                              #
#                                                               #
# Date Last Modified:   10/26/2015 - Added OverflowCounter      #
#                       12/2/2015 - Added Description           #
#                                                               #
# Purpose:  Used to calculate the data                          #
#################################################################

set.seed(10946) # The seed was different on each computer
maxCol <- 10 # Maximum r (when r is the rth record)
maxRow <- 1000 # Maximum data points to calculate
recordValue <- matrix(nrow=maxRow, ncol=maxCol) # Data frame for records
recordValuePosition <- matrix(nrow=maxRow, ncol=maxCol) # Date frame for times
rowNum <- 1 # Current row number

#########################################################################
# For each row, we calculate 10 records. The first data point is always #
# considered to be a record. The first record time, therefore, is       #
# considered to be 1.                                                   #
#########################################################################
for(i in 1:maxRow) {
  count <- 1
  iterationCounter <- 1
  overflowCounter <- 1
  recordValue[rowNum, count] <- rnorm(1, 0, 1)
  recordValuePosition[rowNum, count] <- iterationCounter
  count <- count + 1
  iterationCounter <- iterationCounter + 1
  
  #################################################################################
  # For our current row, loop until we have the desired r records. Simulate       #
  # standard normal data. If the data is higher than our current record,          #
  # it becomes a new record, and we store the time.                               #
  #################################################################################
  while(count <= maxCol) {
    x <- rnorm(1, 0, 1)
    if(x > recordValue[rowNum, count-1]) {
      recordValue[rowNum, count] <- x
      recordValuePosition[rowNum, count] <- iterationCounter
      count <- count + 1
    }
    #################################################
    # This if statement is bad and is only here so  #
    # that the simulations do not take days to do.  #
    #################################################
    if(overflowCounter > 30000000*count) {
      recordValue[rowNum, count] <- recordValue[rowNum, count-1]
      recordValuePosition[rowNum, count] <- 30000000*count
      count <- count + 1
      overflowCounter <- 1
    }
    iterationCounter <- iterationCounter + 1
    overflowCounter <- overflowCounter + 1
  }
  rowNum <- rowNum + 1
  print(i)
}