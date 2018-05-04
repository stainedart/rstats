# Function to pring the lowest value of the provided list.
lowestValue <- function(a) {
   low <- a[1]
   for(i in a) {
   	 if(i < low){
         print(paste(i, "is lower than", low))
       low <- i
     }
   }
    print(paste("Lowest value:", low))
}

# Function to pring the highest value of the provided list.
highestValue <- function(a) {
    high <- a[1]
    for(i in a) {
        if(i > high){
            print(paste(i,"is higher than",high))
            high <- i
        }
    }
    print(paste("Highest value:", high))
}


inputList <- round(runif(100,0,10000), 0)
print ( inputList )

lowestValue(inputList)
highestValue(inputList)

