# Function to pring the lowest value of the provided list.
lowestValue <- function(a) {
   low <- a[1]
   for(i in a) {
   	 if(i < low){
        if(debug){
            print(paste(i, "is lower than", low))
        }
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
            if(debug){
                print(paste(i,"is higher than",high))
            }
            high <- i
        }
    }
    print(paste("Highest value:", high))
}

#Function returning the absolute value of a given number.
absoluteValue <- function(number){
    if(number > 0) {
        return(number)
    } else {
        return(number * -1)
    }
}

#Use to find the square root of a given number
squareRoot <- function(root) {
    if(debug) {
        print("Starting the square root calculation")
    }
    x1 <- (root * 1.0) / 2;
    x2 <- (x1 + (root / x1)) / 2;
    while(absoluteValue(x1 - x2) >= 0.0000001) {
        x1 = x2
        x2 = (x1 + (root / x1)) / 2
        if(debug) {
            print(paste("Square root approximation",x2))
        }
    }
    return(x2)
}

listContains <- function(value, list){
    counter <- 0
    for(i in list){
        if(i == value){
            counter <- counter + 1
        }
    }
    return(counter)
}

mode <- function(numberList){
    #modeCounter <- list()
    k <- 1
    occurences <- 1
    mode <- numberList[1]
    for(i in numberList) {
        occurences <- listContains(i,numberList)
        if(occurences > k){
            mode <- i
            k <- occurences
            if(debug){
                print(paste("New mode identified:", mode, "occurred:", occurences))
            }
        }
    }
    if(k == 1){
        print("No mode")
    } else {
        print(paste("Mode:",mode, "occurred", k, "times"))
    }
}

average <- function(numberList){
    count <- 0
    total <- 0
    for(i in numberList){
        total <- total + i
        count <- count + 1
    }
    print(paste("Average is:", total/count))
    return <- total / count
}

sortBasic <- function(numberList){
    sortedList <- list()
    outerCounter <- 1
    for(i in numberList){
        #sortedList[outerCounter] <- highestValue(numberList)
        highIndex <- 1
        high <- numberList[highIndex]
        innerCounter <- 1
        for(m in numberList) {
            if(m > high){
                if(debug){
                    print(paste(m,"is higher than",high))
                }
                high <- m
                highIndex <- innerCounter
            }
            innerCounter <- innerCounter + 1
        }
        sortedList[outerCounter] <- high
        numberList[highIndex] <- 0
        outerCounter <- outerCounter + 1
    }
    if(debug) {
        print(sortedList)
    }
    return(sortedList)
}

medianValue <- function(numberList) {
    #Be careful the list is backwards
    sortedList <- sortBasic(numberList)
    listLength <- length(sortedList)
    if((listLength %% 2) == 1){
        #odd
        if(debug){
            print("odd list length")
            print(ceiling(listLength / 2))
        }
        print(paste("The median is",sortedList[ceiling(listLength / 2)]))

    } else {
        #even
        median1 <- listLength / 2
        median2 <- (listLength / 2) + 1
        if(debug){
            print("even list length")
            print(paste("median 1 position", median1, "median 2 position", median2))
        }
        median1 <- unlist(sortedList[median1], use.names=FALSE)
        median2 <- unlist(sortedList[median2], use.names=FALSE)
        m <- ((median1[1] + median2[1]) / 2)
        print(paste("The median is", m))
    }
}

standardDeviation <- function(numberList) {
    if(debug) {
        print("Calculating the standard deviation")
    }
    average <- average(numberList)
    upperSum <- 0
    for(i in numberList) {
        upperSum <- (i - average)^2
    }
    denominator <- length(numberList) #- 1
    if(debug) {
        print(paste("Upper sum squared", upperSum))
        print(paste("Denominator", denominator))
    }
    variance <- upperSum / denominator
    if(debug) {
        print(paste("Variance", variance))
    }
    print(paste("Standard deviation", squareRoot(variance)))
}

#Execution configuration
#Used to turn ON or OFF extract print statements for debugging.
debug <- FALSE
debug <- TRUE
#Number of random digits to generate
numberOfDigits <- 4
#Maximum value of the generated digits
maxValue <- 25000


inputList <- round(runif(numberOfDigits,0,maxValue), 0)
inputList <- list(1,3,4,7,8)
if(debug){
    print ( inputList )
}
lowestValue(inputList)
highestValue(inputList)
mode(inputList)
average(inputList)
medianValue(inputList)

standardDeviation(inputList)
