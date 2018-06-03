#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

#Function returning the absolute value of a given number.
absoluteValue=function(number){
    if(number > 0) {
        return(number)
    } else {
        return(number * -1)
    }
}

#Use to find the square root of a given number
squareRoot = function(root) {
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

#method counting the number of occurences of a given number within a list and returning it.
listContains <- function(value, list){
    counter <- 0
    for(i in list){
        if(i == value){
            counter <- counter + 1
        }
    }
    return(counter)
}

#This method will sort backwards the content of the supplied list and return it.
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
    print(paste("Highest value:",sortedList[0]))
    print(paste("Lowest value:",sortedList[length(sortedList)]))
    return(sortedList)
}

#===========================================================================
#Statistics class definition followed by method assignment.
#===========================================================================
setClass("Statistics", representation(minValue = "numeric", maxValue = "numeric", randomList = "numeric", sortedList = "list"))

#Method calculating the average value of the randomList provided in the Statistics object.
setGeneric(name="average",
    def=function(theObject)
    {
        standardGeneric("average")
    }
)
setMethod(f="average", signature="Statistics",
    definition=function(theObject) {
        count <- 0
        total <- 0
        for(i in theObject@randomList){
            total <- total + i
            count <- count + 1
        }
        print(paste("Average is:", total/count))
        return(total / count)
    }
)

#method printing the number that is the most common. If all numbers occur only once no mode is identified.
setGeneric(name="mode",
    def=function(theObject) {
        standardGeneric("mode")
    }
)
setMethod(f="mode", signature="Statistics",
    definition=function(theObject){
        k <- 1
        occurences <- 1
        mode <- theObject@randomList[1]
        for(i in theObject@randomList) {
            occurences <- listContains(i,theObject@randomList)
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
)

#This method will identify the median and print it. If it is even or odd the calculation is done accordingly.
setGeneric(name="medianValue",
    def=function(theObject) {
        standardGeneric("medianValue")
    }
)
setMethod(f="medianValue", signature="Statistics",
    definition=function(theObject){
        #Be careful the list is backwards
        theObject@sortedList <- sortBasic(theObject@randomList)
        listLength <- length(theObject@sortedList)
        if((listLength %% 2) == 1){
            #odd
            if(debug){
                print("odd list length")
                print(ceiling(listLength / 2))
            }
            print(paste("The median is",theObject@sortedList[ceiling(listLength / 2)]))

        } else {
            #even
            median1 <- listLength / 2
            median2 <- (listLength / 2) + 1
            if(debug){
                print("even list length")
                print(paste("median 1 position", median1, "median 2 position", median2))
            }
            median1 <- unlist(theObject@sortedList[median1], use.names=FALSE)
            median2 <- unlist(theObject@sortedList[median2], use.names=FALSE)
            m <- ((median1[1] + median2[1]) / 2)
            print(paste("The median is", m))
        }
    }
)

#This method calculates the standard deviation of the provided list and prints it.
setGeneric(name="standardDeviation",
    def=function(theObject) {
        standardGeneric("standardDeviation")
    }
)
setMethod(f="standardDeviation", signature="Statistics",
    definition=function(theObject){
        if(debug) {
            print("Calculating the standard deviation")
        }
        average <- average(theObject)
        upperSum <- 0
        for(i in theObject@randomList) {
            if(debug) {
                #print(paste(i,(i - average)^2))
            }
            upperSum <- upperSum + (i - average)^2
        }
        denominator <- length(theObject@randomList) #- 1
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
)

#=========================
#Execution configuration
#=========================

#Used to turn ON or OFF extract print statements for debugging.
debug <- FALSE
#debug <- TRUE

#Number of random digits to generate
numberOfDigits <- 100

#Maximum value of the generated digits
maxDigitValue <- 100


#=========================
#Handling command line arguments
#=========================
if (length(args)==0) {
    stop("Please supply the number of digits to be generated, the maximum value of the digits to be generated and the debug value .n", call.=FALSE)
} else {
    numberOfDigits <- as.numeric(args[1])
    maxDigitValue <- as.numeric(args[2])
    debug <- as.logical(args[3])
}
print(paste("Number of digits:", numberOfDigits))
print(paste("Maximum value of digits:", maxDigitValue))
print(paste("Debug Value:", debug))


#=========================
#Creating a Statistics instance and calculating the DESCRIPTIVE_STATISTICS
#=========================
stats <- new("Statistics")
stats@randomList <- round(runif(numberOfDigits,0,maxDigitValue), 0)
# calulated during the standard deviation calculation.
# average(stats)
mode(stats)
medianValue(stats)
standardDeviation(stats)
