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

#method printing the number that is the most common. If all numbers occur only once no mode is identified.
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

#method calculating the average of all the numbers within a given list and returning it.
average <- function(numberList){
    count <- 0
    total <- 0
    for(i in numberList){
        total <- total + i
        count <- count + 1
    }
    print(paste("Average is:", total/count))
    return(total / count)
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
    return(sortedList)
}

#This method will identify the median and print it. If it is even or odd the calculation is done accordingly.
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

#This method calculates the standard deviation of the provided list and prints it.
standardDeviation <- function(numberList) {
    if(debug) {
        print("Calculating the standard deviation")
    }
    average <- average(numberList)
    upperSum <- 0
    for(i in numberList) {
        if(debug) {
            print(paste(i,(i - average)^2))
        }
        upperSum <- upperSum + (i - average)^2
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

#=========================
#Execution configuration
#=========================

#Used to turn ON or OFF extract print statements for debugging.
debug <- FALSE
#debug <- TRUE

#Number of random digits to generate
numberOfDigits <- 100

#Maximum value of the generated digits
maxValue <- 100

#Random list generator based of the above 2 values.
inputList <- round(runif(numberOfDigits,0,maxValue), 0)
print ( inputList )

lowestValue(inputList)
highestValue(inputList)
mode(inputList)
#calulated during the standard deviation calculation.
#average(inputList)
medianValue(inputList)
standardDeviation(inputList)
