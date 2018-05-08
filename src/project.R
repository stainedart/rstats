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
    x1 <- (root * 1.0) / 2;
    x2 <- (x1 + (root / x1)) / 2;
    while(absoluteValue(x1 - x2) >= 0.0000001) {
        x1 = x2
        x2 = (x1 + (root / x1)) / 2
        print(x2)
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
    count <- 1
    total <- 0
    for(i in numberList){
        total <- total + i
        count <- count + 1
    }
    print(paste("Average is:", total/count))
}

#Execution configuration
#Used to turn ON or OFF extract print statements for debugging.
debug <- FALSE
#debug <- TRUE
#Number of random digits to generate
numberOfDigits <- 500
#Maximum value of the generated digits
maxValue <- 25000


inputList <- round(runif(numberOfDigits,0,maxValue), 0)
if(debug){
    print ( inputList )
}

lowestValue(inputList)
highestValue(inputList)
mode(inputList)
average(inputList)