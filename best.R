## Finding the best hospital in a state

## best takes 2 arguments: 
## 1) 2-character abbreviated name of a state
## 2) outcome name

## The function reads "outcome-of-care-measures.csv" returns character 
## vector with name of the hospital that has the best
## (lowest) 30-day mortality for the specified outcome in that state.

## Outcomes can be one of "heart attack". "heart failure", or 
## "pneumonia". 

## Hospitals that do not have data on a particular outcome should be 
## excluded from the set of hospitals when deciding the rankings.

## Hospital name is the name provides in the "Hospital.Name" variable

## In even of a tie, the first alphabetical choice should be chosen
## EX. Hospital b, c & d tie, hospital b wins and is returned


best <- function(state, outcome)
{
    ## 1) Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")

    ## 2) Check State
    ## 2a) Get vector or states
    statelist <- data[ !duplicated( data["State"] ), ] [["State"]]
    statelist <- as.character(statelist)
    sort(statelist)
    
    ## 2b) Traverse for state
    stateBoolean <- FALSE
    for(i in 1:length(statelist))
    {
        if(statelist[i] == state) 
        {
            stateBoolean <- TRUE
            break
        }
    }
    
    if(stateBoolean == FALSE) 
    { stop("invalid state") }
    
    ## 3) Check Outcome
    outcomeList <- c("heart attack", "heart failure", "pneumonia")
    outcomeBoolean <- FALSE
    for(i in 1:length(outcomeList))
    {
        if(outcomeList[i] == outcome)
        {
            outcomeBoolean <- TRUE
        }
    }
    
    if(outcomeBoolean == FALSE) { stop("invalid outcome") }
    
    
    ## 4) Return hospital name in that state with lowest 30-day death
    ID <- numeric()
    if(outcome == "heart attack")        { ID <- 11 }
    else if(outcome == "heart failure")  { ID <- 17 }
    else if(outcome == "pneumonia")      { ID <- 23 }
    
    data2 <- data[data["State"] == state ,]
    data3 <- data2[data2[ID] != "Not Available",][c(2,ID)]
    
    ## 5) Find minimum value
    BEST <- format(min(as.numeric(as.character(data3[,2]))), nsmall = 1)
    hospitalName <- as.character(data3[data3[,2] == BEST, ][[1]])
    hospitalName
}






