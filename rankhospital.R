## 3 arguments: 
## 1) state - state you wish to check
## 2) outcome - health problem
## 3) num - ranking of a hospital in that state;
## num can take "best", "worst" or integer of rank; EX: 5 = 5th lowest
## num larger than numbers returns NA




rankhospital <- function(state, outcome, num = "best")
{
    ## 1) Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
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
    
    
    
    
    ## 4) Ordering Hospitals
    
    ## 4a) Store rows only in state
    data2 <- data[data["State"] == state, ]
    
    ## 4b) Make column variable
    ID <- numeric()
    if(outcome == "heart attack")        { ID <- 11 }
    else if(outcome == "heart failure")  { ID <- 17 }
    else if(outcome == "pneumonia")      { ID <- 23 }
    
    
    
    ## 4c) Get rid of "Not Available"
    data3 <- data2[data2[ID] != "Not Available", ][c(2, ID)]
    
    ## 4d) Check num
    if(num == "best")                   { num <- 1 }
    else if(num == "worst")             { num <- nrow(data3) }
    else if(num > nrow(data2))          { return(NA) }
    else if(class(num) != "numeric")    { return(NA) }
    
    ## 4e) Sort by changing column type
    data3[,2] <- as.numeric(as.character(data3[,2]))
    data3 <- data3[with(data3, order(data3[,2], data3[,1] ) ), ]
    
    
    
    ## 5) Print based on rank
    FINAL <- as.character(data3[num, 1])
    FINAL
 
}








