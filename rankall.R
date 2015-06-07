## Function takes 2 arguments
## 1) outcome - name of ailment
## 2) num - rank <- 


rankall <- function(outcome, num = "best")
{
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv")
    
    ## Check that state and outcome are valid
    ## a) State
    statelist <- data[ !duplicated(data["State"]), ][["State"]]
    statelist <- sort(as.character(statelist))
    
    ## b) Outcome
    outcomeList <- c("heart attack", "heart failure", "pneumonia")
    outcomeBoolean <- FALSE
    for(i in 1:length(outcomeList))
    {
        if(outcomeList[i] == outcome)
            outcomeBoolean <- TRUE
    }
    
    if(outcomeBoolean == FALSE) stop("invalid outcome")
    
    ## c) Set outcome column
    columnID <- numeric()
    if(outcome == "heart attack")       { columnID <- 11 }
    else if(outcome == "heart failure") { columnID <- 17 }
    else if(outcome == "pneumonia")     { columnID <- 23 }
    
    
    ## For each state, find the hospital of the given rank
    ## a) Create dataframe
    x <- data.frame(hospital = 1:length(statelist), state=statelist, 
                    row.names = statelist)
    
    
    ## b) Insert names into data frame
    for(i in 1:length(statelist))
    {
        currentState <- statelist[i]
        data2 <- data[data["State"] == currentState, ]
        data3 <- data2[data2[columnID] != "Not Available", ][c(2,7,columnID)]
        
        data3[,3] <- as.numeric(as.character(data3[,3]))
        data3 <- data3[with(data3, order(data3[,3], data3[,1] ) ), ]
        
        ## Check Num
        num2 <- numeric()
        if(num == "best")           { num2 <- 1 }
        else if(num == "worst")     { num2 <- nrow(data3)}
        else                        { num2 <- num }
        
        
        ## Finish dataframe
        temp <- as.character(data3[num2, 1])
        if(is.na(temp)) { x[i,1] <- "<NA>" }
        else            { x[i,1] <- temp }
        
    }
    
    ## Return a data frame with the hospital names and the state name
    x
    
}