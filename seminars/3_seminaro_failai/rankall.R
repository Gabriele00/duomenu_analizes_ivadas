rankall <- function(outcome, num="best"){
        
        # reads in csv file
        df <- read.csv("outcome-of-care-measures.csv",
                       header = TRUE,
                       sep=",",
                       stringsAsFactors = FALSE,
                       na.strings = "Not Available")
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalide outcome")
        }
        
        if(outcome=="heart attack"){
                df <- df[,c(2,7,11)]
        } else if(outcome=="heart failure"){
                df<- df[,c(2,7,17)]
        } else {
                df<- df[,c(2,7,23)]
        }
        
        #selects only rows with existing values
        df <- df[!is.na(df[,3]),]
        
        #orders rows by value and by hospital name in descdening order
        df <- df[order(df[,2],df[,3], df[,1]),]
        
        # splits dataframe and calculates rank for each subset
        s <- split(df, df$State)
        df$rank <- unlist(lapply(s, function(x) rank(x[,3], ties.method = "first")))
        
        
        if(is.numeric(num)){
                df[df[,4]==num,c(1,2)] 
        } else if(num=="best"){
                df[df[,4]==1,c(1,2)]
        } else if(!num=="worst"){
                stop("num is incorrect") 
        }else{
                dfc <- data.frame(Hospital.name=NA, State=NA)
                s <- split(df, df$State)
                for(i in 1:length(s)){
                        dfc[i,1] <- s[[i]][nrow(s[[i]]),1]
                        dfc[i,2] <- s[[i]][nrow(s[[i]]),2]
                }
                dfc
        }
        
}




