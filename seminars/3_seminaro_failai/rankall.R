rankall <- function(outcome, num="best"){
        
        df <- read.csv("outcome-of-care-measures.csv", 
                       stringsAsFactors = FALSE, 
                       na.strings = "Not Available")
        
        if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
                stop("invalid outcome")
        }
        
        if(outcome=="heart attack"){
                df <- df[,c(2,7,11)]
        }else if(outcome=="heart failure"){
                df <- df[,c(2,7,17)]
        }else{
                df <- df[,c(2,7,23)]}
        df <- df[!is.na(df[,3]),]
        df <- df[order(df[,2], df[,3], df[,1]),]
        df$rank <-unlist(
                lapply(split(df, df$State), function(x) rank(x[order(x[,3], x[,1]),][,3], ties.method = "first" ))
        )
        
        if(is.numeric(num)){
                dfc <- df[df[,4]==num, c(1,2)]
        } else if(num=="best"){
                dfc <- df[df[,4]==1, c(1,2)]
        } else{
                dfc <- data.frame(Hospital.name=NA, State=NA)
                a <- split(df, df$State)
                for(i in 1:length(unique(df$State))){
                        dfc[i,1] <- a[[i]][nrow(a[[i]]),][,1]
                        dfc[i,2] <- unique(df$State)[i]
                }
        }
        
        dfc
}
