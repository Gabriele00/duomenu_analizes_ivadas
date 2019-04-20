best <- function(state, outcome){
  df <- read.csv("outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
  
  if(!state %in% unique(df$State)){
    stop("invalid state")
  }
  
  if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
    stop("invalid outcome")
  }
  
  if(outcome=="heart attack"){
    df <- df[,c(2,7,11)]
  }else if(outcome=="heart failure"){
    df <- df[,c(2,7,17)]
  }else{
    df <- df[,c(2,7,23)]}
  df <- df[df$State==state,]
  df <- df[!is.na(df[,3]),]
  df <- df[order(df[,3], df[,1]),]
  df[1,1]
}
