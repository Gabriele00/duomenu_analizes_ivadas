#load packages
library(tidyverse)
library(quantmod)

#=====================================  Exchange rates example

#retrieve data 
getSymbols(c("USDGBP=X", "EURUSD=X"), warnings = FALSE)
#merge all dataframes together
df <- merge(`GBPUSD=X`, `EURUSD=X`, all = TRUE) 
#creating a dataframe with first column as date that comes from xts object extracted by index()
df <- data.frame(date=index(df), coredata(df))%>% select(1,7,13) 
#renaming columns
colnames(df) <- c("date", "GBPUSD", "EURUSD")
#creating an index=100 for every column
df <- df %>% 
        filter(date>="2018-01-01")%>% 
        mutate(GBPUSD=GBPUSD/GBPUSD[1]*100,
               EURUSD=EURUSD/EURUSD[1]*100)%>%
        gather(Currency, Values, 2:3)

#creating a line plot for all three exchange rate indeces

ggplot(data=df,aes(x=date, y=Values, colour=Currency))+
        geom_line(size=1.05)+
        scale_x_date(date_breaks = "1 month",date_labels = "%Y %B")+
        labs(title='Exchange rates ("GBPUSD", "EURUSD") inedexed to 100 for 2018-01-01',
             x="Date",
             y="Index")+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
