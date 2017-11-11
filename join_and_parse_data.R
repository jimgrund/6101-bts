bts_data <- "data/"
setwd(bts_data)

complete_data <- data.frame(Date=as.Date(character()),
                         File=character(), 
                         User=character(), 
                         stringsAsFactors=FALSE) 

for(Year in 2015:2017) {
  for(Month in 1:12) {
    filename<-(paste(paste("On_Time_On_Time_Performance",Year, Month, sep="_"),".csv",sep=""))
    print(filename)
    mydata <- read.csv(filename)
    relevant_data <- subset(mydata, Origin == "IAD" | Dest == "IAD" |
                              Origin == "BWI" | Dest == "BWI" |
                              Origin == "DCA" | Dest == "DCA")
    
    complete_data <- rbind(complete_data, relevant_data)
    
    rm(mydata, relevant_data)
  }
}