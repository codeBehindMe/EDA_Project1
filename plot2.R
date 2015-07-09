###################### 
require(lubridate)
######################

# read in the table
data_i <- read.table("household_power_consumption.txt",sep=";",header=T,stringsAsFactors=F)
      print("File read sucessfully.")

#now we need to bind the date & Times together
dateTimes <- parse_date_time(paste(data_i$Date,data_i$Time,sep=" "),"dmy_hms")
      print("Parsed date time formats.")

#bind columns
data_i <- cbind(dateTimes,data_i,stringsAsFactors=F)

#release dateTimes
remove(dateTimes)

#now we need to subset to  the data of interest
subData <- function(x="16/12/2006 00:00:00",y=as.character(Sys.time())){
      
      #parse the date time in the correct format
      x <- parse_date_time(x,"dmy_hms")
      y <- parse_date_time(y,"dmy_hms")
      
      subData <- subset(data_i, data_i$dateTimes >= x & data_i$dateTimes < y,row.names=F)
      
      subData
}

#since we are only interested in data falling inbetween 2007/02/01 and 2007/02/03 lets trim the main set
data_i <- subData("01/02/2007 00:00:00","03/02/2007 00:00:00")
      print("Filtered to period of interest.")

# now we need to coerce the columns with values to numeric
cToNumVoid <- function(x=4,y=10){
      
      for (i in x:y){
            
            data_i[,i] <<- as.numeric(data_i[,i])
      }
      
}

#run the function
cToNumVoid()
      print("Coerced numerics")

#plot the graph to the file
png(file="plot2.png",bg="transparent")
with(data_i,plot(data_i$dateTimes,data_i$Global_active_power,type="l",ylab="Global Active Power (KW)",xlab="Day",
                 main="Global Active Power"))
dev.off()
      print("File generation completed, connection closed.")

#remove temporary variables
remove(data_i,cToNumVoid,subData)
      print("Removed temporary variables.")

print(paste("PNG file succesfully written to ",getwd(),"/plot2.png",sep=""))

