
library(plyr)
library(knitr)
library(ggplot2)
library(grid)
library(car)

StormData <- read.csv("C:/Users/Itahisa/Documents/repdata-data-StormData.csv", stringsAsFactors = FALSE, strip.white=TRUE, header=TRUE)                                        

StormData <- StormData[ , c("EVTYPE", "BGN_DATE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
str(StormData)  

StormData$BGN_DATE <- as.POSIXct(StormData$BGN_DATE,format="%m/%d/%Y %H:%M:%S")

StormData$PROPDMGEXP <- factor(tolower(StormData$PROPDMGEXP))
StormData$CROPDMGEXP <- factor(tolower(StormData$CROPDMGEXP))

require(car)
require(plyr)

StormData$PROPDMGEXP <- as.numeric(recode(as.character(StormData$PROPDMGEXP), 
                                          "'0'=1;'1'=10;'2'=10^2;'3'=10^3;'4'=10^4;'5'=10^5;'6'=10^6;'7'=10^7;'8'=10^8;'b'=10^9;'h'=10^2;'k'=10^3;'m'=10^6;'-'=0;'?'=0;'+'=0"))

StormData$CROPDMGEXP <- as.numeric(recode(as.character(StormData$CROPDMGEXP), 
                                          "'0'=1;'1'=10;'2'=10^2;'3'=10^3;'4'=10^4;'5'=10^5;'6'=10^6;'7'=10^7;'8'=10^8;'b'=10^9;'h'=10^2;'k'=10^3;'m'=10^6;'-'=0;'?'=0;'+'=0"))

StormData$PROPDMGDOLLAR <- StormData$PROPDMG * StormData$PROPDMGEXP

StormData$CROPDMGDOLLAR <- StormData$CROPDMG * StormData$CROPDMGEXP

StormDataAgg <- ddply(StormData, ~EVTYPE, summarise, FATALITIES = sum(FATALITIES), 
                      INJURIES = sum(INJURIES), PROPDMG = sum(PROPDMGDOLLAR), CROPDMG = sum(CROPDMGDOLLAR))

Fatalities <- StormDataAgg[order(StormDataAgg$FATALITIES, decreasing = T), c("EVTYPE", 
                                                                             "FATALITIES")][1:20, ]
Injuries <- StormDataAgg[order(StormDataAgg$INJURIES, decreasing = T), c("EVTYPE", 
                                                                         "INJURIES")][1:20, ]
Propdmg <- StormDataAgg[order(StormDataAgg$PROPDMG, decreasing = T), c("EVTYPE", 
                                                                       "PROPDMG")][1:20, ]
Cropdmg <- StormDataAgg[order(StormDataAgg$CROPDMG, decreasing = T), c("EVTYPE", 
                                                                       "CROPDMG")][1:20, ]
par(mfrow = c(1, 2), oma = c(0, 0, 2, 0))
par(mar = c(10, 4, 3, 4))
barplot(Fatalities$FATALITIES, names.arg = Fatalities$EVTYPE, main = "Fatalities", 
        ylab = "fatalities", cex.axis = 0.8, cex.names = 0.7, las = 2)

title("Population health damages caused by \nextreme climatic events in United States", 
      outer = TRUE)

## Install 

install.packages('devtools')

devtools::install_github('rstudio/shinyapps')

library(shinyapps)

shinyapps::setAccountInfo(name='itahisamr',
			  token='425A006AB7CCB13BC43B9BFCC875F0BB',
			  secret='xIGwn2bpvb/sK4uIoEbUdrfWvTaaUiy7OlfOcA9j')
			  
			  
