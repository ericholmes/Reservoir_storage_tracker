## 1) Thousand acre feet
## 2) Reservoir max
## 3) Data caching
## 5) vertical lines for comparison of water years
## 6) faceting?

library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(scales)
library(plotly)

sensor_num <- "15"                 #Reservoir Elevation="6", Reservoir storage="15"
start_date <- "2023-10-24"        #format "YYYY-MM-DD"
end_date <- Sys.Date()            #format "YYYY-MM-DD"

res.list <- c("FMD","HHL","SLS","DNN", "FOL")

download <- data.frame()
for(i in res.list){
  print(paste("Downloading",i, "data"))
  
  temp<-read.table(paste("http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=", i, "&SensorNums=", 15, 
                         "&dur_code=D&&Start=", start_date, "&End=", end_date, sep=""),
                   header=FALSE, sep=",", skip=2, stringsAsFactors = F)
  
  temp <- temp[, c(5, 7, 1)]
  temp[ temp == "m" ] = NA
  temp$V5 <- as.integer(substr(temp$V5, 1,8))
  temp$V7 <- as.integer(temp$V7)
  colnames(temp) <- c("Date", "storage", "Reservoir")
  download <- rbind(download,temp)
}

cache <- read.csv("data/ResCache3.csv", header=T, stringsAsFactors = F)

combined <- rbind(cache,download)

data<-data.frame()
for(j in unique(combined$Reservoir)){
  print(paste("Downloading new data from 10-1-2014 and removing outliers",j, "data"))
  temp <- combined[combined$Reservoir == j,]
  temp$yr <- substr(temp$Date, 1, 4)
  #Remove outliers
  tempmax<-aggregate(storage~yr,max,data=temp)
  tempmin<-aggregate(storage~yr,min,data=temp)
  temp <- subset(temp, temp$storage < quantile(tempmax$storage, probs=.75, na.rm=T) + 1.5 * IQR(tempmax$storage, na.rm = T))
  temp <- subset(temp, temp$storage > quantile(tempmin$storage, probs=.25, na.rm=T) - 1.5 * IQR(tempmin$storage, na.rm = T))
  temp <- subset(temp, temp$storage > 0)
  data <- rbind(data, temp)
}

##-------------------------------------Prepare the data------------------------------------------##

data$Date <- as.Date(paste(substr(data$Date, 1,4), "-", substr(data$Date, 5,6), "-", substr(data$Date, 7, 8), sep=""))
data$storage <- as.character(data$storage)
data$storage <- as.numeric(data$storage)/1000
data<-data[is.na(data$storage) != T,]
data$jday <- strptime(data$Date, "%Y-%m-%d")$yday+1
data <- ddply(data,.(Reservoir,jday), transform, avg.daily=mean(storage))
data$avg.daily <- ifelse(data$jday==366,NA,data$avg.daily)
data$max <- ifelse(data$Reservoir == "FMD", 136.400, ifelse(data$Reservoir == "HHL", 208.400,
                  ifelse(data$Reservoir == "SLS",141.900,ifelse(data$Reservoir == "DNN", 9.700, 977.000))))
data$launch <- ifelse(data$Reservoir == "FMD", 63.497, ifelse(data$Reservoir == "HHL", 105.416, NA))

##------------------------------------Shiny reactive code----------------------------------------##

shinyServer(function(input,output) { 
  
  output$Res <- renderPlot({p <- ggplot(data[data$Reservoir == input$selectRes,], aes(x=Date,y=storage)) + theme_bw() + 
                                    theme(legend.direction= "horizontal", legend.position=c(.5,0.1), legend.key = element_blank(),
                                          legend.background = element_rect(colour = "black"), legend.text=element_text(size=12), axis.text.x = element_text(angle=45, hjust = .85)) + 
                                    geom_line(aes(x=Date,y=avg.daily),alpha=.3, linetype="dashed") +
                                    geom_area(fill="blue",alpha=.15) + geom_line(color = "navy",size=.75) + 
                                    geom_hline(aes(yintercept = max), color= "red") +
                                    geom_hline(aes(yintercept = launch), linetype = "dashed", alpha=.7) +
                                    scale_x_date(breaks = date_breaks(ifelse(as.numeric(difftime(input$range[2],input$range[1],units = "weeks")) > 76,"1 years","1 months")),
                                                 labels=date_format(ifelse(as.numeric(difftime(input$range[2],input$range[1],units = "weeks")) > 76,"%Y","%b-%Y")),limits = input$range) +
                                    labs(list(title=paste("Reservoir Storage at",input$selectRes), y= "Reservoir Storage (1000 Acre-Feet)", x = "Date"))
                            
                             if(input$check == TRUE){
                                if(input$level == TRUE){return(p + geom_vline(aes_string(xintercept = as.numeric(input$vline))) +
                                                               geom_hline(aes_string(yintercept = data[data$Reservoir == input$selectRes & data$Date == input$vline,"storage"])))}
                                else{return(p + geom_vline(aes_string(xintercept = as.numeric(input$vline))))}
                             }
                            else{return(p)}
                            
                            })
  
  output$Current <- renderText(paste("Data for", input$selectRes, "available up to", max(data[data$Reservoir == input$selectRes,"Date"])))
  output$vlinetext <- renderText({
    if(input$check == TRUE){return(paste("Reservoir storage at", input$selectRes, "on", input$vline, "was", 
                                         data[data$Reservoir == input$selectRes & data$Date == input$vline,"storage"], "thousand acre feet"))}
    else{return(NULL)}
  })
})