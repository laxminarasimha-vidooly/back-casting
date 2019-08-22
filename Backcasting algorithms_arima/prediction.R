#Importing libraries
require(rlang)
require(dplyr)
require(httr)
require(curl)
require(RCurl)
library(tidyr)
require(ggplot2)
library('tseries')
library('forecast')


#Running test url for empty data frame
url="http://52.70.219.120/data/getchanneltracks.php?after=2018-06-01&before=2018-12-01&type=daily&channelid=UCM2Fwf8C9iWGVrvUeAjyv2g"
print(url)
smpl<-GET(url,timeout(100))
smpl1<-httr::content(smpl)
print(smpl1)

data2<-data.frame(matrix(nrow=0,ncol=ncol(smpl1)))
colnames(data2)<-names(smpl1)
#Reading channel ids
data<-read.csv("channels_for_forecasting.csv", header=T, stringsAsFactors = F)
View(data)
a<-1:10
ch_ids<-paste0(data[a,], collapse=",")
#Fetching channel daily tracking thorugh API
for (i in (ch_ids))
  {
  
  url<-paste(c("http://52.70.219.120/data/getchanneltracks.php?after=2017-01-01&before=2018-12-01&type=daily&channelid=",as.character(ch_ids)),collapse="")
  print(url)
  url2<-GET(url,timeout(100))
  
  data_main<-httr::content(url2)
  if(nrow(data_main) == 0) 
  {
    data_main <- "N/A"
  } 
  else 
  {
    data2<-rbind(data2,data_main)
  }
  #print(i)
}
#Export daily tracking to csv
write.csv(data2, file="/dailytrackingmodel.csv")

#Processing daily tracking
data33<-data
for (a in data33[4,1])
  
{
  data333<-data2[data2$channelid %in% a,]
  data333$created <- as.Date(data333$created, "%Y-%m-%d")
}

#write.csv(data3, file="E:/Work/daily tracking model_dates.csv")
X <- select(data333, positiveviews,created)
hist(X$positiveviews)
X_1<-X$positiveviews
y_norm<-X_1

#Visualizing the daily tracking pattern of the channel
ggplot(X, aes(created, positiveviews)) + geom_line()+scale_x_date(breaks = "1 month") +xlab("year") + ylab("Views")

###############
#timeseries using ARIMA model####
###############
f<-7
h<-30
X1<-y_norm
X1[is.na(X1)] <- 0
X2<-ts(X1,frequency = f)
revx <- ts(rev(X2), frequency=f)
str(revx)
plot(revx)
bestfit=list(aic=Inf)
P=0:5
I=0:2
Q=0
#Running loop through all possible parameters
for (A in P)
{
  for (B in I)
  {
    for (C in Q)
    {
      fitARIMA <- arima(revx, order=c(A,B,C),seasonal = list(order = c(0,0,0), period = 7),method="ML")
      
      if(fitARIMA$aic<bestfit$aic)
      {
        bestfit=fitARIMA
      }
      else break;
    }
  }
}

#Predicting the next month viewership
futurVal <- forecast(bestfit,h=h)

fc=futurVal
fc$mean <- ts(rev(fc$mean),end=tsp(X2)[1] - 1/f, frequency=f)
fc$upper <- fc$upper[h:1,]
fc$lower <- fc$lower[h:1,]
fc$x <- X2
# Plot result with reversing viewership
plot(fc,xlim=c(tsp(X2)[1]-h/f,tsp(X2)[2]))
futurVa1<-rev(futurVal$mean)
#Prepare data frame with reverse predictions and actual values
X_1
new1<-matrix(nrow=length(futurVa1)+length(X_1),ncol=3)
colnames(new1)<-c("time","pred","act")
new2<-new1
new2<-as.data.frame(new2)
new2$time<-1:nrow(new1)
new1<-as.data.frame(matrix(nrow=length(futurVa1),ncol=3))
colnames(new1)<-c("time","pred","act")
new1$time<-c(1:nrow(new1))
new1$pred<-futurVa1
new2<-as.data.frame(matrix(nrow=length(X_1),ncol=3))
colnames(new2)<-c("time","pred","act")
new2$time<-c((31:(length(futurVa1)+length(X_1))))
new2$act<-X_1
new1<-rbind(new1,new2)
tail(new1)
new3<-new1
new3[is.na(new3)]<-0
#final<-write.table(new3,"final_pred.csv", sep="\t")
#visualizing the result with actual and prediction
library(ggplot2)
library(reshape2)
d <- melt(new1, id.vars="time")

# Everything on the same plot
ggplot(d, aes(time,value, col=variable)) + 
  geom_line() + 
  stat_smooth()+ggtitle("non transformed and non stationary")

