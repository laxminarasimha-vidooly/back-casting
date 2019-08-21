#install.packages("httr")
require(httr)
require(curl)
require(RCurl)
url="http://52.70.219.120/data/getchanneltracks.php?after=2018-06-01&before=2018-12-01&type=daily&channelid=UCM2Fwf8C9iWGVrvUeAjyv2g"
print(url)
smpl<-GET(url,timeout(100))
smpl1<-httr::content(smpl)
print(smpl1)

data2<-data.frame(matrix(nrow=0,ncol=ncol(smpl1)))
colnames(data2)<-names(smpl1)

data<-read.csv(file.choose(), header=T, stringsAsFactors = F)
View(data)
a<-1:10
ch_ids<-paste0(data[a,], collapse=",")

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
write.csv(data2, file="E:/Work/daily tracking model.csv")

data2<-read.csv("F:/Work/dailytrackingmodel.csv", header=T, stringsAsFactors = F)
data<-read.csv("F:/Work/channels_for_forecasting.csv", header=T, stringsAsFactors = F)

data33<-data


for (a in data33[4,1])
  
{
  data333<-data2[data2$channelid %in% a,]
  data333$created <- as.Date(data333$created, "%Y-%m-%d")
}


#write.csv(data3, file="E:/Work/daily tracking model_dates.csv")

#install.packages("rlang")
require(rlang)
#install.packages("dplyr")
require(dplyr)

X <- select(data333, positiveviews,created)
hist(X$positiveviews)
X_1<-X$positiveviews
y_norm<-X_1
#install.packages("tidyr")
library(tidyr)

# data5<- X %>%
#           group_by(channelid, modate) %>%
#                     summarise(sum_viewchange = sum(as.numeric(viewchange)),
#                     sum_subchange = sum(as.numeric(subchange)),
#                     sum_uploadchange = sum(as.numeric(uploadchange)))
# View(data6)
# str(data6)


require(ggplot2)

ggplot(X, aes(created, positiveviews)) + geom_line()+scale_x_date(breaks = "1 month") +xlab("year") + ylab("Views")

############
#timeseries####


#library('ggplot2')
#install.packages("tseries")
library('tseries')

#install.packages("forecast")
library('forecast')


#View(data7)
f<-7
h<-30
X1<-y_norm
X1[is.na(X1)] <- 0
X2<-ts(X1,frequency = f)
revx <- ts(rev(X2), frequency=f)
str(revx)
plot(revx)

# components.ts = decompose(revx)
# plot(components.ts)

#install.packages("fUnitRoots")
# library("fUnitRoots")
# urkpssTest(revx, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
# tsstationary = diff(revx, differences=1)
# plot(tsstationary)
# acf(tsstationary,lag.max=34)
# pacf(tsstationary,lag.max=34)
# timeseriesseasonallyadjusted <- revx-components.ts$seasonal
# tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)

bestfit=list(aic=Inf)
P=0:5
I=0:2
Q=0

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


futurVal <- forecast(bestfit,h=h)

fc=futurVal
fc$mean <- ts(rev(fc$mean),end=tsp(X2)[1] - 1/f, frequency=f)
fc$upper <- fc$upper[h:1,]
fc$lower <- fc$lower[h:1,]
fc$x <- X2
# Plot result
plot(fc,xlim=c(tsp(X2)[1]-h/f,tsp(X2)[2]))


futurVa1<-rev(futurVal$mean)

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

library(ggplot2)
library(reshape2)
d <- melt(new1, id.vars="time")

# Everything on the same plot
ggplot(d, aes(time,value, col=variable)) + 
  geom_line() + 
  stat_smooth()+ggtitle("non transformed and non stationary")

