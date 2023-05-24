setwd("//fos-fs/Shared/Research Department/Oyster Monitoring/R/Oysters/SPAT/")

library(readxl)
spat<- read_excel("//fos-fs/Shared/Research Department/Oyster Monitoring/R/Oysters/SPAT/master_SpatCounts_deploydate.xlsx", 
              sheet = "master_SpatCounts")


spat$spat_count<-spat$twenty_eight_day_month

#Installing & Loading Packages 
#*only need to install once***#--- this will install if you don't have them
#*but won't if you already do

if(!require(dplyr)){install.packages("dplyr")}
if(!require(imager)){install.packages("imager")}
if(!require(lubridate)){install.packages("lubridate")}

#*****Load Packages****#

library(dplyr)
library(imager)
library(lubridate)

logo<-load.image("florida oceanographic keychain logo.jpg")

#date field to date

spat$date<-gsub('2022-08-31','2022-09-01',spat$date)

spat$date <-as.Date(spat$date, format="%Y-%m-%d")


#Adding a month column to the dataframe
spat$month <- month(spat$date)
#spatdata$month<-sort(spatdata$month)
spat$month<-month.abb[spat$month]
#Adding a year column to the dataframe
spat$year <- year(spat$date)



#Create a table of spat averages and standard deviation by month -------
se <- function(x) sqrt(var(x) / length(x))
monthyearavgspat <- aggregate(spat~month+year+location+date, 
                              data=spat, FUN=mean)
monthyearavgspat$sd<-aggregate(spat ~month+year+location+date, 
                               data=spat, FUN=se)
#turn months into character abbreviations
monthyearavgspat$month<-month.abb[monthyearavgspat$month]

monthyearavgspat$month_year<-paste(monthyearavgspat$sd$month, monthyearavgspat$year)

spat_IRL<-dplyr::filter(monthyearavgspat, location=="IRL")
spat_SLE<-filter(monthyearavgspat, location=="SLE")

IRL_2020<-filter(spat_IRL, year=="2020")
SLE_2020<-filter(spat_SLE, year=="2020")

IRL_2021<-filter(spat_IRL, year=="2021")
SLE_2021<-filter(spat_SLE, year=="2021")

IRL_2022<-filter(spat_IRL, year=="2022")
SLE_2022<-filter(spat_SLE, year=="2022")

IRL_2023<-filter(spat_IRL, year=="2023")
SLE_2023<-filter(spat_SLE, year=="2023")

#2022 will be used as base, so need to convert so that all dates are 2022


year(IRL_2020$date)<-2022
year(SLE_2020$date)<-2022

year(IRL_2021$date)<-2022
year(SLE_2021$date)<-2022

year(IRL_2023$date)<-2022
year(SLE_2023$date)<-2022

#plot------------

plot.new()
par(mar = c(5, 5, 2, 5))#set up size of plot
plot(IRL_2022$date, IRL_2022$spat, ylim=c(0,73), ylab="",  #ylim might need to be changed based on max(mean+sd)
     xlab = "", pch=22, bg="chartreuse3", cex=1.75, cex.lab=2, xaxt="n", yaxt="n", xlim=c(as.Date("2022-01-01"), as.Date("2023-01-01")))
axis(side=2, at=c(0,10,20,30,40,50,60,70), cex.axis=1.5)
months<-seq(as.Date("2022-01-01")+1, as.Date("2022-12-01")+1, by= "1 month")-1
axis(1, at = months, labels =FALSE) 
#set y axis
title(ylab="28-Day Month Average spat per shell", line=3, cex.lab=1.5)
#set trendlines

lines(IRL_2022$date,IRL_2022$spat,col="chartreuse3",lwd=2)
lines(SLE_2022$date, SLE_2022$spat, col="deepskyblue1", lwd=2)

#standard error bars
arrows(IRL_2022$date, IRL_2022$spat-IRL_2022$sd$spat ,IRL_2022$date, IRL_2022$spat+IRL_2022$sd$spat, 
       length=0.05, angle=90, code=3, col="chartreuse3")
arrows(SLE_2022$date, SLE_2022$spat-SLE_2022$sd$spat ,SLE_2022$date, SLE_2022$spat+SLE_2022$sd$spat, 
       length=0.05, angle=90, code=3, col="deepskyblue1")
#put points on after so that easier to see above lines-- IRL is plotted again to go over lines 
points(SLE_2022$date, SLE_2022$spat, pch=22, bg="deepskyblue1", cex=1.75)
points(IRL_2022$date, IRL_2022$spat, pch=22, bg="chartreuse3", cex=1.75)

#2020
lines(SLE_2020$date, SLE_2020$spat, col="deepskyblue3", lwd=2, lty=2)
points(SLE_2020$date, SLE_2020$spat, pch=23, bg="deepskyblue3", cex=1.75)

lines(IRL_2020$date, IRL_2020$spat, col="chartreuse", lwd=2, lty=2)
points(IRL_2020$date, IRL_2020$spat, pch=23, bg="chartreuse", cex=1.75)

arrows(IRL_2020$date, IRL_2020$spat-IRL_2020$sd$spat ,IRL_2020$date, IRL_2020$spat+IRL_2020$sd$spat, 
       length=0.05, angle=90, code=3, col="chartreuse")
arrows(SLE_2020$date, SLE_2020$spat-SLE_2020$sd$spat ,SLE_2020$date, SLE_2020$spat+SLE_2020$sd$spat, 
       length=0.05, angle=90, code=3, col="deepskyblue3")


#2021
lines(SLE_2021$date, SLE_2021$spat, col="deepskyblue4", lwd=2, lty=3)
points(SLE_2021$date, SLE_2021$spat, pch=21, bg="deepskyblue4", cex=1.75)

lines(IRL_2021$date, IRL_2021$spat, col="chartreuse4", lwd=2, lty=3)
points(IRL_2021$date, IRL_2021$spat, pch=21, bg="chartreuse4", cex=1.75)

arrows(IRL_2021$date, IRL_2021$spat-IRL_2021$sd$spat ,IRL_2021$date, IRL_2021$spat+IRL_2021$sd$spat, 
       length=0.05, angle=90, code=3, col="chartreuse4")
arrows(SLE_2021$date, SLE_2021$spat-SLE_2021$sd$spat ,SLE_2021$date, SLE_2021$spat+SLE_2021$sd$spat, 
       length=0.05, angle=90, code=3, col="deepskyblue4")


#2023
lines(SLE_2023$date, SLE_2023$spat, col="dodgerblue", lwd=2, lty=4)
points(SLE_2023$date, SLE_2023$spat, pch=24, bg="dodgerblue", cex=1.75)

lines(IRL_2023$date, IRL_2023$spat, col="darkgreen", lwd=2, lty=4)
points(IRL_2023$date, IRL_2023$spat, pch=24, bg="darkgreen", cex=1.75)

arrows(IRL_2023$date, IRL_2023$spat-IRL_2023$sd$spat ,IRL_2023$date, IRL_2023$spat+IRL_2023$sd$spat, 
       length=0.05, angle=90, code=3, col="darkgreen")
arrows(SLE_2023$date, SLE_2023$spat-SLE_2023$sd$spat ,SLE_2023$date, SLE_2023$spat+SLE_2023$sd$spat, 
       length=0.05, angle=90, code=3, col="dodgerblue")


abline(h=0)

#add dates to x axis
months<-seq(as.Date("2022-01-01")+1, as.Date("2022-12-01")+1, by= "1 month")-1

text(x=(months), par("usr")[3]-2, labels=IRL_2022$sd$month, srt=45, pos=1, xpd=TRUE, cex=1.5)




# draw legend with lines and point but without labels and box. x.intersp controls horizontal distance between lines

#lines
legend(x=(as.Date("2022-04-15")),y=max(SLE_2022$spat+SLE_2022$sd$spat), legend = rep(NA,8),pch=c(NA),
           col =c("deepskyblue3","chartreuse","deepskyblue4","chartreuse4","deepskyblue1","chartreuse3","dodgerblue","darkgreen"),  
           ncol=4, bty='n', x.intersp=0.57  ,y.intersp = 1.5, inset=0.02, lty=c(2,2,3,3,1,1,4,4), lwd=2)

#points
L = legend(x=(as.Date("2022-04-15")),y=max(SLE_2022$spat+SLE_2022$sd$spat), legend = rep(NA,8),
           pt.bg=c("deepskyblue3","chartreuse","deepskyblue4","chartreuse4","deepskyblue1","chartreuse3","dodgerblue","darkgreen"),  
           ncol=4, bty='n', x.intersp=2,y.intersp = 1.5, pch=c(23,23,21,21,22,22,24,24),pt.cex = 1.75, inset=0.02)
#years
M= legend(x=(L$rect$left-10),y=(L$rect$top+1.75), legend = c("2020","2021","2022","2023"),
           ncol=4, bty='n', x.intersp=.57,y.intersp = 1, pch=c(NA), inset=-0.05, text.width = 12.75)

#IRL and SLE
legend(x =( L$rect$left-.75), y = L$rect$top, legend = c('St. Lucie Estuary', 'Indian River Lagoon'), 
       col=rep(NA,2), lty=c(1,1), ncol=1, x.intersp = 8.25, bg = NA, bty="n", y.intersp = 1.4, cex=1)


#add logo
#rasterImage(logo, xleft=max(IRL_2022$date-55), ybottom = (max(SLE_2022$spat+SLE_2022$sd$spat)-7.875),
        #    xright = (max(SLE_2022$date)+(3*32)-55), ytop =max(SLE_2022$spat+SLE_2022$sd$spat))
#salinity to figure-------------------------------------------






axis(side=4, at=c(20,30,40,50,60), cex.axis=1.5, labels=c("0","10","20","30","40")) 
#this is changed because adding 20 to the sal later

mtext("Salinity", side=4, line=3, cex=1.5) #axis label


#need to pull data from the IRLON stations every month-- pulled from IRLON IRL-JB and SLE-ME
#need to put it into excel, delete first parts and save as xlsx, if you keep the file with the same name should load into here after you save
library(readxl)
Salinity <- read_excel("//fos-fs/Shared/Research Department/Oyster Monitoring/R/Oysters/SPAT/28 day month/Salinity.xlsx")

#rename and organize columns
Salinity$date<- as.Date(Salinity$`Time (est)`)
Salinity$IRL<-Salinity$`IRL-JB-WQ Salinity (PSU)`
Salinity$SLE<-Salinity$`SLE-ME-WQ Salinity (PSU)`
Salinity$year <- year(Salinity$date)
Salinity$month <- month(Salinity$date)

library(dplyr)
IRL<-Salinity %>%
  select(-c('SLE')) %>%
  filter( `IRL-JB-WQ Salinity (PSU) quality`=="good")# %>%
  #filter(IRL >= 7)
  
SLE<- Salinity %>%
  select(-c('IRL')) %>%
  filter(`SLE-ME-WQ Salinity (PSU) quality`=="good") %>%
 filter(SLE >=0)

#Find average salinities per day
IRL_Sal_avg <- aggregate(IRL~date+year, 
                                           data=IRL, FUN=mean)
IRL_Sal_avg$sd<-aggregate(IRL ~date+year, 
                               data=IRL, FUN=se)


SLE_Sal_avg <- aggregate(SLE~date+year, 
                         data=SLE, FUN=mean)
SLE_Sal_avg$sd<-aggregate(SLE ~date+year, 
                          data=SLE, FUN=se)

#split into years


IRL_Sal_2020<-filter(IRL_Sal_avg, year=="2020")
SLE_Sal_2020<-filter(SLE_Sal_avg, year=="2020")

IRL_Sal_2021<-filter(IRL_Sal_avg, year=="2021")
SLE_Sal_2021<-filter(SLE_Sal_avg, year=="2021")

IRL_Sal_2022<-filter(IRL_Sal_avg, year=="2022")
SLE_Sal_2022<-filter(SLE_Sal_avg, year=="2022")

IRL_Sal_2023<-filter(IRL_Sal_avg, year=="2023")
SLE_Sal_2023<-filter(SLE_Sal_avg, year=="2023")



#put 2022 as base year for plot

year(IRL_Sal_2020$date)<-2022
year(SLE_Sal_2020$date)<-2022

year(IRL_Sal_2021$date)<-2022
year(SLE_Sal_2021$date)<-2022

year(IRL_Sal_2023$date)<-2022
year(SLE_Sal_2023$date)<-2022

#plot salinity via lines
#adding 20 so that plots don't overlap and are legible

#decided to make colors that same as spat points

lines(IRL_Sal_2020$date, IRL_Sal_2020$IRL+20,col=
       # "gold", 
      "chartreuse",
      lwd=2, lty=2)

lines(SLE_Sal_2020$date, SLE_Sal_2020$SLE+20,col=
       # "firebrick",
      "deepskyblue3",
      lwd=2, lty=2)


lines(IRL_Sal_2021$date, IRL_Sal_2021$IRL+20,col=
      #  "goldenrod1",
      "chartreuse4",
      lwd=2, lty=3)

lines(SLE_Sal_2021$date, SLE_Sal_2021$SLE+20,col=
      #  "firebrick2",
        "deepskyblue4",
      lwd=2, lty=3)


lines(IRL_Sal_2022$date, IRL_Sal_2022$IRL+20,col=
      #  "goldenrod4",
        "chartreuse3",
      lwd=2, lty=1)

lines(SLE_Sal_2022$date, SLE_Sal_2022$SLE+20,col=
       # "firebrick4",
        "deepskyblue1",
      lwd=2, lty=1)


lines(IRL_Sal_2023$date, IRL_Sal_2023$IRL+20,col=
       # "tan3",
      "darkgreen",
      lwd=2, lty=4)

lines(SLE_Sal_2023$date, SLE_Sal_2023$SLE+20,col=
      #  "deeppink"
        "dodgerblue",
      lwd=2, lty=4)

#save as height 980 width 1377

