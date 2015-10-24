NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#1. make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 
#2002, 2005, and 2008.
nei1<-subset(NEI,select=c(Emissions,year))
nei2<-transform(NEI,Emissions,year=factor(year))
nei3 <- tapply(nei2$Emissions,nei2$year,sum)
barplot(nei3,main="Total PM2.5 Emission From All Sources Across the United States",ylab="Emissions",xlab="Year")
dev.copy(png,file="plot1.png")
dev.off()

#2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
nei4 <- subset(NEI,select=c(Emissions,year),subset=(fips == "24510"))
nei5 <- transform(nei4,Emissions,year=factor(year))
nei6 <- tapply(nei5$Emissions,nei5$year,sum)
barplot(nei6,main="Total PM2.5 Emission From Baltimore City",ylab="Emissions",xlab="Year")
dev.copy(png,file="plot2.png")
dev.off()

#3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these
#four sources have seen decreases in emissions from 1999¨C2008 for Baltimore City? Which have seen increases in 
#emissions from 1999¨C2008? Use the ggplot2 plotting system to make a plot answer this question.
nei6 <- subset(NEI,select=c(Emissions,year,type),subset=(fips == "24510"))
nei7 <- transform(nei6,year=factor(year),type=factor(type))
nei8 <- tapply(nei7$Emissions,nei7$year,sum)
attach(nei7)
coplot(Emissions~year|type,type="h",lty=1,lwd=5)
dev.copy(png,file="plot3-1.png")
dev.off()
# the graph looks uncleared, so use another method
n <- split(nei7,type)
names(n)<- c("NONROAD","NONPOINT","ONROAD","POINT")
nonroad <- n$NONROAD
nonpoint <- n$NONPOINT
onroad <- n$ONROAD
point <- n$NONPOINT
detach(nei7)
nr <- tapply(nonroad$Emissions,nonroad$year,sum)
np <- tapply(nonpoint$Emissions,nonpoint$year,sum)
or <- tapply(onroad$Emissions,onroad$year,sum)
p <- tapply(point$Emissions,point$year,sum)
par(mfrow=c(2,2),mar=c(5,4,4,1))

barplot(nr,ylab="Emission",main="Emission from Different Years of NON_ROAD Type")
barplot(np,ylab="Emission",main="Emission from Different Years of NON_POINT Type")
barplot(or,ylab="Emission",main="Emission from Different Years of ON_ROAD Type")
barplot(p,ylab="Emission",main="Emission from Different Years of POINT Type")
dev.copy(png,file="plot3-2.png")
dev.off()

#4. # select the SCC value of burning coal
index<-grepl("Coal",levels(SCC$EI.Sector))
coal<-levels(SCC$EI.Sector)[index]
coal2<-subset(SCC,select=SCC,subset=(EI.Sector==coal[1]|EI.Sector==coal[2]|EI.Sector==coal[3]))
# choose the data of burning coal from NEI using coal2 as the index
coal3<-as.character(coal2$SCC)
coal4<-function(i) return(subset(NEI,select=c(Emissions,year),subset=(SCC==coal3[i])))
coal5<-data.frame()
for(i in 1:length(coal3)){
  coal5<-rbind(coal5,coal4(i))
}
# calculate the total emissions of each year and plot it
coal5 <- transform(coal5,Emissions=as.numeric(Emissions),year=factor(year))
coal6<-tapply(coal5$Emissions,coal5$year,sum)
barplot(coal6,main="Coal Emissions Across the United States",ylab="Emissions",xlab="Year")
# from the graph we can see that the coal emissions arised from 1992 to 2002 and decreased from 
# 2005 to 2008
dev.copy(png,file="plot4.png")
dev.off()

#5. How have emissions from motor vehicle sources changed from 1999¨C2008 in Baltimore City?
motor0<-grepl("Mobile",levels(SCC$EI.Sector))
motor1<-levels(SCC$EI.Sector)[motor0]
motor2<-function(i){
  return(subset(SCC,select=SCC,subset=(EI.Sector==motor1[i])))
}
motor3 <- data.frame()
for(i in 1:length(motor1)){
  motor3 <- rbind(motor3,motor2(i))
}
# subset the original dataset
motor4<- as.character(motor3$SCC)
motor5 <- function(j){
  return(subset(NEI,select=c(Emissions,year),subset=(SCC==motor4[j] & fips == "24510")))
}
motor6 <- data.frame()
for(i in 1:length(motor4)){
  motor6 <- rbind(motor6,motor5(i))
}
motor7 <- tapply(motor6$Emissions,motor6$year,sum)
# make a barplot
barplot(motor7,ylab="Emission",main="Emission of motor vehicles from Baltimore city")
dev.copy(png,file="plot5.png")
dev.off()

#6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle 
#sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes 
#over time in motor vehicle emissions?
cali0 <- function(j){
  return(subset(NEI,select=c(Emissions,year),subset=(SCC==motor4[j] & fips == "06037")))
}

cali1 <- data.frame()
for(i in 1:length(motor4)){
  cali1 <- rbind(cali1,cali0(i))
}

# calculate the range and standard deviation of Baltimore city
range_Baltimore <- diff(range(motor7))
sd_Baltimore <- sd(motor7)
# calculate the range and standard deviation of Los Angeles city
cali2 <- tapply(cali1$Emissions,cali1$year,sum)
range_LA <- diff(range(cali2))
sd_LA <- sd(cali2)
# compare the values of them and get the conclusion that Los Angeles have greater changes
result_list <- list(range_Baltimore=range_Baltimore,range_LA=range_LA,sd_Baltimore=sd_Baltimore,sd_LA=sd_LA)
# make a barplot
barplot(c(result_list$range_Baltimore,result_list$range_LA,result_list$sd_Baltimore,result_list$sd_LA),
        names.arg=names(result_list),main="Los Angeles have greater changes in Emissions of motor vehicles")
dev.copy(png,file="plot6.png")
dev.off()






















