# Chunk 1
options(warn=-1)
games=games <- read.csv("input/games.csv")
head(games,3)
# Chunk 2
names(games)
# Chunk 3
#File read.r
#function which prepares the data
#return: a data set without missing values
getGames<-function(){
games <- read.csv("input/games.csv")
initialRows<-nrow(games)
initialCols<-ncol(games)
#Remove unsued columns
games$NA_Sales<-NULL
games$EU_Sales<-NULL
games$JP_Sales<-NULL
games$Other_Sales<-NULL
games$User_Score<-NULL
games$User_Count<-NULL
games$Rating<-NULL
games$Developer<-NULL
games$Name<-NULL
games$Publisher<-NULL
games$Critic_Score<-NULL
games$Critic_Count<-NULL
#Delete rows without a proper Genre
index<-which(games$Genre=="")
games<-games[-index,]
games$Genre<-factor(games$Genre)
#Delete rows without the release year
index<-which(games$Year_of_Release=="N/A")
games<-games[-index,]
#Set year of release as a date.
#dates$Year<-as.Date(games$Year_of_Realese,"%Y")
#Set proper names
names(games)[2]<-"Year"
names(games)[4]<-"Sales"
games<-games[complete.cases(games), ]
message(initialRows- nrow(games)," rows and "
,initialCols - ncol(games)," cols deleted" )
games
}
games<-getGames()
# Chunk 4
qqnorm(games$Sales)
# Chunk 5
qqnorm(log(games$Sales))
# Chunk 6
av.aov<-aov(games$Sales~games$Year)
summary(av.aov)
# Chunk 7
pv.aov<-aov(games$Sales~games$Platform)
summary(pv.aov)
# Chunk 8
gv.aov<-aov(games$Sales~games$Genre)
summary(gv.aov)
# Chunk 9
rm(av.aov)
rm(pv.aov)
rm(gv.aov)
# Chunk 10
table(games$Year)
# Chunk 11
#File clean.R
cleanGames<-function(games){
#Change one error
index<-which(games$Year=="2020")
games[index,"Year"]="2009"
#group the firsts years 1980-1994
fy<-seq(1980,1994)
fyears<-which(games$Year %in% fy)
games$Year<-as.character(games$Year)
games[fyears,"Year"]<-"1980-1994"
#Delete the 3 games from 2017
y2017=which(games$Year=="2017")
games<-games[-y2017,]
games$Year<-as.factor(games$Year)
games
}
games<-cleanGames(games)
# Chunk 12
library("reshape2")
games1<-dcast(games,games$Year+games$Platform~games$Genre, sum)
names(games1)[1]<-"Year"
names(games1)[2]<-"Device" #platform is a genre!!!
#Creating a new column with the sum of all sales
#in all genres per year.
games1$Sales<-rowSums(games1[,3:length(games1)])
#Proportion and then percentages
games2<-sweep(games1[,3:length(games1)],1,games1[,length(games1)],"/")
games2<-games2*100 #more readable
#Adding some columns
games2$Year<-games1$Year
games2$Device<-games1$Device
games2$Sales<-NULL
head(games2,4)
#Construim un altra dataframe només pels anys i gènere,
#idependent de la plataforma
gamesg1<-dcast(games,games$Year ~ games$Genre, sum)
names(gamesg1)[1]<-"Year"
gamesg1$Sales<-rowSums(gamesg1[,2:length(gamesg1)])
gamesg2<-sweep(gamesg1[,2:(length(gamesg1)-1)],1,gamesg1[,length(gamesg1)],"/")
gamesg2<-gamesg2*100
gamesg2$Year<-gamesg1$Year
#A games1 hi ha les vendes per any i device vs genere en format absolut
#A games2 hi ha les vendes per any i device vs genere en percentatge
#A gamesg1 hi ha les vendes per any vs gènere en format absolut
#A games2 hi ha les vendes per any vs gènere en percentatge
# Chunk 13
gamesg1matrix<-gamesg1[,2:length(gamesg1)]
round(cor(gamesg1matrix),2)
# Chunk 14
attach(gamesg1)
lm.sales<-lm(Sales~Adventure)
plot(Adventure,Sales)
lines(Adventure,fitted(lm.sales))
segments(Adventure,fitted(lm.sales),+Adventure,Sales)
# Chunk 15
par(mfrow=c(2,2), mex=0.6)
plot(lm.sales)
# Chunk 16
subset(gamesg1,select=c(3,14))
# Chunk 17
gamesg1[10,]
# Chunk 18
mlm.sales<-lm(Sales~Action+Adventure+Misc+
`Role-Playing`+Shooter+Simulation+Sports)
anova(mlm.sales)
# Chunk 19
mlm.sales<-lm(Sales~Action+Adventure+Sports)
anova(mlm.sales)
par(mfrow=c(2,2), mex=0.6)
plot(mlm.sales)
# Chunk 20
gamesg2matrix<-gamesg2[,1:(length(gamesg2)-1)]
round(cor(gamesg2matrix),2)
# Chunk 21
#Take the position of the greatest value for each row
index<-apply(gamesg2matrix,1,which.max)
#create a new dataframe to save the results
yres<-as.data.frame(matrix(3,23))
#Get the names of the genre
yres$genre<-names(gamesg2matrix)[index]
#Get the values
yres$value<-apply(gamesg2matrix,1,max)
yres$sv<-paste(round(yres$value,0),"%",sep="")
#Get the years
yres$year<-gamesg2$Year
#Concat year and platform
yres$label<-paste(yres$year,yres$genre, sep=" ")
yres$label<-paste(yres$label,yres$sv,sep=" ")
yres$V1<-NULL
par(mfrow=c(1,1), mex=0.6)
plot(yres$year,yres$value, xlab="Year",ylab="% of sales",main="Most popular genre")
text(yres$year,yres$value,yres$label, cex=0.6, pos=1, col="blue")
# Chunk 22
mSales=0.5
par(mfrow=c(1,1), mex=0.6)
#Delete rows with a low sales (~24 rows)
games2<-games2[which(games1$Sales>mSales),]
for (device in levels(games2$Device)){
nr=nrow(games2[which(games2$Device==device ),])
if (nr>1){
plaf=as.data.frame(matrix(1,nr))
plaf<-(games2[which(games2$Device==device ),])
if(nrow(plaf)>1){
plafmatrix<-plaf[,1:(length(gamesg2)-2)]
#Take the position of the greatest value for each row
index<-apply(plafmatrix,1,which.max)
#create a new dataframe to save the results
yres<-as.data.frame(matrix(3,nr))
#Get the names of the genre
yres$genre<-names(plafmatrix)[index]
#Get the values
yres$value<-apply(plafmatrix,1,max)
yres$sv<-paste(round(yres$value,0),"%",sep="")
#Get the years
yres$year<-factor(plaf$Year)
#Concat year and platform
yres$label<-paste(yres$year,yres$genre, sep=" ")
yres$label<-paste(yres$label,yres$sv,sep=" ")
yres$V1<-NULL
tit=paste("Most popular genre for",device, sep=" ")
fil=paste("img/",device,sep="")
fil=paste(fil,".png",sep="")
#png(fil,width = 800, height = 600)
par(mfrow=c(1,1), mex=0.6)
plot(yres$year,yres$value, xlab="Year",ylab="% of sales",main=tit)
text(yres$year,yres$value,yres$label, cex=0.6, pos=1, col="blue")
#dev.off()
}
}
}
# Chunk 23
for (device in levels(games2$Device)){
nr=nrow(games2[which(games2$Device==device ),])
if (nr>1){
plaf=as.data.frame(matrix(1,nr))
plaf<-(games2[which(games2$Device==device ),])
if(nrow(plaf)>1){
plafmatrix<-plaf[,1:(length(gamesg2)-2)]
#Take the position of the greatest value for each row
index<-apply(plafmatrix,1,which.max)
yres<-as.data.frame(matrix(3,nr))
yres$genre<-names(plafmatrix)[index]
yres$year<-as.character(plaf$Year)
yres$yearlable<-yres$year
if ("1980-1994" %in% yres$year)  { #it has to be the first one
yres[1,2]="1987"
}
yres$year<-as.numeric(yres$year)
}
miny=which.min(yres$year)
maxy=which.max(yres$year)
gen=tail(names(sort(table(yres$genre))), 1)
message("\nPlataforma: ",device,". Temps de vida: ",yres[miny,3],
" fins ", yres[maxy,3],".\n Gènere dominant: ",gen)
}
}
# Chunk 24
detach(gamesg1)
savehistory("~/sincron/dataScience/tipologia/pacs/games2/codi.r")
