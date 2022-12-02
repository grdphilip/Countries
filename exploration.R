library("plyr")
library("ggplot2")

#Load all data
Switz1df <- read.csv("./Switzerland/scopus.csv")
Switz2df <- read.csv("./Switzerland/scopus(1).csv")
Switz3df <- read.csv("./Switzerland/scopus(2).csv")
Switz4df <- read.csv("./Switzerland/scopus(3).csv")
Sing1df <- read.csv("./Singapore/scopus(4).csv")
Sing2df <- read.csv("./Singapore/scopus(5).csv")
Ox1df <- read.csv("./Oxbridge/scopus(13).csv")
Ox2df <- read.csv("./Oxbridge/scopus(15).csv")
Ox3df <- read.csv("./Oxbridge/scopus(16).csv")
Ox4df <- read.csv("./Oxbridge/scopus(17).csv")
Ox5df <- read.csv("./Oxbridge/scopus(18).csv")
Ox6df <- read.csv("./Oxbridge/scopus(19).csv")
Ox7df <- read.csv("./Oxbridge/scopus(27).csv")
Mass1df <- read.csv("./Massachusets/scopus(20).csv")
Mass2df <- read.csv("./Massachusets/scopus(21).csv")
Mass3df <- read.csv("./Massachusets/scopus(22).csv")
Mass4df <- read.csv("./Massachusets/scopus(23).csv")
Mass5df <- read.csv("./Massachusets/scopus(24).csv")
Mass6df <- read.csv("./Massachusets/scopus(25).csv")
Mass7df <- read.csv("./Massachusets/scopus(26).csv")

head <- Mass1df[0,]
#Clean data
unique(Sing2df[,4])


c("Switz1df","Switz2df","Switz3df","Switz4df","Sing1df","Sing2df","Ox1df","Ox2df","Ox3df","Ox4df","Ox5df","Ox6df","Ox7df","Mass1df","Mass2df","Mass3df","Mass4df","Mass5df","Mass6df","Mass7df")
#Axlar
Time <- rev(c(2015,2016,2017,2018,2019,2020,2021)) #Y
areas <- c("Switzerland","Singapore","Oxbridge","Massachusets") #X
#Schweiz
Schweiz <- c(4.016,9.902,18.28,23.61,29.78,33.91,44.57)
#Singapore
Singapore <- c(4.255,12.33,19.44,27.52,30.58,39.63,42.29)
#Oxbridge
Oxbridge <- c(4.336,12.15,18.36,24.94,30.75,36.16,44.61)
#Massachusets
Massachusets <- c(4.267,12.19,19.07,27.67,35.69,42.43,48.02)

df <- data.frame(Time,Schweiz,Singapore,Oxbridge,Massachusets,SchweizAdj,SingaporeAdj,OxAdj,MassAdj,Benchmark)
means <- rowMeans(df)
dfmeans <- data.frame(years,means)

SchweizAdj <- Schweiz/means
SingaporeAdj <- Singapore/means
OxAdj <- Oxbridge/means
MassAdj <- Massachusets/means
View(t(df))

#Task2, 
Sw2 <- rbind.fill(Switz1df,Switz2df,Switz3df,Switz4df)
Sing2 <- rbind.fill(Sing1df,Sing2df)
Ox2 <- rbind.fill(Ox1df,Ox2df,Ox3df,Ox4df,Ox5df,Ox6df,Ox7df)
Mass2 <- rbind.fill(Mass1df,Mass2df,Mass3df,Mass4df,Mass5df,Mass6df,Mass7df)

Sw2Top <- as.data.frame(Sw2[order(Sw2[,12], decreasing=TRUE),12])
Sing2Top <- as.data.frame(Sing2[order(Sing2[,12], decreasing=TRUE),12])
Ox2Top <- as.data.frame(Ox2[order(Ox2[,12], decreasing=TRUE),12])
Mass2Top <- as.data.frame(Mass2[order(Mass2[,12], decreasing=TRUE),12])

Sw2Top <- as.data.frame(Sw2Top[1:200,])
Sing2Top <- as.data.frame(Sing2Top[1:200,])
Ox2Top <- as.data.frame(Ox2Top[1:200,])
Mass2Top <- as.data.frame(Mass2Top[1:200,])

dfTop200c <- cbind(Sw2Top,Sing2Top,Ox2Top,Mass2Top)
dfTop200 <- as.data.frame(dfTop200)
meanstop200 <- colMeans(dfTop200) 

meanTop <- (953.015+460.575+1131.310+1131.590)/4
ratio <- meanstop200/meanTop


task3 <- function(df) {
    result <- subset(df, df[,4]==2015)
    result <- as.data.frame(result[order(result[,12], decreasing=TRUE),12])
    femproc <- nrow(result)*0.05
    result <- result[1:femproc,]
    mean(result)

}

task3(Mass7df)

Time <- rev(c(2015,2016,2017,2018,2019,2020,2021)) #Y
areas <- c("Switzerland","Singapore","Oxbridge","Massachusets") #X
#Schweiz
Schweiz3 <- c(19.14115,66.81405,139.7082,162.5573,205.6766,224.7129,362.6244)
#Singapore
Singapore3 <- c(19.74286, 93.74269,130.4845, 198.9427,189.702,291.953, 287.7881)
#Oxbridge
Oxbridge3 <- c(21.94083,95.61509, 134.7877,173.9049, 206.788,244.4059,329.0557)
#Massachusets
Massachusets3 <- c(20.66079,93.3145,135.205,203.6917,260.4603,308.6073,357.189)

df3 <- data.frame(Time,Schweiz3,Singapore3,Oxbridge3,Massachusets3,Schweiz3Adj,Singapore3Adj,Oxbridge3Adj,Massachusets3Adj,Benchmark)
Benchmark <- c(1,1,1,1,1,1,1)

Schweiz3Adj <- Schweiz3/means3
Singapore3Adj <- Singapore3/means3
Oxbridge3Adj <- Oxbridge3/means3
Massachusets3Adj <- Massachusets3/means3
means3 <- rowMeans(df3)

#Plot results
View(df)


plotdf <- ts(df[,6:9], start = 2015, end=2021, frequency = 1)

plot(df$Time,df$SchweizAdj,type = "line", col=2, ylim=c(0.8,1.3)) +
lines(df$Time,df$SingaporeAdj, col=3) +
lines(df$Time,df$OxAdj,col=4) +
lines(df$Time,df$MassAdj,col=5) +
lines(df$Time,df$Benchmark, col=6) +
legend("topright",                           # Add legend to plot
       c("SchweizAdj", "SingaporeAdj", "OxAdj", "MassAdj", "Benchmark"),
       lty = 1,
       col = 2:6)

plot(df3$Time,df$Schweiz3Adj,type = "line", col=2) +
lines(df3$Time,df$Singapore3Adj, col=3) +
lines(df3$Time,df3$OxAdj,col=4) +
lines(df3$Time,df3$MassAdj,col=5) +
lines(df3$Time,df3$Benchmark, col=6) +
legend("topright",                           # Add legend to plot
       c("Schweiz3Adj", "Singapore3Adj", "Ox3Adj", "Mass3Adj", "Benchmark"),
       lty = 1,
       col = 2:6)

