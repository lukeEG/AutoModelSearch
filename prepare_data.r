#prepare data
library(LKT)
library(dplyr)
#set seed
set.seed(1234)

setwd(paste0(getwd(),"/data"))

#ASSISTMENTS 2012#################################################
##################################################################
load("Assistments2012.RData")

# make student stratified folds (for crossvalidation for unseen population)
unq = sample(unique(val3$Anon.Student.Id))
sfold = rep(1:5,length.out=length(unq))
val3$fold = rep(0,length(val3[,1]))
for(i in 1:5){val3$fold[which(val3$Anon.Student.Id %in% unq[which(sfold==i)])]=i}


#CLOZE############################################################
##################################################################
cloze = largerawsample

#clean it up
cloze$KC..Default.<-cloze$Problem.Name
# make it a datatable
cloze= setDT(cloze)

# get the times of each trial in seconds from 1970
cloze$CF..Time.<-as.numeric(as.POSIXct(as.character(cloze$Time),format="%Y-%m-%d %H:%M:%S"))

#make sure it is ordered in the way the code expects
cloze<-cloze[order(cloze$Anon.Student.Id, cloze$CF..Time.),]

#create a binary response column to predict and extract only data with a clozeid clozeue
cloze$CF..ansbin.<-ifelse(tolower(cloze$Outcome)=="correct",1,ifelse(tolower(cloze$Outcome)=="incorrect",0,-1))
cloze<-cloze[cloze$CF..ansbin==0 | cloze$CF..ansbin.==1,]



# create durations
cloze$Duration..sec.<-(cloze$CF..End.Latency.+cloze$CF..Review.Latency.+500)/1000

# this function needs times and durations but you don't need it if you don't want to model time effects
cloze <- computeSpacingPredictors(cloze, "KC..Default.") #allows recency, spacing, forgetting features to run
cloze <- computeSpacingPredictors(cloze, "KC..Cluster.") #allows recency, spacing, forgetting features to run
cloze <- computeSpacingPredictors(cloze, "Anon.Student.Id") #allows recency, spacing, forgetting features to run
cloze <- computeSpacingPredictors(cloze, "CF..Correct.Answer.") #allows recency, spacing, forgetting features to run

unq2 = sample(unique(cloze$Anon.Student.Id))
sfold2 = rep(1:5,length.out=length(unq2))
cloze$fold = rep(0,length(cloze[,1]))
for(i in 1:5){cloze$fold[which(cloze$Anon.Student.Id %in% unq2[which(sfold2==i)])]=i}



#MATHIA###########################################################
##################################################################
datafile = "ds4845_tx_All_Data_6977_2021_0723_141809.txt"
mathia<-read.delim(colClasses = c("Anon.Student.Id"="character"),datafile,sep="\t", header=TRUE,quote="")
mathia=as.data.table(mathia)
mathia$CF..Time.<-as.numeric(as.POSIXct(as.character(mathia$Time),format="%Y-%m-%d %H:%M:%S"))

#make sure it is ordered in the way the code expects
mathia<-mathia[order(mathia$Anon.Student.Id, mathia$CF..Time.),]

#create a binary response column to predict and extract only data with a valid value

mathia$Outcome<-ifelse(tolower(mathia$Outcome)=="ok","CORRECT","INCORRECT")
mathia$CF..ansbin.<-ifelse(tolower(mathia$Outcome)=="correct",1,0)
mathia<-mathia[mathia$CF..ansbin==0 | mathia$CF..ansbin.==1,]

#subtot<-  aggregate(mathia$CF..ansbin.,by=list(mathia$Anon.Student.Id),FUN=length)
# subtot<- subtot[subtot$x<20,]
# mathia<-mathia[!(mathia$Anon.Student.Id %in% subtot$Group.1),]
mathia<-mathia[mathia$Attempt.At.Step==1,]
mathia<-mathia[mathia$KC..MATHia.!="",]
mathia <- suppressWarnings(computeSpacingPredictors(mathia, "KC..MATHia.")) #allows recency, spacing, forgetting features to run
mathia <- suppressWarnings(computeSpacingPredictors(mathia, "Problem.Name")) #allows recency, spacing, forgetting features to run
mathia <- suppressWarnings(computeSpacingPredictors(mathia, "Anon.Student.Id")) #allows recency, spacing, forgetting features to run

unq3 = sample(unique(mathia$Anon.Student.Id))
sfold3 = rep(1:5,length.out=length(unq3))
mathia$fold = rep(0,length(mathia[,1]))
for(i in 1:5){mathia$fold[which(mathia$Anon.Student.Id %in% unq3[which(sfold3==i)])]=i}
