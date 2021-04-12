#WJS - last updated -04-12-21 (commented by KJF)


#R code below was written to determine (for each OTU):
  #how frequently is the OTU found in each climatic region
  #how many isolations are required to discover the OTU in each region (discovery rate)
  #based on the discovery rate in the most densely sampled region, 
      #how many regions is the OTU expected and found in?
      #how many regions is the OTU expected and NOT found in?


#We defined cosmopolitan species as those species that are expected in more than 1 region
#and expected and NOT found in 1 or 0 regions. 


####Begins by importing table S1 as raw_WY_data, 
#   with the column currently named "Climatic Region" instead called
#  "Region". 

# Find all unique regions in data
Regions<- unique(raw_WY_data[c(32)])

####Arctic Unique
ArcticRegion<- raw_WY_data[which(grepl("Arctic", raw_WY_data$Region)), ]
#pull out rows containing Arctic region samples
ArcticRegion<- unique(ArcticRegion[c(29,23,32)])
#in the original code these columsn corresponded to: 29-OTU, 23-Sample ID, 32-Region.
#Pulls out unique isolation events for each OTU-sample-region combination

#Unique Frequency
arcticspfreq<- data.frame(table(ArcticRegion$Species))
#finds how frequently each OTU is found in dataframe of unique isolations and 
#creates new dataframe to store each OTU and its specific frequency

colnames(arcticspfreq)<- c("Species", "UniqueFreq")

#Total Unique isolations
ArcticUnique<-length(unique(ArcticRegion$SetID))
ArcticRegion$TotalUnique<- ArcticUnique
#Finds how many total unique isolations occur in Arctic region and assigns that value to 
#the column ArcticUnique

#Discovery rate
ArcticRegion<-merge(ArcticRegion, arcticspfreq, by="Species")
ArcticRegion$DiscRate<- ArcticRegion$TotalUnique/ArcticRegion$UniqueFreq
#Finds Discovery rate fore each OTU: 
#Discovery rate = total isolations/isolations of each OTU
#Or, how many isolations were required in the data to find a single 
#isolate of the specific OTU?

#Code repeated to generate a dataframe of discovery rates
# for each OTU's found in region. 


####Southeast
SoutheastRegion<- raw_WY_data[which(grepl("Southeast", raw_WY_data$Region)), ]
SoutheastRegion<- unique(SoutheastRegion[c(29, 23, 32)])
southeastspfreq<- data.frame(table(SoutheastRegion$Species))
colnames(southeastspfreq)<-c("Species", "UniqueFreq")

SoutheastUnique<-length(unique(SoutheastRegion$SetID))
SoutheastRegion$TotalUnique<- SoutheastUnique
#Discovery rate
SoutheastRegion<-merge(SoutheastRegion, southeastspfreq, by="Species")
SoutheastRegion$DiscRate<- SoutheastRegion$TotalUnique/SoutheastRegion$UniqueFreq


####OhioValley
OhioValley<- raw_WY_data[which(grepl("Ohio Valley", raw_WY_data$Region)), ]
OhioValley<- unique(OhioValley[c(29, 23, 32)])
ohiovalspfreq<- data.frame(table(OhioValley$Species))
colnames(ohiovalspfreq)<- c("Species", "UniqueFreq")
OhioValleyUnique<-length(unique(OhioValley$SetID))
OhioValley$TotalUnique<- OhioValleyUnique
#Discovery rate
OhioValley<-merge(OhioValley, ohiovalspfreq, by="Species")
OhioValley$DiscRate<- OhioValley$TotalUnique/OhioValley$UniqueFreq


####Upper Midwest
UpperMidwest<- raw_WY_data[which(grepl("Upper Midwest", raw_WY_data$Region)), ]
UpperMidwest<- unique(UpperMidwest[c(29, 23,32)])
uppermidspfrq<- data.frame(table(UpperMidwest$Species))
colnames(uppermidspfrq)<- c("Species", "UniqueFreq")

UpperMidwestUnique<-length(unique(UpperMidwest$SetID))
UpperMidwest$TotalUnique<- UpperMidwestUnique
#Discovery rate
UpperMidwest<-merge(UpperMidwest, uppermidspfrq, by="Species")
UpperMidwest$DiscRate<- UpperMidwest$TotalUnique/UpperMidwest$UniqueFreq

####Northern Rockies and Plains
NorthernRock<- raw_WY_data[which(grepl("Northern Rockies and Plains", raw_WY_data$Region)), ]
NorthernRock<- unique(NorthernRock[c(29,23,32)])
northernrockspfreq<- data.frame(table(NorthernRock$Species))
colnames(northernrockspfreq)<- c("Species", "UniqueFreq") 
NorthernRockUnique<-length(unique(NorthernRock$SetID))
NorthernRock$TotalUnique<- NorthernRockUnique
#Discovery rate
NorthernRock<-merge(NorthernRock, northernrockspfreq, by="Species")
NorthernRock$DiscRate<- NorthernRock$TotalUnique/NorthernRock$UniqueFreq


####Northeast
NortheastRegion<- raw_WY_data[which(grepl("Northeast", raw_WY_data$Region)), ]
NortheastRegion<- unique(NortheastRegion[c(29,23,32)])
northeastspfreq<- data.frame(table(NortheastRegion$Species))
colnames(northeastspfreq)<- c("Species", "UniqueFreq") 
NortheastRegionUnique<-length(unique(NortheastRegion$SetID))
NortheastRegion$TotalUnique<- NortheastRegionUnique
#Discovery rate
NortheastRegion<-merge(NortheastRegion, northeastspfreq, by="Species")
NortheastRegion$DiscRate<- NortheastRegion$TotalUnique/NortheastRegion$UniqueFreq

####Northwest
NorthwestRegion<- raw_WY_data[which(grepl("Northwest", raw_WY_data$Region)), ]
NorthwestRegion<- unique(NorthwestRegion[c(29,23,32)])
northwestspfreq<- data.frame(table(NorthwestRegion$Species))
colnames(northwestspfreq)<- c("Species", "UniqueFreq")
NorthwestRegionUnique<-length(unique(NorthwestRegion$SetID))
NorthwestRegion$TotalUnique<- NorthwestRegionUnique
#Discovery rate
NorthwestRegion<-merge(NorthwestRegion, northwestspfreq, by="Species")
NorthwestRegion$DiscRate<- NorthwestRegion$TotalUnique/NorthwestRegion$UniqueFreq


####West
WestRegion<- raw_WY_data[which(grepl("West", raw_WY_data$Region)), ]
WestRegion<- unique(WestRegion[c(29,23,32)])
westspfreq<- data.frame(table(WestRegion$Species))
colnames(westspfreq)<-c("Species", "UniqueFreq")
WestRegionUnique<-length(unique(WestRegion$SetID))
WestRegion$TotalUnique<- WestRegionUnique
#Discovery rate
WestRegion<-merge(WestRegion, westspfreq, by="Species")
WestRegion$DiscRate<- WestRegion$TotalUnique/WestRegion$UniqueFreq


####South 
SouthRegion<- raw_WY_data[which(grepl("Arkansas", raw_WY_data$State)), ]
Louisiana<- raw_WY_data[which(grepl("Louisiana", raw_WY_data$State)), ]
Mississippi<- raw_WY_data[which(grepl("Mississippi", raw_WY_data$State)), ]
Texas<- raw_WY_data[which(grepl("Texas", raw_WY_data$State)), ]
SouthRegion<-rbind(SouthRegion, Louisiana, Mississippi, Texas)
SouthRegion<- unique(SouthRegion[c(29,23,32)])
southspfreq<- data.frame(table(SouthRegion$Species))
colnames(southspfreq)<-c("Species", "UniqueFreq")
SouthRegionUnique<-length(unique(SouthRegion$SetID))
SouthRegion$TotalUnique<- SouthRegionUnique
#Discovery rate
SouthRegion<-merge(SouthRegion, southspfreq, by="Species")
SouthRegion$DiscRate<- SouthRegion$TotalUnique/SouthRegion$UniqueFreq


#####

##Result Table
RegionalDiscoveryRate<- rbind(ArcticRegion, SoutheastRegion, SouthRegion, UpperMidwest, WestRegion, OhioValley, NorthernRock, NortheastRegion, NorthwestRegion)
#merges all regional rate dataframes by row. 


##Remove singletons
Singletons<- #read in dataframe containing singleton isolates found in Table S2
RegionalDiscoveryRate<- RegionalDiscoveryRate[which(!RegionalDiscoveryRate$Species %in% Singletons$Species), ]
#Remove singleton islations from discovery rate dataframe 

RegionalDiscoveryRate<-unique(RegionalDiscoveryRate[c(1,3,4,5,6)])
#Retains only unique combinations of OTU - region - discovery rate 
#(eliminates redundant isolations of same OTU in multiple regions)


##Because a given OTU will have a unique discovery rate for each region in which it is found,
#we used the discovery rate from the most densely sampled region in which each OTU was found
#to represent its overall discovery rate. 

y<-NULL
## loop to find most sampled region each species was found in and determine the discovery rate
uniquesp<-unique(RegionalDiscoveryRate$Species)
for (i in 1:length(uniquesp)){
  spmatch<- RegionalDiscoveryRate[which(RegionalDiscoveryRate$Species %in% uniquesp[i]), ] 
  for (j in 1:length(spmatch)) {
    maxsampled= max(spmatch$TotalUnique)
    spmatch$DiscRateTrue<- spmatch[which(spmatch$TotalUnique>=maxsampled), 5]
  } 
  y<- rbind(y, spmatch)
  }

x<- unique(y[c(1,6)])
#creates dataframe of each OTU and its respective discovery rate. 
#columns are then created for whether or not each OTU is expected in each region
#based on the discovery rate of that OTU and the density of sampling in that region
x$ArcticExpected<-NA
x$ArcticFound<-NA
x$SouthExpected<-NA
x$SouthFound<-NA
x$WestExpected<-NA
x$WestFound<-NA
x$SoutheastExpected<-NA
x$SoutheastFound<-NA
x$OhioValleyExpected<-NA
x$OhioValleyFound<-NA
x$UpperMidExpected<-NA
x$UpperMidFound<- NA
x$NorthernRockExpected<-NA
x$NorthernRockFound<-NA
x$NortheastExpected<-NA
x$NortheastFound<-NA
x$NorthwestExpected<-NA
x$NorthwestFound<-NA

#loop to fill in whether or not the species is expected and found for each region
for (i in 1:nrow(x)){
  #Arctic
  if (x$DiscRateTrue[i]<=ArcticUnique) {
  x$ArcticExpected[i]=TRUE
} else {
  x$ArcticExpected[i]=FALSE
}
  if (length(which(x$Species[i] %in% ArcticRegion$Species))>0) {
    x$ArcticFound[i]=TRUE
  } else {
    x$ArcticFound[i]=FALSE
  }
  #South
  if (x$DiscRateTrue[i]<=SouthRegionUnique) {
    x$SouthExpected[i]=TRUE
  } else {
    x$SouthExpected[i]=FALSE
  }
  if (length(which(x$Species[i] %in% SouthRegion$Species))>0) {
    x$SouthFound[i]=TRUE
  } else {
    x$SouthFound[i]=FALSE
  }
  #West
  if (x$DiscRateTrue[i]<=WestRegionUnique) {
    x$WestExpected[i]=TRUE
  } else {
    x$WestExpected[i]=FALSE
  }
  if (length(which(x$Species[i] %in% WestRegion$Species))>0) {
    x$WestFound[i]=TRUE
  } else {
    x$WestFound[i]=FALSE
  }
  #Southeast
  if (x$DiscRateTrue[i]<=SoutheastUnique) {
    x$SoutheastExpected[i]=TRUE
  } else {
    x$SoutheastExpected[i]=FALSE
  }
  if (length(which(x$Species[i] %in% SoutheastRegion$Species))>0) {
    x$SoutheastFound[i]=TRUE
  } else {
    x$SoutheastFound[i]=FALSE
  }
  #Ohio Valley
  if (x$DiscRateTrue[i]<=OhioValleyUnique) {
    x$OhioValleyExpected[i]=TRUE
  } else {
    x$OhioValleyExpected[i]=FALSE
  } 
  if (length(which(x$Species[i] %in% OhioValley$Species))>0) {
    x$OhioValleyFound[i]=TRUE
  } else {
    x$OhioValleyFound[i]=FALSE
  }
  #Upper Mid
  if (x$DiscRateTrue[i]<=UpperMidwestUnique) {
    x$UpperMidExpected[i]=TRUE
  } else {
    x$UpperMidExpected[i]=FALSE
  } 
  if (length(which(x$Species[i] %in% UpperMidwest$Species))>0) {
    x$UpperMidFound[i]=TRUE
  } else {
    x$UpperMidFound[i]=FALSE
  }
  #Northern Rockies and Plains
  if (x$DiscRateTrue[i]<=NorthernRockUnique) {
    x$NorthernRockExpected[i]=TRUE
  } else {
    x$NorthernRockExpected[i]=FALSE
  } 
  if (length(which(x$Species[i] %in% NorthernRock$Species))>0) {
    x$NorthernRockFound[i]=TRUE
  } else {
    x$NorthernRockFound[i]=FALSE
  }
  #Northeast
  if (x$DiscRateTrue[i]<=NortheastRegionUnique) {
    x$NortheastExpected[i]=TRUE
  } else {
    x$NortheastExpected[i]=FALSE
  } 
  if (length(which(x$Species[i] %in% NortheastRegion$Species))>0) {
    x$NortheastFound[i]=TRUE
  } else {
    x$NortheastFound[i]=FALSE
  }
  #Northwest
  if (x$DiscRateTrue[i]<=NorthwestRegionUnique) {
    x$NorthwestExpected[i]=TRUE
  } else {
    x$NorthwestExpected[i]=FALSE
  } 
  if (length(which(x$Species[i] %in% NorthwestRegion$Species))>0) {
    x$NorthwestFound[i]=TRUE
  } else {
    x$NorthwestFound[i]=FALSE
  }
}

Rowstoremove<-c()
#We removed from consideration all species that were expected in only one region owing to 
#too little sampling in other regions (all are from the most densely sampled Upper Midwest)
for (i in 1:nrow(x)) {
  booleanvec<-unlist(x[i,(seq(3,19,2))])
  if (length(which(booleanvec))<2) {
    Rowstoremove=append(Rowstoremove, i)
  }
}
x<-x[-Rowstoremove, ]

#dataframe to quantify how many regions each OTU is expected AND found in
#and how many regions each OTU is expected AND NOT found
Cosmoquery<-data.frame(x[1])
Cosmoquery$FoundExpect<- NA
Cosmoquery$NotFoundExpect<-NA

for (i in 1:nrow(x)) {
  foundwhenexpect<-0
  notfoundwhenexpect<-0
  for (j in seq(3,19,2)) {
  arcticvec<-unlist(x[i,c(j,j+1)])
  if (arcticvec[1]) {
   if (arcticvec[2]) {
     foundwhenexpect<- foundwhenexpect+1
   } else {
     notfoundwhenexpect<- notfoundwhenexpect+1
   }
  }
  }
  Cosmoquery$FoundExpect[i]<-foundwhenexpect
  Cosmoquery$NotFoundExpect[i]<-notfoundwhenexpect
}

#We defined cosmopolitan species as those species that are NOT found when expected
#1 or fewer times. 
length(which(Cosmoquery$FoundExpect>0 & Cosmoquery$NotFoundExpect<=1))
