write.table(df, file="COVID_UPDATED.csv", sep=",", row.names=F)
 df

COVIDcases <- read.csv("https://query.data.world/s/lysuc3ab7q3zubthc6paesbrsff3oo", header=TRUE, stringsAsFactors=FALSE);

COVIDcases$Date <- as.Date(COVIDcases$Date, format = "%Y-%m-%d")
library(tibbletime)
COVIDcases <- as_tbl_time(COVIDcases, Date = date)%>%

library(MetFns)

    filter.date(COVIDcases, Date >= as.Date("2020-01-01"), Date <= as.Date("2020-05-14"))

filter(COVIDcases, Date >= as.Date("2020-01-01"), Date <= as.Date("2020-05-14"))

COVIDcases[(COVIDcases$Date > "2020-01-01" & COVIDcases$Date < "2020-05-15"),]

subset(COVIDcases, Date> "2020-01-01" & Date < "2020-05-14")

USConfirmedCasesAndDeaths <- COVIDcases[which(COVIDcases[,7]== "US"),]
USConfirmedCases <- USConfirmedCasesAndDeaths[which(USConfirmedCasesAndDeaths[,1]== "Confirmed"),]
USDeaths <- USConfirmedCasesAndDeaths[which(USConfirmedCasesAndDeaths[,1]== "Deaths"),]
USStates <- COVIDcases[which(COVIDcases[,7]== "US"),]
USDeaths <- USConfirmedCasesAndDeaths[which(USConfirmedCasesAndDeaths[,7]== "US"),]
USDeaths[,7]

NewYorkConfirmedCases <- USConfirmedCases[which(USConfirmedCases[,8]== "New York"),]
NumberOFNewYorkDeaths <- sum(NewYorkDeaths$Difference)
NumberOFNewYorkConfirmedCases <- sum(NewYorkConfirmedCases$Difference)

USDeaths





NewYorkDeaths <- USDeaths[which(USDeaths[,8]== "New York"),]
NewYorkConfirmedCases <- USConfirmedCases[which(USConfirmedCases[,8]== "New York"),]
NumberOfNewYorkDeaths <- sum(NewYorkDeaths$Difference)
NumOfNewYorkConfirmedCases <- sum(NewYorkConfirmedCases$Difference)

AlabamaDeaths <- USDeaths[which(USDeaths[,8]== "Alabama"),]
AlabamaConfirmedCases <- USConfirmedCases[which(USConfirmedCases[,8]== "Alabama"),]
NumOfAlabamaDeaths <- sum(AlabamaDeaths$Difference)
NumOfAlabamaCC <- sum(AlabamaConfirmedCases$Difference)

AZDeaths <- USDeaths[which(USDeaths[,8]== "Arizona"),]
AZCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Arizona"),]
NumAZDeaths <- sum(AZDeaths$Difference)
NumberAZCC <- sum(AZCC$Difference)

AKDeaths <- USDeaths[which(USDeaths[,8]== "Arkansas"),]
AKCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Arkansas"),]
NumAKDeaths <- sum(AKDeaths$Difference)
NumAKCC <- sum(AKCC$Difference)

CalDeaths <- USDeaths[which(USDeaths[,8]== "California"),]
CalCC <- USConfirmedCases[which(USConfirmedCases[,8]== "California"),]
NumCalDeaths <- sum(CalDeaths$Difference)
NumcalCC <- sum(CalCC$Difference)

ColDeaths <- USDeaths[which(USDeaths[,8]== "Colorado"),]
ColCc <- USConfirmedCases[which(USConfirmedCases[,8]== "Colorado"),]
NumColDeaths <- sum(ColDeaths$Difference)
NumColcc <- sum(ColCc$Difference)

CTDeaths <- USDeaths[which(USDeaths[,8]== "Connecticut"),]
CTCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Connecticut"),]
NumCTDeaths <- sum(CTDeaths$Difference)
NumCTCC <- sum(CTCC$Difference)
  
USDeaths$Date
DLDeaths <- USDeaths[which(USDeaths[,8]== "Delaware"),]
DLCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Delaware"),]
NUmDLDeaths <- sum(DLDeaths$Difference)
numDLCC <- sum(DLCC$Difference)

FLDeaths <- USDeaths[which(USDeaths[,8]== "Florida"),]
FLCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Florida"),]
NumFLDeaths <- sum(FLDeaths$Difference)
NumFLCC <- sum(FLCC$Difference)

GADeaths <- USDeaths[which(USDeaths[,8]== "Georgia"),]
GACC <- USConfirmedCases[which(USConfirmedCases[,8]== "Georgia"),]
NumGADeaths <- sum(GADeaths$Difference)
NumGACC <- sum(GACC$Difference)

Hadeaths <- USDeaths[which(USDeaths[,8]== "Hawaii"),]
hacc <- USConfirmedCases[which(USConfirmedCases[,8]== "Hawaii"),]
NumHAdeaths <- sum(Hadeaths$Difference)
NumHAcc <- sum(hacc$Difference)

IDdeaths <- USDeaths[which(USDeaths[,8]== "Idaho"),]
IDCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Idaho"),]
NumIDDeaths <- sum(IDdeaths$Difference)
NumIDCC <- sum(IDCC$Difference)

ILDeaths <- USDeaths[which(USDeaths[,8]== "Illinois"),]
ILCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Illinois"),]
NumILDeaths <- sum(ILDeaths$Difference)
NUmILCC <- sum(ILCC$Difference)

INDdeaths <- USDeaths[which(USDeaths[,8]== "Indiana"),]
INDCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Indiana"),]
NumINDDeaths <- sum(INDdeaths$Difference)
NumINDCC <- sum(INDCC$Difference)

IowaDeaths <- USDeaths[which(USDeaths[,8]== "Iowa"),]
IowaCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Iowa"),]
NumIowaDeaths <- sum(IowaDeaths$Difference)
NumIowaCC <- sum(IowaCC$Difference)

KUDeaths <- USDeaths[which(USDeaths[,8]== "Kansas"),]
KUCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Kansas"),]
NumKUDeaths <- sum(KUDeaths$Difference)
NumKUCC <- sum(KUCC$Difference)

KYDeaths <- USDeaths[which(USDeaths[,8]== "Kentucky"),]
KYCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Kentucky"),]
NumKYdeaths <- sum(KYDeaths$Difference)
NumKYCC <- sum(KYCC$Difference)

LOUDeaths <- USDeaths[which(USDeaths[,8]== "Louisiana"),]
LOUCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Louisiana"),]
NumLOUDeaths <- sum(LOUDeaths$Difference)
NumLOUCC <- sum(LOUCC$Difference)

meds <- USDeaths[which(USDeaths[,8]== "Maine"),]
mecc <- USConfirmedCases[which(USConfirmedCases[,8]== "Maine"),]
NumMEds <- sum(meds$Difference)
NumMEcc <- sum(mecc$Difference)

MDDs <- USDeaths[which(USDeaths[,8]== "Maryland"),]
MDCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Maryland"),]
NumMDDS <- sum(MDDs$Difference)
NumMDCC <- sum(MDCC$Difference)

MassDS <- USDeaths[which(USDeaths[,8]== "Massachusetts"),]
MassCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Massachusetts"),]
NumMassDS <- sum(MassDS$Difference)
NumMassCC <- sum(MassCC$Difference)

MichDeaths <- USDeaths[which(USDeaths[,8]== "Michigan"),]
MichCC <- USConfirmedCases[which(USDeaths[,8]== "Michigan"),]
NumMichDeaths <- sum(MichDeaths$Difference)
NumMICHCC <- sum(MichCC$Difference)

MinnDS <- USDeaths[which(USDeaths[,8]== "Minnesota"),]
MinnCC <- USConfirmedCases[which(USDeaths[,8]== "Minnesota"),]
NumMinnDS <- sum(MinnDS$Difference)
NumMinnCC <- sum(MinnCC$Difference)

MissDS <- USDeaths[which(USDeaths[,8]== "Mississippi"),]
MissCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Mississippi"),]
NumMissDS <- sum(MissDS$Difference)
NumMissCC <- sum(MissCC$Difference)

MizzDS <- USDeaths[which(USDeaths[,8]== "Missouri"),]
MizzCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Missouri"),]
NumMizzDS <- sum(MizzDS$Difference)
NumMizzCC <- sum(MizzCC$Difference)

MontDS <- USDeaths[which(USDeaths[,8]== "Montana"),]
MontCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Montana"),]
NumMontDS <- sum(MontDS$Difference)
NumMontCC <- sum(MontCC$Difference)

NEBraskaDeaths <- USDeaths[which(USDeaths[,8]== "Nebraska"),]
NebraskaCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Nebraska"),]
NumNebraskaDS <- sum(NEBraskaDeaths$Difference)
NumNebraskaCC <- sum(NebraskaCC$Difference)

NHDS <- USDeaths[which(USDeaths[,8]== "New Hampshire"),]
NHCC <- USConfirmedCases[which(USConfirmedCases[,8]== "New Hampshire"),]
NumNHDS <- sum(NHDS$Difference)
NumNHCC <- sum(NHCC$Difference)

NMDS <- USDeaths[which(USDeaths[,8]== "New Mexico"),]
NMCC <- USConfirmedCases[which(USDeaths[,8]== "New Mexico"),]
NumNMDS <- sum(NMDS$Difference)
NumNMCC <- sum(NMCC$Difference)

NCDS <- USDeaths[which(USDeaths[,8]== "North Carolina"),]
NCCC <- USConfirmedCases[which(USConfirmedCases[,8]== "North Carolina"),]
NumNCDS <- sum(NCDS$Difference)
NumNCCC <- sum(NCCC$Difference)

NDDS <- USDeaths[which(USDeaths[,8]== "North Dakota"),]
NDCC <- USConfirmedCases[which(USConfirmedCases[,8]== "North Dakota"),]
NumNDDS <- sum(NDDS$Difference)
NumNDCC <- sum(NDCC$Difference)

NevadaDeaths <- USDeaths[which(USDeaths[,8]== "Nevada"),]
NevadaCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Nevada"),]
NumNVDeaths <- sum(NevadaDeaths$Difference)
NumNVCC <- sum(NevadaCC$Difference)

OhioDeaths <- USDeaths[which(USDeaths[,8]== "Ohio"),]
OhioCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Ohio"),]  
NumOhioDeath <- sum(OhioDeaths$Difference)  
NumOHioCC <- sum(OhioCC$Difference)  
  
OklahomaDeaths <- USDeaths[which(USDeaths[,8]== "Oklahoma"),]
OklahomaCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Oklahoma"),]  
NumOKDeaths <- sum(OklahomaDeaths$Difference)  
NumOKCC <- sum(OklahomaCC$Difference)  

OregonDeaths <- USDeaths[which(USDeaths[,8]== "Oregon"),]
OregonCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Oregon"),]  
NumOregonDeaths <- sum(OregonDeaths$Difference)
NumOregonCC <- sum(OregonCC$Difference)

PADeaths <- USDeaths[which(USDeaths[,8]== "Pennsylvania"),]
PACC <- USConfirmedCases[which(USConfirmedCases[,8]== "Pennsylvania"),]
NumPADeaths <- sum(PADeaths$Difference)
NumPACC <- sum(PACC$Difference)

RIDeaths <- USDeaths[which(USDeaths[,8]== "Rhode Island"),]
RICC <- USConfirmedCases[which(USConfirmedCases[,8]== "Rhode Island"),]
NumRIDeaths <- sum(RIDeaths$Difference)
NumRICC <- sum(RICC$Difference)

SCDeaths <- USDeaths[which(USDeaths[,8]== "South Carolina"),]
SCCC <- USConfirmedCases[which(USConfirmedCases[,8]== "South Carolina"),]
NumSCDeaths <- sum(SCDeaths$Difference)
NumSCCC <- sum(SCCC$Difference)

SDDeaths <- USDeaths[which(USDeaths[,8]== "South Dakota"),]
SDCC <- USConfirmedCases[which(USConfirmedCases[,8]== "South Dakota"),]
NumSDDeaths <- sum(SDDeaths$Difference)
NumSDCC <- sum(SDCC$Difference)

TNDeaths <- USDeaths[which(USDeaths[,8]== "Tennessee"),]
TNCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Tennessee"),]
NumTNDeaths <- sum(TNDeaths$Difference)
NumTNCC <- sum(TNCC$Difference)

TXDeaths <- USDeaths[which(USDeaths[,8]== "Texas"),]
TXCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Texas"),]
NumTXDeaths <- sum(TXDeaths$Difference)
NumTXCC <- sum(TXCC$Difference)

UTDeaths <- USDeaths[which(USDeaths[,8]== "Utah"),]
UTCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Utah"),]
NumUTDeaths <- sum(UTDeaths$Difference)
NumUTCC <- sum(UTCC$Difference)

VTDeaths <- USDeaths[which(USDeaths[,8]== "Vermont"),]
VTCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Vermont"),]
NumVTDeaths <- sum(VTDeaths$Difference)
NumVTCC <- sum(VTCC$Difference)

ViRDeaths <- USDeaths[which(USDeaths[,8]== "Virginia"),]
VirCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Virginia"),]
NumVADeaths <- sum(ViRDeaths$Difference)
NumVACC <- sum(VirCC$Difference)

WashingtonDeaths <- USDeaths[which(USDeaths[,8]== "Washington"),]
WashingtonCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Washington"),]
NumWashDeaths <- sum(WashingtonDeaths$Difference)
NumWashCC <- sum(WashingtonCC$Difference)

WVDeaths <- USDeaths[which(USDeaths[,8]== "West Virginia"),]
WVCC <- USConfirmedCases[which(USConfirmedCases[,8]== "West Virginia"),]
NumWVDeaths <- sum(WVDeaths$Difference)
NumWVCC <- sum(WVCC$Difference)

WYomingDeaths <- USDeaths[which(USDeaths[,8]== "Wyoming"),]
WyomingCC <- USConfirmedCases[which(USConfirmedCases[,8]== "Wyoming"),]
NumWyomingDeaths <- sum(WYomingDeaths$Difference)
NumWyomingCC <- sum(WyomingCC$Difference)


AlaskaDeaths <- USDeaths[which(USDeaths[,8]== "Alaska"),]
AlaskaConfirmedCases <- USConfirmedCases[which(USConfirmedCases[,8]== "Alaska"),]
NumberOFAlaskaDeaths <- sum(AlaskaDeaths$Difference)
NumberOFAlaskaConfirmedDeaths <- sum(AlaskaConfirmedCases$Difference)

WisconsinDeaths <- USDeaths[which(USDeaths[,8]== "Wisconsin"),]
WisconsinConfirmedCases <- USConfirmedCases[which(USConfirmedCases[,8]== "Wisconsin"),]
NumberOFWisconsinDeaths <- sum(WisconsinDeaths$Difference)
NumberOFWisconsinConfirmedCases <- sum(WisconsinConfirmedCases$Difference)

NewJerseyDeaths <- USDeaths[which(USDeaths[,8]== "New Jersey"),]
NewJerseyConfirmedCases <- USConfirmedCases[which(USConfirmedCases[,8]== "New Jersey"),]
NumberOFNewJerseyDeaths <- sum(NewJerseyDeaths$Difference)
NumberOFNewJerseyConfirmedCases <- sum(NewJerseyConfirmedCases$Difference)

`nst.est2019.alldata.(1)` <- read.csv("~/Downloads/nst-est2019-alldata (1).csv", header=FALSE)
USPopulationDataSet <- `nst.est2019.alldata.(1)`
View(USPopulationDataSet)
names(USPopulationDataSet)[names(USPopulationDataSet)=="V5"] <- "StName"
names(USPopulationDataSet)[names(USPopulationDataSet)=="V17"] <- "StPopulation"
str(USPopulationDataSet)
USPopulationDataSet <- USPopulationDataSet[,-18:-151]
str(USPopulationDataSet)
USPopulationDataSet$StPopulation <- as.integer(gsub(',','',USPopulationDataSet$StPopulation))
USPopulationDataSet <- USPopulationDataSet[,-6:-16]
str(USPopulationDataSet)
USPopulationDataSet <- USPopulationDataSet[,-1:-4]
USPopulationDataSet <- USPopulationDataSet[-1:-6,]
USPopulationDataSet <- USPopulationDataSet[-9,]
USPopulationDataSet <- USPopulationDataSet[-51,]


stateName <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia","Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico","New York", "North Carolina", "North Dakota", "Ohio","Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming")
ConfirmedCases <- c(10700, 383, 12216, 4236, 72798, 20475, 34855, 6952, 42402, 35427, 638, 2293, 84694, 25473, 13289, 7518, 6853, 32662, 1515, 34812, 80497, 37710, 36006, 10090, 10404, 462, 8912, 6476, 3299, 141560, 16763, 340661, 16352, 1647, 25721, 4858, 3416, 62101, 11835, 8030, 3732, 16370, 43020, 6620, 929, 26746, 17512, 1398, 10902, 688 )
Deaths <- c(450, 10, 595, 97, 2957, 1062, 3125, 247, 1827, 1517, 17, 69, 3792, 1619, 306, 188, 321, 2381, 66, 1809, 5315, 4714, 638, 465, 551, 16, 103, 331, 150, 9714, 231, 27477, 625, 40, 1483, 278, 134, 4094, 462, 362, 39, 273, 1172, 75, 53, 928, 974, 58, 421, 7)
Population <- c(USPopulationDataSet$StPopulation)

USCOVID19df <- data.frame(stateName, ConfirmedCases, Deaths, Population)
USCOVID19CasesLeastToGreatest <- USCOVID19df[order(USCOVID19df$ConfirmedCases),]
USCOVID19CasesGreatestToLeast <- USCOVID19df[order(-USCOVID19df$ConfirmedCases),]
USCOVID19DeathsLeastToGreatest <- USCOVID19df[order(USCOVID19df$Deaths),]
USCOVID19DeathsGreatestToLeast <- USCOVID19df[order(-USCOVID19df$Deaths),]
Top10COVID19CasesGreatestToLeast <- USCOVID19CasesGreatestToLeast[which(USCOVID19CasesGreatestToLeast$ConfirmedCases > 35500),]
USCOVID19PopulationGreatestToLeast <- USCOVID19df[order(-USCOVID19df$Population),]
Top10PopulationGreatestToLeast <- USCOVID19PopulationGreatestToLeast[which(USCOVID19PopulationGreatestToLeast$Population > 8999999),]
Top10PopulationGreatestToLeast
USCOVID19df["Death per Case"] <- USCOVID19df$Deaths/USCOVID19df$ConfirmedCases

library(ggplot2)
Top10COVID19CasesGreatestToLeast$ConfirmedCases <- format(Top10COVID19CasesGreatestToLeast$ConfirmedCases, scientific = F)
Top10PopulationGreatestToLeast$Population <- format(Top10PopulationGreatestToLeast$Population, scientific = F)
Top10PopulationGreatestToLeast$ConfirmedCases <- format(Top10PopulationGreatestToLeast$ConfirmedCases, scientific = F)

ConfirmedCasesBar <- ggplot(Top10COVID19CasesGreatestToLeast, aes(stateName, ConfirmedCases)) + geom_bar(stat= 'identity') + ggtitle("Top 10 States Confirmed Cases")
PopulationBar <- ggplot(Top10PopulationGreatestToLeast, aes(stateName, Population)) + geom_bar(stat= 'identity') + ggtitle("Top 10 Population Bar")
PopulationAndConfirmedCasesBar <- ggplot(Top10PopulationGreatestToLeast, aes(Population, `Case per Capita` , fill=stateName)) + geom_bar(stat= 'identity') + ggtitle("Top 10 Population compared to Case per Capita Bar")
PopulationAndConfirmedCasesBar <- ggplot(Top10PopulationGreatestToLeast, aes(Population, ConfirmedCases, fill=stateName)) + geom_bar(stat= 'identity') + ggtitle("Top 10 Population Bar")

DeathBar <- ggplot(Top10D)
ConfirmedCasesBar
PopulationBar
cor(USCOVID19df$Population, USCOVID19df$ConfirmedCases)
cor(Top10PopulationGreatestToLeast$Population, Top10PopulationGreatestToLeast$ConfirmedCases)
str(Top10PopulationGreatestToLeast)
SortByDeathPerCase <- USCOVID19df[order(USCOVID19df$`Death per Case`),]

USCOVID19df["Case per Capita"] <- USCOVID19df$ConfirmedCases/USCOVID19df$Population
USCOVID19df
USCOVID19df["Death per Capita"] <- format(USCOVID19df$Deaths/USCOVID19df$Population, scientific = F)

SortByDeathPerCase
SortCasePerCapitaLeastToGreatest <- USCOVID19df[order(USCOVID19df$`Case per Capita`),]
SortCasePerCapitaGreatestToLeast <- USCOVID19df[order(-USCOVID19df$`Case per Capita`),]

CasePerCapitaDf <- SortCasePerCapitaGreatestToLeast
mean(SortCasePerCapitaGreatestToLeast$`Case per Capita`)
mean(CasePerCapitaDf$`Case per Capita`)
CasePerCapitaDf


head(SortCasePerCapitaLeastToGreatest)
tail(SortCasePerCapitaLeastToGreatest)
SortDeathPerCapitaLeastToGreatest <- USCOVID19df[order(USCOVID19df$`Death per Capita`),]
head(SortDeathPerCapitaLeastToGreatest)
tail(SortDeathPerCapitaLeastToGreatest)

library(ggplot2)
library(scales)

CasePerCapitaLineAll50States <- ggplot(aes(x = as.numeric(stateName), y = `Case per Capita`), data = USCOVID19df) + 
  geom_line( ) + 
  ggtitle("Case Per Capita(All States)")

SortCasePerCapitaLeastToGreatest
format(SortCasePerCapitaLeastToGreatest$`Death per Capita`, scientific = F)

Top10CasesPerCapita <- SortCasePerCapitaLeastToGreatest[which(SortCasePerCapitaLeastToGreatest$`Case per Capita` > .006),]
Top10CasesPerCapitaGreatestToLeast <- Top10CasesPerCapita[order(-Top10CasesPerCapita$`Case per Capita`),]
Top10CasesPerCapitaGreatestToLeast$Population <- format(Top10CasesPerCapitaGreatestToLeast$Population, scientific = F)
Top10CasesPerCapitaGreatestToLeast$`Case per Capita` <- format(Top10CasesPerCapitaGreatestToLeast$`Case per Capita`, scientific = F)
Top10CasesPerCapitaGreatestToLeast$ConfirmedCases <- format(Top10CasesPerCapitaGreatestToLeast$ConfirmedCases, scientific = F)

scale_y_continuous(labels=ConfirmedCases{format(n, scientific = FALSE)})
lm_fit <- lm(ConfirmedCases ~ `Case per Capita` + Population, data = Top10CasesPerCapita)
predicted_df <- data.frame(ConfirmedCases_pred = predict(lm_fit, Top10CasesPerCapita), Population=Top10CasesPerCapita$Population)
regressionline <- ggplot(data = Top10CasesPerCapita, aes(x=ConfirmedCases, y=`Case per Capita`, colour=stateName)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
simple_regression <- ggplot(data = Top10CasesPerCapita, aes(x=ConfirmedCases, y=))

Top10CasesPerCapitaScatterPlot <- ggplot(Top10CasesPerCapita, aes(x = `ConfirmedCases`, y = `Case per Capita` , colour = stateName, size = Population)) + geom_point() + ggtitle("Top 10 State Case Per Capita")


Top10CasesPerCapitaScatterPlot1 <- ggplot(Top10CasesPerCapita, aes(x = Population, y = `Case per Capita` , colour = stateName , size = ConfirmedCases)) + geom_point() + ggtitle("Top 10 Cases per Capita")
Top10CasesPerCapitaScatterPlot
plot(Top10PopulationGreatestToLeast$Population, Top10CasesPerCapita$`Case per Capita`)
cor(Top10PopulationGreatestToLeast$Population, Top10CasesPerCapita$`Case per Capita`)


cor(Top10CasesPerCapita$ConfirmedCases, Top10CasesPerCapita$`Case per Capita`)
cor(Top10CasesPerCapita$Population, Top10CasesPerCapita$ConfirmedCases)
model1 <- lm(USCOVID19df$`Case per Capita`~USCOVID19df$Population)
model2 <- lm(USCOVID19df$ConfirmedCases~USCOVID19df$Deaths)
model3 <- lm(USCOVID19df$Population~USCOVID19df$Deaths)
model4 <- lm(USCOVID19df$`Case per Capita`~USCOVID19df$Deaths)
model5 <- lm(USCOVID19df$Population~USCOVID19df$`Death per Capita`)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
abline(lm(USCOVID19df$ConfirmedCases~USCOVID19df$Deaths))

Top10CasePerCapitaLine <- ggplot(aes(x = as.numeric(stateName), y = `Case per Capita`), data = Top10CasesPerCapita) + 
  geom_line( ) + 
  ggtitle("Top 10 State Case Per Capita")

Top10CasePerCapitaLine
Top10CasesPerCapita

summary(model1)

plCasePerCapitaLineAll50StatesScatterPlot <- ggplot(SortCasePerCapitaLeastToGreatest, aes(x = as.numeric(stateName) , y = `Case per Capita`)) + geom_point(aes(size =Population, color=ConfirmedCases))
CasePerCapitaDeathPerCapitaAll50StatesScatterPlot <- ggplot(SortCasePerCapitaLeastToGreatest, aes(x = `Death per Capita`, y = `Case per Capita`, shape=stateName)) + geom_point()
CasePerCapitaDeathPerCapitaAll50StatesScatterPlot
cor(Top10CasesPerCapita$`Case per Capita`, Top10CasesPerCapita$`Death per Capita` )
CasePerCapitaLineAll50StatesScatterPlot

SortDeathPerCapitaLeastToGreatest
Top10DeathPerCapita <- SortDeathPerCapitaLeastToGreatest[which(SortDeathPerCapitaLeastToGreatest$`Death per Capita` > 0.0002991),]
USCOVID19df

plot(USCOVID19df$Population, USCOVID19df$`Case per Capita`, main = "ScatterPlot of States Population and CPC")
cor(USCOVID19df$Population, USCOVID19df$`Case per Capita`)

 model1 <- lm(USCOVID19df$`Case per Capita`~USCOVID19df$Population)
 summary(model1)
 abline(model1, col=4)

 plot(Top10CasesPerCapita$Population, Top10CasesPerCapita$`Case per Capita`, main = "Scatterlot of Top 10 CPC with Population")
cor(Top10CasesPerCapita$Population, Top10CasesPerCapita$`Case per Capita`)

model2 <- lm(Top10CasesPerCapita$`Case per Capita`~Top10CasesPerCapita$Population)
summary(model2)
abline(model2,col=4)

model3 <- lm(Top10CasesPerCapita$`Case per Capita`~Top10CasesPerCapita$Population + Top10CasesPerCapita$ConfirmedCases)
summary(model3)

model4 <- lm(USCOVID19df$`Case per Capita`~USCOVID19df$Population + USCOVID19df$ConfirmedCases)
summary(model4)

model6 <- lm(USCOVID19df$`Case per Capita`~USCOVID19df$ConfirmedCases)
summary(model6)
abline(model1, col=4)
Top10PopulationGreatestToLeast$Population <- as.numeric(Top10PopulationGreatestToLeast$Population)
Top10PopulationGreatestToLeast$ConfirmedCases <- as.numeric(Top10PopulationGreatestToLeast$ConfirmedCases)

cor(Top10PopulationGreatestToLeast$Population, Top10PopulationGreatestToLeast$ConfirmedCases)
str(Top10PopulationGreatestToLeast)
