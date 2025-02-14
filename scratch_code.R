
library("readxl")
library("tidyverse")
library("reshape2")

# load supplemental data tables from Ruggerone & Irvine (2018) for pink salmon (not grabbing the final "total" column)
pink <- list(
  #adult_N, adult_B, avg_wt
  # Table S9 total adult return abundance (hatchery & wild) - millions of fish
       adult_N = read_excel("data/mcf210023-sup-0001-tables1-s24.xlsx", range = "ST 9-12 Total ret (nos) 52-15!A6:O70"),
  # Table S9 total adult return biomass (hatchery & wild) - metric tonnes
       adult_B = read_excel("data/mcf210023-sup-0001-tables1-s24.xlsx", range = "ST 13-16 Tot ret (bioma) 52-15!A6:O70")
  )

# back-calculate average weights used in R&I - kg
pink <- append(pink, list(avg_wt = pink$adult_B[,-1]/pink$adult_N[,-1]/1000))
pink$avg_wt <- cbind(Year = seq(1952,2015,1),pink$avg_wt)

chum <- list(
  # adult_N, adult_B, avg_wt
  # Table S10 total adult return abundance (hatchery & wild) - millions of fish
  adult_N = read_excel("data/mcf210023-sup-0001-tables1-s24.xlsx", range = "ST 9-12 Total ret (nos) 52-15!R6:AF70"),
  # Table S14 adult biomass (hatchery & wild) - metric tonnes 
  adult_B = read_excel("data/mcf210023-sup-0001-tables1-s24.xlsx", range = "ST 13-16 Tot ret (bioma) 52-15!R6:AF70")
)

# back-calculate average weights used in R&I - kg
chum <- append(chum, list(avg_wt = chum$adult_B[,-1]/chum$adult_N[,-1]/1000))
chum$avg_wt <- cbind(Year = seq(1952,2015,1),chum$avg_wt)

sock <- list(
  # adult_N, adult_B, avg_wt
  # Table S11 total adult return abundance (hatchery & wild)
  adult_N = read_excel("data/mcf210023-sup-0001-tables1-s24.xlsx", range = "ST 9-12 Total ret (nos) 52-15!AI6:AW70"),
  # Table S15 adult biomass (hatchery & wild)   
  adult_B = read_excel("data/mcf210023-sup-0001-tables1-s24.xlsx", range = "ST 13-16 Tot ret (bioma) 52-15!AI6:AW70")
)
 
# back-calculate average weights used in R&I - kg
sock <- append(sock, list(avg_wt = sock$adult_B[,-1]/sock$adult_N[,-1]/1000))
sock$avg_wt <- cbind(Year = seq(1952,2015,1),sock$avg_wt)

  
# average weight data in long format
RI_avgwt.long <- melt(pink$avg_wt, id = "Year", variable.name = "Region", value.name = "avg_wt")

#RI_avgwt.long <- rbind(melt(pink$avg_wt, id = "Year", variable.name = "Region", value.name = "avg_wt"),
#                 melt(chum$avg_wt, id = "Year", variable.name = "Region", value.name = "avg_wt"),
#                 melt(sock$avg_wt, id = "Year", variable.name = "Region", value.name = "avg_wt")
#                  )

# plot R&I Alaska pink avg wt data by region  
p <- ggplot(data = filter(RI_avgwt.long, Region %in% c("WAK", "SPen","Kod","CI","PWS","SEAK")), aes(x = Year, y = avg_wt)) + 
     geom_point() + 
     facet_wrap(~Region, scales = "free", ncol=3)+
     ggtitle("Backcalculated Average Weights from R&I - AK pink salmon") +
     ylab("Average Weight (kg)")+
     theme(plot.title = element_text(hjust = 0.5))
  dev.new()
  p

# Ruggerone and Irvine divide Alaska up into 6 regions/areas that include Southeastern Alaska & Yakutat, Prince William Sound, Cook Inlet, Kodiak (Island & Mainland), South Alaska Peninsula (including Chignik), and Western Alaska (including N. Alaska Peninsula, Bristol Bay, Arctic-Yukon-Kuskokwim & Kotzebue)  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
     
# load NPAFC catch data (2024 update), filter to AK commercial catch only and R&I range of years)
NPAFC <- read_csv("data/NPAFC_Catch_Stat-1925-2023_2024-04-29_lngfrm.csv", show_col_types = FALSE)
NPAFC_AK <- filter(NPAFC, Region=="Alaska" & Fishery=="Commercial" & Area !="Whole state" & between(Year,1952,2015) & Species=="Sockeye")

# calculate average weight  (numbers = indiv; weight = MT) and remove extraneous columns
NPAFC_avgwt <- data.frame(NPAFC_AK,avg_wt=NPAFC_AK$Wt_fish/NPAFC_AK$N_fish*1000)
NPAFC_avgwt <- NPAFC_avgwt[c(3,4,6,9)]
NPAFC_avgwt$Area <- factor(NPAFC_avgwt$Area, levels = c("Western","South Central","Southeast","Arctic Yukon Kuskokwim","Westward","Central"))             

rm(NPAFC) 

# plot NPAFC Alaska pink salmon avg wt data by area    
p <- ggplot(data = NPAFC_avgwt, aes(x = Year, y = avg_wt)) + 
     geom_point() + 
     facet_wrap(~Area, scales = "free", ncol=3)+ 
     ggtitle("Calculated Average Weights from NPAFC data - AK pink salmon") +
     ylab("Average Weight (kg)")+
     theme(plot.title = element_text(hjust = 0.5))
  dev.new()
  p  

# Alaska data reported to NPAFC are divided differently before and after 1985 and are called Reporting Areas in the database.
# Pre-1985, the Alaska catch data are summarized into 3 reporting areas: Southeast, South Central (Central in Statistical Yearbooks), and Western.
# 1985 and on, there are 4 reporting areas that match the Division of Commercial Fisheries regions: Southeast, Central (including PWS, CI and Bristol Bay), Westward (including Kodiak Island and Mainland, Chignik, N. & S. Alaska Peninsula), and Arctic-Yukon-Kuskokwim (including Kotzebue)    up into 6 regions/areas that include Southeastern Alaska & Yakutat, Prince William Sound, Cook Inlet, Kodiak (Island & Mainland), South Alaska Peninsula (including Chignik), and Western Alaska (including N. Alaska Peninsula, Bristol Bay, Arctic-Yukon-Kuskokwim & Kotzebue)  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~     

# Southeast Alaska is the same region/area in R&I as reported to NPAFC for the entire time series

# add data source to data frames and rearrange to match; rbind to plot   
RI_avgwt.long <- data.frame(Source=rep("R&I", length(RI_avgwt.long[1])),RI_avgwt.long[1:2],Species=rep("Pink", length(RI_avgwt.long[1])),RI_avgwt.long[3] )
NPAFC_avgwt <- data.frame(Source=rep("NPAFC", length(NPAFC_avgwt[1])), NPAFC_avgwt[,c(3,1,2,4)])
colnames(NPAFC_avgwt)[3] <- "Region"

# plot SEAK data from R&I and NPAFC
data<- rbind(filter(RI_avgwt.long, Region =="SEAK"),filter(NPAFC_avgwt, Region =="Southeast"))
  
p <- ggplot(data, aes(x=Year, y=avg_wt, color=Source))+
     geom_point(data = subset(data, Region =="SEAK"))+
     geom_line(data = subset(data, Region == "Southeast"))+
     ggtitle("Average Weight Comparison - SEAK pink salmon") +
     ylab("Average Weight (kg)")+
     theme(plot.title = element_text(hjust = 0.5))
  dev.new()
  p
  
#-------------------------------------------------------------------------------
 # draft figures comparing R&I with NPAFC data 
  p <-ggplot()+
    geom_point(data = filter(RI_avgwt.long, Region =="WAK"), aes(x = Year, y = avg_wt))+
    geom_line(data = filter(NPAFC_avgwt, Region == "Western"),aes(x = Year, y = avg_wt), color = "red")+
    geom_line(data = filter(NPAFC_avgwt, Region == "Westward"),aes(x = Year, y = avg_wt), color = "blue")
  dev.new()
  p 
  
  p <-ggplot()+
    geom_point(data = filter(RI_avgwt.long, Region =="Kod"), aes(x = Year, y = avg_wt))+
    geom_line(data = filter(NPAFC_avgwt, Region == "South Central"),aes(x = Year, y = avg_wt), color = "red")+
    geom_line(data = filter(NPAFC_avgwt, Region == "Central"),aes(x = Year, y = avg_wt), color = "blue")+
    
    geom_point(data = filter(NPAFC_avgwt, Region == "Westward"),aes(x = Year, y = avg_wt), color = "darkmagenta")+
    geom_line(data = filter(NPAFC_avgwt, Region == "Westward"),aes(x = Year, y = avg_wt), color = "darkmagenta")
  dev.new()
  p 
  
  #plot regions in RI that used South Central/Central Region as basis for avg weight
  dev.new()
  ggplot(data = filter(RI_avgwt.long, Region %in% c("SPen","Kod","CI","PWS")), aes(x = Year, y = avg_wt, colour=Region)) + 
    geom_jitter()
    geom_line(data=NPAFC_avgwt, aes(x=Year, y=avg_wt))

#-------------------------------------------------------------------------------  
    
# Read data file that has 1985-2015 with R&I area splits
ADFG <- read_csv("data/NPAFC_RI_splits1985-2015.csv",show_col_types = FALSE)
ADFG <- filter(ADFG, Species %in% c("Pink","Chum","Sockeye"))    

# sum numbers and weights by R&I areas
ADFG_RI <- na.omit(ADFG) %>%
           group_by(RI_Area, Species, Year) %>% 
           summarise_at(c("Number", "Whole_Wt_ metric_tons"), sum, na.rm = TRUE)

ADFG_RI_avgwt <- cbind(ADFG_RI,avg_wt = ADFG_RI$`Whole_Wt_ metric_tons`*1000/ADFG_RI$Number)
colnames(ADFG_RI_avgwt)[1]<-"Region"
ADFG_RI_avgwt$Region <- factor(ADFG_RI_avgwt$Region, levels =c("WAK", "SPen","Kod","CI","PWS","SEAK"))

ADFG_RI_avgwt <- data.frame(Source=rep("ADFG", length(ADFG_RI_avgwt[1])), ADFG_RI_avgwt[,c(3,1,2,6)])


p <- ggplot(data = filter(ADFG_RI_avgwt, Species=="Sockeye"), aes(x = Year, y = avg_wt))+ 
  geom_point()+ 
  facet_wrap(~Region, scales = "free", ncol=3)+
  ggtitle("Calculated Average Weights from ADF&G data - AK sockeye salmon") +
  ylab("Average Weight (kg)")+
  theme(plot.title = element_text(hjust = 0.5))
dev.new()
p

# calculate average weights by CF regions for comparison with NPAFC data
ADFG_CF <- na.omit(ADFG) %>%
  group_by(Region, Species, Year) %>% 
  summarise_at(c("Number", "Whole_Wt_ metric_tons"), sum, na.rm = TRUE)

ADFG_CF_avgwt <- cbind(ADFG_CF,avg_wt = ADFG_CF$`Whole_Wt_ metric_tons`*1000/ADFG_CF$Number)
ADFG_CF_avgwt$Region <- factor(ADFG_CF_avgwt$Region, levels =c("A-Y-K", "Westward","Central","Southeastern"))

p <- ggplot(data = filter(ADFG_CF_avgwt, Species=="Sockeye"), aes(x = Year, y = avg_wt))+ 
  geom_point()+ 
  facet_wrap(~Region, scales = "free", ncol=2)+
  ggtitle("Calculated Average Weights from ADF&G data - AK sockeye salmon") +
  ylab("Average Weight (kg)")+
  theme(plot.title = element_text(hjust = 0.5))
dev.new()
p


data <- filter(NPAFC_avgwt, Year >= 1985, Region %in% c("Arctic Yukon Kuskokwim", "Westward","Central", "Southeast"))

data2 <-  filter(ADFG_CF_avgwt, Species == "Pink") %>% select(Year, Region, Species,avg_wt)
data2 <- data.frame(Source=rep("ADFG",length(data2[1])),data2)

data3 <- rbind(data,data2)
data3[data3=="Arctic Yukon Kuskokwim"]<-"A-Y-K"
data3[data3=="Southeastern"]<-"Southeast"

p <- ggplot(data=data3, aes(x=Year, y=avg_wt,fill=Source))+
geom_bar(stat="identity", color="black",position = position_dodge())+
  coord_cartesian(ylim=c(0,3))+
  #scale_fill_manual(values=c('#999999','#E69F00'))+
  scale_fill_brewer(palette="Blues")+
  facet_wrap(~Region, scales = "free", ncol=2)
dev.new()
p

################################################################################

RI_adultN_pink <- melt(pink$adult_N %>% select( c(Year,WAK,SPen,Kod,CI,PWS,SEAK)), id = "Year", variable.name = "Region", value.name = "number")
RI_adultN_chum <- melt(chum$adult_N %>% select( c(Year,WAK,SPen,Kod,CI,PWS,SEAK)), id = "Year", variable.name = "Region", value.name = "number")
RI_adultN_sock <- melt(sock$adult_N %>% select( c(Year,WAK,SPen,Kod,CI,PWS,SEAK)), id = "Year", variable.name = "Region", value.name = "number")

RI_adultB_pink <- melt(pink$adult_B %>% select( c(Year,WAK,SPen,Kod,CI,PWS,SEAK)), id = "Year", variable.name = "Region", value.name = "weight")
RI_adultB_pink <- filter(RI_adultB_pink, Year>=1985)

RI_adultB_chum <- melt(chum$adult_B %>% select( c(Year,WAK,SPen,Kod,CI,PWS,SEAK)), id = "Year", variable.name = "Region", value.name = "weight")
RI_adultB_chum <- filter(RI_adultB_chum, Year>=1985)

RI_adultB_sock <- melt(sock$adult_B %>% select( c(Year,WAK,SPen,Kod,CI,PWS,SEAK)), id = "Year", variable.name = "Region", value.name = "weight")
RI_adultB_sock <- filter(RI_adultB_sock, Year>=1985)

New_pink <-merge(filter(RI_adultN_pink, Year>=1985), filter(ADFG_RI_avgwt, Species=="Pink"), by = c("Region","Year"), all = TRUE)
New_pink <- cbind(New_pink, weight = New_pink$number*New_pink$avg_wt*1000)

New_chum <-merge(filter(RI_adultN_chum, Year>=1985), filter(ADFG_RI_avgwt, Species=="Chum"), by = c("Region","Year"), all = TRUE)
New_chum <- cbind(New_chum, weight = New_chum$number*New_chum$avg_wt*1000)

New_sock <-merge(filter(RI_adultN_sock, Year>=1985), filter(ADFG_RI_avgwt, Species=="Sockeye"), by = c("Region","Year"), all = TRUE)
New_sock <- cbind(New_sock, weight = New_sock$number*New_sock$avg_wt*1000)

p <- ggplot(data = New_pink, aes(x=Year, y=weight))+
  geom_bar(stat="identity",color="black")+
  facet_wrap(~Region, scales = "free", ncol=3)
dev.new()
p

New_old_pink<- rbind(
  cbind(Source=rep("R&I",length(RI_adultB_pink[1])),RI_adultB_pink),
  cbind(Source=rep("New",length(New_pink[1])),New_pink[,c(2,1,7)])
)
New_old_pink$Source <- factor(New_old_pink$Source, levels =c("R&I", "New"))

New_old_chum<- rbind(
  cbind(Source=rep("R&I",length(RI_adultB_chum[1])),RI_adultB_chum),
  cbind(Source=rep("New",length(New_chum[1])),New_chum[,c(2,1,7)])
)
New_old_chum$Source <- factor(New_old_chum$Source, levels =c("R&I", "New"))

New_old_sock<- rbind(
  cbind(Source=rep("R&I",length(RI_adultB_sock[1])),RI_adultB_sock),
  cbind(Source=rep("New",length(New_sock[1])),New_sock[,c(2,1,7)])
)
New_old_sock$Source <- factor(New_old_sock$Source, levels =c("R&I", "New"))

p <- ggplot(data=New_old_pink, aes(x=Year, y=weight,fill=Source))+
  geom_bar(stat="identity", color="black",position = position_dodge())+
  #scale_fill_manual(values=c('#999999','#E69F00'))+
  scale_fill_brewer(palette="Blues")+
  facet_wrap(~Region, scales = "free", ncol=2)+
  ggtitle("Comparison of Biomass Estimates - AK pink salmon") +
  ylab("Adult Return Biomass (MT)")+
  theme(plot.title = element_text(hjust = 0.5))
dev.new()
p

p <- ggplot(data=New_old_chum, aes(x=Year, y=weight,fill=Source))+
  geom_bar(stat="identity", color="black",position = position_dodge())+
  #scale_fill_manual(values=c('#999999','#E69F00'))+
  scale_fill_brewer(palette="Blues")+
  facet_wrap(~Region, scales = "free", ncol=2)+
  ggtitle("Comparison of Biomass Estimates - AK chum salmon") +
  ylab("Adult Return Biomass (MT)")+
  theme(plot.title = element_text(hjust = 0.5))
dev.new()
p

p <- ggplot(data=New_old_sock, aes(x=Year, y=weight,fill=Source))+
  geom_bar(stat="identity", color="black",position = position_dodge())+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  #scale_fill_brewer(palette="Blues")+
  facet_wrap(~Region, scales = "free", ncol=2)+
  ggtitle("Comparison of Biomass Estimates - AK sockeye salmon") +
  ylab("Adult Return Biomass (MT)")+
  theme(plot.title = element_text(hjust = 0.5))
dev.new()
p

x<- summarize(New_old_sock$weight,across(c(Pink, Chum, Sockeye, Total), sum))

x <-New_old_sock%>%
  group_by(Source, Year) %>%
  summarise(Biomass = sum(weight))
   
p <- ggplot(data=x, aes(x=Year, y=Biomass,fill=Source))+
  geom_bar(stat="identity", color="black",position = position_dodge())+
  scale_fill_manual(values=c('#999999','#E69F00'))+
  #scale_fill_brewer(palette="Blues")+
  ggtitle("Comparison of Biomass Estimates - AK sockeye salmon") +
  ylab("Adult Return Biomass (MT)")+
  theme(plot.title = element_text(hjust = 0.5))
dev.new()
p
################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~
# extra code

p <- ggplot()+
  ggtitle("Backcalculated Average Weights from R&I - AK pink salmon") +
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point(data = filter(RI_avgwt.long, Region %in% c("WAK", "SPen","Kod","CI","PWS","SEAK")), aes(x = Year, y = avgwt)) + 
  facet_wrap(~Region, scales = "free", ncol=3)+
  geom_line(data = ADFG2, aes(x = Year, y = avg_wt))+
  facet_wrap(~Region, scales = "free", ncol=3)
dev.new()
p

p2 <-ggplot()+
    geom_point(data=RI_avgwt.long, aes(x = Year, y = avgwt))+
       facet_wrap(~RI_Area, scales = "free", ncol=3)
  geom_line(data = filter(NPAFC_AK, Area == "Western"),aes(x = Year, y = avg_wt), color = "red")+

    p <-ggplot()+
    geom_point(data = filter(RI_avgwt.long, Region =="Kod"), aes(x = Year, y = avgwt))+
    geom_line(data = filter(NPAFC_AK, Area == "South Central"),aes(x = Year, y = avg_wt), color = "red")+
    geom_line(data = filter(NPAFC_AK, Area == "Central"),aes(x = Year, y = avg_wt), color = "blue")+
    geom_point(data = filter(NPAFC_AK, Area == "Westward"),aes(x = Year, y = avg_wt), color = "green")
  dev.new()    
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# scratch code to calculate average weight.  The metric tons conversion is what was used by S. Donellan in OceanAK
ADFG2 <- data.frame(ADFG2,ADFG2[,8]/2204.62262) # convert to metric tons
ADFG <- cbind(ADFG,ADFG[,8]*0.45359237) # convert to kg
colnames(ADFG)[13] <- "Wt_kg"       

ADFG <- cbind(ADFG,ADFG$Wt_kg/ADFG$`Number Of Fish (estimated)`)
colnames(ADFG)[14] <- "avg_wt" 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load NPAFC data that is based on query that splits by management area and separates NPen and SPen in Area M
# !!!! Whole weight data are missing or may not be fully reported - if numbers match with current NPAFC data default to those weights
NPAFC_RI <- read.csv("data/NPAFC_RI_splits1985-2015.csv")

# First summarize by Region to cross check with current NPAFC data (does it matter? using it to calc avg weights. comparing weight of catch and possibly avg wts would be useful. Don't expect everything to match because data qa/qc updates, maybe slight difference in methods/values to estimate numbers of fish from weight of catch)
NPAFC_RI1 <- filter(NPAFC_RI, Species %in% c("Pink","Chum","Sockeye"))
NPAFC_RI2 <- NPAFC_RI1 %>%
  group_by(Region, Species, Year) %>% summarise(Number=sum(Number) )

 #can I?: 
    #group_by(Region, Species, Year) %>% summarise(Number=sum(Number), Weight=sum(Weight) )

p <- ggplot(data = NPAFC_RI2, aes(x = Year, y = Number)) + geom_point() + facet_wrap(~Region+Species, scales = "free", ncol=3)  
dev.new()
p

NPAFC <- read_csv("data/NPAFC_Catch_Stat-1925-2023_2024-04-29_lngfrm.csv")
NPAFC_AK <- filter(NPAFC, Region=="Alaska" & Fishery=="Commercial" & between(Year,1985,2015) & Species %in% c("Pink","Chum","Sockeye") & Area %in% c("Southeast", "Central", "Arctic Yukon Kuskokwim", "Westward"))

p <- ggplot(data = NPAFC_AK, aes(x = Year, y = N_fish)) + geom_point() + facet_wrap(~Region+Species, scales = "free", ncol=3)  
windows()
p

colnames(NPAFC_AK)[2] <- "State"
colnames(NPAFC_AK)[3] <- "Region"

NPAFC_AK <- NPAFC_AK %>% 
  mutate(Region = ifelse(as.character(Region) == "Southeast", "Southeastern", as.character(Region)),
         Region = ifelse(as.character(Region) == "Arctic Yukon Kuskokwim", "A-Y-K", as.character(Region))
  )

x <- merge(NPAFC_AK,NPAFC_RI2, by= c("Year","Species","Region"),all=TRUE, na.rm = TRUE)

x <- data.frame(x, y =(x$Number - x$N_fish))
windows()
plot(x$N_fish,x$y)

p <- ggplot(data = x, aes(x = Year, y = y)) + geom_point() + facet_wrap(~Region+Species, scales = "free", ncol=3)  
windows()
p
