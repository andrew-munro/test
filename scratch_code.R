
library("readxl")
library("tidyverse")
library("reshape2")

# load supplemental data tables from Ruggerone & Irvine (2018) for pink salmon (not grabbing the final "total" column)
pink <- list(
  #adult_N, adult_B, avgWt, tot_B
  # Table S9 total adult return abundance (hatchery & wild)
       adult_N = read_excel("data/mcf210023-sup-0001-tables1-s24.xlsx", range = "ST 9-12 Total ret (nos) 52-15!A6:O70"),
  # Table S9 total adult return biomass (hatchery & wild)
       adult_B = read_excel("data/mcf210023-sup-0001-tables1-s24.xlsx", range = "ST 13-16 Tot ret (bioma) 52-15!A6:O70")
  )

# back-calculate average weughts used in R&I 
  pink <- append(pink, list(avgwt = pink$adult_B[,-1]/pink$adult_N[,-1]/1000))
  pink$avgwt <- cbind(Year=seq(1952,2015,1),pink$avgwt)

  RI_avgwt.long <- melt(pink$avgwt, id ="Year", variable.name="Region", value.name="avgwt")

  p <- ggplot(data = filter(RI_avgwt.long, Region %in% c("WAK", "SPen","Kod","CI","PWS","SEAK")), aes(x = Year, y = avgwt)) + geom_point() + facet_wrap(~Region, scales = "free", ncol=3)+
    ggtitle("Backcalculated Average Weights from R&I - AK pink salmon") +
    theme(plot.title = element_text(hjust = 0.5))
  dev.new()
  p
 
# load NPAFC catch data (2024 update), filter to AK commercial catch only and R&I range of years)  
  NPAFC <- read_csv("data/NPAFC_Catch_Stat-1925-2023_2024-04-29_lngfrm.csv")
  NPAFC_AK <- filter(NPAFC, Region=="Alaska" & Fishery=="Commercial" & Area != "Whole state" & between(Year,1952,2015) & Species=="Pink")
  NPAFC_AK <- data.frame(NPAFC_AK,avg_wt=NPAFC_AK$Wt_fish/NPAFC_AK$N_fish*1000)
  
  p <- ggplot(data = NPAFC_AK, aes(x = Year, y = avg_wt)) + geom_point() + facet_wrap(~Area+Species, scales = "free", ncol=3)+  
  ggtitle("Calculated Average Weights from NPAFC data - AK pink salmon") +
    theme(plot.title = element_text(hjust = 0.5))
  dev.new()
  p  
  
 # plot SEAK data from R&I and NPAFC
  p <-ggplot()+
    geom_point(data = filter(RI_avgwt.long, Region =="SEAK"), aes(x = Year, y = avgwt))+
    geom_line(data = filter(NPAFC_AK, Area == "Southeast"),aes(x = Year, y = avg_wt), color = "red")
  dev.new()
  p  
  
  p <-ggplot()+
    geom_point(data = filter(RI_avgwt.long, Region =="WAK"), aes(x = Year, y = avgwt))+
    geom_line(data = filter(NPAFC_AK, Area == "Western"),aes(x = Year, y = avg_wt), color = "red")+
    geom_line(data = filter(NPAFC_AK, Area == "Westward"),aes(x = Year, y = avg_wt), color = "blue")
  dev.new()
  p 
  
  p <-ggplot()+
    geom_point(data = filter(RI_avgwt.long, Region =="Kod"), aes(x = Year, y = avgwt))+
    geom_line(data = filter(NPAFC_AK, Area == "South Central"),aes(x = Year, y = avg_wt), color = "red")+
    geom_line(data = filter(NPAFC_AK, Area == "Central"),aes(x = Year, y = avg_wt), color = "blue")+
    geom_point(data = filter(NPAFC_AK, Area == "Westward"),aes(x = Year, y = avg_wt), color = "green")
  dev.new()
  p  
#plot regions in RI that used South Central/Central Region as basis for avg weight
  dev.new()
  ggplot(data = filter(RI_avg_wt.long, Region %in% c("SPen","Kod","CI","PWS")), aes(x = Year, y = avg_wt, colour=Region)) + 
    geom_jitter()
    geom_line(data=NPAFC_AK, aes(x=Year, y=avg_wt))

    
# Read data file provided for ESR that includes commercial harvest by management area
ADFG <- read_csv("data/Salmon Landings for ESR_Sept 2023.csv",show_col_types = FALSE)

#clean up and simplify
colnames(ADFG) <- c("Year", "Manage_Area", "Region_No","Region","Species_Code", "Species","Landed_Wt", "Whole_Wt","Number","Processor", "Permits","Vessels")
ADFG2 <- filter(ADFG, between (Year,1985,2015))

ADFG2 <- ADFG2 %>% 
  mutate(Species = ifelse(as.character(Species) == "salmon, chinook", "Chinook", as.character(Species)),
         Species = ifelse(as.character(Species) == "salmon, chum", "Chum", as.character(Species)),
         Species = ifelse(as.character(Species) == "salmon, coho", "Coho", as.character(Species)),
         Species = ifelse(as.character(Species) == "salmon, pink", "Pink", as.character(Species)),
         Species = ifelse(as.character(Species) == "salmon, sockeye", "Sockeye", as.character(Species))
         )
ADFG2 <- ADFG2 %>% 
  mutate(Region = ifelse(as.character(Region) == "Southeastern Region", "SEAK", as.character(Region)),
         Region = ifelse(as.character(Region) == "Central Region", "Central", as.character(Region)),
         Region = ifelse(as.character(Region) == "A-Y-K Region", "AYK", as.character(Region)),
         Region = ifelse(as.character(Region) == "Westward Region", "Westward", as.character(Region))
         )

ADFG3 <- filter(ADFG2, Manage_Area != "9" & Species %in% c("Pink","Chum","Sockeye"))

ADFG4 <- ADFG3 %>%
        group_by(Region, Species, Year) %>% summarise(Number=sum(Number) )

p <- ggplot(data = ADFG4, aes(x = Year, y = Number)) + geom_point() + facet_wrap(~Region+Species, scales = "free", ncol=3)  
windows()
p

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
