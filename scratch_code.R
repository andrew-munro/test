
library("readxl")
library("tidyverse")
library(reshape2)

# Read data file provided for ESR that includes commercial harvest by management area
ADFG <- read_csv("data/Salmon Landings for ESR_Sept 2023.csv")

#clean up and simplify
ADFG2 <- filter(ADFG, between (`DFB Year`,1985,2015))
colnames(ADFG2) <- c("Year", "Manage_Area", "Region_No","Region","Species_Code", "Species","Landed_Wt", "Whole_Wt","Number","Processor", "Permits","Vessels")


ADFG2 <- ADFG2 %>% 
  mutate(Species = ifelse(as.character(Species) == "salmon, chinook", "chinook", as.character(Species)),
         Species = ifelse(as.character(Species) == "salmon, chum", "chum", as.character(Species)),
         Species = ifelse(as.character(Species) == "salmon, coho", "coho", as.character(Species)),
         Species = ifelse(as.character(Species) == "salmon, pink", "pink", as.character(Species)),
         Species = ifelse(as.character(Species) == "salmon, sockeye", "sockeye", as.character(Species))
         )
ADFG2 <- ADFG2 %>% 
  mutate(Region = ifelse(as.character(Region) == "Southeastern Region", "SEAK", as.character(Region)),
         Region = ifelse(as.character(Region) == "Central Region", "Central", as.character(Region)),
         Region = ifelse(as.character(Region) == "A-Y-K Region", "AYK", as.character(Region)),
         Region = ifelse(as.character(Region) == "Westward Region", "Westward", as.character(Region))
         )

ADFG <- cbind(ADFG,ADFG[,8]*0.45359237)
colnames(ADFG)[13] <- "Wt_kg"       
ADFG <- cbind(ADFG,ADFG$Wt_kg/ADFG$`Number Of Fish (estimated)`)
colnames(ADFG)[14] <- "avg_wt" 

ADFG_pink <- filter(ADFG, ADFG$`Species Name` == "salmon, pink" & between (`DFB Year`,1985,2015))

w <- ifelse((ADFG$`Management Area` == "A") | (ADFG$`Management Area` == "B") | (ADFG$`Management Area` == "C") | (ADFG$`Management Area` == "D"), "SEAK", +
              ifelse((ADFG$`Management Area` == "E"), "PWS", ifelse((ADFG$`Management Area` == "H"), "CI", ifelse((ADFG$`Management Area` == "K") | (ADFG$`Management Area` == "L"), "KOD","Other"))  ))   

p <- ggplot(data = ADFG_pink, aes(x = `DFB Year`, y = `Number Of Fish (estimated)`)) + geom_point()
p + facet_wrap(~`Management Area`)  



NPAFC <- read_csv("data/NPAFC_Catch_Stat-1925-2023_2024-04-29_lngfrm.csv")
NPAFC_AK <- filter(NPAFC, Region=="Alaska" & Fishery=="Commercial" & between(Year,1952,2015) & Species=="Pink")
