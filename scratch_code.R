
library("readxl")
library("tidyverse")
<<<<<<< HEAD
library(reshape2)

# Read data file provided for ESR that includes commercial harvest by management area
ADFG <- read_csv("data/Salmon Landings for ESR_Sept 2023.csv")

#clean up and simplify
ADFG2 <- filter(ADFG, between (`DFB Year`,1985,2015))
colnames(ADFG2) <- c("Year", "Manage_Area", "Region_No","Region","Species_Code", "Species","Landed_Wt", "Whole_Wt","Number","Processor", "Permits","Vessels")

=======
library("reshape2")

# Read data file provided for ESR that includes commercial harvest by management area
ADFG <- read_csv("data/Salmon Landings for ESR_Sept 2023.csv",show_col_types = FALSE)

#clean up and simplify
colnames(ADFG) <- c("Year", "Manage_Area", "Region_No","Region","Species_Code", "Species","Landed_Wt", "Whole_Wt","Number","Processor", "Permits","Vessels")
ADFG2 <- filter(ADFG, between (Year,1985,2015))
>>>>>>> 178f16d8f6b3ded8559093d94cabb8c9c7ecd553

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

<<<<<<< HEAD
ADFG <- cbind(ADFG,ADFG[,8]*0.45359237)
colnames(ADFG)[13] <- "Wt_kg"       
=======
ADFG3 <- filter(ADFG2, Manage_Area != "9" & Species %in% c("pink","chum","sockeye"))

ADFG4 <- ADFG3 %>%
        group_by(Region, Species, Year) %>% summarise(Number=sum(Number) )

ADFG <- cbind(ADFG,ADFG[,8]*0.45359237)
colnames(ADFG)[13] <- "Wt_kg"       

>>>>>>> 178f16d8f6b3ded8559093d94cabb8c9c7ecd553
ADFG <- cbind(ADFG,ADFG$Wt_kg/ADFG$`Number Of Fish (estimated)`)
colnames(ADFG)[14] <- "avg_wt" 

ADFG_pink <- filter(ADFG, ADFG$`Species Name` == "salmon, pink" & between (`DFB Year`,1985,2015))

w <- ifelse((ADFG$`Management Area` == "A") | (ADFG$`Management Area` == "B") | (ADFG$`Management Area` == "C") | (ADFG$`Management Area` == "D"), "SEAK", +
              ifelse((ADFG$`Management Area` == "E"), "PWS", ifelse((ADFG$`Management Area` == "H"), "CI", ifelse((ADFG$`Management Area` == "K") | (ADFG$`Management Area` == "L"), "KOD","Other"))  ))   

<<<<<<< HEAD
p <- ggplot(data = ADFG_pink, aes(x = `DFB Year`, y = `Number Of Fish (estimated)`)) + geom_point()
p + facet_wrap(~`Management Area`)  
=======
p <- ggplot(data = ADFG4, aes(x = Year, y = Number)) + geom_point()
p + facet_wrap(~Region+Species, scales = "free")  
>>>>>>> 178f16d8f6b3ded8559093d94cabb8c9c7ecd553



NPAFC <- read_csv("data/NPAFC_Catch_Stat-1925-2023_2024-04-29_lngfrm.csv")
NPAFC_AK <- filter(NPAFC, Region=="Alaska" & Fishery=="Commercial" & between(Year,1952,2015) & Species=="Pink")
