
library("readxl")
library("tidyverse")
library("reshape2")

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

# First summarize by Region to cross check with current NPAFC data
NPAFC_RI1 <- filter(NPAFC_RI, Species %in% c("Pink","Chum","Sockeye"))
NPAFC_RI2 <- NPAFC_RI1 %>%
  group_by(Region, Species, Year) %>% summarise(Number=sum(Number) )

p <- ggplot(data = NPAFC_RI2, aes(x = Year, y = Number)) + geom_point() + facet_wrap(~Region+Species, scales = "free", ncol=3)  
windows()
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
