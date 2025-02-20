library("readxl")
library(dplyr)
library("tidyverse")
tabnames <- excel_sheets("1963.xls")
TOC <- read_excel("1963.xls", sheet=1, col_names=c("Number","Caption"))
                                                   
grep("Alaska", TOC$Caption)


