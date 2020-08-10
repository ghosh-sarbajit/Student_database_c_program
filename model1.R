library(tidyverse)
library(readr)
IBBL_data_BBS <- read_csv("./csv/Minerva/BBSProductionDetailsIBBL.csv")
IBBL_data_MBZ <- read_csv("./csv/Minerva/MBZProductionDetailsIBBL.csv")
IBBL_data_SLT <- read_csv("./csv/Minerva/SLTProductionDetailsIBBL.csv")

IBBL_BBS_data2 <- read_csv("./csv/Minerva/IBBL_BBS_Summary.csv")

Model1 <-  lm(IBBL_BBS_data2$`#_Of_Obsen`~ IBBL_BBS_data2$Tot_TAT_In_Sec + IBBL_BBS_data2$TotBBox)
Model1$residuals
plot(Model1)

summary(Model1)
anova(Model1)
