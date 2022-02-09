library(tidyverse)
library(tidyr)
library(lubridate)
Combined_table <- data.table::fread("./csv/Athena2/MSL.csv")


tMSL_SLT <- Combined_table %>% filter(Centre == "SLT")
tMSL_MBZ <- Combined_table %>% filter(Centre == "MBZ")

# write.csv(tMSL_SLT,"./csv/Athena/msl1_SLT.csv")
write.csv(tMSL_MBZ,"./csv/Athena/msl1_MBZ.csv")


tMSL_MBZ <- tMSL_MBZ %>% filter(Wday > 5)
Model_MBZ <- lm(AvgJobs ~ AvgOKTAMinutes, data = tMSL_MBZ)
summary(Model_MBZ)


tMSL_SLT <- tMSL_SLT %>% filter(Wday > 5)
Model_SLT <- lm(AvgJobs ~ AvgOKTAMinutes, data = tMSL_SLT)
summary(Model_SLT)
