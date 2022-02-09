# data subsetting part


require(tidyverse)
require(lubridate)
tMSL <- data.table::fread("./csv/Athena/MSL-Production-Report.csv")


# tMSL1 <- select(tMSL,`Email`, `Minutes to Complete`, `Task Count`) %>% group_by(`Email`) %>% summarise( OKTAMinutes = sum(`Minutes to Complete`) , Wday = n(), TotJobs = sum(`Task Count`)) %>% mutate(AvgOKTAMinutes = OKTAMinutes/Wday, AvgJobs = TotJobs/Wday) %>% select(`Email`, AvgOKTAMinutes, TotJobs, AvgJobs, Wday)

# TempCol <- cbind(tMSL$`Started Date`, tMSL$Email)
# colnames(TempCol) <- c("Date", "Email")
# TempCol <- as_tibble(TempCol)
# TempCol <- TempCol %>% mutate(Date1 = mdy(Date)) %>% group_by(Email) %>% summarise(DateMin = min(Date1), DateMax = max(Date1))

tMSL1 <- select(tMSL,`Email`, `Minutes to Complete`, `Task Count`, `Started Date`) %>%  mutate(Date1 = mdy(`Started Date`))  %>%  group_by(`Email`) %>% summarise( OKTAMinutes = sum(`Minutes to Complete`) , Wday = n(), TotJobs = sum(`Task Count`), StartDate = min(Date1), EndDate = max(Date1)) %>% mutate(AvgOKTAMinutes = OKTAMinutes/Wday, AvgJobs = TotJobs/Wday) %>% select(`Email`, AvgOKTAMinutes, TotJobs, AvgJobs, Wday, StartDate, EndDate)

write.csv(tMSL1, "./csv/Athena/MSL.csv")



ModelMSL1 <- lm(AvgJobs ~ AvgOKTAMinutes, data = tMSL1)
ModelMSL1
summary(ModelMSL1)
