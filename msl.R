tMSL <- data.table::fread("./csv/Athena/MSL-Production-Report.csv")


tMSL1 <- select(tMSL,`Email`, `Minutes to Complete`, `Task Count`) %>% group_by(`Email`) %>% summarise( OKTAMin = sum(`Minutes to Complete`) , Wday = n(), TotJobs = sum(`Task Count`)) #%>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday)
