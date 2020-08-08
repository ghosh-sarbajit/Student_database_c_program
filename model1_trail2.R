library(tidyverse)
library(readr)
# install.packages("utiml")
# install.packages("nnet")
library(utiml)

tBBS_IBBL_m2 <- data.table::fread("./csv/Minerva/Copy-of-Minerva-BCP-Planning-BBS-Production-Details-IBBL.csv")

tBBS_IBBL.PKT_m2 <- data.table::fread("./csv/Minerva/BBSR_Mar_20-Scorecard.csv")

tBBS_IBBL.1_m2 <- select(tBBS_IBBL_m2,`Emp. ID`,`Employee Name`,OKTASec,TotCount) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(TotCount)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs)

tBBS_IBBL.PKT.1_m2 <- cbind(tBBS_IBBL.PKT_m2$EmpName,tBBS_IBBL.PKT_m2$Total)
colnames(tBBS_IBBL.PKT.1_m2) <- c("Employee Name", "Total")
tBBS_IBBL.PKT.1_m2 <- as_tibble(tBBS_IBBL.PKT.1_m2)

tBBS_IBBL.comb_m2 <- tBBS_IBBL.1_m2 %>% inner_join(tBBS_IBBL.PKT.1_m2, by = "Employee Name")


ModelBBS1_m2 <- lm(AvgJobs ~ Total + AvgOKTASec, data = tBBS_IBBL.comb_m2)
ModelBBS1_m2
summary(ModelBBS1_m2)


ModelBBS1_m3 <- lm(AvgJobs ~ AvgOKTASec, data = tBBS_IBBL.comb_m2)
ModelBBS1_m3
summary(ModelBBS1_m3)
