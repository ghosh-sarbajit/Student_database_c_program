# install.packages("olsrr")
library(olsrr)


tBBS <- data.table::fread("./csv/Minerva2/New-BBS-Minerva.csv")
tSLT <- data.table::fread("./csv/Minerva2/New-SLT-Minerva.csv")
tMBZ <- data.table::fread("./csv/Minerva2/New-MBZ-Minerva.csv")

Comb_Minerva <- rbind(tBBS,tSLT,tMBZ)


Model1 <- lm(AvgJobs ~ AvgOKTASec, data = Comb_Minerva)
summary(Model1)

Model_at_1 <- lm(AvgJobs ~ AvgOKTASec + Par1 + Par2 + at, data = Comb_Minerva)
summary(Model_at_1)




Model_at_2 <- lm(AvgJobs ~ AvgOKTASec + Par3 + Par4 + at, data = Comb_Minerva)
summary(Model_at_2)









