SLT_DEC <- data.table::fread("./csv/Min3/IBBL SLT Dec.xlsx - MyCalc.csv",header = T)
Model1 <- lm(AvgTPTInSec2 ~ `AvgBBox/task` + PKT, data = SLT_DEC)
summary(Model1)


SLT_JAN <- data.table::fread("./csv/Min3/IBBL SLT Jan1.xlsx - MyCalc.csv",header = T)
Model2 <- lm(AvgTPTInSec2 ~  `AvgBBox/task` + PKT, data = SLT_JAN)
summary(Model2)


SLT_JAN2 <- data.table::fread("./csv/Min3/IBBL SLT  Jan2 Feb1 (23rd Jan'20-18th Feb) - MyCalc.csv",header = T)
Model3 <- lm(AvgTPTInSec2 ~ `AvgBBox/task` + PKT, data = SLT_JAN2)
summary(Model3)


SLT_FEB <- data.table::fread("./csv/Min3/IBBL SLT Feb2 March1.xlsx - MyCalc.csv",header = T)
Model4 <- lm(AvgTPTInSec2 ~  `AvgBBox/task` + PKT, data = SLT_FEB)
summary(Model4)


SLT_MAR <- data.table::fread("./csv/Min3/IBBL SLT March2.xlsx - MyCalc.csv",header = T)
Model5 <- lm(AvgTPTInSec2 ~  `AvgBBox/task` + PKT, data = SLT_MAR)
summary(Model5)



IBBL_DEC <- data.table::fread("./csv/Min3/3 step IBBL_BBS(Dec) - MyCalc.csv",header = T)
Model6 <- lm(AvgTPTInSec2 ~  `AvgBBox/task` + PKT, data = IBBL_DEC)
summary(Model6)


IBBL_JAN <- data.table::fread("./csv/Min3/3 step IBBL_BBS(23th Dec to 10th Jan'20).xlsx - MyCalc.csv",header = T)
Model7 <- lm(AvgTPTInSec2 ~  `AvgBBox/task` + PKT, data = IBBL_JAN)
summary(Model7)


IBBL_FEB <- data.table::fread("./csv/Min3/3 step IBBL_BBS(30th Jan to 11th Feb'20).xlsx - MyCalc.csv",header = T)
Model8 <- lm(AvgTPTInSec2 ~  `AvgBBox/task` + PKT, data = IBBL_FEB)
summary(Model8)


IBBL_MAR <- data.table::fread("./csv/Min3/3 step IBBL_BBS-9(17th March to 31st March).xlsx - MyCalc.csv",header = T)
Model9 <- lm(AvgTPTInSec2 ~  `AvgBBox/task` + PKT, data = IBBL_MAR)
summary(Model9)

IBBL_COMB <- rbind( SLT_JAN, SLT_JAN2, SLT_FEB, SLT_MAR, IBBL_DEC, IBBL_JAN, IBBL_FEB, IBBL_MAR)
IBBL_COMB <- IBBL_COMB[complete.cases(IBBL_COMB),]
write.csv(IBBL_COMB, "./csv/Min3/IBBL_without_quality_seperated.csv", row.names = F)

Model10 <- lm(AvgTPTInSec2 ~  `AvgBBox/task` + PKTG, data = IBBL_COMB)
summary(Model10)

Model11 <- lm(AvgTPTInSec2 ~ `AvgBBox/task` + NOT + PKT, data = IBBL_COMB)
summary(Model11)



