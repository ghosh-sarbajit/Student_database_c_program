library(tidyverse)
library(readr)
library(lubridate)
# install.packages("utiml")
# install.packages("nnet")
library(utiml)


# Import BBS-Production-Details-2D-Hea.csv

tBBS_2D_Hea <- data.table::fread("./csv/Minerva2/BBS-Production-Details-2D-Hea.csv")




tBBS_2D_Hea <- select(tBBS_2D_Hea,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>% mutate(Date1 = dmy(Date)) %>%  group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)



# Import BBS-Production-Details-2D-Ima.csv

tBBS_2D_Ima <- data.table::fread("./csv/Minerva2/BBS-Production-Details-2D-Ima.csv")

tBBS_2D_Ima <- select(tBBS_2D_Ima,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>%  mutate(Date1 = dmy(Date)) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)

# Import BBS-ProductionDetails-IBBL.csv

tBBS_IBBL <- data.table::fread("./csv/Minerva2/BBS-ProductionDetails-IBBL.csv")

# tBBS_IBBL1 <- cbind(tBBS_IBBL$`Employee Name`, tBBS_IBBL$Date)
# colnames(tBBS_IBBL1) <- c("Name", "Date")
# tBBS_IBBL1 <- as_tibble(tBBS_IBBL1)
# tBBS_IBBL1 <- tBBS_IBBL1 %>% mutate(Date1 = dmy(Date)) %>% group_by(Name) %>% summarise(DateMin = min(Date1), DateMax = max(Date1))


tBBS_IBBL <- select(tBBS_IBBL,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>%  mutate(Date1 = dmy(Date)) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)

# Import BBS-Production-Details-TLI.csv

tBBS_TLI <- data.table::fread("./csv/Minerva2/BBS-Production-Details-TLI.csv")

tBBS_TLI <- select(tBBS_TLI,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>%  mutate(Date1 = dmy(Date)) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)


# Import MBZBBS-2D-Image-Mask-Productio.csv

tMBZBBS_2D_Image_Mask_prod <- data.table::fread("./csv/Minerva2/MBZBBS-2D-Image-Mask-Productio.csv")

tMBZBBS_2D_Image_Mask_prod <- select(tMBZBBS_2D_Image_Mask_prod,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>%  mutate(Date1 = dmy(Date)) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)

# Import MBZ-Production-Details-2D-Ima.csv

tMBZ_2D_Ima <- data.table::fread("./csv/Minerva2/MBZ-Production-Details-2D-Ima.csv")

tMBZ_2D_Ima <- select(tMBZ_2D_Ima,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>%  mutate(Date1 = dmy(Date)) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)


# Import MBZ-Production-Details-2D-Hea.csv

tMBZ_2D_Hea <- data.table::fread("./csv/Minerva2/MBZ-Production-Details-2D-Hea.csv")

tMBZ_2D_Hea <- select(tMBZ_2D_Hea,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>%  mutate(Date1 = dmy(Date)) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)


# Import MBZ-Production-Details-IBBL.csv

tMBZ_IBBL <- data.table::fread("./csv/Minerva2/MBZ-Production-Details-IBBL.csv")

tMBZ_IBBL <- select(tMBZ_IBBL,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>%  mutate(Date1 = dmy(Date)) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)

# Import MBZ-Production-Details-TLI.csv

tMBZ_TLI <- data.table::fread("./csv/Minerva2/MBZ-Production-Details-TLI.csv")

tMBZ_TLI <- select(tMBZ_TLI,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>%  mutate(Date1 = dmy(Date)) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)

# Import SLT-Production-Details-IBBL.csv

tSLT_IBBL <- data.table::fread("./csv/Minerva2/SLT-Production-Details-IBBL.csv")

tSLT_IBBL <- select(tSLT_IBBL,`Emp. ID`,`Employee Name`,OKTASec,Totp, Date) %>%  mutate(Date1 = dmy(Date)) %>% group_by(`Employee Name`) %>% summarise( OKTASec = sum(OKTASec) , Wday = n(), TotJobs = sum(Totp), MinDate = min(Date1), MaxDate = max(Date1)) %>% mutate(AvgOKTASec = OKTASec/Wday, AvgJobs = TotJobs/Wday) %>% select(`Employee Name`, AvgOKTASec, AvgJobs, Wday, MinDate, MaxDate)


# Lets go beanch wise tBBS_2D_Hea, tBBS_2D_Ima, tBBS_IBBL, tBBS_TLI, tMBZBBS_2D_Image_Mask_prod, tMBZ_2D_Ima, tMBZ_2D_Hea, tMBZ_IBBL, tMBZ_TLI, tSLT_IBBL

tBBS <- rbind(tBBS_2D_Hea,tBBS_2D_Ima,tBBS_IBBL,tBBS_TLI,tMBZBBS_2D_Image_Mask_prod)

# tBBS <- rbind(tBBS_2D_Hea,tBBS_2D_Ima,tBBS_IBBL,tBBS_TLI)
tBBS <- rbind(tBBS_2D_Hea,tBBS_2D_Ima,tBBS_TLI,tMBZBBS_2D_Image_Mask_prod)



write.csv(tBBS,"./csv/Minerva2/BBS-Minerva.csv")

tMBZ <- rbind(tMBZ_2D_Ima,tMBZ_2D_Hea,tMBZ_TLI)

write.csv(tMBZ,"./csv/Minerva2/MBZ-Minerva.csv")

tSLT <- tSLT_IBBL

write.csv(tSLT,"./csv/Minerva2/SLT-Minerva.csv")



# Model Building Part

tBBS <- tBBS %>% filter(AvgJobs > 5)

Model_BBS <- lm(AvgJobs ~ AvgOKTASec, data = tBBS)
Model_BBS
summary(Model_BBS)

Model_BBS_IBBL <- lm(AvgJobs ~ AvgOKTASec, data = tBBS_IBBL)
Model_BBS_IBBL
summary(Model_BBS_IBBL)

# tMBZ <- tMBZ %>% filter(AvgJobs > 5)
Model_MBZ_IBBL <- lm(AvgJobs ~ AvgOKTASec, data = tMBZ_IBBL)
Model_MBZ_IBBL
summary(Model_MBZ_IBBL)

Model_MBZ <- lm(AvgJobs ~ AvgOKTASec, data = tMBZ)
Model_MBZ
summary(Model_MBZ)

tSLT <- tSLT %>% filter(AvgJobs > 5)
Model_SLT <- lm(AvgJobs ~ AvgOKTASec, data = tSLT)
Model_SLT
summary(Model_SLT)


