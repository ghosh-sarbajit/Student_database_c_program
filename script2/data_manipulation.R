tBBS <- data.table::fread("./csv/Minerva2/New-BBS-Minerva.csv")
tSLT <- data.table::fread("./csv/Minerva2/New-SLT-Minerva.csv")
tMBZ <- data.table::fread("./csv/Minerva2/New-MBZ-Minerva.csv")
tBBS <- tBBS %>% mutate(Branch = rep("BBS", nrow(tBBS)))
tSLT <- tSLT %>% mutate(Branch = rep("SLT", nrow(tSLT)))
tMBZ <- tMBZ %>% mutate(Branch = rep("MBZ",nrow(tMBZ)))
Comb_Minerva <- rbind(tBBS,tSLT,tMBZ)
Comb_Minerva <- Comb_Minerva %>% select(-1)
Comb_Minerva <- Comb_Minerva %>% mutate(Sco = Par1 + Par3 + Par56 + Par62)


Comb_Minerva_output1 <- Comb_Minerva %>% select("Employee Name", Par1, Par3, Par56, Par62, AvgOKTAMin, AvgJobs, Gender, at, Profit, Branch)
write.csv(Comb_Minerva_output1, "./csv/Minerva2/PlotData_trimmed.csv",row.names = F)

# Normalsation part
Par1Sd <- Comb_Minerva_output1$Par1
Par1Sd <- (Par1Sd - mean(Par1Sd))/sd(Par1Sd)

Par3Sd <- Comb_Minerva_output1$Par3
Par3Sd <- (Par3Sd - mean(Par3Sd))/sd(Par3Sd)

Par56Sd <- Comb_Minerva_output1$Par56
Par56Sd <- (Par56Sd - mean(Par56Sd))/sd(Par56Sd)

Par62Sd <- Comb_Minerva_output1$Par62
Par62Sd <- (Par62Sd - mean(Par62Sd))/sd(Par62Sd)

#AvgOKTAMinSd
AvgOKTAMinSd <- Comb_Minerva_output1$AvgOKTAMin
AvgOKTAMinSd <- (AvgOKTAMinSd - mean(AvgOKTAMinSd))/sd(AvgOKTAMinSd)

#AvgJobsStd
AvgJobsStd <- Comb_Minerva_output1$AvgJobs
AvgJobsStd <- (AvgJobsStd - mean(AvgJobsStd))/sd(AvgJobsStd)

atSd <- Comb_Minerva_output1$at
atSd <- (atSd - mean(atSd))/sd(atSd)

ProfitSd <- Comb_Minerva_output1$Profit
ProfitSd <- (ProfitSd - mean(ProfitSd))/sd(ProfitSd)


Comb_Minerva_output1_std <-  Comb_Minerva %>% select("Employee Name", Gender, Branch)



df <- cbind(Par1Sd, Par3Sd, Par56Sd, Par62Sd,AvgOKTAMinSd, AvgJobsStd, atSd, ProfitSd)
colnames(df) <- c("Par1Sd", "Par2Sd", "Par56Sd", "Par62Sd", "AvgOKTAMinSd", "AvgJobsStd", "atSd", "ProfitSd")
Comb_Minerva_output1_std <- cbind(Comb_Minerva_output1_std,df)
Comb_Minerva_output1_std <- Comb_Minerva_output1_std %>% mutate(Sco = Par1Sd + Par3Sd + Par56Sd + Par62Sd)




# set.seed(45)
# RandSuffle <- sample(nrow(Comb_Minerva))
# Comb_Minerva <- Comb_Minerva[RandSuffle,]
# write.csv(Comb_Minerva,"./csv/Minerva2/PlotData.csv",row.names = F)
write.csv(Comb_Minerva_output1_std, "./csv/Minerva2/PlotData_trimmed_std.csv",row.names = F)


# Model on combined std Minerva
plot(Comb_Minerva_output1_std$ProfitSd,Comb_Minerva_output1_std$atSd)
plot(Comb_Minerva_output1_std$ProfitSd, Comb_Minerva_output1_std$AvgJobsStd)
plot(Comb_Minerva_output1_std$ProfitSd, Comb_Minerva_output1_std$Sco)



library(olsrr)
Model1 <-lm(ProfitSd ~  AvgOKTAMinSd + Sco + atSd, data = Comb_Minerva_output1_std)
summary(Model1)

AllModel <-ols_step_all_possible(Model1)

AllModel <- as.data.frame(AllModel)


FWDfit.p <- ols_step_forward_p(Model1,penter=.01)
FWDfit.p
plot(FWDfit.p)

mydata <- Comb_Minerva_output1_std[, c(8,9,10,11,12)]
cormat <- round(cor(mydata),2)
library(reshape2)
melted_cormat <- melt(cormat)
