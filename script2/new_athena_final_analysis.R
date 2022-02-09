library(olsrr)
library(faraway)
library(quantmod)
library(corrplot)
library(MASS)

# # gathering all data it will goto google-sheet and there socio-eco will be added
# prod_may <- data.table::fread("./csv/Ath3/Ath_may_production_data.csv",header = T)
# prod_june <- data.table::fread("./csv/Ath3/Ath_june_production_data.csv",header = T)
# prod_july <- data.table::fread("./csv/Ath3/Ath_july_production_data.csv",header = T)
# names(prod_may)
# names(prod_june)
# names(prod_july)
# prod_total <- rbind(prod_may,prod_june,prod_july)
# write.csv(prod_total, "./csv/Ath3/MSL_semistuctured.csv", row.names = F)


# May 65/193
# June 108/207
# July 51/215

# dataset with randomized socio-eco by adding and substracting sd
# Athena_data <- data.table::fread("./csv/Ath3/MSL_semistuctured_3.csv",header = T)
# Athena_data <- Athena_data[complete.cases(Athena_data),]


# another dataset with randomized socio-eco by adding and substracting sd
Athena_data <- data.table::fread("./csv/Ath3/MSL_semistuctured_2.csv",header = T)
# Athena_data <- Athena_data[complete.cases(Athena_data),]

(Model0.1 <- lm(TPT ~  `Task Type`,data = Athena_data))
summary(Model01)

(Model0.2 <- lm(TPT ~  `Frame-Info`,data = Athena_data))
summary(Model0.2)


(Model1 <- lm(TPT ~ `Frame-Info` + `Task Type`,data = Athena_data))
summary(Model1)


(Model2.1 <- lm(TPT ~ `Frame-Info` + `Task Type` + `Employee Branch`,data = Athena_data))
summary(Model2.1)

(Model2.2 <- lm(TPT ~ `Frame-Info` + `Task Type` + NOT,data = Athena_data))
summary(Model2.2)

(Model2.3 <- lm(TPT ~ `Frame-Info` + `Task Type` + PKT,data = Athena_data))
summary(Model2.3)


(FullModel1 <- lm(TPT ~ `Frame-Info` + `Task Type` + NOT + PKT + Par0 + Par1 + Par2 + Par3 + Par4 
                  + Par51 + Par52 + Par53 + Par54 + Par55 + Par56 
                  + Par61 + Par62 + Par63 + Par64  + Par65 +Par66 , data = Athena_data))
summary(FullModel1)


(FWDfit.p.1 <- ols_step_forward_p(FullModel1,penter=.01))
(FWDfit.AIC.1 <- ols_step_forward_aic(FullModel1))
(BWDfit.p <- ols_step_backward_p(FullModel1,prem=.00001))



(Model4 <- lm(TPT ~ `Frame-Info` + `Task Type` + Par0,data = Athena_data))
summary(Model4)

(Model5 <- lm(TPT ~ `Frame-Info` + `Task Type` + Par1 + Par53 + NOT,data = Athena_data))
summary(Model5)


(FullModel2 <- lm(TPT ~ `Frame-Info` + `Task Type` + Par0 + Par1 + Par2 + Par3 + Par4 
                  + Par51 + Par52 + Par53 + Par54 + Par55 + Par56 
                  + Par61 + Par62 + Par63 + Par64  + Par66 , data = Athena_data))
summary(FullModel2)

(FWDfit.p.1 <- ols_step_forward_p(FullModel2,penter=0.0001))
(FWDfit.AIC.1 <- ols_step_forward_aic(FullModel2))
(BWDfit.p <- ols_step_backward_p(FullModel2,prem=.00001))


Model7 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par0,data = Athena_data)
summary(Model7)

Model8 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par1,data = Athena_data)
summary(Model8)


Model9 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par2,data = Athena_data)
summary(Model9)

Model10 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par3,data = Athena_data)
summary(Model10)

Model11 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par4,data = Athena_data)
summary(Model11)


Model12 <- lm(TPT ~ `Frame-Info` + `Task Type` + Par51,data = Athena_data)
summary(Model12)

Model13 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par52,data = Athena_data)
summary(Model13)

Model14 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par53,data = Athena_data)
summary(Model14)

Model15 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par54,data = Athena_data)
summary(Model15)

Model16 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par55,data = Athena_data)
summary(Model16)


Model17 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par56,data = Athena_data)
summary(Model17)

Model18 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par61,data = Athena_data)
summary(Model18)

Model19 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par62,data = Athena_data)
summary(Model19)

Model20 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par63,data = Athena_data)
summary(Model20)

Model21 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par64,data = Athena_data)
summary(Model21)

Model22 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par65,data = Athena_data)
summary(Model22)

Model23 <- lm(TPT ~ `Frame-Info` + `Task Type` +  Par66,data = Athena_data)
summary(Model23)


