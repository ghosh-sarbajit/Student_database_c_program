# Minerva

Minerva_data <- data.table::fread("./csv/min5/IBBL_semistructured_4.csv",header = T)
Minerva_data <- subset(Minerva_data, complete.cases(AvgTPTInSec2))
write.csv(Minerva_data,"./csv/min5/IBBL_semistructured_2.csv", row.names = FALSE)

PKT.median <- Minerva_data %>% group_by(Branch,NOT) %>% summarise(PKT.median = median(PKT, na.rm = T))

PKT.imputed <- c()
for(i in 1:nrow(Minerva_data)){
  if(is.na(Minerva_data$PKT[i])){
    if(Minerva_data$Branch[i]=='BBS' && Minerva_data$NOT[i] == 1){
      PKT.imputed[i] <- 59.6
    }else if(Minerva_data$Branch[i]=='BBS' && Minerva_data$NOT[i] == 2){
      PKT.imputed[i] <- 69.7
    }else if(Minerva_data$Branch[i]=='BBS' && Minerva_data$NOT[i] == 3){
      PKT.imputed[i] <- 80.0
    }else if(Minerva_data$Branch[i]=='BBS' && Minerva_data$NOT[i] == 4){
      PKT.imputed[i] <- 69.9
    }else if(Minerva_data$Branch[i]=='SLT' && Minerva_data$NOT[i] == 2){
      PKT.imputed[i] <- 64.9
    }else if(Minerva_data$Branch[i]=='SLT' && Minerva_data$NOT[i] == 3){
      PKT.imputed[i] <- 80.4
    }else if(Minerva_data$Branch[i]=='SLT' && Minerva_data$NOT[i] == 4){
      PKT.imputed[i] <- 80.0
    }else if(Minerva_data$Branch[i]=='SLT' && Minerva_data$NOT[i] == 5){
      PKT.imputed[i] <- 79.6
    }
  }else{
    PKT.imputed[i] <- Minerva_data$PKT[i]
  }
}
Minerva_data <- Minerva_data %>% select(-c("PKT"))
Minerva_data$PKT <- PKT.imputed
write.csv(Minerva_data,"./csv/min5/IBBL_semistructured_2.csv", row.names = F)
