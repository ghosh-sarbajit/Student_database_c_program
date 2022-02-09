library(tidyverse, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)


Minerva_data <- data.table::fread("./csv/min5/IBBL_semistructured_2.csv",header = T)
Minerva_data <- Minerva_data %>% select(-c("Name", "AvgTPTInSec1", `#_Of_Tasks`, `PKTG`))
WorkType <- c()
for(i in 1:nrow(Minerva_data)){
  if(Minerva_data$IL[i] == 1){
    WorkType[i] <- 'IL'
  }else if(Minerva_data$IQ[i]==1){
    WorkType[i] <- 'IQ'
  }else{
    WorkType[i] <- 'FQ'
  }
}
Minerva_data$WorkType <- WorkType
rm(WorkType)
Minerva_data <- Minerva_data %>% select(-c("IL","IQ","FQ"))

NOT1 <- c()
for(i in 1:nrow(Minerva_data)){
  if(Minerva_data$NOT[i] == 1){
    NOT1[i] <- '1'
  }else if(Minerva_data$NOT[i]==2){
    NOT1[i] <- '2'
  }else if(Minerva_data$NOT[i]==3){
    NOT1[i] <- '3'
  }else if(Minerva_data$NOT[i]==4){
    NOT1[i] <- '4'
  }else{
    NOT1[i] <- '5'
  }
}
Minerva_data$NOT1 <- NOT1
Minerva_data <- Minerva_data %>% select(-c("NOT"))
rm(NOT1)


Minerva_data <- as_tibble(Minerva_data)
# class(Minerva_data$WorkType)
Minerva_data <- Minerva_data[complete.cases(Minerva_data),]
# write.csv(Minerva_data, "./csv/min5/Minerva_data_plot.csv", row.names = FALSE)
# MD1

MeanTPTByBranch <- Minerva_data %>% group_by(Branch) %>% summarise(Mean = mean(AvgTPTInSec2))



ggplot(data = Minerva_data,mapping = aes(AvgTPTInSec2, fill = Branch)) +
  geom_density(alpha = 0.5) +
  xlim(50,350) +
  labs(x = "Time Per Task in sec", y = "") +
  ggtitle("Minerva") +
  geom_vline(data = MeanTPTByBranch, aes(xintercept = Mean, color = Branch))


ggplot(data = Minerva_data,mapping = aes(AvgTPTInSec2, fill = Branch)) +
  geom_histogram( alpha=0.5, position="identity") +
  xlim(50,300) +
  labs(x = "Time Per Task in sec", y = "") +
  ggtitle("Minerva") +
  geom_vline(data = MeanTPTByBranch, aes(xintercept = Mean, color = Branch))


ggplot(data = Minerva_data,mapping = aes(y = AvgTPTInSec2, fill = Branch)) +
  geom_boxplot() +
  ylim(50,300) +
  labs(y = "Time Per Task in sec", x = "Branch") +
  ggtitle("Minerva")


ggplot(data = Minerva_data,mapping = aes(factor(Branch),AvgTPTInSec2, fill = Branch)) +
  geom_violin() +
  ylim(0,400) +
  labs(y = "Time Per Task in sec", x = "Branch") +
  ggtitle("Minerva") +
  geom_hline(data = MeanTPTByBranch, aes(yintercept = Mean, color = Branch))




# MD2
MeanTPTByWorkType <- Minerva_data %>% group_by(WorkType) %>% summarise(Mean = mean(AvgTPTInSec2))

ggplot(data = Minerva_data,mapping = aes(AvgTPTInSec2, fill = WorkType)) +
  geom_density(alpha = 0.5) +
  xlim(50,350) +
  labs(x = "Time Per Task in sec") +
  ggtitle("Minerva") +
  geom_vline(data = MeanTPTByWorkType, aes(xintercept = Mean, color = WorkType))


ggplot(data = Minerva_data,mapping = aes(AvgTPTInSec2, fill = WorkType)) +
  geom_histogram( alpha=0.5, position="identity") +
  xlim(50,350) +
  labs(x = "Time Per Task in sec") +
  ggtitle("Minerva") +
  geom_vline(data = MeanTPTByWorkType, aes(xintercept = Mean, color = WorkType))


ggplot(data = Minerva_data,mapping = aes(y = AvgTPTInSec2, fill = WorkType)) +
  geom_boxplot() +
  ylim(50,300) +
  labs(y = "Time Per Task in sec", x = "WorkType") +
  ggtitle("Minerva")

ggplot(data = Minerva_data,mapping = aes(factor(WorkType),AvgTPTInSec2, fill = WorkType)) +
  geom_violin() +
  ylim(50,400) +
  labs(y = "Time Per Task in sec", x = "WorkType") +
  ggtitle("Minerva") +
  geom_hline(data = MeanTPTByWorkType, aes(yintercept = Mean, color = WorkType))

# MD3

MeanTPTByNOT1 <- Minerva_data %>% group_by(NOT1) %>% summarise(Mean = mean(AvgTPTInSec2))

ggplot(data = Minerva_data,mapping = aes(AvgTPTInSec2, fill = NOT1)) +
  geom_density(alpha = 0.4) +
  xlim(75,325) +
  labs(x = "Time Per Task in sec") +
  ggtitle("Minerva") +
  geom_vline(data = MeanTPTByNOT1, aes(xintercept = Mean, color = NOT1))



ggplot(data = Minerva_data,mapping = aes(AvgTPTInSec2, fill = NOT1)) +
  geom_histogram( alpha=0.4, position="identity", bins = 10) +
  xlim(75,325) +
  labs(x = "Time Per Task in sec") +
  ggtitle("Minerva") +
  geom_vline(data = MeanTPTByNOT1, aes(xintercept = Mean, color = NOT1))

ggplot(data = Minerva_data,mapping = aes(y = AvgTPTInSec2, fill = NOT1)) +
  geom_boxplot() +
  ylim(50,350) +
  labs(y = "Time Per Task in sec", x = "Number Of Trainings") +
  ggtitle("Minerva") 


ggplot(data = Minerva_data,mapping = aes(factor(NOT1),AvgTPTInSec2, fill = NOT1)) +
  geom_violin() +
  ylim(50,400) +
  labs(y = "Time Per Task in sec", x = "NOT1") +
  ggtitle("Minerva") +
  geom_hline(data = MeanTPTByNOT1, aes(yintercept = Mean, color = NOT1))


# MD4

ggplot(data = Minerva_data,mapping = aes(AvgTPTInSec2,PKT, color = Branch )) + 
  geom_count() + facet_grid(`NOT1` ~ .)+
  xlim(50,500) +
  labs(x = "Time Per Task in sec", y = "PKT score And Number of Training") +
  ggtitle("Minerva")


ggplot(data = Minerva_data,mapping = aes(AvgTPTInSec2,PKT, color = Branch )) + 
  geom_jitter() + facet_grid(`WorkType` ~ .)+
  xlim(50,500) +
  labs(x = "Time Per Task in sec", y = "PKT score And WorkType") +
  ggtitle("Minerva")

# MD5
Minerva_data %>% group_by(`Branch`, `WorkType`) %>% summarise(Count = n()) %>% 
  ggplot(aes(x = `Branch`, y = Count, fill = `WorkType`) ) +
  geom_bar( position = "dodge", stat = "identity")  +
  ggtitle("Minerva")


# Gender related plots

Minerva_data %>% group_by(`Gender`, `WorkType`) %>% summarise(Mean = median(AvgTPTInSec2)) %>% 
  ggplot(aes(x = `Gender`, y = Mean, fill = `WorkType`) ) +
  geom_bar( position = "dodge", stat = "identity")  +
  labs(y = "Median Time Per Task in sec") +
  ggtitle("Minerva")

# Easy interpretable bar diagrams

Minerva_data %>% group_by(Branch) %>% summarise(Median = median(AvgTPTInSec2)) %>% 
  ggplot(aes(x = `Branch`, y = Median, fill = `Branch`) ) +
  geom_bar( position = "dodge", stat = "identity")  +
  labs(y = "Median Time Per Task in sec") +
  ggtitle("Minerva")

Minerva_data %>% group_by(`WorkType`,`Branch`) %>% summarise(Median = median(AvgTPTInSec2)) %>% 
  ggplot(aes(x = `Branch`, y = Median, fill = `WorkType`) ) +
  geom_bar( position = "dodge", stat = "identity")  +
  labs(y = "Median Time Per Task in sec") +
  ggtitle("Minerva")

Minerva_data %>% group_by(`NOT1`,`Branch`, `WorkType`) %>% summarise(Median = median(AvgTPTInSec2)) %>% 
  ggplot(aes(x = `Branch`, y = Median, fill = `NOT1`) ) +
  geom_bar( position = "dodge", stat = "identity")  +
  labs(y = "Median Time Per Task in sec") +
  ggtitle("Minerva")

Minerva_data %>% group_by(`NOT1`,`Branch`, `WorkType`) %>% summarise(Median = median(AvgTPTInSec2)) %>% 
  ggplot(aes(x = `Branch`, y = Median, fill = `NOT1`) ) +
  geom_bar( position = "dodge", stat = "identity") + facet_grid(`WorkType` ~ .) +
  labs(y = "Median Time Per Task in sec") +
  ggtitle("Minerva")


