library(tidyverse, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)

Athena_data <- data.table::fread("./csv/Ath3/MSL_semistuctured_2.csv",header = T)
Athena_data <- Athena_data %>% select(-c("Employee Name", "Gender", "Employee Branch", "works", "TimeTaken","Wday", "RAND"))
Athena_data <- Athena_data[complete.cases(Athena_data),]

NOT1 <- c()
for(i in 1:nrow(Athena_data)){
  if(Athena_data$NOT[i] == 1){
    NOT1[i] <- '1'
  }else if(Athena_data$NOT[i]==2){
    NOT1[i] <- '2'
  }else{
    NOT1[i] <- '3'
  }
}
Athena_data$NOT1 <- NOT1
Athena_data <- Athena_data %>% select(-c("NOT"))
Athena_data <- as_tibble(Athena_data)

# AD4

MeanTPTByNOT1 <- Athena_data %>% group_by(NOT1) %>% summarise(Mean = mean(TPT))

ggplot(data = Athena_data,mapping = aes(TPT, fill = NOT1)) +
  geom_density(alpha = 0.5) +
  xlim(0,2000) +
  labs(x = "Time Per Task in minute") +
  ggtitle("Athena") +
  geom_vline(data = MeanTPTByNOT1, aes(xintercept = Mean, color = NOT1))



ggplot(data = Athena_data,mapping = aes(TPT, fill = NOT1)) +
  geom_histogram( alpha=0.5, position="identity", bins = 10) +
  xlim(0,2000) +
  ylim(0,15) +
  labs(x = "Time Per Task in minute") +
  ggtitle("Athena") +
  geom_vline(data = MeanTPTByNOT1, aes(xintercept = Mean, color = NOT1))

ggplot(data = Athena_data,mapping = aes(y = TPT, fill = NOT1)) +
  geom_boxplot() +
  ylim(50,350) +
  labs(y = "Time Per Task in minute", x = "Number Of Trainings") +
  ggtitle("Athena") 


ggplot(data = Athena_data,mapping = aes(factor(NOT1),TPT, fill = NOT1)) +
  geom_violin() +
  ylim(0,2000) +
  labs(y = "Time Per Task in minute", x = "NOT1") +
  ggtitle("Athena") +
  geom_hline(data = MeanTPTByNOT1, aes(yintercept = Mean, color = NOT1))

# -----------------------------------------

Athena_data_2 <- data.table::fread("./csv/Ath3/Ath_total_production_data.csv",header = T)

Athena_data_2 <- Athena_data_2 %>% select(-c("Employee Name", "Submit Status", "Shift Date"))

discard_lim <- 20

# AD2 
ggplot(data = Athena_data_2, aes(x = `Tot Time in Min`, color = `Task Type`)) + geom_histogram(binwidth=1) + xlim(discard_lim,150) + labs(title = "Athena Production")


ggplot(data = Athena_data_2, aes(x = `Tot Time in Min`, color = `Task Type`)) + geom_histogram(binwidth=1) + xlim(discard_lim,150) + facet_grid(`Task Type` ~ .) + ylim(0,100) + labs(title = "Athena Production")




# AD3

ggplot(data = Athena_data_2, aes(x = `Tot Time in Min`, color = `Frame-Info`)) + geom_histogram(binwidth=1) + xlim(discard_lim,1000) + ylim(0,10) + labs(title = "Athena Production")

ggplot(data = Athena_data_2, aes(x = `Tot Time in Min`, color = `Frame-Info`)) + geom_histogram(binwidth=1) + xlim(discard_lim,2000) + facet_grid(`Frame-Info` ~ .) + ylim(0,10) + labs(title = "Athena Production")


ggplot(data = Athena_data_2, aes(x = `Tot Time in Min`, color = `Task Type`)) + geom_histogram(binwidth=1) + xlim(discard_lim,150) + facet_wrap(`Task Type` ~ `Frame-Info`) + ylim(0,50) + labs(title = "Athena Production")


# AD1



ggplot(data = Athena_data_2, aes(x = `Tot Time in Min`, color = `Employee Branch`)) + geom_histogram(binwidth=1) + xlim(discard_lim,1000) + ylim(0,10) + labs(title = "Athena Production")

ggplot(data = Athena_data_2, aes(x = `Tot Time in Min`, color = `Employee Branch`)) + geom_histogram(binwidth=1) + xlim(discard_lim,2000) + facet_grid(`Employee Branch` ~ .) + ylim(0,10) + labs(title = "Athena Production")





# AD1 2

MeanTPTByBranch <- Athena_data %>% group_by(`Branch`) %>% summarise(Mean = mean(`TPT`))

ggplot(data = Athena_data,mapping = aes(TPT, fill = Branch)) +
  geom_density(alpha = 0.25) +
  xlim(0,2000) +
  labs(x = "Time Per Task in minute", y = "") +
  ggtitle("Atena") +
  geom_vline(data = MeanTPTByBranch, aes(xintercept = Mean, color = Branch))





ggplot(data = Athena_data,mapping = aes(y = TPT, fill = Branch)) +
  geom_boxplot() +
  ylim(100,2000) +
  labs(y = "Time Per Task in minute", x = "Branch") +
  ggtitle("Atena")


ggplot(data = Athena_data,mapping = aes(factor(Branch),TPT, fill = Branch)) +
  geom_violin() +
  ylim(0,400) +
  labs(y = "Time Per Task in minute", x = "Branch") +
  ggtitle("Atena") +
  geom_hline(data = MeanTPTByBranch, aes(yintercept = Mean, color = Branch))


# AD5 PKT related graph

ggplot(data = Athena_data,mapping = aes(TPT,PKT, color = Branch )) + 
  geom_jitter() + facet_grid(`NOT1` ~ .)+
  xlim(50,1000) + ylim(70,100) +
  labs(x = "Time Per Task in Minute", y = "PKT score And Number of Training") +
  ggtitle("Athena")


ggplot(data = Athena_data,mapping = aes(TPT,PKT, color = Branch )) + 
  geom_jitter() + facet_grid(`Task Type` ~ .)+
  xlim(50,1000) + ylim(70,100) +
  labs(x = "Time Per Task in Minute", y = "PKT score And Task Type") +
  ggtitle("Athena")


ggplot(data = Athena_data,mapping = aes(TPT,PKT, color = Branch )) + 
  geom_jitter() + facet_grid(`Frame-Info` ~ .)+
  xlim(50,1000) + ylim(70,100) +
  labs(x = "Time Per Task in Minute", y = "PKT score And Task Type wrt Frame") +
  ggtitle("Athena")


# AD6
Athena_data_2 %>% group_by(`Frame-Info`,`Task Type`, `Employee Branch`) %>% summarise(Count = n()) %>% 
  ggplot(aes(x = `Task Type`, y = Count, fill = `Frame-Info`) ) +
  geom_bar( position = "dodge", stat = "identity") + facet_grid(`Employee Branch` ~ .) +
  ggtitle("Athena")




# easy interpretable graphs

Athena_data_2 %>% group_by(`Frame-Info`,`Task Type`, `Employee Branch`) %>% summarise(Median = median(`Tot Time in Min`)) %>% 
  ggplot(aes(x = `Task Type`, y = Median, fill = `Frame-Info`) ) +
  geom_bar( position = "dodge", stat = "identity") + facet_grid(`Employee Branch` ~ .) +
  labs( y = "Median Time Per Task")
  ggtitle("Athena")
  
Athena_data %>% group_by(`NOT1`, `Branch`) %>% summarise(Median = median(`Tot Time in Min`)) %>% 
    ggplot(aes(x = `Task Type`, y = Median, fill = `Frame-Info`) ) +
    geom_bar( position = "dodge", stat = "identity") + facet_grid(`Employee Branch` ~ .) +
    labs( y = "Median Time Per Task")
  ggtitle("Athena")
