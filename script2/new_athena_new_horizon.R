require(tidyverse)
require(dplyr)
require(tidyr)
require(lubridate)
discard_lim <- 10.00 # min

Ath_MSL_july <- data.table::fread("./csv/Ath3/Task_Detail_Report_2020-07-01__2020-07-31.csv",header = T)
names(Ath_MSL_july)
# Ath_MSL_july <- as_tibble(Ath_MSL_july)
# table(Ath_MSL_july$`Person Count`)
# table(Ath_MSL_july$`Total Object Count`)
# table(Ath_MSL_july$`Remarks`)
table(Ath_MSL_july$`Frame-Info`)




Ath_MSL_july_1 <- Ath_MSL_july %>% select(`Employee Name`, `Tot Time in Min`, `Task Type`, `Submit Status`, `Employee Branch`, `Shift Date`, `Frame-Info`)
# rm(Ath_MSL_july)

# Filteringout unsubmitted works
Ath_MSL_july_1 <- Ath_MSL_july_1 %>% filter(`Submit Status` == 'submitted')
# for testing whether they are filterd or not
table(Ath_MSL_july_1$`Submit Status`)

# Filteringout ohterworks
Ath_MSL_july_other <- Ath_MSL_july_1 %>% filter(`Frame-Info` == 'other')
table(Ath_MSL_july_other$`Frame-Info`)
Ath_MSL_july_1 <- Ath_MSL_july_1 %>% filter(`Frame-Info` != 'other')
table(Ath_MSL_july_1$`Frame-Info`)

# Filtering low-time works
Ath_MSL_july_1 <- Ath_MSL_july_1 %>% filter(`Tot Time in Min` >= discard_lim)


# Mean and boxplot wrt group
table(Ath_MSL_july_1$`Task Type`, Ath_MSL_july_1$`Frame-Info`)
(Mean_july <- Ath_MSL_july_1 %>% group_by(`Task Type`) %>% summarise(Min = min(`Tot Time in Min`), Mean = mean(`Tot Time in Min`), Max = max(`Tot Time in Min`)))

Ath_MSL_july_1 %>% group_by(`Task Type`) %>% ggplot(mapping = aes(x = `Task Type`, y = `Tot Time in Min`)) + geom_boxplot()

ggplot(data = Ath_MSL_july_1, aes(x = `Tot Time in Min`, color = `Task Type`)) + geom_histogram(binwidth=1) + xlim(discard_lim,150) + labs(title = "July Production")

ggplot(data = Ath_MSL_july_1, aes(x = `Tot Time in Min`, color = `Task Type`)) + geom_histogram(binwidth=1) + xlim(discard_lim,150) + facet_grid(`Task Type` ~ .) + ylim(0,100) + labs(title = "July Production")

ggplot(data = Ath_MSL_july_1, aes(x = `Tot Time in Min`, color = `Task Type`)) + geom_histogram(binwidth=1) + xlim(discard_lim,150) + facet_wrap(`Task Type` ~ `Frame-Info`) + ylim(0,75) + labs(title = "July Production")

# Final subset phase1

Ath_MSL_july_save <- Ath_MSL_july_1 %>% group_by(`Employee Name`, `Task Type`, `Employee Branch`,  `Frame-Info`) %>% summarise(works = n(), TimeTaken = sum(`Tot Time in Min`),TPT = sum(`Tot Time in Min`)/n() , Wday = n_distinct(`Shift Date`)) 


# Model
table(Ath_MSL_july_1$`Task Type`, Ath_MSL_july_1$`Frame-Info`)
Model1 <- lm(TPT ~ `Task Type` + `Frame-Info`, data = Ath_MSL_july_save)
summary(Model1)
write.csv(Ath_MSL_july_save, "./csv/Ath3/Ath_july_production_data.csv", row.names = F)

# different name extraction
write.csv(Ath_MSL_july_save %>% group_by(`Employee Name`) %>% summarise(), "./csv/Ath3/Ath_July_Name2.csv", row.names = F)



# -------------------------------------------------------------------------
#             June
# -------------------------------------------------------------------------
discard_lim <- 10.00

Ath_MSL_june <- data.table::fread("./csv/Ath3/Task_Detail_Report_2020-06-01__2020-06-30.csv",header = T)
names(Ath_MSL_june)
Ath_MSL_june <- as_tibble(Ath_MSL_june)

# # different name extraction
# Ath_MSL_June_1 <- Ath_MSL_june %>% select(`Employee Name`)
# Ath_MSL_June_1 <- Ath_MSL_June_1 %>% group_by(`Employee Name`) %>% summarise()
# write.csv(Ath_MSL_June_1, "./csv/Ath3/Ath_June_Name.csv", row.names = F)



Ath_MSL_june_1 <- Ath_MSL_june %>% select(`Employee Name`, `Tot Time in Min`, `Task Type`, `Submit Status`, `Employee Branch`, `Shift Date`, `Frame-Info`)
Ath_MSL_june_1 <- Ath_MSL_june_1 %>% filter(`Task Type` != 'Image Editor')
table(Ath_MSL_june_1$`Task Type`)

# Filteringout unsubmitted works
Ath_MSL_june_1 <- Ath_MSL_june_1 %>% filter(`Submit Status` == 'submitted')
# for testing whether they are filterd or not
table(Ath_MSL_june_1$`Submit Status`)

# Filteringout ohterworks
Ath_MSL_june_other <- Ath_MSL_june_1 %>% filter(`Frame-Info` == 'other')
table(Ath_MSL_june_other$`Frame-Info`)
Ath_MSL_june_1 <- Ath_MSL_june_1 %>% filter(`Frame-Info` != 'other')
table(Ath_MSL_june_1$`Frame-Info`)

# Filtering low-time works
Ath_MSL_june_1 <- Ath_MSL_june_1 %>% filter(`Tot Time in Min` >= discard_lim)


# Mean and boxplot wrt group
(Mean_june <- Ath_MSL_june_1 %>% group_by(`Task Type`) %>% summarise(Min = min(`Tot Time in Min`), Mean = mean(`Tot Time in Min`), Max = max(`Tot Time in Min`)))


Ath_MSL_june_1 %>% group_by(`Task Type`) %>% ggplot(mapping = aes(x = `Task Type`, y = `Tot Time in Min`)) + geom_boxplot()

ggplot(data = Ath_MSL_june_1) +
  stat_summary( # default geom_pointrange
    mapping = aes(x = `Task Type`, y = `Tot Time in Min`),
    fun.min = min,
    fun.max = max,
    fun = median
  )

Ath_MSL_june_save <- Ath_MSL_june_1 %>% group_by(`Employee Name`, `Task Type`, `Employee Branch`,  `Frame-Info`) %>% summarise(works = n(), TimeTaken = sum(`Tot Time in Min`),TPT = sum(`Tot Time in Min`)/n() , Wday = n_distinct(`Shift Date`)) 
write.csv(Ath_MSL_june_save, "./csv/Ath3/Ath_june_production_data.csv", row.names = F)


Ath_MSL_june_save %>% group_by(`Employee Branch`) %>% summarise(tworks = sum(works))

# Model
table(Ath_MSL_june_1$`Task Type`, Ath_MSL_june_1$`Frame-Info`)
Model <- lm(TPT ~ `Task Type` + `Frame-Info`, data = Ath_MSL_june_save)
summary(Model)

# different name extraction
write.csv(Ath_MSL_june_save %>% group_by(`Employee Name`) %>% summarise(), "./csv/Ath3/Ath_June_Name2.csv", row.names = F)

# -------------------------------------------------------------------------
#             May
# -------------------------------------------------------------------------



Ath_MSL_may <- data.table::fread("./csv/Ath3/Task_Detail_Report_2020-05-01__2020-05-31.csv",header = T)
names(Ath_MSL_may)
Ath_MSL_may <- as_tibble(Ath_MSL_may)
Ath_MSL_may_1 <- Ath_MSL_may %>% select(`Employee Name`, `Tot Time in Min`, `Task Type`, `Submit Status`, `Employee Branch`, `Shift Date`, `Frame-Info`)
table(Ath_MSL_may_1$`Task Type`)

# # different name extraction
# Ath_MSL_may_1 <- Ath_MSL_may %>% select(`Employee Name`)
# Ath_MSL_may_1 <- Ath_MSL_may_1 %>% group_by(`Employee Name`) %>% summarise()
# write.csv(Ath_MSL_may_1, "./csv/Ath3/Ath_may_Name.csv", row.names = F)

# Filteringout unsubmitted works
Ath_MSL_may_1 <- Ath_MSL_may_1 %>% filter(`Submit Status` == 'submitted')
# for testing whether they are filterd or not
table(Ath_MSL_may_1$`Submit Status`)

# Filteringout ohterworks
# Ath_MSL_may_other <- Ath_MSL_may_1 %>% filter(`Frame-Info` == 'other')
# table(Ath_MSL_may_other$`Frame-Info`)
Ath_MSL_may_1 <- Ath_MSL_may_1 %>% filter(`Frame-Info` != 'other')
table(Ath_MSL_may_1$`Frame-Info`)

# Filtering low-time works
Ath_MSL_may_1 <- Ath_MSL_may_1 %>% filter(`Tot Time in Min` >= discard_lim)


# Mean and boxplot wrt group
(Mean_may <- Ath_MSL_may %>% group_by(`Task Type`) %>% summarise(Mean = mean(`Tot Time in Min`)))
Ath_MSL_may %>% group_by(`Task Type`) %>% ggplot(mapping = aes(x = `Task Type`, y = `Tot Time in Min`)) + geom_boxplot()


Ath_MSL_may_save <- Ath_MSL_may_1 %>% group_by(`Employee Name`, `Task Type`, `Employee Branch`,  `Frame-Info`) %>% summarise(works = n(), TimeTaken = sum(`Tot Time in Min`),TPT = sum(`Tot Time in Min`)/n() , Wday = n_distinct(`Shift Date`))

write.csv(Ath_MSL_may_save, "./csv/Ath3/Ath_may_production_data.csv", row.names = F)

# Model
table(Ath_MSL_may_1$`Task Type`, Ath_MSL_may_1$`Frame-Info`)
Model <- lm(TPT ~ `Task Type` + `Frame-Info`, data = Ath_MSL_may_save)
summary(Model)

Ath_MSL_may_save %>% group_by(`Employee Branch`) %>% summarise(tworks = sum(works))


MSL_three_month <- rbind(Ath_MSL_july_1, Ath_MSL_may_1, Ath_MSL_june_1)
write.csv(MSL_three_month, "./csv/Ath3/Ath_total_production_data.csv", row.names = F)

# different name extraction
write.csv(Ath_MSL_may_save %>% group_by(`Employee Name`) %>% summarise(), "./csv/Ath3/Ath_may_Name2.csv", row.names = F)
