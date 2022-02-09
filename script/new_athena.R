require(tidyverse)
require(dplyr)
require(tidyr)
require(lubridate)

Ath_MSL_july <- data.table::fread("./csv/Ath3/Task_Detail_Report_2020-07-01__2020-07-31.csv",header = T)
names(Ath_MSL_july)
Ath_MSL_july <- as_tibble(Ath_MSL_july)
# table(Ath_MSL_july$`Person Count`)
# table(Ath_MSL_july$`Total Object Count`)
# table(Ath_MSL_july$`Remarks`)


# # different name extraction
# Ath_MSL_july_1 <- Ath_MSL_july %>% select(`Employee Name`)
# Ath_MSL_july_1 <- Ath_MSL_july_1 %>% group_by(`Employee Name`) %>% summarise()
# write.csv(Ath_MSL_july_1, "./csv/Ath3/Ath_July_Name.csv", row.names = F)


Ath_MSL_july_1 <- Ath_MSL_july %>% select(`Employee Name`, `Tot Time in Min`, `Task Type`, `Submit Status`, `Employee Branch`, `Shift Date`)
rm(Ath_MSL_july)

# Filteringout unsubmitted works

# some prework
table(Ath_MSL_july_1$`Submit Status`)
Ath_MSL_july_giveup <- Ath_MSL_july_1 %>% group_by(`Employee Name`) %>% summarise(works = n(), givup = sum(`Submit Status` == 'giveup'), givup_ratio = sum(`Submit Status` == 'giveup')/n())
# Ath_MSL_july_giveup %>% ggplot(mapping = aes(y = `Employee Name`, x = givup_ratio)) + geom_bar()

# filtering
Ath_MSL_july_1 <- Ath_MSL_july_1 %>% filter(`Submit Status` == 'submitted')
# for testing whether they are filterd or not
table(Ath_MSL_july_1$`Submit Status`)


# Mean and boxplot wrt group
table(Ath_MSL_july_1$`Task Type`)
(Mean_july <- Ath_MSL_july_1 %>% group_by(`Task Type`) %>% summarise(Min = min(`Tot Time in Min`), Mean = mean(`Tot Time in Min`), Max = max(`Tot Time in Min`)))
Ath_MSL_july_1 %>% group_by(`Task Type`) %>% ggplot(mapping = aes(x = `Task Type`, y = `Tot Time in Min`)) + geom_boxplot()


# Final subset phase1

Ath_MSL_july_save <- Ath_MSL_july_1 %>% group_by(`Employee Name`, `Task Type`, `Employee Branch`) %>% summarise(works = n(), TimeTaken = sum(`Tot Time in Min`), Wday = n_distinct(`Shift Date`))






# -------------------------------------------------------------------------


Ath_MSL_june <- data.table::fread("./csv/Ath3/Task_Detail_Report_2020-06-01__2020-06-30.csv",header = T)
names(Ath_MSL_june)
Ath_MSL_june <- as_tibble(Ath_MSL_june)

# # different name extraction
# Ath_MSL_June_1 <- Ath_MSL_june %>% select(`Employee Name`)
# Ath_MSL_June_1 <- Ath_MSL_June_1 %>% group_by(`Employee Name`) %>% summarise()
# write.csv(Ath_MSL_June_1, "./csv/Ath3/Ath_June_Name.csv", row.names = F)



Ath_MSL_june_1 <- Ath_MSL_june %>% select(`Employee Name`, `Tot Time in Min`, `Task Type`, `Branch`, `Date`)
Ath_MSL_june_1 <- Ath_MSL_june_1 %>% filter(`Task Type` != 'Image Editor')
table(Ath_MSL_june_1$`Task Type`)


# Mean and boxplot wrt group
Mean_june <- Ath_MSL_june_1 %>% group_by(`Task Type`) %>% summarise(Min = min(`Tot Time in Min`), Mean = mean(`Tot Time in Min`), Max = max(`Tot Time in Min`))


Ath_MSL_june_1 %>% group_by(`Task Type`) %>% ggplot(mapping = aes(x = `Task Type`, y = `Tot Time in Min`)) + geom_boxplot()

ggplot(data = Ath_MSL_june_1) +
  stat_summary( # default geom_pointrange
    mapping = aes(x = `Task Type`, y = `Tot Time in Min`),
    fun.min = min,
    fun.max = max,
    fun = median
  )
 
Ath_MSL_june_save <- Ath_MSL_june_1 %>% group_by(`Employee Name`, `Task Type`, `Branch`) %>% summarise(works = n(), TimeTaken = sum(`Tot Time in Min`), Wday = n_distinct(`Date`))
Ath_MSL_june_save %>% group_by(`Branch`) %>% summarise(tworks = sum(works))

# -------------------------------------------------------------------------


Ath_MSL_may <- data.table::fread("./csv/Ath3/Task_Detail_Report_2020-05-01__2020-05-31.csv",header = T)
names(Ath_MSL_may)
Ath_MSL_may <- as_tibble(Ath_MSL_may)
Ath_MSL_may_1 <- Ath_MSL_may %>% select(`Employee Name`, `Tot Time in Min`, `Task Type`, `Employee Branch`, `Shift Date`)
table(Ath_MSL_may_1$`Task Type`)

# # different name extraction
# Ath_MSL_may_1 <- Ath_MSL_may %>% select(`Employee Name`)
# Ath_MSL_may_1 <- Ath_MSL_may_1 %>% group_by(`Employee Name`) %>% summarise()
# write.csv(Ath_MSL_may_1, "./csv/Ath3/Ath_may_Name.csv", row.names = F)

# Mean and boxplot wrt group
Mean_may <- Ath_MSL_may %>% group_by(`Task Type`) %>% summarise(Mean = mean(`Tot Time in Min`))
Ath_MSL_may %>% group_by(`Task Type`) %>% ggplot(mapping = aes(x = `Task Type`, y = `Tot Time in Min`)) + geom_boxplot()


Ath_MSL_may_save <- Ath_MSL_may_1 %>% group_by(`Employee Name`, `Task Type`, `Employee Branch`) %>% summarise(works = n(), TimeTaken = sum(`Tot Time in Min`), Wday = n_distinct(`Shift Date`))

Ath_MSL_may_save %>% group_by(`Employee Branch`) %>% summarise(tworks = sum(works))

