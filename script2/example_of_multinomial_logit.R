# Part 1

# install.packages("vcdExtra")
require(vcdExtra)
data(Alligator)
Alligator


allitable <- xtabs(count~lake+sex+size+food, data=Alligator)
allitable # to view the contingency tables

# Table 7.1 in Agresti's book
structable(food~lake+sex+size, allitable)


## Illustrative plots
plot(allitable, shade=TRUE)
mosaic(~food+lake+size, allitable, shade=TRUE)
mosaic(~food+lake+size, allitable, shade=TRUE,
       expected=~lake:size+food)
# install.packages(ggplot2)
require(ggplot2)
Alg1a=aggregate(count~food,data=Alligator,sum)
ggplot(Alg1a,aes(food,count)) + geom_bar(stat="identity")


Alg1b=aggregate(count~size,data=Alligator,sum)
ggplot(Alg1b,aes(size,count)) + geom_bar(stat="identity")
Alg1c=aggregate(count~sex,data=Alligator,sum)
ggplot(Alg1c,aes(sex,count)) + geom_bar(stat="identity")
Alg1d=aggregate(count~lake,data=Alligator,sum)
ggplot(Alg1d,aes(lake,count)) + geom_bar(stat="identity")


pairs(xtabs(count ~ ., Alligator)) ## present in library vcd


doubledecker(xtabs(count ~ lake + sex, data = Alligator),
             gp = gpar(fill = c("grey90", "steelblue")))

doubledecker(xtabs(count ~ food + size, data = Alligator),
             gp = gpar(fill = c("grey90", "tomato")))


Alg2=aggregate(count~sex+food+size,data=Alligator,sum)
ggplot(Alg2,aes(food,count,fill=sex))+geom_bar(stat="identity") +
  facet_grid(size ~ sex )+theme(legend.position="none")


ggplot(Alligator, aes(food, count, fill=sex)) +
  geom_bar(stat = "identity") +
  facet_grid(lake ~ sex + size) +
  theme(legend.position="none")

require(nnet) ## contains the function multinom
mod1=multinom(food~lake+size,data=Alligator,weights=count)

## multinom aranges all the levels of the predcitors
## alphabetically in an ascending order and uses bird
## as the baseline level for food, large as the baseline
## level for size and George as the baseline level
## (also called reference level) for lake.

mod1
summary(mod1)

# ------------------------------------------------------------------------------------------------------
# Part 2


