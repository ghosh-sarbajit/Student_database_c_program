Table11 <- read.csv("./csv/lets_see.csv",header = T)
attach(Table11)

t.test(TPTIL , TPTIQ, mu = 0 , var.equal = F, paired = T)
t.test(TPTIL , TPTIQ, mu = 0 , var.equal = F, paired = F)

t.test(TPTIL , TPTFQ, mu = 0 , var.equal = F, paired = T)
t.test(TPTIL , TPTFQ, mu = 0 , var.equal = F, paired = F)


TPTIL <- as.numeric(TPTIL)
TPTIQ <- as.numeric(TPTIQ)

boxplot(TPTIL ~ TPTIQ)
