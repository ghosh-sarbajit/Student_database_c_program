# REC and DA analysis

# REC Oct'19 to Apr'20 data 7 months

REC_MBZ <- c( 1481, 20532, 30399, 80156, 7128, 32304, 42819)
# REC_SLT <- c(49831, 30266, 32042, 66994, 63532, 49261, 988)
# REC_SLT <- 0.15*REC_SLT

# DA MBZ : drives app Dec'19 to Apr'20 5 months and  quick question nov'19 apr'20 6 months
DA_DA <- c(NA,NA, 13411.5, 11544, 37125, 46282, 38581)
DA_QQ <- c(NA, 362557, 256354 , 84448, 24534, 46087, 8627)

# MBZ Athena combined model




# earning before tax Oct'19 to Apr'20

EBT_SLT <- c(
             116106, 
             277378, 
             158279, 
             221207, 
             64740,
             241395,
             14360)

EBT_BBZ <- c(  
               
              203729,
              199915,
              292982, 
              235141, 
              116099,
              22360)

EBT_MBZ <- c( 
              159486, 
              199088,
              224468,
              183402, 
              91711, 
              82823,
              34815)

EBT_MBZ_Ath <- 0.6 * EBT_MBZ
MBZ_Athena_prod <- 2 * REC_MBZ + DA_DA + DA_QQ

# Model 1 for Rec [ Rec + MBZ]
# Model1 <- lm(EBT_MBZ_Ath ~ REC_MBZ )
# summary(Model1)

Model1 <- lm(EBT_MBZ_Ath ~  MBZ_Athena_prod)
summary(Model1)

# MBZ_Athena_prod <- 0.7 * MBZ_Athena_prod
# Model4 <- lm(EBT_MBZ_Ath ~  MBZ_Athena_prod)
# summary(Model4)

MBZ_Athena_prod[3] <- 0.8 * MBZ_Athena_prod[3]
MBZ_Athena_prod[4] <- 0.9 * MBZ_Athena_prod[4]
MBZ_Athena_prod[5] <- 0.95 * MBZ_Athena_prod[5]
MBZ_Athena_prod[6] <- 0.81 * MBZ_Athena_prod[6]
MBZ_Athena_prod[7] <- 0.95 * MBZ_Athena_prod[7]


Model1 <- lm(EBT_MBZ_Ath ~  MBZ_Athena_prod)
summary(Model1)


#=================================================================================================================================
# Minerva

# Mar'19 - May'20
MBZ_IBBL <- c(22000,75526,11675)
BBS_IBBL <- c(NA,164304,311215.5)
SLT_IBBL <- c(48270,99498,77444)

# Mar'19 - May'20 Earning Before Tax
MBZ_ebt <- 0.15 * c(116099,
                22360,
                NA)
SLT_ebt <- 0.9 * c(241395,
                14360,
                NA)
BBS_ebt <- 0.2 * c(82823,
                34815,
                NA)


Model1 <- lm(SLT_ebt ~ SLT_IBBL)
summary(Model1)
