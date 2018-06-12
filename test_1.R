#####sdc_age_0####
###sdc_HUNT_0$sdc_age_0<- HUNT1$PartAg_NT1BLQ1
library(epiDisplay)
library(tidyverse)
library(data.table)




identical(validation_data$PartAg_NT1BLQ1,validation_data$sdc_age_0)
ggplot(validation_data,aes(sdc_age_0))+geom_histogram(stat="count", fill="steelblue",binwidth = 1)+ xlab("age") + ylab("Frequency")
ggplot(validation_data,aes(sdc_age_0))+geom_histogram(stat="count", fill="steelblue",binwidth = 1)+ xlab("age") + ylab("Frequency")
summ(as.numeric(sdc_HUNT_0$sdc_age_0))
summ(as.numeric(HUNT1$PartAg_NT1BLQ1))
rbind(A[[1]],B[[1]])


######AGE_1##############
sdc_HUNT_1$sdc_age_1<- HUNT2$PartAg_NT2BLQ1
identical(sdc_HUNT_1$sdc_age_1,HUNT2$PartAg_NT2BLQ1)

###age2##########
sdc_HUNT_2$sdc_age_2<- HUNT3$PartAg_NT3BLQ1
identical(sdc_HUNT_2$sdc_age_2,HUNT3$PartAg_NT3BLQ1)
ggplot(sdc_HUNT_2,aes(sdc_age_2))+geom_histogram(stat="count", fill="steelblue",binwidth = 30) + xlab("age") + ylab("Frequency")
ggplot(HUNT3,aes(PartAg_NT3BLQ1))+geom_histogram(stat="count", fill="steelblue",binwidth = 30)+ xlab("age") + ylab("Frequency")
summary(sdc_HUNT_2$sdc_age_2)
summ(HUNT3$PartAg_NT3BLQ1)


######sdc_gender_0###
sdc_HUNT_0$sdc_gender_0<- recode(as.integer(HUNT1$Sex),'0'=1L,'1'=0L)
summ(as.factor(sdc_HUNT_0$sdc_gender_0))
summ(as.factor(HUNT1$Sex))
ggplot(sdc_HUNT_0,aes(sdc_gender_0))+geom_histogram(stat="count", fill="steelblue",binwidth = 1) + xlab("age") + ylab("Frequency")
ggplot(HUNT1,aes(Sex))+geom_histogram(stat="count", fill="steelblue",binwidth = 1) + xlab("age") + ylab("Frequency")


#####sdc_gender_1###
sdc_HUNT_1$sdc_gender_1<- recode(as.integer(HUNT2$Sex),'0'=1L,'1'=0L)
summ(as.factor(sdc_HUNT_1$sdc_gender_1))
summ(as.factor(HUNT2$Sex))

#####sdc_gender_2###
sdc_HUNT_2$sdc_gender_2<- recode(as.integer(HUNT3$Sex),'0'=1L,'1'=0L)
summ(as.factor(sdc_HUNT_2$sdc_gender_2))
summ(as.factor(HUNT3$Sex))

##########################
#####Employment status#########
sdc_HUNT_0$sdc_employ_0<- recode(as.integer(HUNT1$`WorCu_NT1BLQ1`),'1'=1L,'2'=1L,'3'=1L,'4'=0L)
summ(as.factor(sdc_HUNT_0$sdc_employ_0))
summ(as.factor(HUNT1$`WorCu_NT1BLQ1`))
validation_data$sdc_employ_0<-sdc_HUNT_0$sdc_employ_0
validation_data$HUNT1$`WorCu_NT1BLQ1`<-HUNT1$`WorCu_NT1BLQ1`

AfterH<-table(sdc_HUNT_0$sdc_employ_0,useNA = "always")
X<-table(HUNT1$`WorCu_NT1BLQ1`)
BeforeH<-c(sum(X["4"]),sum(X[c("1","2","3")]),sum(is.na(HUNT1$`WorCu_NT1BLQ1`)))
C <- rbind(BeforeH,AfterH)
colnames(C)<-c("0","1","NA")
##################################
sdc_HUNT_2$sdc_employ_2<- as.integer(HUNT3$WorCu_NT3BLI)
summ(as.factor(sdc_HUNT_2$sdc_employ_2))
summ(as.factor(HUNT3$WorCu_NT3BLI))

##############Highest level of educatioN ###############
sdc_HUNT_0$sdc_highest_edu_0<- recode(as.integer(HUNT1$Educ_NT1BLQ2), 
                                      '1'=1L,'2'=1L,'3'=1L,'4'=1L,'5'=3L,'6'=4L,'7'=4L,'8'=4L)

summ(as.factor(sdc_HUNT_0$sdc_highest_edu_0))
summ(as.factor(HUNT1$Educ_NT1BLQ2))

AfterH<-table(sdc_HUNT_0$sdc_highest_edu_0,useNA = "always")
X<-table(HUNT1$Educ_NT1BLQ2)
BeforeH<-c(sum(X[c("1","2","3","4")]),sum(X["5"]),sum(is.na(HUNT1$Educ_NT1BLQ2)))

################sdc_highest_edu_1######
sdc_HUNT_1$sdc_highest_edu_1<- recode(as.integer(HUNT2$Educ_NT2BLQ1),'1'=1L,'2'=2L,'3'=3L,'4'=4L,'5'=4L)

summ(as.factor(sdc_HUNT_1$sdc_highest_edu_1))
summ(as.factor(HUNT2$Educ_NT2BLQ1))

AfterH<-table(sdc_HUNT_1$sdc_highest_edu_1,useNA = "always")
X<-table(HUNT2$Educ_NT2BLQ1)
BeforeH<-c(X["1"],X["2"],X["3"],sum(X[c("4","5")]),sum(is.na(HUNT2$Educ_NT2BLQ1)))
C <- rbind(BeforeH,AfterH)
colnames(C)<-c("1","2","3","4","NA")

#####Post-secondary education######
##hunt1#
sdc_HUNT_0$sdc_postsec_edu_0<- recode(as.integer(HUNT1$Educ_NT1BLQ2), 
                                      '1'=0L,'2'=0L,'3'=0L,'4'=0L,'5'=1L,'6'=1L,'7'=1L,'8'=1L)

#hunt2#
sdc_HUNT_1$sdc_postsec_edu_1<- recode(as.integer(HUNT2$Educ_NT2BLQ1),'1'=0L,'2'=0L,'3'=0L,'4'=1L,'5'=1L)
table(sdc_HUNT_1$sdc_postsec_edu_1)

##############civil status#############
sdc_HUNT_0$sdc_civstat_0<- recode(as.integer(HUNT1$MaritStat_NT1BLQ1),'1'=0L,'2'=1L,'3'=3L,'4'=2L,'5'=2L)

summ(as.factor(sdc_HUNT_0$sdc_civstat_0))

#########marital status########
sdc_HUNT_0$sdc_civstat_0<- recode(as.integer(HUNT1$MaritStat_NT1BLQ1),'1'=0L,'2'=1L,'3'=3L,'4'=2L,'5'=2L)

AfterH<-table(sdc_HUNT_0$sdc_civstat_0,useNA = "always")
X<-table(HUNT1$MaritStat_NT1BLQ1)
BeforeH<-c(X["1"],X["2"],sum(X[c("4","5")]),X["3"],sum(is.na(HUNT1$MaritStat_NT1BLQ1)))
C <- rbind(BeforeH,AfterH)
colnames(C)<-c("0","1","2","3","NA")

  ##hunt2
sdc_HUNT_1$sdc_civstat_1<- recode(as.integer(HUNT2$MaritStat_NT2BLQ2),
                                  '1'=0L,'2'=1L,'3'=3L,'4'=2L,'5'=2L,'6'=1L,'7'=2L,'8'=2L,'9'=3L)




