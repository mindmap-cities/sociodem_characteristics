###Verification age
summ(sdc_HUNT_0$sdc_age_0)
summ(HUNT1$PartAg_NT1BLQ1)


summ(sdc_HUNT_1$sdc_age_1)
summ(HUNT2$PartAg_NT2BLQ1)

summ(sdc_HUNT_2$sdc_age_2)
summ(HUNT3$PartAg_NT3BLQ1)

################
table(sdc_HUNT_0$sdc_gender_0)
table(HUNT1$Sex)

summ(as.factor(sdc_HUNT_0$sdc_gender_0))

########emplyment
table(sdc_HUNT_0$sdc_employ_0)
table(HUNT1$`WorCu_NT1BLQ1`)



AfterH<-table(sdc_HUNT_2$sdc_civstat_2,useNA = "always")
X<-table(HUNT3$MaritStat_NT3BLQ1)
BeforeH<-c(X["1"],sum(X[c("2","6")]),sum(X[c("4","5","8")]),sum(X[c("3","9")]),sum(is.na(HUNT3$MaritStat_NT3BLQ1)))
C <- data.frame(rbind(BeforeH,AfterH))
colnames(C)<-c("0","1","2","3","NA")

table(sdc_HUNT_2$sdc_civstat_2,HUNT3$MaritStat_NT3BLQ1)


AfterH<-table(sdc_HUNT_2$sdc_employ_2,useNA = "always")
X<-table(HUNT3$WorCu_NT3BLI)
BeforeH<-c(X[1],X[2],sum(is.na(HUNT3$WorCu_NT3BLI)))
C <- data.table(rbind(BeforeH,AfterH))
colnames(C)<-c(names(attributes(HUNT3$WorCu_NT3BLI)[[9]][1:2]),"NA")
rownames(C)<-c("BeforeH","AfterH")


attributes(HUNT1$Sex)[9]
attributes(HUNT1$Sex)[7]

ggplot(AfterH, aes(x=factor(Sex))) + geom_bar(stat="count", width=0.4, fill="steelblue") + xlab("Yes","No","Missing") + ylab("Frequency")


cd<-validator( sdc_HUNT_0$sdc_age_0>20)
summary()

fwrite(C,)
