data(ToothGrowth)
str(ToothGrowth)
    #data contains 60 obs
    #3 variables
summary(ToothGrowth$len)
summary(ToothGrowth$dose)
table(ToothGrowth$supp)


ggplot(data=ToothGrowth,aes(len) )+ geom_histogram(color="white",fill="darkblue")+ ggtitle("Distribution of len variable")
ggplot(data=ToothGrowth,aes(dose) )+ geom_histogram(color="white",fill="darkblue")+ ggtitle("Distribution of dose variable")

table(ToothGrowth[ToothGrowth$supp=="OJ",]$dose)
table(ToothGrowth[ToothGrowth$supp=="VC",]$dose)

ggplot(data=ToothGrowth,aes(dose) )+ geom_bar()+ facet_wrap(~supp)

?ToothGrowth


table(ToothGrowth$supp,ToothGrowth$dose)



ToothGrowth$group<-"OJ_0.5";
ToothGrowth[ToothGrowth$supp=="OJ" & ToothGrowth$dose==1,]$group<-"OJ_1";
ToothGrowth[ToothGrowth$supp=="OJ" & ToothGrowth$dose==2,]$group<-"OJ_2";
ToothGrowth[ToothGrowth$supp=="VC" & ToothGrowth$dose==0.5,]$group<-"VC_0.5";
ToothGrowth[ToothGrowth$supp=="VC" & ToothGrowth$dose==1,]$group<-"VC_1";
ToothGrowth[ToothGrowth$supp=="VC" & ToothGrowth$dose==2,]$group<-"VC_2";




t1<-aggregate(len~group, FUN=mean,data=ToothGrowth)

ggplot(t1,aes(group,len,fill=group))+geom_bar(stat="identity",position="dodge")+
  theme(axis.text.x =element_text(size  = 10,angle = 90,hjust = 1,vjust = 1))+
  scale_fill_discrete(name="Group",breaks=c("OJ_0.5", "OJ_1", "OJ_2","VC_0.5","VC_1","VC_2"),
  labels=c("Orange_0.5", "Orange_1", "Orange_2","VitaminC_0.5","VitaminC_1","VitaminC_2"))+ggtitle("Average mean of tooth length by group")



tapply(ToothGrowth$len, ToothGrowth$group, var)
    #take the subset
group1_2<-ToothGrowth[ToothGrowth$group %in% c("OJ_0.5","OJ_1"),]
    #calculate CI
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group1_2)$conf
group1_3<-ToothGrowth[ToothGrowth$group %in% c("OJ_0.5","OJ_2"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group1_3)$conf
group1_4<-ToothGrowth[ToothGrowth$group %in% c("OJ_0.5","VC_0.5"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group1_4)$conf
group1_5<-ToothGrowth[ToothGrowth$group %in% c("OJ_0.5","VC_1"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group1_5)$conf
group1_6<-ToothGrowth[ToothGrowth$group %in% c("OJ_0.5","VC_2"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group1_6)$conf

group2_3<-ToothGrowth[ToothGrowth$group %in% c("OJ_1","OJ_2"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group2_3)$conf
group2_4<-ToothGrowth[ToothGrowth$group %in% c("OJ_1","VC_0.5"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group2_4)$conf
group2_5<-ToothGrowth[ToothGrowth$group %in% c("OJ_1","VC_1"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group2_5)$conf
group2_6<-ToothGrowth[ToothGrowth$group %in% c("OJ_1","VC_2"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group2_6)$conf

group3_4<-ToothGrowth[ToothGrowth$group %in% c("OJ_2","VC_0.5"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group3_4)$conf
group3_5<-ToothGrowth[ToothGrowth$group %in% c("OJ_2","VC_1"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group3_5)$conf
group3_6<-ToothGrowth[ToothGrowth$group %in% c("OJ_2","VC_2"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group3_6)$conf

group4_5<-ToothGrowth[ToothGrowth$group %in% c("VC_0.5","VC_1"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group4_5)$conf
group4_6<-ToothGrowth[ToothGrowth$group %in% c("VC_0.5","VC_2"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group4_6)$conf

group5_6<-ToothGrowth[ToothGrowth$group %in% c("VC_1","VC_2"),]
t.test(len ~ group, paired = FALSE, var.equal = FALSE, data = group5_6)$conf
