library(ggplot2)    
set.seed(123)
rexp_1<-data.frame(rexp=rexp(40,rate=0.2))
rexp_2<-data.frame(rexp=rexp(1000,rate=0.2))
r1<-ggplot(data=rexp_1, aes(rexp)) + geom_histogram(binwidth=1,aes(y = ..density..),fill="darkblue")+ theme_bw()+ggtitle("Exp. distribution of 40 elements")
r2<-ggplot(data=rexp_2, aes(rexp)) + geom_histogram(binwidth=1,aes(y = ..density..),fill="darkblue")+ theme_bw()+ggtitle("Exp. distribution of 1000 elements")
multiplot(r1,r2,cols=2)

mns = NULL
for (i in 1 : 1000) mns = c(mns, mean(rexp(40,rate=0.2)))
mns_1<-data.frame(mns)
ggplot(data=mns_1, aes(mns)) + geom_histogram(binwidth=0.3,aes(y = ..density..),fill="darkblue",colour="white")+ geom_density(color="purple",size=2)+
  theme_bw()+
  ggtitle("Averages of 1000 simulations (exp.distribution of 40 elements)")+labs(x="Averages of 1000 exponential distributions")+
  scale_x_continuous(breaks=seq(1,10,0.5))+ geom_vline(x=5,color="red",size=2)


vars = NULL
vars2=NULL
set.seed(123)
for (i in 1 : 1000) vars = c(vars, var(rexp(40,rate=0.2)))
for (i in 1 : 1000) vars2 = c(vars2, var(rexp(1000,rate=0.2)))
vars_1<-data.frame(vars)
vars_2<-data.frame(vars2)
p1<-ggplot(data=vars_1, aes(vars)) + geom_histogram(binwidth=0.3,aes(y = ..density..),fill="darkblue",colour="white")+ geom_density(color="purple",size=2)+
  theme_bw()+
  ggtitle("Variances of 1000 simulations (exp.distr. of 40 elements)")+labs(x="Variances of 1000 exp. distr.")+
  geom_vline(x=25,color="red",size=2)+ scale_x_continuous(breaks=seq(0,80,5))
p2<-ggplot(data=vars_2, aes(vars2)) + geom_histogram(binwidth=0.3,aes(y = ..density..),fill="darkblue",colour="white")+ geom_density(color="purple",size=2)+
  theme_bw()+
  ggtitle("Variances of 1000 simulations (exp.dist. of 1000 el.)")+labs(x="Variances of 1000 exp. distr.")+
 geom_vline(x=25,color="red",size=2)
multiplot(p1,p2)


    # CLT #

mu=5
sigma=5

for_std_normal=((mns-mu)/(sigma/sqrt(1000)))
for_std<-data.frame(x_clt=for_std_normal)
ggplot(data=for_std, aes(x_clt)) + geom_histogram(binwidth=0.3,aes(y = ..density..),fill="darkblue",colour="white")+ geom_density(color="purple",size=2)+
theme_bw()+
ggtitle(" CLT - simulation of 1000 exponential distributions") + labs(x="(mu-sigma)/(population_sd/sqrt(n))")+ geom_vline(x=0,color="red",size=2)





