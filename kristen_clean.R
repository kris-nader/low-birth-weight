# install packages
# clear all plots : if(!is.null(dev.list())) dev.off()
install.packages("ROCR")
install.packages("AICcmodavg")
install.packages("Amelia")
install.packages("mlbench")
install.packages("corrplot")
install.packages("DataExplorer")
install.packages("ggplot2")

# Read the data
lowbwt.df<-read.table(file=file.choose(),header=TRUE)
fix(lowbwt.df)
summary(lowbwt.df)
dim(lowbwt.df)
# we only have 189 individuals (small Dataset)

#Check type of each variable
sapply(lowbwt.df,class)

# Exploring the data and plotting 
library(DataExplorer)
plot_str(lowbwt.df)

# check for missing values

library(Amelia)
library(mlbench)
missmap(lowbwt.df, col=c("green", "pink"), legend=FALSE)

# checking frequencies
table(as.factor(lowbwt.df$FTV))
table(as.factor(lowbwt.df$FTV), as.factor(lowbwt.df$LOW))

# checking correlations 
library(corrplot)
# correlation matrix plot to given an idea of which variables change together
correlations <- cor(lowbwt.df)

corrplot(correlations, method="shade")
# warning, this should only be done for the continuous but I ran it for all

par(mfrow=c(1,1))
library(ggplot2)
attach(lowbwt.df)
boxplot(AGE~LOW,main="AGE~LOW")
boxplot(LWT~LOW,main="LWT~LOW")
boxplot(FTV~LOW,main="FTV~LOW")

boxplot(BWT~FTV)
ggplot(lowbwt.df,aes(x=as.factor(FTV),y=BWT,fill=as.factor(LOW)))+geom_boxplot()
ggplot(lowbwt.df,aes(x=as.factor(HT),y=BWT,fill=as.factor(LOW)))+geom_boxplot()
ggplot(lowbwt.df,aes(x=as.factor(UI),y=BWT,fill=as.factor(LOW)))+geom_boxplot()
ggplot(lowbwt.df,aes(x=as.factor(PTL),y=BWT,fill=as.factor(LOW)))+geom_boxplot()
ggplot(lowbwt.df,aes(x=as.factor(RACE),y=BWT,fill=as.factor(LOW)))+geom_boxplot()

detach(lowbwt.df)



# making a new dataframe, removing ID and BWT and factor-ing 
lowbwt.rdf<-lowbwt.df
lowbwt.rdf$RACE<-as.factor(lowbwt.rdf$RACE)
lowbwt.rdf$SMOKE<-as.factor(lowbwt.rdf$SMOKE)
lowbwt.rdf=lowbwt.rdf[,c(-1,-11)]
names(lowbwt.rdf)

# making a new df of only the 4 predictors
lowbwt.sdf<-lowbwt.rdf[,c(1,2,3,4,9)]

# Making models
attach(lowbwt.sdf)
lw_m0<-glm(LOW~AGE+LWT+RACE+FTV,data=lowbwt.sdf,family=binomial(link="logit"))
summary(lw_m0)

lw_m1<-glm(LOW~AGE,data=lowbwt.sdf,family=binomial(link="logit"))
summary(lw_m1)

lw_m2<-glm(LOW~LWT,data=lowbwt.sdf,family=binomial(link="logit"))
summary(lw_m2)

lw_m3<-glm(LOW~RACE,data=lowbwt.sdf,family=binomial(link="logit"))
summary(lw_m3)

lw_m4<-glm(LOW~FTV,data=lowbwt.sdf,family=binomial(link="logit"))
summary(lw_m4)

lw_m5<-glm(LOW~.^2,data=lowbwt.sdf,family=binomial(link="logit"))
summary(lw_m5)

lw_m6<-step(lw_m5,direction="both")
summary(lw_m6)


# Evaluating the models
# analysiss 1

# models and AUC
lowbwt.slist<-list()
lowbwt.slist[[1]]=lw_m0 # 0.65
lowbwt.slist[[2]]=lw_m1 # 0.55
lowbwt.slist[[3]]=lw_m2 # 0.61
lowbwt.slist[[4]]=lw_m3 # 0.59
lowbwt.slist[[5]]=lw_m4 # 0.54
lowbwt.slist[[6]]=lw_m5 # 0.69
lowbwt.slist[[7]]=lw_m6 # 0.67


lowbwt.smodnames<-c("age+lwt+race+ftv","age","lwt","race","FTV","all+.^2","step")

# evaluating models
library(AICcmodavg)
lowbwt.saictab<-aictab(cand.set=lowbwt.slist,modnames=lowbwt.smodnames)
lowbwt.saictab

# ROC curve on each model


par(mfrow=c(3,3))
library("ROCR")
auc.list=list()

for (i in seq(1,length(lowbwt.slist))){
predict<-fitted(lowbwt.slist[[i]])
pred<- prediction(predict,lowbwt.sdf$LOW)
perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf,main="sensitivity vs false positive rate",colorize=TRUE, colorkey.relwidth=0.5,lwd=4.5)
perf_auc<-performance(pred,measure="auc")
abline(0,1,lty=2)

auc.list[i]=perf_auc
}

# doing backward selection 
lw_m7<-step(lw_m5,direction="backward")
summary(lw_m7)

detach()

#anaysis 2

# change dataframe
lowbwt.a2df<-lowbwt.df[,c(-1,-11)]
lowbwt.a2df$RACE<-as.factor(lowbwt.a2df$RACE)
lowbwt.a2df$SMOKE<-as.factor(lowbwt.a2df$SMOKE)
lowbwt.a2df$HT<-as.factor(lowbwt.a2df$HT)
lowbwt.a2df$UI<-as.factor(lowbwt.a2df$UI)


plot_str(lowbwt.a2df)


lw_a10<-glm(LOW~.,data = lowbwt.a2df,family=binomial(link = "logit"))
summary(lw_a10)

lw_a11<-glm(LOW~.+AGE:FTV,data = lowbwt.a2df,family=binomial(link = "logit"))
summary(lw_a11)

lw_a12<-glm(LOW~.+LWT:FTV,data = lowbwt.a2df,family=binomial(link = "logit"))
summary(lw_a12)

lw_a13<-glm(LOW~.+PTL:UI,data = lowbwt.a2df,family=binomial(link = "logit"))
summary(lw_a13)

lw_a14<-glm(LOW~.+SMOKE:RACE,data = lowbwt.a2df,family=binomial(link = "logit"))
summary(lw_a14)

lw_a15<-glm(LOW~.+AGE:SMOKE,data = lowbwt.a2df,family=binomial(link = "logit"))
summary(lw_a15)

lw_a16<-glm(LOW~.+AGE:SMOKE+SMOKE:RACE+PTL:UI+LWT:FTV+AGE:FTV,data = lowbwt.a2df,family=binomial(link = "logit"))
summary(lw_a16)

lw_a17<-glm(LOW~.^2,data = lowbwt.a2df,family=binomial(link = "logit"))
summary(lw_a17)

lw_a18<-step(lw_a17,direction ="both")
summary(lw_a18)


# evaluating models and AUC
lowbwt.alist<-list()
lowbwt.alist[[1]]=lw_a10 # 0.74
lowbwt.alist[[2]]=lw_a11 # 0.79
lowbwt.alist[[3]]=lw_a12 # 0.74
lowbwt.alist[[4]]=lw_a13 # 0.76
lowbwt.alist[[5]]=lw_a14 # 0.76
lowbwt.alist[[6]]=lw_a15 # 0.74
lowbwt.alist[[7]]=lw_a16 # 0.80
lowbwt.alist[[8]]=lw_a17 # 0.72
lowbwt.alist[[9]]=lw_a18 # 0.83

lowbwt.amodnames<-c("main effects","age:ftv","lwt:ftv","Ptl:ui","smoke:race","age:smoke","specific inter","all+.^2","step")

# evaluating models
library(AICcmodavg)
lowbwt.aaictab<-aictab(cand.set=lowbwt.alist,modnames=lowbwt.amodnames)
lowbwt.aaictab


# using ROCR

library("ROCR")
auc.alist=list()


par(mfrow=c(3,3))
for (i in seq(1,length(lowbwt.alist))){
  predict<-fitted(lowbwt.alist[[i]])
  pred<- prediction(predict,lowbwt.a2df$LOW)
  perf<-performance(pred,measure="tpr",x.measure="fpr")
  plot(perf,main="sensitivity vs false positive rate",colorize=TRUE, colorkey.relwidth=0.5,lwd=4.5)
  perf_auc<-performance(pred,measure="auc")
  
  auc.alist[i]=perf_auc
}


# using model : AGE, weight after last cycle, smoking, hhistory of premature labor,hypertension and uterine irritavility

log_mp1<-glm(LOW~AGE+LWT+SMOKE+PTL+HT+UI, data=lowbwt.a2df,family=binomial(link = "logit"))
summary(log_mp1)

