# install packages
# install.packages("visreg")
# install.packages("AICcmodavg")
# install.packages("Amelia")
# install.packages("mlbench")
# install.packages("corrplot")
# install.packages("DataExplorer") 
# install.packages("ROCR")


# read the data

# lowbwt.df<-read.table(file=file.choose(),header=TRUE)
lowbwt.df<-read.table('lowbwt.dat', header=T, row.names=1)
fix(lowbwt.df)
summary(lowbwt.df)
sapply(lowbwt.df,class)
#plot(lowbwt.df$AGE,lowbwt.df$LOW,col=as.factor(lowbwt.df$SMOKE))

#data exploration
summary(lowbwt.df)
pairs(lowbwt.df)
lowbwt.rdf<-lowbwt.df
lowbwt.rdf$RACE<-as.factor(lowbwt.rdf$RACE)
lowbwt.rdf$SMOKE<-as.factor(lowbwt.rdf$SMOKE)
lowbwt.rdf=lowbwt.rdf[,c(-1,-11)]

summary(lowbwt.rdf)
names(lowbwt.rdf)
names(lowbwt.df)
# youngest girl is age 14
# Check for missing data

library(Amelia)
library(mlbench)
missmap(lowbwt.df, col=c("green", "pink"), legend=FALSE)



library(corrplot)
# orrelation matrix plot to given an idea of which variables change together
correlations <- cor(lowbwt.df)

corrplot(correlations, method="shade")

# explanation of data
library(DataExplorer)
plot_str(lowbwt.df)

plot(lowbwt.df$AGE)
summary(lowbwt.df$AGE)


# Using the 1st 4 variables
m0<-glm(LOW~RACE+SMOKE,data=lowbwt.rdf,family=binomial(link="logit"))
summary(m0)

library(AICcmodavg)
lowbwt_1<-glm(LOW~AGE+LWT+as.factor(RACE)+FTV,data=lowbwt.df,family=binomial(link="logit"))
summary(lowbwt_1)

lowbwt_2<-glm(LOW~AGE+LWT+as.factor(RACE)+FTV+FTV*AGE,data=lowbwt.df,family=binomial(link="logit"))
summary(lowbwt_2)

#All terms+ interaction terms
lowbwt_3<-glm(LOW~AGE+LWT+as.factor(RACE)+FTV+FTV*AGE+FTV*as.factor(RACE)+FTV*LWT+AGE*LWT+AGE*as.factor(RACE)+LWT*as.factor(RACE),data=lowbwt.df,family=binomial(link="logit"))
summary(lowbwt_3)

lowbwt.backward <- step(lowbwt_3, direction="backward")
summary(lowbwt.backward)

lowbwt.list<-list()
lowbwt.list[[1]]=lowbwt_1
lowbwt.list[[2]]=lowbwt_2
lowbwt.list[[3]]=lowbwt_3
lowbwt.list[[4]]=lowbwt.backward


lowbwt.modnames<-c("age+lwt+race+ftv","age+lwt+race+ftv+ftv*age","all","backward")

# evaluating models
lowbwt.aictab<-aictab(cand.set=lowbwt.list,modnames=lowbwt.modnames)
lowbwt.aictab

# using 4 predictors, get all interactions and do a backward selection
plot_str(lowbwt.subset)

lowbwt.subset<-cbind(lowbwt.rdf$LOW,lowbwt.rdf$AGE,lowbwt.rdf$RACE,lowbwt.rdf$FTV,lowbwt.rdf$LWT )
colnames(lowbwt.subset)=c("LOW","AGE","RACE","FTV","LWT")
fix(lowbwt.subset)
lowbwt.subset=as.data.frame(lowbwt.subset)
sapply(lowbwt.subset,class)

correlations_sub <- cor(lowbwt.subset)
corrplot(correlations_sub, method="number")

lowbwt.subset$RACE=as.factor(lowbwt.subset$RACE)

fit.subset <- glm( LOW ~ .^2, data=lowbwt.subset,family=binomial(link="logit"))
summary(fit.subset)

lowbwt.fit.subset.backward <- step(fit.subset, direction="backward")
summary(lowbwt.fit.subset.backward)

lowbwt.fit.subset.stepwise <- step(fit.subset, direction="both")
summary(lowbwt.fit.subset.stepwise)

library("ROCR")
predict<-fitted(lowbwt.fit.subset.backward)
pred<- prediction(predict,lowbwt.subset$LOW)
perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf,main="sensitivity vs false positive rate",colorize=TRUE, colorkey.relwidth=0.5,lwd=4.5)
perf_auc<-performance(pred,measure="auc")
perf_auc


# making a full model

lowbwt_f<-glm(LOW~.,data=lowbwt.rdf,family=binomial(link="logit"))
summary(lowbwt_f)

# all possible 2 way interactions for the full model


fit <- glm( LOW ~ .^2, data=lowbwt.rdf,family=binomial(link="logit"))
summary(fit)

# use backward selection
lowbwt.fit.backward <- step(fit, direction="backward")
summary(lowbwt.fit.backward)

#use stepwise selection
lowbwt.fit.stepwise <- step(fit, direction="both")
summary(lowbwt.fit.stepwise)


library("ROCR")
predictb<-fitted(lowbwt.fit.backward)
predb<- prediction(predictb,lowbwt.subset$LOW)
perfb<-performance(predb,measure="tpr",x.measure="fpr")
plot(perf,main="sensitivity vs false positive rate",colorize=TRUE, colorkey.relwidth=0.5,lwd=4.5)
perf_aucb<-performance(predb,measure="auc")
perf_aucb

plot(perf,colorize=TRUE)
plot(perfb,add=TRUE,colorize=TRUE)
abline(0,1,lty=2)


cor(lowbwt.df,method="spearman")
dim(lowbwt.df)




