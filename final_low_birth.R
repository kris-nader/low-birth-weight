##Low Birth Weight Logistic Regression Analysis
# Aditya Badola   r0768685      Danie Daaboul       r0775655
# Matthew Watson  r0766802      Kristen Nader       r0771801
# Prabhat Juyal   r0772775      Magdalena Zielonka  r0769926
# Michael Neilson r0770285      James O'Reilly      r0773125
##


## Load the required packages 
list.of.packages <- c("ggplot2", "tidyverse","AUCcmodavg","MASS","reshape2","grid","PerformanceAnalytics")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) 
library(ggplot2)
library(tidyverse)
library(AUCcmodavg)
library(MASS)
library(reshape2)
library(grid)
library(PerformanceAnalytics)


## Load dataset
lowbwt=read.table(file=file.choose(),header=T,row.names = 1)
# Set Race as factor
lowbwt$RACE=as.factor(lowbwt$RACE)

## Exploratory Analysis
#check the number of women in the study
nrow(lowbwt)

#check variables 
summary(lowbwt)

# Check numbers of LOW using a table
xtabs(~LOW, data = lowbwt)

# Correlation between continuous variables and BWT
chart.Correlation(subset(lowbwt,select=c("BWT","AGE","LWT","FTV", "PTL")),
                  histogram=TRUE, pch=19, method = "spearman",exact = FALSE)

p_age <- cor.test(lowbwt$BWT, lowbwt$AGE, method="spearman",exact = FALSE)$p.value
p_lwt <- cor.test(lowbwt$BWT, lowbwt$LWT, method="spearman",exact = FALSE)$p.value
p_ftv <- cor.test(lowbwt$BWT, lowbwt$FTV, method="spearman",exact = FALSE)$p.value
p_ptl <- cor.test(lowbwt$BWT, lowbwt$PTL, method="spearman",exact = FALSE)$p.value

rho_age <- cor.test(lowbwt$BWT, lowbwt$AGE, method="spearman",exact = FALSE)$estimate
rho_lwt <- cor.test(lowbwt$BWT, lowbwt$LWT, method="spearman",exact = FALSE)$estimate
rho_ftv <- cor.test(lowbwt$BWT, lowbwt$FTV, method="spearman",exact = FALSE)$estimate
rho_ptl <- cor.test(lowbwt$BWT, lowbwt$PTL, method="spearman",exact = FALSE)$estimate

correlations.df <- data.frame(c(rho_age, rho_lwt, rho_ftv, rho_ptl),
                              c(p_age, p_lwt, p_ftv, p_ptl), 
                              row.names=c("AGE", "LWT", "FTV", "PTL"))

colnames(correlations.df) <- c("rho", "p")
round(correlations.df,3)

##Plots

fill <- "#4271AE"
lines <- "#1F3552"

plots.list=list()

#subset of LOW dataset with categorical variables and BWT
data_cat=subset(lowbwt, select=c( "UI", "HT", "RACE","SMOKE", "PTL", "FTV","BWT"))

#loop over the set of variables 
for( i in 1:6)
{
  varName=colnames(data_cat)[i]
  
  
  temp=data_cat[,c(i,7)]
  temp[,1]=as.factor(temp[,1])
  plotVar = ggplot(temp, aes_string(x = varName, y = "BWT")) +
    geom_boxplot(colour = lines, fill = fill, size = 1) +
    scale_y_continuous(name = "Birth Weight")+
    scale_x_discrete(name = varName) +
    ggtitle(paste("Distribution of Birth Weight by  ", varName," ")) +
    theme_bw() +
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(size = 12, family = "Tahoma", face = "bold", hjust=0.5),
          text=element_text(family = "Tahoma"),
          axis.text.x = element_text(colour="black", size = 11),
          axis.text.y = element_text(colour="black", size = 9),
          axis.line = element_line(size=0.5, colour = "black"))
  
  plots.list[[i]]=plotVar+geom_jitter()
  
  #Save boxplots as png 
ggsave(paste0(varName, ".png"), plot = plots.list[[i]])
  
  
}

plots.list2=list()

#dataset with continuous variables and LOW
data_cont=subset(lowbwt, select=c( "AGE", "LWT","LOW"))

for( i in 1:2)
{
  varName=colnames(data_cont)[i]
  temp=data_cont[,c(i,3)]
  temp[,2]=as.factor(temp[,2])
  plotVar = ggplot(temp, aes_string(x = "LOW", y = varName)) +
    geom_boxplot(colour = lines, fill = fill,
                 size = 1) +
    scale_y_continuous(name = varName)+
    scale_x_discrete(name = "LOW") +
    ggtitle(paste("Distribution of ", varName," by LOW",  " ")) +
    theme_bw() +
    theme(panel.grid.major = element_line(colour = "#d3d3d3"),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(size = 12, family = "Tahoma", face = "bold", hjust=0.5),
          text=element_text(family = "Tahoma"),
          axis.text.x = element_text(colour="black", size = 11),
          axis.text.y = element_text(colour="black", size = 9),
          axis.line = element_line(size=0.5, colour = "black"))
  
  plots.list2[[i]]=plotVar+geom_jitter()
  
ggsave(paste0(varName, ".png"), plot = plots.list2[[i]])
  
}



#interaction PTL and UI
plotVar = ggplot(lowbwt, aes_string(x = "PTL", y = "BWT")) +
  geom_boxplot(colour = lines, fill = fill,
               size = 1) +
  scale_y_continuous(name = "BWT")+
  scale_x_discrete(name = "PTL") +
  ggtitle("Distribution of BWT by PTL based on UI") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Tahoma", face = "bold", hjust=0.5),
        text=element_text(family = "Tahoma"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black")) +facet_wrap(~UI)+geom_jitter()
ggsave("PtlUi.png", plot = plotVar)



#interaction between AGE and FTV  
plotVar = ggplot(lowbwt, aes_string(x = "LOW", y = "AGE")) +
  geom_boxplot(colour = lines, fill = fill,
               size = 1) +
  scale_y_continuous(name = "AGE")+
  scale_x_discrete(name = "LOW") +
  ggtitle("Distribution of AGE by LOW foreach FTV Count") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Tahoma", face = "bold",  hjust=0.5),
        text=element_text(family = "Tahoma"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black")) +facet_wrap(~FTV)+geom_jitter()
ggsave("AgeFtv.png", plot = plotVar) 


#interaction LWT and FTV
plotVar = ggplot(lowbwt, aes_string(x = "LOW", y = "LWT")) +
  geom_boxplot(colour = lines, fill = fill,
               size = 1) +
  scale_y_continuous(name = "LWT")+
  scale_x_discrete(name = "LOW") +
  ggtitle("Distribution of LWT by LOW foreach FTV Count") +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Tahoma", face = "bold", hjust=0.5),
        text=element_text(family = "Tahoma"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black")) +facet_wrap(~FTV)+geom_jitter()
ggsave("LwtFtv.png", plot = plotVar)

#interaction between Age and Smoke
plotVar=ggplot(lowbwt, aes(AGE,BWT, color=as.factor(SMOKE))) + 
  geom_point() + 
  geom_smooth(se=F, size=1.7) +
  ggtitle("Distribution for BWT by Age \n for Smokers and Non-Smokers" ) +
  theme_bw() +
  labs(color='SMOKE') +
  scale_color_manual(values=c("#74a9cf", "#045a8d"))+
  theme(panel.grid.major = element_line(colour = "#d3d3d3"),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 12, family = "Tahoma", face = "bold", hjust=0.5),
        text=element_text(family = "Tahoma"),
        axis.title = element_text(face="bold"),
        legend.title = element_text(face="bold"),
        axis.text.x = element_text(colour="black", size = 11),
        axis.text.y = element_text(colour="black", size = 9),
        axis.line = element_line(size=0.5, colour = "black")) 
ggsave("AgevsSmoke.png", plot = plotVar)

#interaction plot between PTL and UI
interaction.plot(lowbwt$UI, lowbwt$PTL, lowbwt$BWT, 
                 col = c( "#bdc9e1", "#74a9cf", "#2b8cbe", "#045a8d"), lty = 1, lwd = 3,
                 main = "Distribution For BWT By UI Status \n For Each PTL Count",
                 xlab = "UI", ylab ="Mean BWT", trace.label = "PTL",
                 cex.lab = 1.2, 
                 cex.main = 1, 
                 font.main=2,
                 fixed=T, 
                 xpd = FALSE)


#interaction plot between RACE and SMOKE 
interaction.plot(lowbwt$RACE, lowbwt$SMOKE, lowbwt$BWT, 
                 col = c("#74a9cf", "#045a8d"), lty = 1, lwd = 3,
                 main = "Distribution For BWT By Age \n For Smokers and Non-Smokers",
                 xlab = "RACE", ylab = "Mean BWT", trace.label = "SMOKE",
                 cex.lab = 1.2, cex.main=1,
                 font.main=2,
                 fixed=T, 
                 xpd=FALSE)

## Analysis 1
## CREATING THE MODEL
## BACKWARD ELIMINATION 4 VARIABLES-- remove iteratively the highest P value

#model 1 4 variables
half_mod=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV+LWT:FTV, data=lowbwt, family=binomial(link="logit"))
summary(half_mod)

# FTV:LWT highest p-value
#model 2 without FTV:LWT
half_mod2=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV, data=lowbwt, family=binomial(link="logit"))
summary(half_mod2)

# RACE highest p-value
#model 3 without RACE
half_mod3=glm(LOW~AGE+FTV+LWT+AGE:FTV, data=lowbwt, family=binomial(link="logit"))
summary(half_mod3)

## Analysis 2
## BACKWARD ELIMINATION ALL VARIABLES
#model 1 with all variables and interactions
full.model=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+SMOKE:RACE+AGE:SMOKE+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(full.model)

# SMOKE:RACE highest p-value
#model 2 without SMOKE:RACE
model2=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+AGE:SMOKE+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(model2)

# SMOKE:AGE highest p-value
#model 3 without SMOKE:AGE
model3=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(model3)

# FTV:LWT highest p-value
#model 4 without FTV:LWT
model4=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(model4)

# RACE3 highest p-value
#model 5 without RACE
model5=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+AGE:FTV+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(model5)
#We can leave RACE because AIC did not improve much and one category was significant

## Analysis 1
## AIC WITH 4 VARIABLES

half_mod=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV+LWT:FTV, data=lowbwt, family=binomial(link="logit"))

#using stepwise selection using AIC
step.model <- step(half_mod, direction = "both")

low.list=list()

#Model of main predictors only -- full
low.list[[1]]=glm(LOW~AGE+RACE+FTV+LWT, data=lowbwt, family=binomial(link="logit"))

#full model of main effects and interactions -- full_interaction
low.list[[2]]=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV+LWT:FTV, data=lowbwt, family=binomial(link="logit"))

#Main effects + AGE:FTV interaction -- full_AgeFtv
low.list[[3]]=glm(LOW~AGE+RACE+FTV+LWT+AGE:FTV, data=lowbwt, family=binomial(link="logit"))

#Main effects + LWT:FTV interaction --full_LwtFtv
low.list[[4]]=glm(LOW~AGE+RACE+FTV+LWT+LWT:FTV, data=lowbwt, family=binomial(link="logit"))

#Main effects without Race -- noRace
low.list[[5]]=glm(LOW~AGE*FTV+LWT, data=lowbwt, family=binomial(link="logit"))

#Main effects without LWT -- noLWT
low.list[[6]]=glm(LOW~AGE*FTV+RACE, data=lowbwt, family=binomial(link="logit"))

low.modnames=c("full", "full_interaction", "full_AgeFtv", "full_LwtFtv", "noRACE", "noLWT")

low.aictab=aictab(cand.set = low.list,modnames = low.modnames)
low.aictab

## Analysis 2
## AIC WITH ALL VARIABLES}

full.model=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+SMOKE:RACE+AGE:SMOKE+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(full.model)

#using stepwise selection using AIC
step.model <- step(full.model, direction = "both")


low.list.full=list()

#full
low.list.full[[1]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE, data=lowbwt, family=binomial(link="logit"))

#full_interaction
low.list.full[[2]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+SMOKE:RACE+AGE:SMOKE+PTL:UI+HT:LWT, data=lowbwt, family=binomial(link="logit"))

#full_AgeFtv
low.list.full[[3]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV, data=lowbwt, family=binomial(link="logit"))

#full_FtvLwt
low.list.full[[4]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT, data=lowbwt, family=binomial(link="logit"))

#full_SmokeRace
low.list.full[[5]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+SMOKE:RACE, data=lowbwt, family=binomial(link="logit"))

#full_AgeSmoke
low.list.full[[6]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+AGE:SMOKE, data=lowbwt, family=binomial(link="logit"))

#full_PtlUi
low.list.full[[7]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+PTL:UI, data=lowbwt, family=binomial(link="logit"))

#full_HtLwt
low.list.full[[8]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+HT:LWT, data=lowbwt, family=binomial(link="logit"))

#noAGEFTV 
low.list.full[[9]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+PTL:UI, data=lowbwt, family=binomial(link="logit"))

#noRACE
low.list.full[[10]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+SMOKE+PTL:UI+AGE:FTV, data=lowbwt, family=binomial(link="logit"))

#noSmokeRACE
low.list.full[[11]]=glm(LOW~AGE+FTV+PTL+LWT+UI+HT+PTL:UI+AGE:FTV, data=lowbwt, family=binomial(link="logit"))

#noAGE
low.list.full[[12]]=glm(LOW~FTV+PTL+LWT+UI+HT+SMOKE+RACE+PTL:UI, data=lowbwt, family=binomial(link="logit"))

#noUiHt
low.list.full[[13]]=glm(LOW~AGE+FTV+LWT+SMOKE+RACE+PTL+AGE:FTV, data=lowbwt, family=binomial(link="logit"))

low.modnames.full=c("full", "full_interaction", "full_AgeFtv", "full_FtvLwt", "full_SmokeRace", "full_AgeSmoke", "full_PtlUi", "full_HtLwt", "noAGEFTV", "noRACE", "noSmokeRACE", "noAGE", "noUiHt")

low.aictab.full=aictab(cand.set = low.list.full,modnames = low.modnames.full)
low.aictab.full


## Testing  interactions
## interaction model

#Test all interactions for the 4 variables model
allInt_mod=glm(LOW~(AGE+RACE+FTV+LWT)^2, data=lowbwt, family=binomial(link="logit"))
summary(allInt_mod)

## Merging groups 2 and 3 in RACE

#Replace 3 with 2 in RACE
lowbwt$RACE[which(lowbwt$RACE=="3")]="2"

#To drop level 3
lowbwt=droplevels(lowbwt)

#Model with all variables
full=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+SMOKE:RACE+AGE:SMOKE+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(full)

#without SMOKE:RACE
noSmokeRace=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+AGE:SMOKE+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(noSmokeRace)

#without AGE:SMOKE
noAgeSmoke=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+FTV:LWT+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(noAgeSmoke)

#without FTV:LWT
noFtvLwt=glm(LOW ~ AGE+FTV+PTL+LWT+UI+HT+SMOKE+RACE+AGE:FTV+PTL:UI, data=lowbwt, family=binomial(link="logit"))
summary(noFtvLwt)

####### FINAL MODELS ########

# four variables 


install.packages("ROCR")
library(ROCR)
par(mfrow=c(1,2))
final_model_a1=glm(LOW~AGE+FTV+LWT+AGE*FTV,data=lowbwt,family=binomial(link="logit"))
summary(final_model_a1)
exp(final_model_a1$coefficients)

predict<-fitted(final_model_a1)
pred<- prediction(predict,lowbwt$LOW)
perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf,main="sensitivity vs false positive rate",colorize=TRUE, colorkey.relwidth=0.5,lwd=4.5)
perf_auc_a1<-performance(pred,measure="auc")
abline(0,1,lty=2)
perf_auc_a1 #0.67

# using all effects 

final_model_a2=glm(LOW~AGE+FTV+LWT+AGE*FTV+PTL+UI+HT+PTL*UI,data=lowbwt,family=binomial(link="logit"))
summary(final_model_a2)
exp(final_model_a2$coefficients)


predict<-fitted(final_model_a2)
pred<- prediction(predict,lowbwt$LOW)
perf<-performance(pred,measure="tpr",x.measure="fpr")
plot(perf,main="sensitivity vs false positive rate",colorize=TRUE, colorkey.relwidth=0.5,lwd=4.5)
perf_auc_a2<-performance(pred,measure="auc")
abline(0,1,lty=2) 
perf_auc_a2 #0.78



# anova on the final models
anova(final_model_a1,final_model_a2,test="Chisq")


