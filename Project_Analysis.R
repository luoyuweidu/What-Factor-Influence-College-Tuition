#Read College Data
CollegeData=read.csv("College.csv")
names(CollegeData)

#Fix College names as row names
rownames (CollegeData)=CollegeData[,1]
fix(CollegeData)
CollegeData =CollegeData[,-1]
attach(CollegeData)

#Data Summmary
str(CollegeData)
summary(CollegeData)

#Pairwise scatterplots
library(psych)
pairs.panels(CollegeData)

#VIF Calculation
library(usdm)
fit=lm(Outstate ~ ., data=CollegeData)
X=model.matrix(Outstate ~ ., data=CollegeData)[,-1]
vifstep(X, th=10)

#varaiables after removing correlation
Y = model.matrix(Outstate ~ Private + Apps + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate, data = CollegeData)[,-1]

#Using Cp criteria method
library(leaps)
result1=leaps(Y, Outstate, int=TRUE, method=c("Cp"), nbest=16)
which.min(result1$Cp)
result1$Cp[144]
result1$which[144,]
fit.cp = lm(Outstate ~ Private + Top10perc + Room.Board + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate, data = CollegeData)
summary(fit.cp)

#Using adjr2 criteria method
result2=leaps(Y, Outstate, int=TRUE, method=c("adjr2"), nbest=16)
which.max(result2$adjr2)
result2$adjr2[192]
result2$which[192,]
fit.adjr = lm(Outstate ~ Private + Apps + Top10perc + F.Undergrad + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate, data=CollegeData)
summary(fit.adjr)

#Using AIC Criteria method
model1=lm(Outstate~.,data=as.data.frame(Y))
fit.AIC = step(model1,direction="both",k=2)
summary(fit.AIC)

#Using BIC Criteria method
fit.BIC = step(model1,direction="both",k=log(777))
summary(fit.BIC)

#Fnding Interaction terms
with(subset(CollegeData,Private=="Yes"),plot(Outstate~Top10perc, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~Top10perc,col="blue"))
fit1 = lm(Outstate~Top10perc, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~Top10perc, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

with(subset(CollegeData,Private=="Yes"),plot(Outstate~ Room.Board, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~ Room.Board,col="blue"))
fit1 = lm(Outstate~ Room.Board, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~ Room.Board, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

with(subset(CollegeData,Private=="Yes"),plot(Outstate~ Books, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~ Books,col="blue"))
fit1 = lm(Outstate~ Books, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~ Books, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

with(subset(CollegeData,Private=="Yes"),plot(Outstate~ Personal, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~ Personal,col="blue"))
fit1 = lm(Outstate~ Personal, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~ Personal, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

with(subset(CollegeData,Private=="Yes"),plot(Outstate~ PhD, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~ PhD,col="blue"))
fit1 = lm(Outstate~ PhD, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~ PhD, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

with(subset(CollegeData,Private=="Yes"),plot(Outstate~ Terminal, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~ Terminal,col="blue"))
fit1 = lm(Outstate~ Terminal, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~ Terminal, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

with(subset(CollegeData,Private=="Yes"),plot(Outstate~ S.F.Ratio, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~ S.F.Ratio,col="blue"))
fit1 = lm(Outstate~ S.F.Ratio, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~ S.F.Ratio, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

with(subset(CollegeData,Private=="Yes"),plot(Outstate~ perc.alumni, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~ perc.alumni,col="blue"))
fit1 = lm(Outstate~ perc.alumni, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~ perc.alumni, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

with(subset(CollegeData,Private=="Yes"),plot(Outstate~ Expend, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~ Expend,col="blue"))
fit1 = lm(Outstate~ Expend, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~ Expend, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

with(subset(CollegeData,Private=="Yes"),plot(Outstate~ Grad.Rate, col="red"))
with(subset(CollegeData,Private=="No"),points(Outstate~ Grad.Rate,col="blue"))
fit1 = lm(Outstate~ Grad.Rate, subset=Private=="Yes")#model for females
fit2 = lm(Outstate~ Grad.Rate, subset=Private=="No") #model for males
abline(fit1,lty=1) #lty=1 uses solid line
abline(fit2,lty=2) #lty=2 uses dashed line

############ Add Interaction terms in Models ############################
#Cp Model
fit.cp1 = lm(Outstate ~ Private + Top10perc + Room.Board + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate + Personal*Private + Terminal*Private + S.F.Ratio*Private + Grad.Rate*Private + perc.alumni*Private, data = CollegeData)
summary(fit.cp1)

#Adjr2 Model
fit.adjr1 = lm(Outstate ~ Private + Apps + Top10perc + F.Undergrad + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate + Personal*Private + Terminal*Private + S.F.Ratio*Private + Grad.Rate*Private + perc.alumni*Private, data = CollegeData)
summary(fit.adjr1)

#AIC Model
fit.AIC1 = lm(Outstate ~ Private+Apps+Top10perc+F.Undergrad+Room.Board+Personal+PhD+Terminal+S.F.Ratio+perc.alumni+Expend+Grad.Rate+Personal*Private + Terminal*Private + S.F.Ratio*Private + Grad.Rate*Private + perc.alumni*Private, data = CollegeData)
summary(fit.AIC1)

#BIC Model
fit.BIC1 = lm(Outstate ~ Private + Room.Board + Personal + Terminal + perc.alumni + Expend + Grad.Rate + Personal*Private + Terminal*Private  + Grad.Rate*Private + perc.alumni*Private, data = CollegeData)
summary(fit.BIC1)

############ Partial F-test for models ##################################
#Cp model
anova(fit.cp1)
F_stat = ((1799662 + 33579 + 13934410)/3)/(2980928212/761)
pf(F_stat,df1=3, df2=761, lower.tail = FALSE)

#Adjr model
anova(fit.adjr1)
F_stat2 = ((1659743 + 212417 + 13037033)/3)/(2957955810/758)
pf(F_stat2,df1=3, df2=758, lower.tail = FALSE)

#AIC model
anova(fit.AIC1)
F_stat3 = ((1815604 + 163004 + 13808533)/3)/(2959145777/759)
pf(F_stat3,df1=3, df2=759, lower.tail = FALSE)

#AIC model
anova(fit.BIC1)
F_stat4 = ((280429+13877826)/2)/(3035429922/765)
pf(F_stat4,df1=2, df2=765, lower.tail = FALSE)

##################### Remove some Interaction terms
fit.cp2 = lm(Outstate ~ Private + Top10perc + Room.Board + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate + Personal*Private + Terminal*Private, data = CollegeData)
summary(fit.cp2)

fit.adjr2 = lm(Outstate ~ Private + Apps + Top10perc + F.Undergrad + Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate + Personal*Private + Terminal*Private , data = CollegeData)
summary(fit.adjr2)

#AIC Model
fit.AIC2 = lm(Outstate ~ Private + Apps + Top10perc + F.Undergrad + Room.Board + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate + Personal*Private + Terminal*Private, data = CollegeData)
summary(fit.AIC2)

#BIC Model
fit.BIC2 = lm(Outstate ~ Private + Room.Board + Personal + Terminal + perc.alumni + Expend + Grad.Rate + Personal*Private + Terminal*Private, data = CollegeData)
summary(fit.BIC2)

###################### Cross validation:
#PRESS
library(asbio)
press(fit.cp2)
#3124506861

press(fit.adjr2)
#3171141666

press(fit.AIC2)
#3165547476

press(fit.BIC2)
#3153020388

#k-fold cross vaidation
library(DAAG) 
out1 <- CVlm(CollegeData, form.lm = fit.cp2, m=10)
#4010605 

out2 <- CVlm(CollegeData, form.lm = fit.adjr2, m=10)
#4088727

out3 <- CVlm(CollegeData, form.lm = fit.AIC2, m=10)
#4080693

out4 <- CVlm(CollegeData, form.lm = fit.BIC2, m=10)
#4061999

################## Selected Cp as Optimal model based on prediction error #####################
#residual plot: e versus y.hat
par(mfrow=c(2,2))
plot(fit.cp2)

par(mfrow=c(1,1))
plot(fit.cp2, which=c(4))
CollegeData.new = CollegeData[-(which(cooks.distance(fit.cp2) >= 0.04)),]

#Refit model
fit.cp3 = lm(Outstate ~ Private + Top10perc + Room.Board + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate + Personal*Private + Terminal*Private, data = CollegeData.new)
summary(fit.cp3)

#Cross validation:
library(asbio)
press(fit.cp3)
2905986226

#k-fold cross vaidation
library(DAAG) 
out1 <- CVlm(CollegeData, form.lm = fit.cp3, m=10)
4010605 
