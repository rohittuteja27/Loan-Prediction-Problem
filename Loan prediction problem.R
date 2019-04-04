library(gains)
library(dplyr)
library(irr)
library(caret)
library(car)
library(ROCR)

trn=read.csv(file.choose())
test=read.csv(file.choose())

trn%>%mutate(Target=ifelse(trn$Loan_Status=="Y",1,0))->trn
trn%>%select(-c(Loan_ID,Loan_Status))->trn
test%>%select(Loan_ID)->test

summary(trn)
class(trn$Target)
table(trn$Target)
trn$Target=as.factor(trn$Target)
summary(trn)
summary(test)
length(test$Loan_ID)

ifelse(trn$Gender=='',NA,as.character(trn$Gender))->trn$Gender
trn$Gender=as.factor(trn$Gender)
summary(trn$Gender)

ifelse(trn$Married=='',NA,as.character(trn$Married))->trn$Married
trn$Married=as.factor(trn$Married)
summary(trn$Married)

class(trn$Dependents)
summary(trn$Dependents)
ifelse(trn$Dependents=='',NA,as.character(trn$Dependents))->trn$Dependents
trn$Dependents=as.factor(trn$Dependents)
summary(trn$Dependents)

class(trn$Education)

summary(trn$Self_Employed)
ifelse(trn$Self_Employed=='',NA,as.character(trn$Self_Employed))->trn$Self_Employed
trn$Self_Employed=as.factor(trn$Self_Employed)
summary(trn$Self_Employed)

summary(trn$LoanAmount)

summary(trn$Loan_Amount_Term)

summary(trn$Credit_History)

class(trn$Property_Area)
trn=na.omit(trn)
summary(trn)

summary(test)
ifelse(test$Gender=='',NA,as.character(test$Gender))->test$Gender
test$Gender=as.factor(test$Gender)
ifelse(test$Married=='',NA,as.character(test$Married))->test$Married
test$Married=as.factor(test$Married)
ifelse(test$Dependents=='',NA,as.character(test$Dependents))->test$Dependents
test$Dependents=as.factor(test$Dependents)
ifelse(test$Self_Employed=='',NA,as.character(test$Self_Employed))->test$Self_Employed
test$Self_Employed=as.factor(test$Self_Employed)
test=na.omit(test)
summary(test)


mod<-glm(formula=Target~.,data=trn,family='binomial')
summary(mod)
step(mod,direction = 'both')

mod1=glm(formula = Target ~ Married + LoanAmount + Credit_History + 
      Property_Area, family = "binomial", data = trn)
summary(mod1)

trn$Property_AreaSemiurban_d=ifelse(trn$Property_Area=='Semiurban',1,0)
test$Property_AreaSemiurban_d=ifelse(test$Property_Area=='Semiurban',1,0)

mod2=glm(formula=Target~Married+LoanAmount+
           Credit_History+Property_AreaSemiurban_d,data=trn,family='binomial')
summary(mod2)


pred<-predict(mod2,type="response",newdata=test)


pred<-ifelse(pred>=0.399,1,0)
pred=as.factor(pred)
View(pred)
