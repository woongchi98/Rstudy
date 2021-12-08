# Chapter 7 Lab: Non-linear Modeling

library(ISLR)
attach(Wage)
t(head(Wage)) #Wage Data Set확인

# Polynomial Regression and Step Functions
fit=lm(wage~poly(age,4),data=Wage) #age에 대해 4차식 적합
coef(summary(fit)) 

fit2=lm(wage~poly(age,4,raw=T),data=Wage) # 직교 다항식 적용 X
coef(summary(fit2))

fit2a=lm(wage~age+I(age^2)+I(age^3)+I(age^4),data=Wage) #바로 위에랑 같음
coef(fit2a)

fit2b=lm(wage~cbind(age,age^2,age^3,age^4),data=Wage) #이것도 
coef(fit2b)

agelims=range(age) #18 80
age.grid=seq(from=agelims[1],to=agelims[2]) #grid
preds=predict(fit,newdata=list(age=age.grid),se=TRUE) #4차 직교다항식으로 예측
se.bands=cbind(preds$fit+2*preds$se.fit,preds$fit-2*preds$se.fit) #예측값 +-2*se


par(mfrow=c(1,1),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey") #나이와 봉급 산점도
title("Degree-4 Polynomial Orthogonal",outer=T) 
lines(age.grid,preds$fit,lwd=2,col="blue") #다항식 예측 그래프
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3) #신뢰범위


fit2=lm(wage~poly(age,4,raw=T),data=Wage) # 직교 다항식 적용 X
preds2=predict(fit2,newdata=list(age=age.grid),se=TRUE) #직교다항식 적용x 적합
max(abs(preds$fit-preds2$fit)) #두 값의 차이의 절댓값의 최댓값 : 매우 작음

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey") #나이와 봉급 산점도
title("Degree-4 Polynomial",outer=T) 
lines(age.grid,preds2$fit,lwd=2,col="blue") #다항식 예측 그래프
se.bands=cbind(preds2$fit+2*preds2$se.fit,preds2$fit-2*preds2$se.fit) #예측값 +-2*se
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3) #신뢰범위


fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage) #1차~5차까지 적합

anova(fit.1,fit.2,fit.3,fit.4,fit.5) #5차식은 잘 적합 x
      
coef(summary(fit.5))

#이걸로 해도 똑같음음
(-11.983)^2 # 143.5923

fit.1=lm(wage~education+age,data=Wage) #직교다항식 아니여도 쓸 수 있음
fit.2=lm(wage~education+poly(age,2),data=Wage)
fit.3=lm(wage~education+poly(age,3),data=Wage)
anova(fit.1,fit.2,fit.3) #ㅇㅋ

fit=glm(I(wage>250)~poly(age,4),data=Wage,family=binomial) #로지스틱 다항식으로 적용
preds=predict(fit,newdata=list(age=age.grid),se=T) #예측

pfit=exp(preds$fit)/(1+exp(preds$fit))# 신뢰구간 계산을 위해 로짓변환
se.bands.logit = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))
preds=predict(fit,newdata=list(age=age.grid),type="response",se=T)

plot(age,I(wage>250),xlim=agelims,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),cex=.5,pch="|",col="darkgrey")
lines(age.grid,pfit,lwd=2, col="blue")  #line
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)#---

table(cut(age,4))
fit=lm(wage~cut(age,4),data=Wage)
coef(summary(fit))

# Splines
par(mfrow=c(1,1),mar=c(4.5,4.5,1,1),oma=c(0,0,4,0))

library(splines)
fit=lm(wage~bs(age,knots=c(25,40,60)),data=Wage)
(summary(fit))
pred=predict(fit,newdata=list(age=age.grid),se=T)
plot(age,wage,col="gray")
lines(age.grid,pred$fit,lwd=2)
lines(age.grid,pred$fit+2*pred$se,lty="dashed")
lines(age.grid,pred$fit-2*pred$se,lty="dashed")

dim(bs(age,knots=c(25,40,60)))
dim(bs(age,df=6))
attr(bs(age,df=6),"knots")

fit2=lm(wage~ns(age,df=4),data=Wage)
pred2=predict(fit2,newdata=list(age=age.grid),se=T)
lines(age.grid, pred2$fit,col="red",lwd=2)
summary(fit2)

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit=smooth.spline(age,wage,df=16)
fit2=smooth.spline(age,wage,cv=TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),
       lty=1,lwd=2,cex=.8)

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit=loess(wage~age,span=.2,data=Wage)
fit2=loess(wage~age,span=.5,data=Wage)
lines(age.grid,predict(fit,data.frame(age=age.grid)),col="red",lwd=2)
lines(age.grid,predict(fit2,data.frame(age=age.grid)),col="blue",lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)

# GAMs
#library(mgcv)

library(gam)
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue")
plot.Gam(gam1, se=TRUE, col="red")

gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)
anova(gam.m1,gam.m2,gam.m3,test="F")
summary(gam.m3)

preds=predict(gam.m2,newdata=Wage)
gam.lo=gam(wage~s(year,df=4)+lo(age,span=0.7)+education,data=Wage)

plot.Gam(gam.lo, se=TRUE, col="green")

gam.lo.i=gam(wage~lo(year,age,span=0.5)+education,data=Wage)

library(akima)
plot(gam.lo.i)
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green")

table(education,I(wage>250))

gam.lr.s=gam(I(wage>250)~year+s(age,df=5)+education,
             family=binomial,data=Wage,
              subset=(education!="1. < HS Grad"))
plot(gam.lr.s,se=T,col="green")

