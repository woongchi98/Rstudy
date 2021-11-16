library(glmnet)

data <- iris

x = model.matrix(Species~.,data)
y = data$Species

grid = 10^seq(10,-2,length=100)

ridge.mod = glmnet(x,y,alpha=0, lambda=grid, family="multinomial")
coef(ridge.mod)

train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
test
y.test = y[test]

cv.out=cv.glmnet(x[train,],y[train],alpha=0,family="multinomial")
plot(cv.out)

bestlam=cv.out$lambda.min  
bestlam #최적의 lambda값
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])

mean((ridge.pred-y.test)^2) 
out=glmnet(x,y,alpha=0, family="multinomial")
predict(out,type="coefficients",s=bestlam)
#############################라쏘
data <- iris

x = model.matrix(Species~.,data)
y = data$Species
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)

grid = 10^seq(10,-2,length=100)

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid, family="multinomial")

cv.out=cv.glmnet(x[train,],y[train],alpha=1, family="multinomial")
plot(cv.out)

bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])

out=glmnet(x,y,alpha=1,lambda=grid, family="multinomial")
lasso.coef=predict(out,type="coefficients",s=bestlam)
lasso.coef

lasso.coef[lasso.coef!=0] #라쏘로 11개만 살아남은 상태

bestlam1=cv.out$lambda.1se
lasso.pred1=predict(lasso.mod,s=bestlam1,newx=x[test,])

lasso.coef1=predict(out,type="coefficients",s=bestlam1)
lasso.coef1 #cof값을 확인해보니 4개가 살아남음(단순한 모델을 원할 땐 위와 같이 사용하면됨)







###########################3
grid = 10^seq(10,-2,length=100)
ridge.mod = glmnet(x,y,alpha=0, lambda=grid) 
glmnet:::glmnet

np = dim(x)
nobs = as.integer(np[1])
nvars = as.integer(np[2])
weights = rep(1, nobs)

