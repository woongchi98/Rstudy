library('mlbench')
library('glmnet')

data(Sonar)

X = as.matrix(Sonar[,1:12])
y = as.numeric(as.factor(Sonar[,61]))-1

data = data.frame(y,X)
Sonar$Class = y

n = nrow(X)
m = ncol(X)

beta = rep(0.001, ncol(X))
iter = 1

logit = glm(y~0+., data=data, family='binomial')
summary(logit)


start = 0
end = 0.01
by = 0.0005
tuning = seq(start, end, by)
len = length(c(tuning))

bic_1 = vector()
tuning_1 = vector()
loglike_1 = vector()

beta_1 = matrix(nrow=m, ncol=len)
SE_beta_1 = matrix(nrow=m, ncol=len)

for(i in 1:len){
  tun = tuning[i]
  iter = 1
  beta = logit$coef
  eps = 10^(-5)
  eta = 0.01
  
  repeat{
    a = X %*% beta
    m = 1 / (1+exp(-a))
    WL = diag( c(tun/abs(beta+10^(-6))) )
    
    d_b = t(X) %*% c(y-m)
    d_p = d_b - n * WL %*% beta
    
    W = diag(c((-m)/(1+exp(a))))
    dd_b = t(X) %*% W %*% X
    dd_p = dd_b - n * WL
    
    beta_a = c(beta - eta*solve(dd_p)%*%d_p)
    
    if(max(abs(beta_a - beta)) < 10^(-6) | iter>10000) (break)
    
    iter = iter+1
    beta = beta_a
  }
  
  iter
  beta = round(beta,5)
  beta
  
  loglike = sum(y*a-log(1+exp(a)))
  
  cov = solve(dd_p) %*% dd_b %*% solve(dd_p)
  SE_beta = sqrt(diag(-cov))
  
  df = sum(diag(solve(dd_p)%*%dd_b))
  bic = -2*loglike+log(n)*df
  
  tuning_1[i] = tun
  bic_1[i] = bic
  loglike_1[i] = loglike
  
  beta_1[,i] = beta
  SE_beta_1[,i] = SE_beta
  }

tun = tuning_1[which.min(bic_1)]
loglike = loglike_1[which.min(bic_1)]
beta = c(beta_1[,which.min(bic_1)])
SE_beta = c(SE_beta_1[,which.min(bic_1)])
bic = min(bic_1)

B = as.matrix(cbind(round(beta,5), round(SE_beta,5)))
colnames(B)= c("beta", "SE")
B

loglike
bic
tun

#####################################################################
cv.lasso = cv.glmnet(X, y, alpha=1, family="binomial", type.measure='auc')
lasso = glmnet(X,y,alpha=1,family='binomial', lambda=cv.lasso$lambda.min)
lasso$beta
