## -----------------------------------------------------------------------------
library(knitr)
data1 <- head(state.x77)
kable(data1)

## -----------------------------------------------------------------------------
library(pheatmap)
data2 <- as.matrix((scale(state.x77)))
p <- pheatmap(data2,fontsize=10,fontsize_row=7,color=colorRampPalette(c("navy","white","firebrick3"))(50))

## -----------------------------------------------------------------------------
par(mfrow=c(2,3))
n <- 1000
for (i in seq(0.1, 1, by=0.4)){  
  sigma <- i
  u <- runif(n)  #step1
  x1 <- sqrt(-2* sigma^2 * log(1-u)) #step2
  t <- seq(0,100,.01)
  hist(x1,prob=TRUE,main=paste ("when i=", as.character(i), sep = "")) #对标题进行修改
  lines(t, t/(sigma^2)*exp(-t^2/(2*sigma^2)))
}


for (i in seq(1, 10, by=4)){
  sigma <- i
  u <- runif(n)  #step1
  x2 <- sqrt(-2* sigma^2 * log(1-u)) #step2
  t <- seq(0,100,.01)
  hist(x2,prob=TRUE,main=paste ("when i=", as.character(i), sep = ""))
  lines(t, t/(sigma^2)*exp(-t^2/(2*sigma^2)))
}


## -----------------------------------------------------------------------------
n <- 1e4 #生成1000个随机数
X1 <- rnorm(n,0,1) 
X2 <- rnorm(n,3,1)
p <- sample(c(0,1),n,replace=TRUE,prob=c(0.25,0.75)) #控制 p1=0.75 和 p2 = 1 − p1=0.25
Z1 <- p*X1+(1-p)*X2 #生成混合分布
hist(Z1,prob=TRUE) #做直方图

## -----------------------------------------------------------------------------
out1 <- list()
for (i in 1:1000){
  t <- 10
  alpha <- 4
  beta <- 3
  lambda <- 5
  nt <- rpois(1,lambda*t)
  y <- rgamma(nt,alpha,beta)
  xt<- sum(y)
  out1[[i]] <- xt 
}
out1 <- as.vector(unlist(out1))

Ext <- mean(out1)
Varxt <- var(out1)
Extl <- lambda*t*alpha/beta
Varxtl <- lambda*t*(alpha/beta^2+(alpha/beta)^2)
diffExt <- Extl-Ext 
diffvarxt <- Varxtl-Varxt 
print(diffExt)
print(diffvarxt)

## -----------------------------------------------------------------------------
out1 <- list()
for (i in 1:1000){
  t <- 10
  alpha <- 2
  beta <- 6
  lambda <- 15
  nt <- rpois(1,lambda*t)
  y <- rgamma(nt,alpha,beta)
  xt<- sum(y)
  out1[[i]] <- xt 
}
out1 <- as.vector(unlist(out1))

Ext <- mean(out1)
Varxt <- var(out1)
Extl <- lambda*t*alpha/beta
Varxtl <- lambda*t*(alpha/beta^2+(alpha/beta)^2)
diffExt <- Extl-Ext 
diffvarxt <- Varxtl-Varxt
print(diffExt)
print(diffvarxt)

## -----------------------------------------------------------------------------
theta.hat <- rep(0,9)
B <- beta(3,3)
m <- 1000
for (t in seq(1,9,by=1)){
  y <- runif(m,0,t/10)  #t是从0.1-0.9的
  theta.hat[t] <- mean(t/10*(y^4-2*y^3+y^2))/B
}
print(theta.hat)

t <- seq(0.1,0.9,0.1)
p <- pbeta(t,3,3)
diff <- theta.hat-p
print(p)
print(diff)

## -----------------------------------------------------------------------------
MC.Phi <- function(x, R = 10000, antithetic = TRUE) {
  u <- runif(R/2,0,x)
  sigma <- 2
  if (!antithetic) v <- runif(R/2) else v <- 1 - u
  u <- c(u, v)
  cdf <- numeric(length(x))
  for (i in 1:length(x)) {
    g <- x[i] * u / (sigma^2) * exp(-(u)^2 / 2 /(sigma^2)) 
    cdf[i] <- mean(g) 
  }
  cdf
}
m <- 1000
MC1 <- MC2 <- numeric(m)
x <- 1.95
for (i in 1:m) {
  MC1[i] <- MC.Phi(x, R = 1000, anti = FALSE)
  MC2[i] <- MC.Phi(x, R = 1000)
}

print(c(var(MC1),var(MC2),var(MC2)/var(MC1)))

## -----------------------------------------------------------------------------
x <- seq(1, 5, .01)
w <- 2

g <-  x^2 * exp(-x^2/2) / sqrt(2 * pi) 
f0 <- rep(1,length(x))
f1 <- x*exp(-x^2/2)/exp(1)^(-1/2)
f2 <- x^{-6/5}/5
gs <- c(expression(g(x)==x^2*e^{-x^2/2}/sqrt(2*pi)),
        expression(f[0](x)==1),
        expression(f[1](x)==x*e^{-x^2/2}/e^{-1/2}),
        expression(f[2](x)==x^{-6/5}/5))
#for color change lty to col
par(mfrow=c(1,2))
#figure (a)
plot(x, g, type = "l", ylab = "",
     ylim = c(0,2), lwd = w,col=1,main='(A)')
lines(x, f0, lty = 2, lwd = w,col=2)
lines(x, f1, lty = 3, lwd = w,col=3)
lines(x, f2, lty = 4, lwd = w,col=4)
legend("topright", legend = gs,
       lty = 1:4, lwd = w, inset = 0.02,col=1:4)

#figure (b)
plot(x, g/f0, type = "l", ylab = "",
     ylim = c(0,3.2), lwd = w, lty = 2,col=2,main='(B)')
lines(x, g/f1, lty = 3, lwd = w,col=3)
lines(x, g/f2, lty = 4, lwd = w,col=4)
legend("topright", legend = gs[-1],
       lty = 2:4, lwd = w, inset = 0.02,col=2:4)

## -----------------------------------------------------------------------------
library(knitr)
m <- 1000
est <- sd <- numeric(3)
g <- function(x) {
  exp(-x^2/2) * x^2/ sqrt(2*pi)
}
x <- runif(m) #using f0
fg <- g(x)
est[1] <- mean(fg)
sd[1] <- sd(fg)

x <- runif(m) #using f1
fg <- g(x) / (x*exp(-x^2/2)/exp(1)^(-1/2))
est[2] <- mean(fg)
sd[2] <- sd(fg)

x <- runif(m) #using f2
fg <- g(x) / (x^(-6/5)/5)
est[3] <- mean(fg)
sd[3] <- sd(fg)

res <- rbind(est=round(est,3), sd=round(sd,3))
colnames(res) <- paste0('f',0:2)
kable(res)

## -----------------------------------------------------------------------------
n <- 1000
u <- runif(n) 
x <- sqrt(-2* log(1-u+u/sqrt(exp(1)))) 
theta <- 1/2-mean(x)/sqrt(2*pi)*(1-1/sqrt(exp(1))) #根据原公式计算值
print(theta)

## -----------------------------------------------------------------------------
n <- 20
s <- 0
x1 <- numeric(1000) 
x2 <- numeric(1000) 
alpha <- .05 
for (i in 1:1000){
  x <- rchisq(n,df=2)
  x1[i] <- mean(x)- qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n)
  x2[i] <- mean(x)+ qt(1-alpha/2,df=n-1)*sd(x)/sqrt(n) 
  if (x1[i] < 2  && 2< x2[i])
  s <- s+1
}
print(s/1000)

## -----------------------------------------------------------------------------
set.seed(12)
n <- 100 #任意抽取n个样本
alpha <- .05
mu0 <- 1 #经过推导，可以知道卡方的自由度=均值

m <- 1000
p <- numeric(m) 
for (j in 1:m) {
  x <- rchisq(n, df=1)
  ttest <- t.test(x, mu = mu0) #做检验
  p[j] <- ttest$p.value
}

p.hat <- mean(p < alpha)
print(p.hat)

## -----------------------------------------------------------------------------
set.seed(12)
n <- 100
alpha <- .05
mu0 <- 1 #均匀分布的均值为(a+b)/2

m <- 1000
p <- numeric(m) 
for (j in 1:m) {
  x <- runif(n,0,2)
  ttest <- t.test(x, mu = mu0)
  p[j] <- ttest$p.value
}

p.hat <- mean(p < alpha)
print(p.hat)

## -----------------------------------------------------------------------------
set.seed(12)
n <- 100
alpha <- .05
mu0 <- 1 #指数分布的均值为1/lambda

m <- 1000
p <- numeric(m) 
for (j in 1:m) {
  x <- rexp(n, 1)
  ttest <- t.test(x, mu = mu0)
  p[j] <- ttest$p.value
}

p.hat <- mean(p < alpha)
print(p.hat)

## -----------------------------------------------------------------------------
library(MASS)
n <- c(10, 20, 30, 50) 
m <- 100
d <- 3
c <- qchisq(0.95, d*(d+1)*(d+2)/6)
p.reject <- numeric(length(n))

funb1d <- function(x) {
  xbar <- colMeans(x) #每一列的平均值
  sigma <- cov(x)
  b1d <- 0
  for (p in 1:n[i]){
    for (q in 1:n[i]){
       b1d <- b1d +( (x[p,]-xbar) %*% solve(sigma) %*% (x[q,]-xbar)  )^3
    }
  }
  b1d <- b1d/n[i]/6
  return( b1d )
}

for (i in 1:length(n)) {
  test <- numeric(m) 
  for (j in 1:m) {
    x <- matrix(rnorm(n[i]*d), nrow = n[i], ncol = d)
    test[j] <- as.integer(funb1d(x) >= c)
  }
  p.reject[i] <- mean(test)
}

p.reject

## -----------------------------------------------------------------------------
alpha <- .1
n <- 30
m <- 100
d <-3 
epsilon <- c(seq(0, 0.15, .01),seq(0.15, 1, .05))
N <- length(epsilon)
pw <- numeric(N)
c <- qchisq(1-alpha, d*(d+1)*(d+2)/6)

funb1d <- function(x) { #原本的
  xbar <- colMeans(x) #每一列的平均值
  COV <- cov(x)
  b1d <- 0
  for (p in 1:n){
    for (q in 1:n){
      b1d <- b1d +( (x[p,]-xbar) %*% solve(COV) %*% (x[q,]-xbar)  )^3
    }
  }
  b1d <- b1d/n/6
  return(b1d )
}

for (j in 1:N) {
  e <- epsilon[j]
  test <- numeric(m)
  for (i in 1:m) { 
    sigma <- sample(c(1, 10), replace = TRUE, size = n*d, prob = c(1-e, e))
    x <- matrix(rnorm(n*d, 0, sigma), nrow = n, ncol = d)
    test[i] <- as.integer(funb1d(x) >= c)
  }
  pw[j] <- mean(test)
}

plot(epsilon, pw, type = "b", xlab = bquote(epsilon), ylim = c(0,1))
se <- sqrt(pw * (1-pw) / m) 
lines(epsilon, pw+se, lty = 3)
lines(epsilon, pw-se, lty = 3)

## -----------------------------------------------------------------------------
library(bootstrap)
library(boot)
lam_hat <- eigen(cov(scor))$values 
the_hat <- lam_hat[1] /sum(lam_hat) #原本的\theta统计量
B <- 50

the_b <- numeric(B)
func <- function(dat,d){
     x <- dat[d,]
     lam <- eigen(cov(scor[x,]))$values
     the <- lam[1] /sum(lam)
     return(the)
}

result <- boot(data = cbind(scor$mec,scor$vec,scor$alg,scor$ana,scor$sta),statistic = func,R=B)

the_b <- result$t
round(c(bias_boot=mean(the_b)-the_hat,se_boot=sqrt(var(the_b))),5)

## -----------------------------------------------------------------------------
n <- nrow(scor)
the_j <- rep(0,n)
for (i in 1:n){
  x <- scor[-i,]
  lam <- eigen(cov(x))$values
  the_j[i] <- lam[1] /sum(lam)
}
bias_jack <- (n-1)*(mean(the_j)-the_hat)
se_jack <-  (n-1)*sqrt(var(the_j)/n)
round(c(bias_jack=(n-1)*(mean(the_j)-the_hat),se_jack=(n-1)*sqrt(var(the_j)/n)),5)

## ----eval=FALSE---------------------------------------------------------------
#  boot.ci(boot.out = result, conf = 0.95, type = c("perc","bca"))

## -----------------------------------------------------------------------------
mu<-0;n<-100;m<-1e3
library(boot)
set.seed(1)
func <- function(x,i){
  xbar <- mean(x[i])
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2)
for(i in 1:m){
  x <-rnorm(n)
  de <- boot(data=x,statistic=func, R = 999)
  ci <- boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3];ci.basic[i,]<-ci$basic[4:5]
  ci.perc[i,]<-ci$percent[4:5]
}
round(c(norm = mean(ci.norm[,1]<=mu & ci.norm[,2]>=mu),basic = mean(ci.basic[,1]<=mu & ci.basic[,2]>=mu),perc =mean(ci.perc[,1]<=mu & ci.perc[,2]>=mu),right.norm = sum(ci.norm[,1]>=mu)/m,left.norm = sum(ci.norm[,2]<=mu)/m,right.basic = sum(ci.basic[,1]>=mu)/m,left.basic = sum(ci.basic[,2]<=mu)/m,right.perc = sum(ci.perc[,1]>=mu)/m,left.perc= sum(ci.perc[,2]<=mu)/m ),4)

## -----------------------------------------------------------------------------
b <- 2*(5/2)/(1/2)^3 / (10)^{1.5}  
set.seed(12)
n<-100
m<-1e3
library(boot)
func <- function(x,i){
  xbar <- mean(x[i])
  m3 <- mean((x - xbar)^3)
  m2 <- mean((x - xbar)^2)
  return( m3 / m2^1.5 )
}
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2)
for(i in 1:m){
  x <- rchisq(n,5)
  de <- boot(data=x,statistic=func, R = 999)
  ci <- boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3];ci.basic[i,]<-ci$basic[4:5]
  ci.perc[i,]<-ci$percent[4:5]
}
round(c(norm = mean(ci.norm[,1]<=b & ci.norm[,2]>=b),basic = mean(ci.basic[,1]<=b & ci.basic[,2]>=b),perc =mean(ci.perc[,1]<=b & ci.perc[,2]>=b),right.norm = sum(ci.norm[,1]>=b)/m,left.norm = sum(ci.norm[,2]<=b)/m,right.basic = sum(ci.basic[,1]>=b)/m,left.basic = sum(ci.basic[,2]<=b)/m,right.perc = sum(ci.perc[,1]>=b)/m,left.perc= sum(ci.perc[,2]<=b)/m ),4)

## -----------------------------------------------------------------------------
v1 <- c(0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55)  #随意选取的数据
v2 <- c(1, 0.95, 0.95, 0.9, 0.85, 0.7, 0.65, 0.6, 0.55, 0.42)
m <- cor.test(v1, v2, alternative = "two.sided", method = "spearman", exact = FALSE)  #m为cor.test检验的结果

R <- 999
v <- c(v1,v2)
K <- 1:20
n <- length(v1)
set.seed(123)
reps <- numeric(R)
t0 <- cor(v1,v2,method = "spearman")
for (i in 1:R){                                        #做permutation test
  k <- sample(K,size=n,replace=FALSE)
  x1 <- v[k]
  y1 <- v[-k]
  reps[i] <- cor(x1,y1,method = "spearman")
}
p <- mean(abs(c(t0,reps)) >= abs(t0))
round(c(p,m$p.value),3)

## -----------------------------------------------------------------------------
library(RANN)
library(boot)
library(Ball)
library(energy)
m <- 100; k<-3; p<-2; set.seed(123)
n1 <- n2 <- 50; R<-999; n <- n1+n2; N = c(n1,n2)
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n)
}
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rnorm(n1*p,0,1),ncol=p);
  y <- matrix(rnorm(n2*p,0,1.5),ncol=p);
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}
pow <- colMeans(p.values<0.1) #alpha <- 0.1 
print(pow)

## -----------------------------------------------------------------------------
m <- 100; k<-3; p<-2; set.seed(12345)
n1 <- n2 <- 50; R<-999; n <- n1+n2; N = c(n1,n2)
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n)
}
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rnorm(n1*p,1,1),ncol=p);
  y <- matrix(rnorm(n2*p,1.3,1.5),ncol=p);
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}

alpha <- 0.1; 
pow <- colMeans(p.values<alpha)
print(pow)

## -----------------------------------------------------------------------------
m <- 100; k<-3; p<-2; set.seed(12345)
n1 <- n2 <- 50; R<-999; n <- n1+n2; N = c(n1,n2)
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n)
}
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rt(n1*p,1),ncol=p);
  m1 <- rnorm(n2*p,0,1) 
  m2 <- rnorm(n2*p,3,10)
  y <- matrix(0.75*m1+0.25*m2 ,ncol=p);
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}

alpha <- 0.1; 
pow <- colMeans(p.values<alpha)
print(pow)

## -----------------------------------------------------------------------------
m <- 100; k<-3; p<-2; set.seed(123)
n1 <- 4; n2 <- 100; R<-999; n <- n1+n2; N = c(n1,n2)
Tn <- function(z, ix, sizes,k) {
  n1 <- sizes[1]; n2 <- sizes[2]; n <- n1 + n2
  if(is.vector(z)) z <- data.frame(z,0);
  z <- z[ix, ];
  NN <- nn2(data=z, k=k+1) # what's the first column?
  block1 <- NN$nn.idx[1:n1,-1] 
  block2 <- NN$nn.idx[(n1+1):n,-1] 
  i1 <- sum(block1 < n1 + .5); i2 <- sum(block2 > n1+.5) 
  (i1 + i2) / (k * n)
}
eqdist.nn <- function(z,sizes,k){
  boot.obj <- boot(data=z,statistic=Tn,R=R,sim = "permutation", sizes = sizes,k=k)
  ts <- c(boot.obj$t0,boot.obj$t)
  p.value <- mean(ts>=ts[1])
  list(statistic=ts[1],p.value=p.value)
}
p.values <- matrix(NA,m,3)
for(i in 1:m){
  x <- matrix(rnorm(n1*p,0,1),ncol=p);
  y <- matrix(rnorm(n2*p,1.5,1.5),ncol=p);
  z <- rbind(x,y)
  p.values[i,1] <- eqdist.nn(z,N,k)$p.value
  p.values[i,2] <- eqdist.etest(z,sizes=N,R=R)$p.value
  p.values[i,3] <- bd.test(x=x,y=y,num.permutations=999,seed=i*12345)$p.value
}

alpha <- 0.1; 
pow <- colMeans(p.values<alpha)
print(pow)

## ----eval=FALSE---------------------------------------------------------------
#  set.seed(123)
#  f <- function(x, theta, nita) {
#    stopifnot(theta > 0)
#    return(1/ (theta*pi*(1+((x-nita)/theta)^2)))  #柯西分布的分布函数
#  }
#  
#  m <- 10000
#  theta <- 1
#  nita <- 0 #设定标准柯西分布
#  sigma <- 2.5
#  x <- numeric(m)
#  x[1] <- rnorm(1,0,sigma) #正态分布
#  k <- 0
#  u <- runif(m)
#  for (i in 2:m) {
#    xt <- x[i-1]
#    y <- rnorm(1,xt,sigma)
#    num <- f(y, theta, nita) * dnorm(xt, y, sigma)
#    den <- f(xt, theta, nita) * dnorm(y, xt, sigma)
#    if (u[i] <= num/den) x[i] <- y else {
#      x[i] <- xt
#      k <- k+1  #拒绝次数
#    }
#  }
#  print(k)
#  index <- 5000:5500
#  y1 <- x[index]
#  plot(index, y1, type="l", main="", ylab="x")
#  
#  a <- c(.1, seq(.2, .8, .1), .9)
#  Q <- qt(a, 1)
#  rw <- x
#  mc <- rw[1001:m]
#  print(round(cbind(Q, mc =quantile(x, a)),3))

## ----eval=FALSE---------------------------------------------------------------
#  Gelman.Rubin <- function(psi) {
#    psi <- as.matrix(psi)
#    n <- ncol(psi)
#    k <- nrow(psi)
#  
#    psi.means <- rowMeans(psi)
#    B <- n * var(psi.means)
#    psi.w <- apply(psi, 1, "var")
#    W <- mean(psi.w)
#    v.hat <- W*(n-1)/n + (B/n)
#    r.hat <- v.hat / W
#    return(r.hat)
#  }
#  
#  f <- function(x, theta, nita) {
#    stopifnot(theta > 0)
#    return(1/ (theta*pi*(1+((x-nita)/theta)^2)))  #柯西分布的分布函数
#  }
#  
#  normal.chain <- function(sigma, N, X1) {
#    x <- rep(0, N)
#    x[1] <- X1
#    u <- runif(N)
#  
#    for (i in 2:N) {
#      xt <- x[i-1]
#      y <- rnorm(1, xt, sigma)     #candidate point
#      r1 <- f(y, 1, 0) * dnorm(xt, y, sigma)
#      r2 <- f(xt, 1, 0) * dnorm(y, xt, sigma)
#      r <- r1 / r2
#      if (u[i] <= r) x[i] <- y else
#        x[i] <- xt
#    }
#    return(x)
#  }
#  
#  sigma <- 1
#  k <- 4
#  n <- 20000
#  b <- 1000
#  
#  x0 <- c(-10, -5, 5, 10)
#  
#  set.seed(12345)
#  X <- matrix(0, nrow=k, ncol=n)
#  for (i in 1:k)
#    X[i, ] <- normal.chain(sigma, n, x0[i])
#  
#  psi <- t(apply(X, 1, cumsum))
#  for (i in 1:nrow(psi))
#    psi[i,] <- psi[i,] / (1:ncol(psi))
#  
#  par(mfrow=c(2,2))
#  for (i in 1:k)
#    plot(psi[i, (b+1):n], type="l",
#         xlab=i, ylab=bquote(psi))
#  par(mfrow=c(1,1))
#  
#  rhat <- rep(0, n)
#  for (j in (b+1):n)
#    rhat[j] <- Gelman.Rubin(psi[,1:j])
#  plot(rhat[(b+1):n], type="l", xlab="", ylab="R")
#  abline(h=1.2, lty=2)

## ----eval=FALSE---------------------------------------------------------------
#  N <- 5000
#  burn <- 1000
#  X <- matrix(0, N, 2)
#  a <- 2
#  b <- 4
#  n <- 16
#  m1 <- 0.5
#  m2 <- 0.5
#  
#  X[1, ] <- c(m1, m2)
#  for (i in 2:N) {
#    s <- X[i-1, 2]
#    X[i, 1] <- rbinom(1, n, s)
#    t <- X[i, 1]
#    X[i, 2] <- rbeta(1, t+a, n-t+b)
#  }
#  b <- burn + 1
#  x <- X[b:N, ]
#  plot(x, main="", cex=.5, xlab=bquote(X[1]), ylab=bquote(X[2]), ylim=range(x[,2]))

## ----eval=FALSE---------------------------------------------------------------
#  library(stableGR)
#  set.seed(123)
#  gibbs.chain <- function(m1,m2,N,n,a,b) {
#  X <- matrix(0, N, 2)
#  X[1, ] <- c(m1, m2)
#  for (i in 2:N) {
#    s <- X[i-1, 2]
#    X[i, 1] <- rbinom(1, n, s)
#    t <- X[i, 1]
#    X[i, 2] <- rbeta(1, t+a, n-t+b)
#  }
#  return(X)
#  }
#  chain1 <- gibbs.chain(0.1,0.9,1000,16,2,4)
#  chain2 <- gibbs.chain(0.2,0.7,1000,16,2,4)
#  chain3 <- gibbs.chain(0.5,0.5,1000,16,2,4)
#  chain4 <- gibbs.chain(0.6,0.4,1000,16,2,4)
#  m<- list(chain1,chain2,chain3,chain4)
#  stable.GR(m,mapping="maxeigen")

## ----eval=FALSE---------------------------------------------------------------
#  Gelman.Rubin <- function(psi) {
#    # psi[i,j] is the statistic psi(X[i,1:j])
#    # for chain in i-th row of X
#    psi <- as.matrix(psi)
#    n <- ncol(psi)
#    k <- nrow(psi)
#  
#    psi.means <- rowMeans(psi)     #row means
#    B <- n * var(psi.means)        #between variance est.
#    psi.w <- apply(psi, 1, "var")  #within variances
#    W <- mean(psi.w)               #within est.
#    v.hat <- W*(n-1)/n + (B/n)     #upper variance est.
#    r.hat <- v.hat / W             #G-R statistic
#    return(r.hat)
#  }
#  
#  # 生成二元随机变量的Gibbs sampler
#  # X1为初始值
#  Bivariate.Gibbs <- function(N, X1){
#    a <- b <- 1
#    X <- matrix(0, N, 2)
#    X[1,] <- X1    #初始值
#    for(i in 2:N){
#      X2 <-  X[i-1, 2]
#      X[i,1] <- rbinom(1,25,X2)
#      X1 <- X[i,1]
#      X[i,2] <- rbeta(1,X1+a,25-X1+b)
#    }
#    return(X)
#  }
#  
#  k <- 4
#  N <- 8000
#  b <- 1000    #burn-in length
#  X1 <- cbind(c(2,7,10,15),runif(4)) #初始值
#  
#  #生成4条样本，每个第一维的放在X中，第二维的放在Y中
#  set.seed(12345)
#  X <- matrix(0, nrow=k, ncol=N)
#  Y <- matrix(0, nrow=k, ncol=N)
#  for (i in 1:k){
#    BG <- Bivariate.Gibbs(N, X1[i,])
#    X[i, ] <- BG[,1]
#    Y[i, ] <- BG[,2]
#  }
#  
#  # 先考虑第一维样本X
#  
#  #compute diagnostic statistics
#  psi <- t(apply(X, 1, cumsum))
#  for (i in 1:nrow(psi))
#    psi[i,] <- psi[i,] / (1:ncol(psi))
#  
#  #plot the sequence of R-hat statistics
#  rhat <- rep(0, N)
#  for (j in (b+1):N)
#    rhat[j] <- Gelman.Rubin(psi[,1:j])
#  plot(rhat[(b+1):N], type="l", xlab="", ylab="R")
#  abline(h=1.2, lty=2)
#  
#  # 再考虑第二维样本Y
#  
#  #compute diagnostic statistics
#  psi <- t(apply(Y, 1, cumsum))
#  for (i in 1:nrow(psi))
#    psi[i,] <- psi[i,] / (1:ncol(psi))
#  
#  #plot the sequence of R-hat statistics
#  rhat <- rep(0, N)
#  for (j in (b+1):N)
#    rhat[j] <- Gelman.Rubin(psi[,1:j])
#  plot(rhat[(b+1):N], type="l", xlab="", ylab="R")
#  abline(h=1.2, lty=2)

## -----------------------------------------------------------------------------
k <- 3
a <- c(1,2)
d <- 2
norm_vec <- function(x) sqrt(sum(x^2)) #计算向量欧几里得范数
kth <- (-1)^k * norm_vec(a)^(2*k+2)/ factorial(k) / (2^k) / (2*k+1) / (2*k+2) * exp(lgamma((d+1)/2) + lgamma(k+3/2) - lgamma(k+d/2+1))
print(kth)

## -----------------------------------------------------------------------------
k <- 400
a <- c(1,2)
d <- 400
norm_vec <- function(x) sqrt(sum(x^2)) #计算向量欧几里得范数
kth <- (-1)^k * norm_vec(a)^(2*k+2) / factorial(k) / (2^k) / (2*k+1) / (2*k+2) * exp(lgamma((d+1)/2) + lgamma(k+3/2) - lgamma(k+d/2+1))
print(kth)

## -----------------------------------------------------------------------------
norm_vec <- function(x) sqrt(sum(x^2)) #计算向量欧几里得范数
kth <- function(k,a,d){
  (-1)^k * norm_vec(a)^(2*k+2) / factorial(k) / (2^k) / (2*k+1) / (2*k+2) * exp(lgamma((d+1)/2) + lgamma(k+3/2) - lgamma(k+d/2+1))
}
sum <- 0
for(k in 1:200){
  a <- c(1,1,3,4)
  d <- 3
  sum <- sum + kth(k,a,d)
}
print(sum)

## -----------------------------------------------------------------------------
norm_vec <- function(x) sqrt(sum(x^2)) #计算向量欧几里得范数
kth <- function(k,a,d){
  (-1)^k * norm_vec(a)^(2*k+2) / factorial(k) / (2^k) / (2*k+1) / (2*k+2) * exp(lgamma((d+1)/2) + lgamma(k+3/2) - lgamma(k+d/2+1))
}
sum <- 0
for(k in 1:400){
  a <- c(1,2)
  d <- 2
  sum <- sum + kth(k,a,d)
}
print(sum)

## -----------------------------------------------------------------------------
fun <- function(a,k) {
  g1 <- function(u) (1+u^2/k)^(-(k+1)/2)
  g2 <- integrate(g1,0,sqrt(a^2*(k)/(k+1-a^2)))$value
  g <-  2*g2 * exp(lgamma((k+1)/2) -  lgamma(k/2) -log(sqrt(pi*k)))  #对gamma函数取对数
}

out <- rep(0,25)
i <- 1
 for (k in c(4:25)){ #和11.4一样的范围
  f <- function(a) fun(a,k-1)-fun(a,k)  #题目给定的方程
  o <- uniroot(f ,c(0.01,sqrt(k)-0.3)) #对上下界进行一些限制
  out[i] <- o$root
  i <- i+1
}

i <- 23
for (k in c(100,500,1000)){
  f <- function(a) fun(a,k-1)-fun(a,k) 
  o <- uniroot(f ,c(0.01,2))
  out[i] <- o$root
  i <- i+1
}
print(out)

## -----------------------------------------------------------------------------
#用MLE算到的结果
library(stats4)
data <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
mlogl <- function(lam=0.5) {
  return(-(7*log(lam) - lam * sum(data)))
}
fit <- mle(mlogl)@coef
print(fit)

## -----------------------------------------------------------------------------
#下面是EM算法得到的结果
data <- c(0.54, 0.48, 0.33, 0.43, 1.00, 1.00, 0.91, 1.00, 0.21, 0.85)
N <- 1000
tol <- .Machine$double.eps^0.5
l <- 0.5
l.old <- l + 1

for (i in 1:N){
  l <- 10/( 3/l + sum(data) )
  if (abs(l - l.old)/l.old < tol) break
  l.old <- l
}

print(list(lambda = l, iter = i, tol = tol))

## -----------------------------------------------------------------------------
trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(100)
m <- lapply(trims, function(trim) mean(x, trim = trim))
n <- lapply(trims, mean, x = x)
print(m)
print(n)

## -----------------------------------------------------------------------------
formulas <- list(mpg ~ disp, mpg ~ I(1 / disp), mpg ~ disp + wt, mpg ~ I(1 / disp) + wt)
l1 <- lapply(formulas, lm, mtcars)
l2 <- list(1,2,3,4)
for (i in 1:length(formulas)){
  l2[[i]]<- lm(formulas[[i]],mtcars)
}
rsq <- function(mod) summary(mod)$r.squared
r1 <- lapply(l1,rsq)
r2 <- lapply(l2,rsq)
print(r1)
print(r2)

## -----------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
})
l1 <- lapply(bootstraps, lm, formula=mpg ~ disp)
l2 <- list(rep(0,10))
for (i in 1:length(bootstraps)){
  l2[[i]]<- lm(mpg ~ disp,bootstraps[[i]])
}
rsq <- function(mod) summary(mod)$r.squared
r1 <- lapply(l1,rsq)
r2 <- lapply(l2,rsq)
print(r1)
print(r2)

## -----------------------------------------------------------------------------
vapply(mtcars, sd, numeric(1))

## -----------------------------------------------------------------------------
vapply(mtcars[vapply(mtcars,is.numeric,TRUE)], sd, numeric(1))

## -----------------------------------------------------------------------------
#利用老师上课讲的代码计算sapply.
start <- Sys.time()
summary <- function(x) {
  funs <- c(mean, median, sd, mad, IQR)
  sapply(funs, function(f) f(x, na.rm = TRUE))
}
end <- Sys.time()
print(end-start)


#利用老师上课讲的代码计算parSapply.
library(parallel)
start <- Sys.time()
  cl.cores <- detectCores(logical = F) #计算电脑核心数
  cl <- makeCluster(cl.cores) # 初始化要使用的核心数
summary <- function(x) {
  funs <- c(mean, median, sd, mad, IQR)
  parSapply(cl,funs, function(f) f(x, na.rm = TRUE))
}
end <- Sys.time()
print(end-start)


## ----eval=FALSE---------------------------------------------------------------
#  #include <Rcpp.h>
#  using namespace Rcpp;
#  
#  // [[Rcpp::export]]
#  NumericMatrix gibbsC(int N, int t) {
#    NumericMatrix mat(N, 2);
#    double x = 1, y = 0.5;
#    for(int i = 0; i < N; i++) {
#      for(int j = 0; j < t; j++) {
#        x = rbinom(1, 2, y)[0];
#        y = rbeta(1, x + 1,  2-x+1)[0];
#      }
#      mat(i, 0) = x;
#      mat(i, 1) = y;
#    }
#    return(mat);
#  }

## ----eval=FALSE---------------------------------------------------------------
#  library(Rcpp)
#  dir_cpp <- '../Rcpp/'
#  sourceCpp(paste0(dir_cpp,"gibbsC.cpp"))

## ----eval=FALSE---------------------------------------------------------------
#  gibbsR <- function(N, thin){ # N is the length of chain
#    X <- matrix(0,nrow = N,ncol = 2) # 样本阵
#    x <- 1
#    y <- 0.5    # 初始值
#    for(i in 2:N){
#      for(j in 1:thin){
#        x <- rbinom(1,2,prob = y) # 更新x
#        y <- rbeta(1,x+1,2-x+1)          # 更新y
#      }
#      X[i,] <- c(x,y)
#    }
#    return(X)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  gC <- gibbsC(2000,10)
#  gR <- gibbsR(2000,10)
#  qqplot(gC[,1],gR[,1],xlab=" BiGibbsC-x",ylab=" BiGibbsR-x")
#  qqplot(gC[,2],gR[,2],xlab=" BiGibbsC-y",ylab=" BiGibbsR-y")

## ----eval=FALSE---------------------------------------------------------------
#  library(microbenchmark)
#  ts <- microbenchmark(gibbsR=gibbsR(2000,10),gibbsC=gibbsC(2000,10))
#  summary(ts)[,c(1,3,5,6)]

