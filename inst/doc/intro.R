## ----eval=FALSE---------------------------------------------------------------
#  qua95 <- function(N){
#    rej1 <- list()
#    for (z in 1:N) {
#      EA <- matrix(0,100,100)
#      Q <- matrix(c(0.3,0.1,0.1,0.3), 2, 2)
#      z1 <- matrix(c(1,0),2,1)
#      z2 <- matrix(c(0,1),2,1)
#      EA[1:50, 1:50] <- matrix(t(z1) %*% Q %*% z1, 50, 50)
#      EA[51:100, 1:50] <- matrix(t(z2) %*% Q %*% z1, 50, 50)
#      EA[1:50, 51:100] <- matrix(t(z1) %*% Q %*% z2, 50, 50)
#      EA[51:100, 51:100] <- matrix(t(z2) %*% Q %*% z2, 50, 50)
#      A <- matrix(0,100,100)
#      for (i in 1:100){
#        for(j in 1:100){
#          A[i,j] <- rbinom(1,1,EA[i,j])
#        }
#      }
#      A1 <- A
#      A1[lower.tri(A1)] <- t(A1)[lower.tri(A1)]
#      A3 <- A1[-1,-1]
#  
#      b <- A3[1,]
#      Nr1 <- length(which (b == 1, arr.ind = T))
#      Nr2 <- length(which (b == 0, arr.ind = T))
#  
#      u <- (sqrt(Nr1-1)+sqrt(Nr2))^2
#      sigma <- sqrt(u)*{(1/sqrt(Nr1-1)+1/sqrt(Nr2))^(1/3)}
#  
#      X1 <- matrix(rnorm(Nr1*Nr1,0,1),nrow=Nr1,ncol=Nr1)
#      diag(X1) <- rnorm(Nr1,0,2)
#      X1[lower.tri(X1)] <- t(X1)[lower.tri(X1)]
#      X1 <- X1/sqrt(Nr1)
#      X2 <- matrix(rnorm(Nr1*Nr2,0,1),nrow=Nr1,ncol=Nr2)
#      rt11 <- abs((max(eigen(t(X2) %*% X2)$values)-u)/sigma)
#      rt12 <- abs(max(eigen(X1)$values)-2)*Nr1^(2/3)
#      rej1[[z]] <- rt11+rt12
#    }
#  
#    rej1<- as.vector(unlist(rej1))
#    new_rej12 <- rej1[order(rej1)][95]*2
#    return (new_rej12)
#  }

## ----eval=FALSE---------------------------------------------------------------
#  subg=function(A,L)
#  {
#    n=dim(A)[1]
#    C=B=matrix(rep(0,n^2),nrow=n)
#    if (L>1)
#    {
#      for (i in 1:(n-1))
#        for (j in (i+1):n)
#          if (A[1,i]==1&A[i,j]==1)
#          {B[i,j]=B[j,i]=1
#          } else   if (A[1,j]==1&A[i,j]==1)
#          {B[i,j]=B[j,i]=1
#          }
#    } else {B[1,]=A[1,]
#    B[,1]=A[1,]
#    }
#  
#    if (L>2)
#    {
#      for (t in 3:L)
#      {
#        C=matrix(rep(0,n^2),nrow=n)
#        for (i in 2:(n-1))
#          for (j in (i+1):n)
#            if (B[i,j]==1)
#            { a=(2:n)[-(i-1)]
#            C[i,a]=A[i,a]
#            C[a,i]=A[a,i]
#            a=(2:n)[-(j-1)]
#            C[j,a]=A[j,a]
#            C[a,j]=A[a,j]
#            }
#        B=B+C
#        for (i in 1:(n-1))
#          for (j in (i+1):n)
#            B[i,j]=B[j,i]=min(B[i,j],1)
#      }
#    }
#    return(B)
#  }

## -----------------------------------------------------------------------------
library(StatComp21015)
t <- qua95(100)
A <- matrix(c(0,1,1,1,0,0,1,0,1,0,0,0,1,1,0,0,0,0,1,0,0,0,1,1,0,0,0,1,0,1,0,0,0,1,1,0),6,6)
B <- subg(A,2)
print(t)
print(B)

