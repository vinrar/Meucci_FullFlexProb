EntropyProg <- function(p,A,b,Aeq,beq)
{
  
# Compute posterior (=change of measure) with Entropy Pooling, as described in
# A. Meucci - "Fully Flexible Views: Theory and Practice" 
# Risk Magazine, October 2008, p 97-102
# 
#  Most recent version of article and code available at
#  http://www.symmys.com/node/158

  K_ <- dim(A)[1]
  K  <- dim(A)[1]
  
  A_ <- t(A)
  b_ <- t(b)
  Aeq_ <- t(Aeq)
  beq_ <- t(beq)
  x0 <- matrix(0,K_+K,1)
  InqMat <- -1 * diag(K_+K)
  InqMat <- InqMat[1:K_,]
  InqVec <- matrix(0,K_,1)
  
###### incomplete ################
#  write the function  ###########
# search about fminunc & fmincon source code#
# source code not found#
#incomplete#

}

################################check about v, whether it is a vector/matrix

nestedfunU <- function(v)

{
  x <- exp( log(p)-1-Aeq_*v );
  x <- pmax(x,10^(-32));
  L <- t(x)%*%(log(x)-log(p)+Aeq_*v)-beq_*v;
  mL <- -L;    
       
  g <- [beq-Aeq*x];    
  H <- [Aeq %*% (( x %*% matrix(1,1,K)) %*% Aeq_)];  # Hessian computed by Chen Qing, Lin Daimin, Meng Yanyan, Wang Weijun 
  result <- c(mL ,g ,H)
  return result
}

nestedfunC <- function(lv)

{
  #l=lv(1:K_);
   #     v=lv(K_+1:end);
        x <- exp( log(p)-1-A_%*%l-Aeq_%*%v );
        x <- pmax(x,10^(-32));
        L <- t(x)%*%(log(x)-log(p))+t(l)%*%(A%*%x-b)+t(v)%*%(Aeq%*%x-beq);
        mL=-L;
    
        g = [ b - A %*% x; beq - Aeq %*% x];    
        H = c( A %*% ((x %*% matrix(1,1,K)) %*% A_),  A %*% ((x %*% matrix(1,1,K))) %*% Aeq_),
            Aeq %*%((x %*% matrix(1,1,K)) %*% A_),   Aeq %*%((x %*% matrix(1,1,K)) %*% Aeq_); # Hessian computed by Chen Qing, Lin Daimin, Meng Yanyan, Wang Weijun  
        result <- c(mL ,g ,H)
        return result
}



