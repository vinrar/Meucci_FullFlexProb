Fit2Moms <- function(X,m,S)
{
# This script uses Entropy Pooling to compute a double-decay covariance matrix, as described in  
# A. Meucci, "Personalized Risk Management: Historical Scenarios with Fully Flexible Probabilities"
# GARP Risk Professional, Dec 2010, p 47-51
# 
#  Most recent version of article and code available at
#  http://www.symmys.com/node/150
 
 T <- dim(X)[1]
 N <- dim(X)[2]
 Aeq <- matrix(1,1,T) # constrain probabilities to sum to one...
 beq <- 1
 
 
 
 Aeq <- rbind( Aeq, t(X))  # ...constrain the first moments...
 beq <- rbind( beq, m)

 SecMom <- S + (m %*% t(m))
 
 for(k in 1:N)
  {
    for(l in k:N)
     {
       Aeq <- rbind( Aeq, t( X[,k] * X[,l])) 
       
       beq <- rbind( beq, SecMom[k,l])
     }
  }
p_0 <- matrix( 1, T, 1)

p <- EntropyProg( p_0, , , Aeq, beq)

return(p)

}
