# This script uses Entropy Pooling to compute least information kernel smoothing 
# A. Meucci, "Personalized Risk Management: Historical Scenarios with Fully Flexible Probabilities"
# GARP Risk Professional, Dec 2010, p 47-51
# 
#  Most recent version of article and code available at
#  http://www.symmys.com/node/150

LeastInfoKernel <- function( Y, y, h2)
{
  T <- dim(Y)[1]
  N <- dim(Y)[2]
  Aeq <- matrix(1,1,T) #constrain probabilities to sum to one...
  beq <- 1

  Aeq <- rbind( Aeq, t(Y)) #...constrain the first moments...
  beq <- rbind( beq, y)
 
  if (!( is.nan( h2)){
   
      SecMom <- h2 + y %*% t(y) #...constrain the second moments...
    
      for( k in 1:N){
         
        for( l in k:N){
         
         Aeq <- rbind( Aeq, t( Y[,k] * Y[,l])) 
         beq <- rbind( beq, SecMom[k,l])
        }

      }   

  }
  
  p_0 <- matrix( 1, T, 1)

####check here for the inputs of EntropyProg.R

p <- EntropyProg( p_0, [], [], Aeq, beq) #...compute posterior probabilities

return(p)

}
