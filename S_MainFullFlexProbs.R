# This script uses Entropy Pooling to compute Fully Flexible Probabilities for historical scenarios
# based on time periods, market conditions, constraints on moments, etc., as described in
# A. Meucci, "Personalized Risk Management: Historical Scenarios with Fully Flexible Probabilities"
# GARP Risk Professional, Dec 2010, p 47-51
# 
#  Most recent version of article and code available at
#  http://www.symmys.com/node/150

#####################################################################################################
#risk drivers scenarios
#####################################################################################################
library(R.matlab)
x <- readMat("db.mat")

Infl <- as.matrix(x$Data[,9])
Vix <- as.matrix(x$Data[,8])
Crude <- as.matrix(x$Data[,6])
Swp10 <- as.matrix(x$Data[,2])
SnP <- as.matrix(x$Data[,4])

lenSnP <- length(SnP)-1
SnPld <- matrix(1:lenSnP)
for( i in 1:lenSnP) { SnPld[i] = log(SnP[i+1]) - log(SnP[i])}
Vixld <- matrix(1:lenSnP)
for( i in 1:lenSnP) { Vixld[i] = log(Vix[i+1]) - log(Vix[i])}
Swp10ld <- matrix(1:lenSnP)
for( i in 1:lenSnP) { Swp10ld[i] = log(Swp10[i+1]) - log(Swp10[i])}

X <- cbind( SnPld, Vixld, Swp10ld)
Y <- as.matrix(Infl[1:lenSnP])

#####################################################################################################
#assign probabilities to historical scenarios
#####################################################################################################
# DefineProbs = 1 : rolling window
# DefineProbs = 2 : exponential smoothing
# DefineProbs = 3 : market conditions
# DefineProbs = 4 : kernel damping
# DefineProbs = 5 : partial information prox. kernel damping
# DefineProbs = 6 : partial information: match covariance

  DefineProbs <- 6

  T <- dim(X)[1]
  p <- matrix(0,T,1)
  
  if(DefineProbs == 1){ tau = 2*252;
                        p[1:tau] = 1;
                        p = as.matrix(p) ;
                        p = p/sum(p);
                      }
                      
  if(DefineProbs == 2){ lmd = 0.0166;
                        p <- exp(-lmd * (t(T-t(seq(1:T))))); 
                        p = p/sum(p);
                        }
                        
  if(DefineProbs == 3){ Cond <- (Y >= 2.8);
                                  p[Cond] <- 1; 
                                  p <- p/sum(p);
                                  }
                                  
  if(DefineProbs == 4){ y <- 3;
                       for( i in 1:lenSnP) { Yd[i] = (Y[i+1]) - (Y[i])} ;
                                             h2 <- cov(Yd);
                                             p <- dmvnorm(Y,y,h2); 
                                             p <- p/sum(p);
                                           }
                                                           
  if (DefineProbs == 5){ y <- 3;
                         h2 <- NaN;
                         h2 <- cov(1*diff(Y));
                         p <- LeastInfoKernel( Y, y, h2);
                       }
                                           
  if (DefineProbs == 6){ l_c <- 0.0055;
                         l_s <- 0.0166;
                         res <- DoubleDecay( X, l_c, l_s);
                         m <-res[1];
                         S <- res[2];
                         p <- Fit2Moms( X, m, S);
                        }
                      
 #################               
 # P&L scenarios #
 #################
 
 N <- 20;
 
 #call parameters
 
 S_0 <- SnP[length(SnP)];
 vol_0 <- Vix[length(Vix)];
 rf_0 <- Swp10[length(Swp10)];
 K <- S_0 * (seq(0.8, 1.1, length.out = N));
 Expiry <- seq(2, N+1 , by = 1)/252;
 
 S_T <- S_0 * exp(X[,1]);
 vol_T <- vol_0 * exp(X[,2]);
 rf_T <- rf_0 * exp(X[,3]);
 
 #securities scenario
 PnL <- matrix(0,(length(SnP)-1),N)
 for (n in 1: N) {
 
        Call_1 <- CallPrice(S_T, K[n], rf_T, (Expiry[n]-(1/252)), vol_T);
        Call_0 <- CallPrice(S_0, K[n], rf_0, Expiry[n], vol_0);
        PnL[,n] <- as.matrix(Call_1 - Call_0);
         
 }
 
 #portfolio scenarios
 A <- matrix(1,N/2,1)
 B <- -1*A
 u <- -rbind(B,A)
 PnL_u <- PnL%*%u;
