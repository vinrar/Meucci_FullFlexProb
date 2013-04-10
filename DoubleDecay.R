DoubleDecay <- function(X, lmd_c, lmd_s)
{
  dim_val <- dim(X)
  T <- dim_val[1]
  N <- dim_val[2]
  m <- matrix(0,N,1)


  p_c <- exp(-lmd_c*(t(T-t(seq(1:T)))));
  
  p_c <- p_c/sum(p_c);
  p_c <- p_c[ , rep(seq_len(ncol(p_c)), N)]
  

  S_1 <- t((p_c %*% X))%*% X;
  C <- cov2cor(S_1);

  p_s <- exp(-lmd_s*(t(T-t(seq(1:T)))));
  p_s <- p_s/sum(p_s);
  p_s <- p_s[ , rep(seq_len(ncol(p_s)), N)]
  S_2 <- t((p_s %*% X)) %*% X;
  R <- cov2cor(S_2)
  
  
  s <- diag(R)
  
  S <- diag(s)*C*diag(s);
  result <- c(m ,S);
  return(result);
}
