CallPrice <- function(P, K, r, t, s) {
    d_1 <- log(P / K)+( r + s^2 / 2) * t;
    d_2 <- d_1 - s * sqrt(t);
    return (P * pnorm(d_1) - K * exp( -r * t) * pnorm(d_2))
}
