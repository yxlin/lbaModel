# q(save = "no")
cat("\n\n-------------------- Testing fptpdf --------------------")
rm(list = ls())
pkg <- c("lbaModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")
param_list2mat <- function(param_list) {
  n_row <- length(param_list[[1]])
  n_col <- length(param_list)
  out <- matrix(NA, nrow = n_row, ncol = n_col)

  for (i in seq_len(n_col)) {
    out[, i] <- param_list[[i]]
  }
  t(out)
}



## Basic test ----
fptpdf_old <- function(z, x0max, chi, v, sdv) {
  if (x0max == 0) {
    return((chi / z^2) * dnorm(chi / z, mean = v, sd = sdv))
  }
  zs <- z * sdv
  zu <- z * v
  chiminuszu <- chi - zu
  chizu <- chiminuszu / zs
  chizumax <- (chiminuszu - x0max) / zs
  (v * (pnorm(chizu) - pnorm(chizumax)) +
    sdv * (dnorm(chizumax) - dnorm(chizu))) / x0max
}

fptcdf_old <- function(z, x0max, chi, v, sdv) {
  if (x0max == 0) {
    return(pnorm(chi / z, mean = v, sd = sdv, lower.tail = F))
  }
  zs <- z * sdv
  zu <- z * v
  chiminuszu <- chi - zu
  xx <- chiminuszu - x0max
  chizu <- chiminuszu / zs
  chizumax <- xx / zs
  tmp1 <- zs * (dnorm(chizumax) - dnorm(chizu))
  tmp2 <- xx * pnorm(chizumax) - chiminuszu * pnorm(chizu)
  1 + (tmp1 + tmp2) / x0max
}

## PDF --------
mean_v <- 2.4
A <- 1.2
b <- 2.7
t0 <- .2
sd_v <- 1
st0 <- 0
positive_drift_r <- TRUE


params <- list(
  A = rep(A, 2),
  b = rep(b, 2),
  mean_v = rep(mean_v, 2),
  sd_v = rep(sd_v, 2),
  st0 = rep(st0, 2),
  t0 = rep(t0, 2)
)



n_accumulator <- length(params$A)
is_positive_drift <- rep(TRUE, n_accumulator)
params_mat <- param_list2mat(params)


RT <- 0.3
res2 <- rtdists:::dlba_norm_core(RT, A, b, t0, mean_v, sd_v)
res3 <- fptpdf_old(RT, x0max = A, chi = b, v = mean_v, sdv = sd_v)
res4 <- fptpdf(RT, params_mat, is_positive_drift, TRUE)
round(c(res2, res3, res4), 3)

testthat::expect_equal(res2, res4)

## A range
dt <- seq(0, 10, .01)
RT <- dt + t0


res2 <- rtdists:::dlba_norm_core(RT, A, b, t0, mean_v, sd_v)
res4 <- fptpdf(RT, params_mat, is_positive_drift)
testthat::expect_equal(res2, res4)

plot(RT, res4)
lines(RT, res2, col = "red")


## CDF --------
RT <- 0


res2 <- rtdists:::plba_norm_core(RT, A, b, t0, mean_v, sd_v)
res3 <- fptcdf_old(RT - t0, x0max = A, chi = b, v = mean_v, sdv = sd_v)
res4 <- fptcdf(RT, params_mat, is_positive_drift)
c(res2, res3, res4)

RT <- 0.6

res2 <- rtdists:::plba_norm_core(RT, A, b, t0, mean_v, sd_v)
res3 <- fptcdf_old(RT - t0, x0max = A, chi = b, v = mean_v, sdv = sd_v)
res4 <- fptcdf(RT, params_mat, is_positive_drift)
c(res2, res3, res4)

testthat::expect_equal(res2, res4)

RT <- seq(0, 10, .01) + t0

res2 <- rtdists:::plba_norm_core(RT, A, b, t0, mean_v, sd_v)
res3 <- fptcdf_old(RT - t0, x0max = A, chi = b, v = mean_v, sdv = sd_v)
res4 <- fptcdf(RT, params_mat, is_positive_drift)
c(res2, res3, res4)


testthat::expect_equal(res2, res4)



plot(RT, res4)
lines(RT, res2, col = "red")
lines(RT, res3, col = "lightgreen")


mean_v <- 2.4
A <- 1.2
b <- 2.7
t0 <- .2
sd_v <- 1
st0 <- 0
posdrift <- FALSE


RT <- seq(0, .6, .01) + t0
mean_v <- seq(1.2, 5, .01)
for (i in seq_len(length(mean_v)))
{
  params$mean_v <- mean_v[i]
  params_mat <- param_list2mat(params)
  res4 <- fptcdf(RT, params_mat, is_positive_drift)
  res2 <- rtdists:::plba_norm_core(RT, A, b, t0, mean_v[i], sd_v)

  if (!all.equal(res2, res4)) {
    cat("Unequal CDF when using mean_v = ", mean[i], "\n")
  }
}
