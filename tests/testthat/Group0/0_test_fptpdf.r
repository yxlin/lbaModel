# q(save = "no")
cat("\n\n-------------------- Testing fptpdf --------------------")
rm(list = ls())
pkg <- c("lbaModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

# test-fptpdf.R
# Unit tests for fptpdf and fptcdf against rtdists reference implementations


# Helper: convert parameter list to matrix
param_list2mat <- function(param_list) {
  n_row <- length(param_list[[1]])
  n_col <- length(param_list)
  out <- matrix(NA, nrow = n_row, ncol = n_col)
  for (i in seq_len(n_col)) {
    out[, i] <- param_list[[i]]
  }
  t(out)
}

# Old reference implementations
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
    return(pnorm(chi / z, mean = v, sd = sdv, lower.tail = FALSE))
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

# Common parameter setup
mean_v <- 2.4
A <- 1.2
b <- 2.7
t0 <- 0.2
sd_v <- 1
st0 <- 0
params <- list(
  A = rep(A, 2),
  b = rep(b, 2),
  mean_v = rep(mean_v, 2),
  sd_v = rep(sd_v, 2),
  st0 = rep(st0, 2),
  t0 = rep(t0, 2)
)
params_mat <- param_list2mat(params)
is_positive_drift <- rep(TRUE, length(params$A))

# ---------------- PDF TESTS ----------------

test_that("fptpdf matches dlba_norm_core at single RT", {
  RT <- 0.3
  ref <- rtdists:::dlba_norm_core(RT, A, b, t0, mean_v, sd_v)
  target <- fptpdf(RT, params_mat, is_positive_drift, TRUE)
  expect_equal(ref, target, tolerance = 1e-6)
})

test_that("fptpdf matches dlba_norm_core across a range of RTs", {
  RT <- seq(0, 10, 0.01) + t0
  ref <- rtdists:::dlba_norm_core(RT, A, b, t0, mean_v, sd_v)
  target <- fptpdf(RT, params_mat, is_positive_drift)
  expect_equal(ref, target, tolerance = 1e-6)
})



# ---------------- PARAMETER SWEEP ----------------

test_that("fptcdf matches plba_norm_core for varying mean_v", {
  RT <- seq(0, 0.6, 0.01) + t0
  mean_v_seq <- seq(1.2, 5, 0.5) # shortened step for speed
  for (mv in mean_v_seq) {
    params$mean_v <- mv
    params_mat <- param_list2mat(params)
    ref <- rtdists:::plba_norm_core(RT, A, b, t0, mv, sd_v)
    target <- fptcdf(RT, params_mat, is_positive_drift)
    expect_equal(ref, target, tolerance = 1e-6)
  }
})
