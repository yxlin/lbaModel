rm(list = ls())
require(lbaModel)
###################################################
# Example 1: theoretical_dlba and theoretical_plba
###################################################
param_list2mat <- function(param_list) {
  n_row <- length(param_list[[1]])
  n_col <- length(param_list)
  out <- matrix(NA, nrow = n_row, ncol = n_col)
  for (i in seq_len(n_col)) {
    out[, i] <- param_list[[i]]
  }
  t(out)
}
params_tmp <- list(
  A = c(0.5, 0.5),
  b = c(1.0, 1.0),
  mean_v = c(2.0, 1.0),
  sd_v = c(1.0, 1.0),
  st0 = c(0.0, 0.0),
  t0 = c(0.2, 0.2)
)
dt <- 0.01
min_dt <- 0
max_dt <- 5
DT <- seq(min_dt, max_dt, dt)
time_parameter_r <- c(min_dt, max_dt, dt)
params <- param_list2mat(params_tmp)
nv <- ncol(params)
is_positive_drift <- rep(TRUE, nv)

pdf_densities <- theoretical_dlba(
  params, is_positive_drift, time_parameter_r
)
cdf_densities <- theoretical_plba(
  params, is_positive_drift, time_parameter_r
)



pdf_all <- pdf_densities[[1]] * dt + pdf_densities[[2]] * dt
cdf_all <- cdf_densities[[1]] + cdf_densities[[2]]
res1 <- cumsum(pdf_all)
res2 <- cdf_all
res3 <- res1[length(pdf_all)]
res4 <- cdf_all[length(cdf_all)]
cat("The cumulative densities, [PDF, CDF] = [", res3, ", ", res4, "]\n")
d_pdf <- data.frame(
  x = rep(DT, 2), y = c(
    pdf_densities[[1]] * dt,
    pdf_densities[[2]] * dt
  ),
  gp = rep(c("Accumulator 1", "Accumulator 2"), each = length(DT))
)
d_cdf <- data.frame(
  x = rep(DT, 2), y = c(res1, res2),
  gp = rep(c("Cumulated PDF", "CDF"), each = length(DT))
)

par(mfrow = c(2, 1), mar = c(5, 5, 2, 1))
# PDF plot
plot(DT, pdf_densities[[1]] * dt, type = "l", col = "blue", 
     ylim = c(0, max(pdf_all)), xlab = "DT", ylab = "Density", 
     main = "Theoretical PDF")

lines(DT, pdf_densities[[2]] * dt, col = "red")
legend("topright", legend = c("Accumulator 1", "Accumulator 2"),
       col = c("blue", "red"), lty = 1, bty = "n")

# CDF plot
plot(DT, res1, type = "l", col = "blue", ylim = c(0, 1),
     xlab = "DT", ylab = "Cumulative", main = "Cumulated PDF and CDF")
lines(DT, res2, col = "red")
legend("bottomright", legend = c("Cumulated PDF", "CDF"),
       col = c("blue", "red"), lty = 1, bty = "n")

###################################################
# Example 2: plba (Base R Plotting)
###################################################
# Parameter setup
params_tmp <- list(
  A = c(0.5, 0.5),
  b = c(1.0, 1.0),
  mean_v = c(2.0, 1.0),
  sd_v = c(1.0, 1.0),
  st0 = c(0.0, 0.0),
  t0 = c(0.2, 0.2)
)

params <- param_list2mat(params_tmp)
nv <- ncol(params)
is_positive_drift <- rep(TRUE, nv)
# Response times (shifted by non-decision time)
RT <- seq(0, 3, 0.001) + params[6, 1]
# Different dt setups
time_param1 <- c(0, 10, 0.01)
time_param2 <- c(0, 5, 0.1)
time_param3 <- c(0, 5, 0.2)
cdf1 <- plba(RT, params, is_positive_drift, time_param1)
cdf2 <- plba(RT, params, is_positive_drift, time_param2)
cdf3 <- plba(RT, params, is_positive_drift, time_param3)

par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
plot(RT, cdf1[[1]], type = "l", ylim = c(0, 1), xlab = "RT", ylab = "CDF",
     main = "LBA CDF Estimates under Different Time Grids", col = "black",
     lwd = 2)
lines(RT, cdf1[[2]], col = "black", lwd = 2, lty = 2)
lines(RT, cdf2[[1]], col = "red", lwd = 2)
lines(RT, cdf2[[2]], col = "red", lwd = 2, lty = 2)
lines(RT, cdf3[[1]], col = "blue", lwd = 2)
lines(RT, cdf3[[2]], col = "blue", lwd = 2, lty = 2)
legend("bottomright",
       legend = c("Acc1 (dt=0.0001)", "Acc2 (dt=0.0001)",
                  "Acc1 (dt=0.1)", "Acc2 (dt=0.1)",
                  "Acc1 (dt=0.2)", "Acc2 (dt=0.2)"),
       col = c("black", "black", "red", "red", "blue", "blue"),
       lty = c(1, 2, 1, 2, 1, 2), lwd = 2, bty = "n")


head(plba)
