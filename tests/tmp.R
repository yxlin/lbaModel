q(save = "no")

rm(list = ls())

library(lbaModel)

if (requireNamespace("ggdmcModel", quietly = TRUE)) {
  BuildModel <- getFromNamespace("BuildModel", "ggdmcModel")
  model <- BuildModel(
    p_map = list(
      A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1",
      st0 = "1"
    ),
    match_map = list(M = list("s1" = "r1", "s2" = "r2")),
    factors = list(S = c("s1", "s2")),
    constants = c(sd_v = 1, st0 = 0),
    accumulators = c("r1", "r2"),
    type = "lba"
  )
}

p_vector <- c(
  A = .75, B = 1.25, mean_v.false = 1.5, mean_v.true = 2.5,
  t0 = 0.15
)
sub_model <- setLBA(model)
sim_dat <- simulate(sub_model,
  nsim = 256, parameter_vector = p_vector,
  n_subject = 1
)

head(sim_dat)



param_list2mat <- function(x) do.call(rbind, x)
params <- param_list2mat(list(
  A = c(0.5, 0.5),
  b = c(1.0, 1.0),
  mean_v = c(2.0, 1.0),
  sd_v = c(1.0, 1.0),
  st0 = c(0.0, 0.0),
  t0 = c(0.2, 0.2)
))
is_pos <- rep(TRUE, ncol(params))

dt <- 0.05
time_param <- c(0, 2, dt)
# Theoretical densities/cdfs for two accumulators
pdfs <- theoretical_dlba(params, is_pos, time_param)
cdfs <- theoretical_plba(params, is_pos, time_param)


# Combine to check mass and monotonicity quickly
mass_from_pdf <- sum((pdfs[[1]] + pdfs[[2]]) * dt)
tail_cdf_sum <- tail(cdfs[[1]] + cdfs[[2]], 1)
# Both should be close to 1 (within coarse-grid tolerance)
round(c(mass_from_pdf = mass_from_pdf, tail_cdf_sum = tail_cdf_sum), 3)

# Spot-check dlba/plba at a few RTs (shifted by t0)
RT <- c(0.25, 0.50, 0.75) + params[6, 1]
pl <- plba(RT, params, is_pos, time_param)
head(pl[[1]])


# Reuse objects from above; create a denser grid for nicer plots
dt2 <- 0.01
time_param2 <- c(0, 5, dt2)
DT <- seq(time_param2[1], time_param2[2], by = time_param2[3])
pdfs2 <- theoretical_dlba(params, is_pos, time_param2)
cdfs2 <- theoretical_plba(params, is_pos, time_param2)

pdf_all <- (pdfs2[[1]] + pdfs2[[2]]) * dt2
cdf_all <- cdfs2[[1]] + cdfs2[[2]]
par(mfrow = c(2, 1), mar = c(5, 5, 2, 1))
# PDF
plot(DT, pdfs2[[1]] * dt2,
  type = "l",
  xlab = "DT", ylab = "Density", main = "Theoretical PDF"
)
lines(DT, pdfs2[[2]] * dt2, lty = 2)
legend("topright", legend = c("Acc 1", "Acc 2"), lty = c(1, 2), bty = "n")
# Cumulated PDF vs CDF
plot(DT, cumsum(pdf_all),
  type = "l",
  xlab = "DT", ylab = "Cumulative", main = "Cumulated PDF and CDF",
  ylim = c(0, 1)
)
lines(DT, cdf_all, lty = 2)
legend("bottomright",
  legend = c("Cumulated PDF", "CDF"),
  lty = c(1, 2), bty = "n"
)

#'
#'   # Optional: grid-comparison for plba
#'   RT2 <- seq(0, 3, by = 0.002) + params[6, 1]
#'   c1 <- plba(RT2, params, is_pos, c(0, 10, 0.01))
#'   c2 <- plba(RT2, params, is_pos, c(0, 5, 0.10))
#'   c3 <- plba(RT2, params, is_pos, c(0, 5, 0.20))
#'
#'   par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
#'   plot(RT2, c1[[1]],
#'     type = "l", ylim = c(0, 1), xlab = "RT", ylab = "CDF",
#'     main = "LBA CDF Estimates under Different Time Grids", lwd = 2
#'   )
#'   lines(RT2, c1[[2]], lwd = 2, lty = 2)
#'   lines(RT2, c2[[1]], lwd = 2, col = "gray40")
#'   lines(RT2, c2[[2]], lwd = 2, lty = 2, col = "gray40")
#'   lines(RT2, c3[[1]], lwd = 2, col = "gray60")
#'   lines(RT2, c3[[2]], lwd = 2, lty = 2, col = "gray60")
#'   legend("bottomright",
#'     legend = c(
#'       "Acc1 (dt=0.01)", "Acc2 (dt=0.01)",
#'       "Acc1 (dt=0.1)", "Acc2 (dt=0.1)",
#'       "Acc1 (dt=0.2)", "Acc2 (dt=0.2)"
#'     ),
#'     lty = c(1, 2, 1, 2, 1, 2), lwd = 2, bty = "n"
#'   )



pdfs
plot(pdfs[[1]], type = "l")
lines(pdfs[[2]])
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
library(lbaModel)
pdf_densities <- theoretical_dlba(
  params, is_positive_drift, time_parameter_r
)
cdf_densities <- theoretical_plba(
  params, is_positive_drift, time_parameter_r
)
params
is_positive_drift
time_parameter_r
#-------------------------------------------------#
# Example 1: theoretical_dlba and theoretical_plba
#-------------------------------------------------#
# Tiny helper to build the parameter matrix
# (rows = A, b, mean_v, sd_v, st0, t0)
param_list2mat <- function(x) do.call(rbind, x)
params <- param_list2mat(list(
  A = c(0.5, 0.5),
  b = c(1.0, 1.0),
  mean_v = c(2.0, 1.0),
  sd_v = c(1.0, 1.0),
  st0 = c(0.0, 0.0),
  t0 = c(0.2, 0.2)
))
params

is_pos <- rep(TRUE, ncol(params))
is_pos

# Use a coarse, short grid so it runs quickly
dt <- 0.05
time_param <- c(0, 2, dt)
time_param

# Theoretical densities/cdfs for two accumulators
pdfs <- theoretical_dlba(params, is_pos, time_param)
cdfs <- theoretical_plba(params, is_pos, time_param)
# Combine to check mass and monotonicity quickly
mass_from_pdf <- sum((pdfs[[1]] + pdfs[[2]]) * dt)
tail_cdf_sum <- tail(cdfs[[1]] + cdfs[[2]], 1)

# Both should be close to 1 (within coarse-grid tolerance)
round(c(mass_from_pdf = mass_from_pdf, tail_cdf_sum = tail_cdf_sum), 3)

# Spot-check dlba/plba at a few RTs (shifted by t0)
params[6, 1]
RT <- c(0.25, 0.50, 0.75) + params[6, 1]
pl <- plba(RT, params, is_pos, time_param)


## ---- Extended (plots; only in interactive sessions) --------------------
@examplesIf interactive()
{
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)

  # Reuse objects from above; create a denser grid for nicer plots
  dt2 <- 0.01
  time_param2 <- c(0, 5, dt2)
  DT <- seq(time_param2[1], time_param2[2], by = time_param2[3])
  pdfs2 <- theoretical_dlba(params, is_pos, time_param2)
  cdfs2 <- theoretical_plba(params, is_pos, time_param2)
  pdf_all <- (pdfs2[[1]] + pdfs2[[2]]) * dt2
  cdf_all <- cdfs2[[1]] + cdfs2[[2]]
  par(mfrow = c(2, 1), mar = c(5, 5, 2, 1))
  # PDF
  plot(DT, pdfs2[[1]] * dt2,
    type = "l",
    xlab = "DT", ylab = "Density", main = "Theoretical PDF"
  )
  lines(DT, pdfs2[[2]] * dt2, lty = 2)
  legend("topright", legend = c("Acc 1", "Acc 2"), lty = c(1, 2), bty = "n")
  # Cumulated PDF vs CDF
  plot(DT, cumsum(pdf_all),
    type = "l",
    xlab = "DT", ylab = "Cumulative", main = "Cumulated PDF and CDF",
    ylim = c(0, 1)
  )
  lines(DT, cdf_all, lty = 2)
  legend("bottomright",
    legend = c("Cumulated PDF", "CDF"),
    lty = c(1, 2), bty = "n"
  )
  # Optional: grid-comparison for plba
  RT2 <- seq(0, 3, by = 0.002) + params[6, 1]
  c1 <- plba(RT2, params, is_pos, c(0, 10, 0.01))
  c2 <- plba(RT2, params, is_pos, c(0, 5, 0.10))
  c3 <- plba(RT2, params, is_pos, c(0, 5, 0.20))
  par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
  plot(RT2, c1[[1]],
    type = "l", ylim = c(0, 1), xlab = "RT", ylab = "CDF",
    main = "LBA CDF Estimates under Different Time Grids", lwd = 2
  )
  lines(RT2, c1[[2]], lwd = 2, lty = 2)
  lines(RT2, c2[[1]], lwd = 2, col = "gray40")
  lines(RT2, c2[[2]], lwd = 2, lty = 2, col = "gray40")
  lines(RT2, c3[[1]], lwd = 2, col = "gray60")
  lines(RT2, c3[[2]], lwd = 2, lty = 2, col = "gray60")
  legend("bottomright",
    legend = c(
      "Acc1 (dt=0.01)", "Acc2 (dt=0.01)",
      "Acc1 (dt=0.1)", "Acc2 (dt=0.1)",
      "Acc1 (dt=0.2)", "Acc2 (dt=0.2)"
    ),
    lty = c(1, 2, 1, 2, 1, 2), lwd = 2, bty = "n"
  )
}



#' @examples
#' ## ---- Core (CRAN-safe, fast) --------------------------------------------
#' # Tiny helper to build the parameter matrix (rows = A,b,mean_v,sd_v,st0,t0)
#' param_list2mat <- function(x) t(do.call(rbind, x))
#'
#' params <- param_list2mat(list(
#'     A = c(0.5, 0.5),
#'     b = c(1.0, 1.0),
#'     mean_v = c(2.0, 1.0),
#'     sd_v = c(1.0, 1.0),
#'     st0 = c(0.0, 0.0),
#'     t0 = c(0.2, 0.2)
#' ))
#' is_pos <- rep(TRUE, ncol(params))
#'
#' # Use a coarse, short grid so it runs very quickly
#' dt <- 0.05
#' time_param <- c(0, 2, dt)
#'
#' # Theoretical densities/cdfs for two accumulators
#' pdfs <- theoretical_dlba(params, is_pos, time_param)
#' cdfs <- theoretical_plba(params, is_pos, time_param)
#'
#' # Combine to check mass and monotonicity quickly
#' mass_from_pdf <- sum((pdfs[[1]] + pdfs[[2]]) * dt)
#' tail_cdf_sum <- tail(cdfs[[1]] + cdfs[[2]], 1)
#'
#' # Both should be close to 1 (within coarse-grid tolerance)
#' round(c(mass_from_pdf = mass_from_pdf, tail_cdf_sum = tail_cdf_sum), 3)
#'
#' # Spot-check dlba/plba at a few RTs (shifted by t0)
#' RT <- c(0.25, 0.50, 0.75) + params[6, 1]
#' pl <- plba(RT, params, is_pos, time_param)
#' head(pl[[1]])
#'
#'
#' ## ---- Extended (plots; only in interactive sessions) --------------------
#' @examplesIf interactive()
#' {
#'     oldpar <- par(no.readonly = TRUE)
#'     on.exit(par(oldpar), add = TRUE)
#'
#'     # Reuse objects from above; create a denser grid for nicer plots
#'     dt2 <- 0.01
#'     time_param2 <- c(0, 5, dt2)
#'     DT <- seq(time_param2[1], time_param2[2], by = time_param2[3])
#'
#'     pdfs2 <- theoretical_dlba(params, is_pos, time_param2)
#'     cdfs2 <- theoretical_plba(params, is_pos, time_param2)
#'     pdf_all <- (pdfs2[[1]] + pdfs2[[2]]) * dt2
#'     cdf_all <- cdfs2[[1]] + cdfs2[[2]]
#'
#'     par(mfrow = c(2, 1), mar = c(5, 5, 2, 1))
#'     # PDF
#'     plot(DT, pdfs2[[1]] * dt2,
#'         type = "l",
#'         xlab = "DT", ylab = "Density", main = "Theoretical PDF"
#'     )
#'     lines(DT, pdfs2[[2]] * dt2, lty = 2)
#'     legend("topright", legend = c("Acc 1", "Acc 2"), lty = c(1, 2), bty = "n")
#'
#'     # Cumulated PDF vs CDF
#'     plot(DT, cumsum(pdf_all),
#'         type = "l",
#'         xlab = "DT", ylab = "Cumulative", main = "Cumulated PDF and CDF", ylim = c(0, 1)
#'     )
#'     lines(DT, cdf_all, lty = 2)
#'     legend("bottomright", legend = c("Cumulated PDF", "CDF"), lty = c(1, 2), bty = "n")
#'
#'     # Optional: grid-comparison for plba
#'     RT2 <- seq(0, 3, by = 0.002) + params[6, 1]
#'     c1 <- plba(RT2, params, is_pos, c(0, 10, 0.01))
#'     c2 <- plba(RT2, params, is_pos, c(0, 5, 0.10))
#'     c3 <- plba(RT2, params, is_pos, c(0, 5, 0.20))
#'
#'     par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
#'     plot(RT2, c1[[1]],
#'         type = "l", ylim = c(0, 1), xlab = "RT", ylab = "CDF",
#'         main = "LBA CDF Estimates under Different Time Grids", lwd = 2
#'     )
#'     lines(RT2, c1[[2]], lwd = 2, lty = 2)
#'     lines(RT2, c2[[1]], lwd = 2, col = "gray40")
#'     lines(RT2, c2[[2]], lwd = 2, lty = 2, col = "gray40")
#'     lines(RT2, c3[[1]], lwd = 2, col = "gray60")
#'     lines(RT2, c3[[2]], lwd = 2, lty = 2, col = "gray60")
#'     legend("bottomright",
#'         legend = c(
#'             "Acc1 (dt=0.01)", "Acc2 (dt=0.01)",
#'             "Acc1 (dt=0.1)", "Acc2 (dt=0.1)",
#'             "Acc1 (dt=0.2)", "Acc2 (dt=0.2)"
#'         ),
#'         lty = c(1, 2, 1, 2, 1, 2), lwd = 2, bty = "n"
#'     )
#' }
