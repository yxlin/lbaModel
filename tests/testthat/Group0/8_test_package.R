#  q(save = "no")
cat("\n\n---------- test package examples ----------")
rm(list = ls())

pkg <- c("lbaModel", "ggplot2")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

# A complete example ---------------------------------------
pop_mean <- c(A = .4, B = 1.6, mean_v.true = 3.5, mean_v.false = 2.38,
              t0 = 0.05)
pop_scale <- c(A = 2, B = .2, mean_v.true = .25, mean_v.false = .1, 
               t0 = 0.01)

if (requireNamespace("ggdmcModel", quietly = TRUE)) {

BuildModel <- getFromNamespace("BuildModel", "ggdmcModel")

model <- BuildModel(
  p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", 
               st0 = "1"),
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

sim_dat <- simulate(sub_model, nsim = 256, parameter_vector = p_vector, n_subject = 1)

head(sim_dat)

#---------------------------------------
if (requireNamespace("ggdmcPrior", quietly = TRUE)) {
 
 BuildPrior <- getFromNamespace("BuildPrior", "ggdmcPrior")
 
 pop_dist <- BuildPrior(
   p0 = pop_mean,
   p1 = pop_scale,
   lower = rep(NA, model@npar),
   upper = rep(NA, model@npar),
   dists = rep("tnorm", model@npar),
   log_p = rep(FALSE, model@npar))
}

#---------------------------------------
rt_model <- setLBA(model, population_distribution = pop_dist)
res_process_model <- simulate(rt_model)
res_inverse_method <- simulate(rt_model, use_inverse_method = TRUE)

param_list2mat <- function(param_list) {
  n_row <- length(param_list[[1]])
  n_col <- length(param_list)
  tmp <- matrix(NA, nrow = n_row, ncol = n_col)
  for (i in seq_len(n_col)) {
    tmp[, i] <- param_list[[i]]
  }
  t(tmp)
}

params_tmp <- list(
  A = c(0.74, 0.74),
  b = c(1.25 + 0.74, 1.25 + 0.74),
  mean_v = c(2.52, 1.50),
  sd_v = c(1.0, 1.0),
  st0 = c(0.0, 0.0),
  t0 = c(0.04, 0.04)
)
params_tmp

params <- param_list2mat(params_tmp)
print(params)

n_acc <- 2
is_positive_drift <- rep(TRUE, n_acc)

res <- lbaModel::n1PDF(res_process_model$RT, params, is_positive_drift, TRUE)

cat("Print first 30 density values from the LBA node 1 function: \n")
print(head(round(res, 2), 30))

# A simple example ---------------------------------------
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
print(params_mat)

RT <- 0.3
result <- fptpdf(RT, params_mat, is_positive_drift, TRUE)

## A range of values
dt <- seq(0, 10, .01)
RT <- dt + t0
result <- fptpdf(RT, params_mat, is_positive_drift)

RT <- seq(0, 10, .01) + t0
result <- fptcdf(RT, params_mat, is_positive_drift)

# n1PDF_fixed_t0 ------------
A <- 1.2
b <- 2.7
t0 <- .2
mean_v <- c(2.4, 2.2)
sd_v <- c(1, 1)
RT <- seq(0, 3, .4) + t0
posdrift <- TRUE
nv <- length(mean_v)
params_tmp <- list(
  A = rep(A, 2),
  b = rep(b, 2),
  mean_v = mean_v,
  sd_v = sd_v,
  st0 = rep(0, 2),
  t0 = rep(t0, 2)
)

 params <- param_list2mat(params_tmp)
 is_positive_drift <- rep(TRUE, nv)

result <- n1PDF(RT, params, is_positive_drift, TRUE)


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



require(ggplot2)
 p0 <- ggplot(data = d_pdf, mapping = aes(x = x, y = y, colour = gp)) +
   geom_point() +
   xlab("DT") +
   ylab("Density") +
   guides(colour = guide_legend(title = NULL)) +
   theme_minimal(base_size = 16) +
   theme(
     legend.position.inside = c(0.85, 0.85), # x, y coordinates (0 to 1)
     legend.justification = c(1, 1) # anchor point inside the legend box
   )

 p1 <- ggplot(data = d_cdf, mapping = aes(x = x, y = y, colour = gp)) +
   geom_point() +
   xlab("DT") +
   ylab("Density") +
   guides(colour = guide_legend(title = NULL)) +
   theme_minimal(base_size = 16) +
   theme(
     legend.position.inside = c(0.85, 0.85), # x, y coordinates (0 to 1)
     legend.justification = c(1, 1) # anchor point inside the legend box
   )
p0
p1
#' # png("theoretical_pdf_cdf.png")
#' gridExtra::grid.arrange(p0, p1)
#' # dev.off()
#' ###################################################
#' # Example 2: plba
#' ###################################################
#' params_tmp <- list(
#'   A = c(0.5, 0.5),
#'   b = c(1.0, 1.0),
#'   mean_v = c(2.0, 1.0),
#'   sd_v = c(1.0, 1.0),
#'   st0 = c(0.0, 0.0),
#'   t0 = c(0.2, 0.2)
#' )
#'
#' params <- param_list2mat(params_tmp)
#' nv <- ncol(params)
#' is_positive_drift <- rep(TRUE, nv)
#' RT <- seq(0, 3, 0.001) + params[6, 1]
#'
#' dt <- 0.0001
#' min_dt <- 0
#' max_dt <- 10
#'
#' DT <- seq(min_dt, max_dt, dt)
#' time_parameter_r <- c(min_dt, max_dt, dt)
#'
#' cdf_densities <- plba(
#'   RT, params, is_positive_drift, time_parameter_r
#' )
#'
#' dt <- 0.1
#' min_dt <- 0
#' max_dt <- 5
#' time_parameter_r <- c(min_dt, max_dt, dt)
#'
#' cdf_densities2 <- plba(
#'   RT, params, is_positive_drift, time_parameter_r
#' )
#'
#' dt <- 0.2
#' min_dt <- 0
#' max_dt <- 5
#' time_parameter_r <- c(min_dt, max_dt, dt)
#'
#' cdf_densities3 <- plba(
#'   RT, params, is_positive_drift, time_parameter_r
#' )
#'
#' # png("theoretical_cdf.png")
#' plot(RT, cdf_densities[[1]], ylim = c(0, 1), ylab = "Density")
#' lines(RT, cdf_densities[[2]])
#' points(RT, cdf_densities2[[1]], col = "red")
#' points(RT, cdf_densities2[[2]], col = "lightgreen")
#'
#' points(RT, cdf_densities3[[1]], col = "orange")
#' points(RT, cdf_densities3[[2]], col = "lightblue")
#' # dev.off()
#' }
