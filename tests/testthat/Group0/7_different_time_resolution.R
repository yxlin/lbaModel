# q(save = "no")
cat("\n\n---------- Testing dlba fine time resolution ----------")
rm(list = ls())

pkg <- c("lbaModel", "ggplot2")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")
<<<<<<< HEAD


=======
>>>>>>> 41bef569990fe0630c3041017163ba2935486e74
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

params <- param_list2mat(params_tmp)
nv <- ncol(params)
is_positive_drift <- rep(TRUE, nv)
RT <- seq(0, 3, 0.001) + params[6, 1]

dt <- 0.0001
min_dt <- 0
max_dt <- 10

DT <- seq(min_dt, max_dt, dt)
time_parameter_r <- c(min_dt, max_dt, dt)

cdf_densities <- plba(
    RT, params, is_positive_drift, time_parameter_r
)

dt <- 0.1
min_dt <- 0
max_dt <- 5
time_parameter_r <- c(min_dt, max_dt, dt)

cdf_densities2 <- plba(
    RT, params, is_positive_drift, time_parameter_r
)

dt <- 0.2
min_dt <- 0
max_dt <- 5
time_parameter_r <- c(min_dt, max_dt, dt)

cdf_densities3 <- plba(
    RT, params, is_positive_drift, time_parameter_r
)

png("theoretical_cdf.png")
plot(RT, cdf_densities[[1]], ylim = c(0, 1), ylab = "Density")
lines(RT, cdf_densities[[2]])
points(RT, cdf_densities2[[1]], col = "red")
points(RT, cdf_densities2[[2]], col = "lightgreen")

points(RT, cdf_densities3[[1]], col = "orange")
points(RT, cdf_densities3[[2]], col = "lightblue")
dev.off()
