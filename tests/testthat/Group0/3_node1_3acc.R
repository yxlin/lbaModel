# q(save = "no")
cat("\n\n---------- Testing Node 1 PDF > 3 Accumulators ----------")
rm(list = ls())
pkg <- c("lbaModel", "microbenchmark")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

param_list2mat <- function(param_list) {
    n_row <- length(param_list[[1]])
    n_col <- length(param_list)

    tmp <- matrix(NA, nrow = n_row, ncol = n_col)

    for (i in seq_len(n_col)) {
        tmp[, i] <- param_list[[i]]
    }
    t(tmp)
}


## 3 accumulators -------------
A <- 1.2
b <- 2.7
t0 <- .2

mean_v <- c(2.4, 2.2, 1.5)
sd_v <- c(1, 1, 1.5)

RT <- seq(0, 3, .4) + t0
posdrift <- TRUE
nv <- length(mean_v)


res0 <- lbaModel::old_n1PDF_fixed_t0(
    rt_r = RT, A_r = rep(A, nv), b_r = rep(b, nv),
    mean_v_r = mean_v, sd_v_r = sd_v,
    t0_r = rep(t0, nv), st0_r = rep(0, nv), positive_drift_r = posdrift
)

res1 <- rtdists::n1PDF(RT,
    A = rep(A, nv), b = rep(b, nv), t0 = rep(t0, nv),
    mean_v = mean_v, sd_v = sd_v, silent = TRUE
)


params_tmp <- list(
    A = rep(A, nv),
    b = rep(b, nv),
    mean_v = mean_v,
    sd_v = sd_v,
    st0 = rep(0, nv),
    t0 = rep(t0, nv)
)

params <- param_list2mat(params_tmp)
is_positive_drift <- rep(TRUE, nv)

res3 <- lbaModel::new_n1PDF_fixed_t0(RT, params, is_positive_drift, TRUE)
res4 <- lbaModel::new_n1PDF(RT, params, is_positive_drift, TRUE)


testthat::expect_equal(res0, res1)
testthat::expect_equal(res1, res3)
testthat::expect_equal(res1, res4)

round(cbind(res0, res1, res3, res4), 3)


# res <- microbenchmark(
#     res0 <- lbaModel::old_n1PDF_fixed_t0(
#         rt_r = RT, A_r = rep(A, nv), b_r = rep(b, nv),
#         mean_v_r = mean_v, sd_v_r = sd_v,
#         t0_r = rep(t0, nv), st0_r = rep(0, nv), positive_drift_r = posdrift
#     ),
#     res1 <- rtdists::n1PDF(RT,
#         A = rep(A, nv), b = rep(b, nv), t0 = rep(t0, nv),
#         mean_v = mean_v, sd_v = sd_v, silent = TRUE
#     ),
#     res3 <- lbaModel::new_n1PDF_fixed_t0(RT, params, is_positive_drift),
#     res4 <- lbaModel::new_n1PDF(RT, params, is_positive_drift),
#     times = 10000L
# )
# res
