# q(save = "no")
cat("\n\n---------- Testing Node 1 PDF > 3 Accumulators ----------")
rm(list = ls())
pkg <- c("lbaModel", "rtdists", "foreach", "doParallel")
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
## Testing over a range of values ------------------

# Set up parallel processing
cat("Run the script separately, because it will run many parallel cores.\n")

# cl <- makeCluster(detectCores() - 2)
# print(cl)

# registerDoParallel(cl)
# mean_v <- c(2.4, 2.2, 1.5)
# sd_v <- c(1, 1, 1.5)
# nv <- length(mean_v)
# A <- seq(0.01, 5, .01)
# b <- seq(0.01, 5, .01)
# t0 <- seq(.01, .2, .01)
# dt <- seq(0, 3, .4)
# is_positive_drift <- rep(TRUE, nv)
# # Pre-compute RT values
# RT_vals <- outer(dt, t0, `+`)

# results <- foreach(i = seq_along(A), .combine = "rbind", .packages = c("rtdists", "lbaModel")) %dopar% {
#     for (j in seq_along(b)) {
#         for (k in seq_along(t0)) {
#             params_tmp <- list(
#                 A = rep(A[i], nv),
#                 b = rep(b[j] + A[i], nv),
#                 mean_v = mean_v,
#                 sd_v = sd_v,
#                 st0 = rep(0, nv),
#                 t0 = rep(t0[k], nv)
#             )

#             params <- param_list2mat(params_tmp)
#             RT <- RT_vals[, k]

#             res1 <- rtdists::n1PDF(RT,
#                 A = rep(A[i], nv), b = rep(b[j] + A[i], nv),
#                 t0 = rep(t0[k], nv), mean_v = mean_v, sd_v = sd_v, silent = TRUE
#             )
#             res3 <- lbaModel::new_n1PDF_fixed_t0(RT, params, is_positive_drift)
#             res4 <- lbaModel::new_n1PDF(RT, params, is_positive_drift)

#             if (!all.equal(res1, res3)) {
#                 warning(sprintf("New n1PDF fixed t0 not the same A=%.2f b=%.2f t0=%.2f", A[i], b[j], t0[k]))
#             }
#             if (!all.equal(res1, res4)) {
#                 warning(sprintf("New n1PDF not the same A=%.2f b=%.2f t0=%.2f", A[i], b[j], t0[k]))
#             }
#         }
#     }
#     NULL
# }

# stopCluster(cl)
