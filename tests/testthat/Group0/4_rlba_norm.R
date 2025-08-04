# q(save = "no")
cat("\n\n---------- Testing rlba ----------")
rm(list = ls())

pkg <- c("lbaModel", "rtdists", "microbenchmark")
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
plot_overlay_hist <- function(data1, data2, data3, data4,
                              col1 = rgb(0, 0, 1, 0.5),
                              col2 = rgb(1, 0, 0, 0.5),
                              col3 = rgb(0, 1, 0, 0.5),
                              col4 = rgb(1, 0.65, 0, 0.5),
                              xlim = c(0, 5),
                              title = "Overlayed Histogram",
                              file_out = "hist.pdf") {
    pdf(file_out)
    par(mfrow = c(2, 2))

    # Plot 1 with RT1 and RT7

    hist(data1,
        breaks = "fd", freq = FALSE, xlim = c(0, 5),
        col = col1, border = NA,
        main = title
    )
    hist(data2,
        breaks = "fd", freq = FALSE, xlim = xlim,
        col = col2, border = NA,
        add = TRUE
    )
    # Plot 2 with RT2 and RT8
    hist(data3,
        breaks = "fd", freq = FALSE, xlim = c(0, 5),
        col = col3, border = NA,
        main = title
    )
    hist(data4,
        breaks = "fd", freq = FALSE, xlim = c(0, 5),
        col = col4, border = NA,
        add = TRUE
    )

    # Original plots 3 and 4 (unchanged)
    hist(data2,
        breaks = "fd", freq = FALSE, xlim = c(0, 5),
        col = col2, border = NA
    )

    hist(data4,
        breaks = "fd", freq = FALSE, xlim = c(0, 5),
        col = col4, border = NA
    )
    par(mfrow = c(1, 1))
    dev.off()
}



# 2 accumulators -------------
A <- 1.2
b <- 2.7
t0 <- .2

mean_v <- c(2.4, 2.2)
sd_v <- c(1, 1)

RT <- seq(0, 3, .4) + t0
posdrift <- TRUE
nv <- length(mean_v)

st0 <- 0

params_tmp <- list(
    A = rep(A, nv),
    b = rep(b, nv),
    mean_v = mean_v,
    sd_v = sd_v,
    st0 = rep(st0, nv),
    t0 = rep(t0, nv),
    is_positive_drift = rep(TRUE, nv)
)

params <- param_list2mat(params_tmp)
<<<<<<< HEAD
params
=======
>>>>>>> 41bef569990fe0630c3041017163ba2935486e74
is_positive_drift <- rep(TRUE, nv)

n <- 1
seed <- 123

dt <- 0.01
min_dt <- 0
max_dt <- 5
time_parameter_r <- c(min_dt, max_dt, dt)
<<<<<<< HEAD
time_parameter_r
=======

>>>>>>> 41bef569990fe0630c3041017163ba2935486e74

set.seed(seed)
res0 <- lbaModel::old_rlba_norm(n, A, b, mean_v, sd_v, t0, st0, posdrift)
set.seed(seed)

res1 <- lbaModel::rlba(params, is_positive_drift, time_parameter_r,
    seed = seed, debug = TRUE
)
res1

res2 <- lbaModel::rlba(params, is_positive_drift, time_parameter_r,
                       debug = TRUE
)
res2

res0_df <- data.frame(RT = res0[1, 1], R = res0[1, 2])
print(res0_df)
print(res1)

verbose <- TRUE
n <- 3000000
res0 <- lbaModel::old_rlba_norm(n, A, b, mean_v, sd_v, t0, st0, posdrift)
res1 <- lbaModel::rlba(n, params, is_positive_drift, time_parameter_r, verbose = verbose)
res2 <- lbaModel::rlba(n, params, is_positive_drift, time_parameter_r, use_inverse_method = TRUE, verbose = verbose)


cat("Proportion of R0 vs R1\n")
print(round(table(res0[, 2]) / n, 6))
print(round(table(res1[, 2]) / n, 6))
print(round(table(res2[, 2]) / n, 6))


RT1 <- res0[, 1][res0[, 2] == 1]
RT2 <- res0[, 1][res0[, 2] == 2]
RT3 <- res1[, 1][res1[, 2] == 0]
RT4 <- res1[, 1][res1[, 2] == 1]
RT5 <- res2[, 1][res2[, 2] == 0]
RT6 <- res2[, 1][res2[, 2] == 1]

plot_overlay_hist(RT1, RT3, RT2, RT4, file_out = "hist34.pdf")
plot_overlay_hist(RT1, RT5, RT2, RT5, file_out = "hist56.pdf")


# n <- 1000
# verbose <- FALSE
# times <- 1000L
# res <- microbenchmark(
#     res0 <- lbaModel::old_rlba_norm(n, A, b, mean_v, sd_v, t0, st0, posdrift),
#     res1 <- lbaModel::new_rlba(n, params, is_positive_drift, use_inverse_method = TRUE, verbose = verbose),
#     res2 <- lbaModel::new_rlba(n, params, is_positive_drift, verbose = verbose),
#     times = times
# )
# res

## Testing over a range of values ------------------
## 3 accumulators -------------

# Set up parallel processing
# cl <- makeCluster(detectCores() - 2)
# print(cl)

# registerDoParallel(cl)

# mean_v <- c(2.4, 2.2, 1.5)
# sd_v <- c(1, 1, 1.5)
# nv <- length(mean_v)
# A <- seq(0.01, 5, .1)
# b <- seq(0.01, 5, .1)
# t0 <- seq(.01, .2, .1)
# is_positive_drift <- rep(TRUE, nv)
# n <- 12
# verbose <- FALSE

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

#             res0 <- lbaModel::old_rlba_norm(n, A[i], b[j], mean_v, sd_v, t0[k], st0, posdrift)

#             res1 <- lbaModel::new_rlba(n, params, is_positive_drift, time_parameter_r, verbose = verbose)

#             res2 <- lbaModel::new_rlba(n, params, is_positive_drift, time_parameter_r,use_inverse_method = TRUE, verbose = verbose)
#         }
#     }
#     NULL
# }

# stopCluster(cl)
