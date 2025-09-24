# q(save = "no")
cat("\n\n---------- Testing rlba ----------")
rm(list = ls())
pkg <- c("lbaModel", "rtdists")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")


# test-rlba.R
# Unit tests for rlba random sampling

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

test_that("rlba is reproducible with a fixed seed", {
    # Parameters for 2 accumulators
    A <- 1.2
    b <- 2.7
    t0 <- 0.2
    mean_v <- c(2.4, 2.2)
    sd_v <- c(1, 1)
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
    is_positive_drift <- rep(TRUE, nv)

    # Simulation settings
    n <- 1000
    seed <- 123
    time_parameter_r <- c(0, 5, 0.01)

    set.seed(seed)
    res1 <- lbaModel::rlba(params, is_positive_drift, time_parameter_r, n, seed = seed)
    set.seed(seed)
    res2 <- lbaModel::rlba(params, is_positive_drift, time_parameter_r, n, seed = seed)

    expect_equal(res1, res2)
})

test_that("rlba inverse method gives similar results to direct method", {
    # Parameters for 2 accumulators
    A <- 1.2
    b <- 2.7
    t0 <- 0.2
    mean_v <- c(2.4, 2.2)
    sd_v <- c(1, 1)
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
    is_positive_drift <- rep(TRUE, nv)

    # Simulation settings
    n <- 5000
    time_parameter_r <- c(0, 5, 0.01)

    set.seed(456)
    res_direct <- lbaModel::rlba(params, is_positive_drift, time_parameter_r, n)
    set.seed(456)
    res_inverse <- lbaModel::rlba(params, is_positive_drift, time_parameter_r, n, use_inverse_method = TRUE)

    # Compare response proportions (not exact samples, but distribution)
    prop_direct <- table(res_direct[, 2]) / n
    prop_inverse <- table(res_inverse[, 2]) / n

    expect_equal(as.numeric(prop_direct), as.numeric(prop_inverse), tolerance = 0.05)
})
