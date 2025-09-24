# q(save = "no")
cat("\n\n-------------------- Testing Node 1 PDF --------------------")
rm(list = ls())
pkg <- c("lbaModel")
suppressPackageStartupMessages(tmp <- sapply(pkg, require, character.only = TRUE))
cat("\nWorking directory: ", getwd(), "\n")

# test-n1pdf.R
# Unit tests for n1PDF implementation in lbaModel

# Helper: convert parameter list to matrix
param_list2mat <- function(param_list) {
    n_row <- length(param_list[[1]])
    n_col <- length(param_list)
    tmp <- matrix(NA, nrow = n_row, ncol = n_col)
    for (i in seq_len(n_col)) {
        tmp[, i] <- param_list[[i]]
    }
    t(tmp)
}

test_that("n1PDF matches rtdists::n1PDF for two accumulators", {
    # Parameters
    A <- 1.2
    b <- 2.7
    t0 <- 0.2
    mean_v <- c(2.4, 2.2)
    sd_v <- c(1, 1)
    RT <- seq(0, 3, 0.4) + t0
    nv <- length(mean_v)
    is_positive_drift <- rep(TRUE, nv)

    # Reference (rtdists)
    ref <- rtdists::n1PDF(
        RT,
        A = rep(A, nv),
        b = rep(b, nv),
        t0 = rep(t0, nv),
        mean_v = mean_v,
        sd_v = sd_v,
        silent = TRUE
    )

    # Test target (lbaModel)
    params_tmp <- list(
        A = rep(A, nv),
        b = rep(b, nv),
        mean_v = mean_v,
        sd_v = sd_v,
        st0 = rep(0, nv),
        t0 = rep(t0, nv)
    )
    params <- param_list2mat(params_tmp)

    target <- lbaModel::n1PDF(RT, params, is_positive_drift, TRUE)

    # Check equality
    expect_equal(ref, target, tolerance = 1e-6)
})



test_that("n1PDF matches rtdists::n1PDF for three accumulators", {
    # Parameters
    A <- 1.2
    b <- 2.7
    t0 <- 0.2
    mean_v <- c(2.4, 2.2, 1.5)
    sd_v <- c(1, 1, 1.5)
    RT <- seq(0, 3, 0.4) + t0
    nv <- length(mean_v)
    is_positive_drift <- rep(TRUE, nv)

    # Reference (rtdists)
    ref <- rtdists::n1PDF(
        RT,
        A = rep(A, nv),
        b = rep(b, nv),
        t0 = rep(t0, nv),
        mean_v = mean_v,
        sd_v = sd_v,
        silent = TRUE
    )

    # Target (lbaModel)
    params_tmp <- list(
        A = rep(A, nv),
        b = rep(b, nv),
        mean_v = mean_v,
        sd_v = sd_v,
        st0 = rep(0, nv),
        t0 = rep(t0, nv)
    )
    params <- param_list2mat(params_tmp)

    target <- lbaModel::n1PDF(RT, params, is_positive_drift, TRUE)

    # Check equality
    expect_equal(ref, target, tolerance = 1e-6)
})
