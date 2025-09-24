# test-dlba-all.R
# Unit tests for theoretical_dlba and theoretical_plba

library(testthat)
library(lbaModel)

# Helper
param_list2mat <- function(param_list) {
    n_row <- length(param_list[[1]])
    n_col <- length(param_list)
    out <- matrix(NA, nrow = n_row, ncol = n_col)
    for (i in seq_len(n_col)) {
        out[, i] <- param_list[[i]]
    }
    t(out)
}

test_that("theoretical_dlba and theoretical_plba are consistent", {
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
    time_parameter_r <- c(min_dt, max_dt, dt)
    DT <- seq(min_dt, max_dt, dt)

    params <- param_list2mat(params_tmp)
    nv <- ncol(params)
    is_positive_drift <- rep(TRUE, nv)

    pdf_densities <- theoretical_dlba(params, is_positive_drift, time_parameter_r)
    cdf_densities <- theoretical_plba(params, is_positive_drift, time_parameter_r)

    # Sum of PDFs should approximate 1
    pdf_all <- pdf_densities[[1]] * dt + pdf_densities[[2]] * dt
    expect_equal(sum(pdf_all), 1, tolerance = 1e-3)

    # CDF at max time should be ~1
    cdf_all <- cdf_densities[[1]] + cdf_densities[[2]]
    expect_equal(tail(cdf_all, 1), 1, tolerance = 1e-3)

    # CDF should match cumulative PDF
    res1 <- cumsum(pdf_all)
    res2 <- cdf_all
    expect_equal(res1, res2, tolerance = 1e-3)
})
