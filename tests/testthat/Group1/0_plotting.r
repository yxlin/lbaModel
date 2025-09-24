# scripts/plot-lba-theoretical.R
# Plots for LBA theoretical checks:
# 1) PDF vs CDF consistency
# 2) CDF stability across different time resolutions

suppressPackageStartupMessages({
    library(lbaModel)
    library(ggplot2)
    library(gridExtra)
})

# ---------------- Helpers ----------------

param_list2mat <- function(param_list) {
    n_row <- length(param_list[[1]])
    n_col <- length(param_list)
    out <- matrix(NA, nrow = n_row, ncol = n_col)
    for (i in seq_len(n_col)) out[, i] <- param_list[[i]]
    t(out)
}

# ---------------- Common parameters ----------------

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
stopifnot(nv == 2)
is_positive_drift <- rep(TRUE, nv)

# ===================================================
# Figure 1: Theoretical PDF vs CDF consistency
# ===================================================

dt1 <- 0.01
min_dt1 <- 0
max_dt1 <- 5
time_parameter_r1 <- c(min_dt1, max_dt1, dt1)
DT1 <- seq(min_dt1, max_dt1, dt1)

pdf_densities <- theoretical_dlba(params, is_positive_drift, time_parameter_r1)
cdf_densities <- theoretical_plba(params, is_positive_drift, time_parameter_r1)

# Sum PDFs across accumulators and compare with theoretical CDF
pdf_all <- pdf_densities[[1]] * dt1 + pdf_densities[[2]] * dt1
cdf_all <- cdf_densities[[1]] + cdf_densities[[2]]
cum_pdf <- cumsum(pdf_all)

d_pdf <- data.frame(
    x = rep(DT1, 2),
    y = c(pdf_densities[[1]] * dt1, pdf_densities[[2]] * dt1),
    gp = rep(c("Accumulator 1 (PDF*dt)", "Accumulator 2 (PDF*dt)"), each = length(DT1))
)

d_cdf <- data.frame(
    x = rep(DT1, 2),
    y = c(cum_pdf, cdf_all),
    gp = rep(c("Cumulated PDF", "CDF (sum over accumulators)"), each = length(DT1))
)

p_pdf <- ggplot(d_pdf, aes(x = x, y = y, colour = gp)) +
    geom_line() +
    labs(
        x = "Time", y = "Mass", colour = NULL,
        title = "Figure 1a. Theoretical PDFs (per-accumulator) × dt"
    ) +
    theme_minimal(base_size = 14)

p_cdf <- ggplot(d_cdf, aes(x = x, y = y, colour = gp)) +
    geom_line() +
    labs(
        x = "Time", y = "CDF", colour = NULL,
        title = "Figure 1b. Cumulated PDF vs Theoretical CDF"
    ) +
    theme_minimal(base_size = 14)

png("theoretical_pdf_cdf.png", width = 1100, height = 520)
grid.arrange(p_pdf, p_cdf, ncol = 2)
dev.off()

# ===================================================
# Figure 2: CDF stability across time resolutions
# ===================================================

# Evaluation times (RT) used for plba() comparisons
RT <- seq(0, 3, 0.001) + params["t0", 1]

# Three time grid settings
grids <- list(
    list(dt = 0.0001, min_dt = 0, max_dt = 10),
    list(dt = 0.1, min_dt = 0, max_dt = 5),
    list(dt = 0.2, min_dt = 0, max_dt = 5)
)

# Compute plba for each grid
cdf_list <- lapply(grids, function(g) {
    tp <- c(g$min_dt, g$max_dt, g$dt)
    plba(RT, params, is_positive_drift, tp)
})

# Build a tidy data frame: rows for each dt, accumulator, and RT
build_cdf_df <- function(RT, cdf_list, grids) {
    out <- list()
    for (i in seq_along(cdf_list)) {
        acc1 <- cdf_list[[i]][[1]]
        acc2 <- cdf_list[[i]][[2]]
        df_i <- rbind(
            data.frame(RT = RT, CDF = acc1, Accumulator = "Acc1", dt = grids[[i]]$dt),
            data.frame(RT = RT, CDF = acc2, Accumulator = "Acc2", dt = grids[[i]]$dt)
        )
        out[[i]] <- df_i
    }
    do.call(rbind, out)
}

d_res <- build_cdf_df(RT, cdf_list, grids)

# Plot CDFs for each accumulator with color = dt
p_cdf_acc1 <- ggplot(
    subset(d_res, Accumulator == "Acc1"),
    aes(x = RT, y = CDF, colour = factor(dt))
) +
    geom_line() +
    labs(
        x = "RT", y = "CDF", colour = "dt",
        title = "Figure 2a. Accumulator 1: CDF vs time resolution"
    ) +
    theme_minimal(base_size = 14)

p_cdf_acc2 <- ggplot(
    subset(d_res, Accumulator == "Acc2"),
    aes(x = RT, y = CDF, colour = factor(dt))
) +
    geom_line() +
    labs(
        x = "RT", y = "CDF", colour = "dt",
        title = "Figure 2b. Accumulator 2: CDF vs time resolution"
    ) +
    theme_minimal(base_size = 14)

png("theoretical_cdf_resolution.png", width = 1100, height = 520)
grid.arrange(p_cdf_acc1, p_cdf_acc2, ncol = 2)
dev.off()

message("Saved: theoretical_pdf_cdf.png, theoretical_cdf_resolution.png")
