# q(save = "no")
cat("\n\n---------- Testing dlba_all ----------")
rm(list = ls())

pkg <- c("lbaModel", "microbenchmark", "ggplot2")
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

# pdf_densities[[1]]
# round(cumsum(pdf_all), 2)
# sum(pdf_all)

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

png("theoretical_pdf_cdf.png")
gridExtra::grid.arrange(p0, p1)
dev.off()
