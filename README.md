# lbaModel
lbaModel provides density, distribution and random generation functions for the Linear Ballistic Accumulator (LBA) model. Supports model specification, parameter estimation, and likelihood computation.

# Getting Started
<<<<<<< HEAD
Although the package is primarily designed to support the ggdmc framework, it can also be applied to other use cases.
=======
The package is mainly to support ggdmc, so you can use it together with other ggdmc supporting packages.
>>>>>>> 41bef569990fe0630c3041017163ba2935486e74

```
# A minimal LBA model
model <- ggdmcModel::BuildModel(
    p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1"),
    match_map = list(M = list(s1 = "r1", s2 = "r2")),
    factors = list(S = c("s1", "s2")),
    constants = c(st0 = 0, sd_v = 1),
    accumulators = c("r1", "r2"),
    type = "lba"
)
pop_mean <- c(A = .4, B = .5, mean_v.false = 0.15, mean_v.true = 2.5, t0 = 0.3)
pop_scale <- c(A = .1, B = .1, mean_v.false = .2, mean_v.true = .2, t0 = 0.05)
pop_dist <- ggdmcPrior::BuildPrior(
    p0 = pop_mean,
    p1 = pop_scale,
    lower = c(0, 0, 0, 0, 0),
    upper = rep(NA, model@npar),
    dists = rep("tnorm", model@npar),
    log_p = rep(F, model@npar)
)

ggdmcPrior::plot_prior(pop_dist)

# ---------------------------------------
sub_model <- setLBA(model)
pop_model <- setLBA(model, population_distribution = pop_dist)

p_vector <- c(A = .75, B = 1.25, mean_v.false = 1.5, mean_v.true = 2.5, t0 = .15)


# Use the simulate method in lbaModel to simulate LBA model associated with the 
# `model` design:
# One subject
dat <- simulate(sub_model, nsim = 256, parameter_vector = p_vector, n_subject = 1)

# Multiple subjects
hdat <- simulate(pop_model, nsim = 128, n_subject = 32)

```

You could use its functions with or without other ggdmc supporting packages.

```

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

```

# Prerequisites
R (>= 3.3.0), ggdmcPrior, ggdmcModel, Rcpp (>= 1.0.7), RcppArmadillo (>= 0.10.7.5.0), and ggdmcHeaders.

<<<<<<< HEAD
=======
See DESCRIPTION for details

>>>>>>> 41bef569990fe0630c3041017163ba2935486e74
# Installation

From CRAN:
```
install.packages("lbaModel")
```