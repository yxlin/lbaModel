# 📦 lbaModel

<!-- Badges -->
[![CRAN Status](https://www.r-pkg.org/badges/version/lbaModel)](https://cran.r-project.org/package=lbaModel)
[![Downloads](https://cranlogs.r-pkg.org/badges/lbaModel)](https://cran.r-project.org/package=lbaModel)
[![License: GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/yxlin/lbaModel/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yxlin/lbaModel/actions/workflows/R-CMD-check.yaml)

## 🧠 Overview
`lbaModel` provides **fast and flexible tools** for working with the **Linear Ballistic Accumulator (LBA)** model, a widely used framework in cognitive psychology and neuroscience for simulating and analysing choice and response time (RT) data.

Key features:

- ⚡ **Likelihood computation** for parameter estimation
- 🎯 **Data simulation** at the subject or population level
- 📊 **Probability density (PDF) & cumulative distribution (CDF)** evaluation
- 🔗 **Design-based parameter mapping** for factorial experiments
- 💻 **C++ backend** for high-performance computation

While designed to be part of the [`ggdmc`](https://cran.r-project.org/package=ggdmc) ecosystem, `lbaModel` is also fully functional as a standalone package.

## 📐 Conceptual Illustration
Here’s a schematic of the **LBA model** showing accumulators racing to threshold (similar to diffusion models, but with linear deterministic growth.)

![Conceptual illustration of the LBA Model](man/figures/lba_schematic_t0_A.svg)

*Figure: The LBA model assumes evidence accumulates linearly and independently across choices, with start points, drawing from a uniform distribution and drift rates. The first accumulator to hit its threshold determines the response and response time.*


## 🚀 Getting Started

### Example: Minimal LBA Model 

```r
library(ggdmcModel)
library(ggdmcPrior)

model <- BuildModel(
  p_map = list(A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1", st0 = "1"),
  match_map = list(M = list(s1 = "r1", s2 = "r2")),
  factors = list(S = c("s1", "s2")),
  constants = c(st0 = 0, sd_v = 1),
  accumulators = c("r1", "r2"),
  type = "lba"
)

# Set population-level prior
pop_mean <- c(A = 0.4, B = 0.5, mean_v.false = 0.15, mean_v.true = 2.5, t0 = 0.3)
pop_scale <- c(A = 0.1, B = 0.1, mean_v.false = 0.2, mean_v.true = 0.2, t0 = 0.05)

pop_dist <- BuildPrior(
  p0 = pop_mean,
  p1 = pop_scale,
  lower = c(0, 0, 0, 0, 0),
  upper = rep(NA, length(pop_mean)),
  dists = rep("tnorm", length(pop_mean)),
  log_p = rep(FALSE, length(pop_mean))
)

plot_prior(pop_dist)
```

### Simulating Data

```r
sub_model <- setLBA(model)
pop_model <- setLBA(model, population_distribution = pop_dist)

# One subject
p_vector <- c(A = 0.75, B = 1.25, mean_v.false = 1.5, mean_v.true = 2.5, t0 = 0.15)
dat <- simulate(sub_model, nsim = 256, parameter_vector = p_vector, n_subject = 1)

# Multiple subjects
hdat <- simulate(pop_model, nsim = 128, n_subject = 32)
```

### 📊 Visualising LBA Densities

```r
# Parameters
params_tmp <- list(
  A = c(0.5, 0.5),
  b = c(1.0, 1.0),
  mean_v = c(2.0, 1.0),
  sd_v = c(1.0, 1.0),
  st0 = c(0.0, 0.0),
  t0 = c(0.2, 0.2)
)

# Convert to matrix
param_list2mat <- function(param_list) {
  n_row <- length(param_list[[1]])
  n_col <- length(param_list)
  out <- matrix(NA, nrow = n_row, ncol = n_col)
  for (i in seq_len(n_col)) out[, i] <- param_list[[i]]
  t(out)
}

params <- param_list2mat(params_tmp)
time_params <- c(0, 5, 0.01)
nv <- ncol(params)
is_pos <- rep(TRUE, nv)

pdfs <- theoretical_dlba(params, is_pos, time_params)
cdfs <- theoretical_plba(params, is_pos, time_params)
```

## 📦 Installation
### From CRAN (Recommended)

```r
install.packages("lbaModel")
```

### From GitHub (Development Version)
> ⚠️ Requires development tools and extra dependencies.

```r
# install.packages("devtools")
devtools::install_github("yxlin/lbaModel")
```

## 🔧 Dependencies
- R (≥ 3.3.0)
- ggdmcPrior
- ggdmcModel
- Rcpp (≥ 1.0.7)
- RcppArmadillo (≥ 0.10.7.5.0)
- ggdmcHeaders

## 📚 Reference

If you use `lbaModel`, please cite:

- Brown & Heathcote (2008). The simplest complete model of choice response time: Linear ballistic accumulation. _Cognitive Psychology_, 57(3), 153–178.: [https://doi.org/10.1016/j.cogpsych.2007.12.002](https://doi.org/10.1016/j.cogpsych.2007.12.002)
- Lin & Strickland (2020). Evidence accumulation models with R: A practical guide to hierarchical Bayesian methods. _The Quantitative Methods for Psychology_, 16(2), 133–149.
[https://doi.org/10.20982/tqmp.16.2.p133](https://doi.org/10.20982/tqmp.16.2.p133)


## 🔄 Related R Packages


- [`glba`](https://cran.r-project.org/package=glba): Generalised LBA model fitting via MLE
- [`rtdists`](https://cran.r-project.org/package=rtdists): Density/distribution
for LBA and diffusion models.

### Why `lbaModel`?

- Tight integration with `ggdmc` for hierarchical Bayesian inference
- Full support for **design-based parameter mapping** in factorial designs
- Optimised **C++ backend** with extensions (`ggdmcLikelihood`, `pPDA`) for high-performance parallelised LBA

## 🤝 Contributing
Contributions are welcome! Please feel free to submit issues, fork the repo, or open pull requests.

## 📬 Contact
- Maintainer: Yi-Shin Lin
- 📧 yishinlin001@gmail.com

