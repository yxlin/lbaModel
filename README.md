# lbaModel

`lbaModel` provides fast and flexible tools for working with the **Linear Ballistic Accumulator (LBA)** model, a widely used model in cognitive psychology for simulating and analysing response time (RT) and choice data.

The package supports:

- ✅ Likelihood computation  
- ✅ Parameter estimation  
- ✅ Data simulation  
- ✅ Probability density and cumulative distribution evaluation  

While designed to be part of the [`ggdmc`](https://cran.r-project.org/package=ggdmc) ecosystem, `lbaModel` is also fully functional as a standalone package.

📖 For a mathematical description of the minimal LBA model, see:

> Brown, S. D., & Heathcote, A. (2008).  
> *The simplest complete model of choice response time: Linear ballistic accumulation*.  
> *Cognitive Psychology*, 57(3), 153–178. https://doi.org/10.1016/j.cogpsych.2007.12.002  

🆕 For a tutorial and practical guidance on how to set up a design-based LBA model, see:

> Lin, Y.-S., & Strickland, L. (2020).  
> *Evidence accumulation models with R: A practical guide to hierarchical Bayesian methods*.  
> *The Quantitative Methods for Psychology*, 16(2), 133–149. https://doi.org/10.20982/tqmp.16.2.p133

---

## 🚀 Getting Started

### Minimal LBA model example

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
Use theoretical_dlba() and theoretical_plba() to compute and visualise theoretical PDF and CDF values:

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
You can install `lbaModel` either from CRAN (recommended) or from the development version on GitHub.

### From CRAN (Recommended)

```r
install.packages("lbaModel")

```

### From GitHub (Development Version)
> ⚠️ This may require installing development tools and many unrelated dependencies.

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

## 📚 Citation

If you use `lbaModel` in your research, please cite:

- Brown & Heathcote (2008): https://doi.org/10.1016/j.cogpsych.2007.12.002
- Lin & Strickland (2020): https://doi.org/10.20982/tqmp.16.2.p133


## 🔄 Similar R Packages

`lbaModel` is part of a growing ecosystem of tools for modelling decision processes using the Linear Ballistic Accumulator (LBA) framework. Several other R packages also support LBA-related computations:

- [`glba`](https://cran.r-project.org/package=glba): Provides generalised LBA model fitting and simulation using maximum likelihood estimation.
- [`rtdists`](https://cran.r-project.org/package=rtdists): Offers density, distribution, and random generation functions for several response time models, including the LBA and diffusion decision models.

While these packages are valuable in their own right, `lbaModel` offers several distinguishing features:

- Integration with the `ggdmc` framework for hierarchical Bayesian modelling.
- Full support for design-based parameter mapping in factorial experimental designs.
- Fast CPU-accelerated simulation capabilities through its C++ backend—when used alongside `ggdmcLikelihood` and its high-performance extension `pLBA`, implemented in the `pPDA` package.

## 🤝 Contributing
Contributions are welcome! Please feel free to submit issues, fork the repo, or open pull requests.

## 📬 Contact
Maintainer: Yi-Shin Lin
📧 yishinlin001@gmail.com
