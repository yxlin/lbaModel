## Test environments
- local macOS (Apple clang), R 4.4.1
- Ubuntu 22.04 (GCC), R 4.4.1 (GitHub Actions)
- Windows Server 2022 (Rtools), R 4.4.1 (GitHub Actions)
- win-builder (devel, release)

## R CMD check results
0 errors | 0 warnings | 1 note

* Note: New submission.

## Summary
`ddModel` provides fast R/C++ implementations for the Diffusion Decision Model (DDM):
- density, distribution, random sampling, and likelihood evaluation
- vectorised functions for large datasets
- optional integration with the `ggdmc` ecosystem for hierarchical inference

## Dependencies & System requirements
- Imports: Rcpp (>= 1.0.7), RcppArmadillo (>= 0.10.7.5.0)
- SystemRequirements: C++17
- Suggests: ggdmc, ggdmcModel, ggdmcPrior (examples that use these are wrapped in
  `if (requireNamespace("ggdmcModel", quietly = TRUE)) { ... }` etc.)

## Examples & Run time
- Examples are short and self-contained (no I/O, no network, no parallelism).
- Typical runtime < 5 seconds on win-builder.
- All randomness uses local `set.seed()` and does not modify global RNGKind.

## URLs / DOIs
- All URLs verified with `R CMD check --as-cran`.
- DOIs formatted as `https://doi.org/...`.

## Data / Size
- No large data files in the tarball.
- README images are under `man/figures/` and small (<1MB).

## Native code
- C++ via Rcpp/RcppArmadillo only.
- Compiles with CRAN’s default toolchains on all platforms.
- No OpenMP and no non-portable flags.

## Notes for reviewers
- This is a first submission. There are no reverse dependencies yet.
- The package does not write to user directories and uses `tempdir()` where needed.
- No internet access at build or run time.


- First submission of lbaModel—R + C++ tools for the Linear Ballistic Accumulation Model. 
- Tested on Windows and Fedora Linux 38 [R Under Development (Unstable) (2025-09-02 r88773)] and macOS-arm64 (GitHub Action via rhub).
- Uses Rcpp/RcppArmadillo; System Requirements: C++17.
- Locally check, R CMD check: 0 errors | 0 warnings | 0 notes (new submission).

Fix the significant warnings: "Using fallback compilation with Armadillo 14.6.3. Please consider defining -DARMA_USE_CURRENT." by adding "PKG_CPPFLAGS = -DARMA_USE_CURRENT" in src/Makevars.

The examples in the help document, 'lba_distributions' in the file, lbaModel.R
were replaced by "@examplesIf interactive()...", as in

#' @examplesIf interactive()
#' {
#'   oldpar <- par(no.readonly = TRUE)
#'   on.exit(par(oldpar), add = TRUE)
#'   ...
#'
#'   par(mfrow = c(2, 1), mar = c(5, 5, 2, 1))
#'   # PDF
#'   plot(DT, pdfs2[[1]] * dt2,
#'     type = "l",
#'     xlab = "DT", ylab = "Density", main = "Theoretical PDF"
#'   )
#' ...
#'
#'   par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
#'   plot(RT2, c1[[1]],
#'     type = "l", ylim = c(0, 1), xlab = "RT", ylab = "CDF",
#'     main = "LBA CDF Estimates under Different Time Grids", lwd = 2,
#'     col = col1
#'   )
#' ...
#' }

to reset the par() back to user's options().

