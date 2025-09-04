#' The Linear Ballistic Accumulaion
#'
#' Provides density, distribution and random generation functions for the
#' Linear Ballistic Accumulator (LBA) model, a widely used choice response
#' time model in cognitive psychology. The package supports model
#' specifications, parameter estimation, and likelihood computation,
#' facilitating simulation and statistical inference for LBA-based experiments.
#' For background on the LBA model, see Brown and Heathcote (2008)
#' <doi:10.1016/j.cogpsych.2007.12.002>.
#'
#' @keywords package
#'
#' @name lbaModel
#' @keywords internal
#' @author  Yi-Shin Lin <yishinlin001@gmail.com> \cr
#' @importFrom Rcpp evalCpp
#' @useDynLib lbaModel
"_PACKAGE"
NULL

#' @name First_Passage_Time
#' @title First_Passage_Time
#'
#' @description
#' The functions, \code{fptpdf}, \code{fptcdf}, and \code{n1PDF}, calculate
#' first passage time distributions for Linear Ballistic Accumulation models.
#' This includs the probability density function \code{fptpdf}, the
#' cumulative distribution function \code{fptcdf}, and the node 1 density
#' function, \code{n1PDF}.
#'
#' @param rt_r A numeric vector of response times (RTs).
#' @param parameter_r A numeric matrix of model parameters, with each row
#' representing a core model parameter and each column representing an
#' accumulator. Expected row (in fixed order): \code{A}, \code{b},
#' \code{mean_v}, \code{sd_v}, \code{st0}, and \code{t0}.
#' @param is_positive_drift_r A logical vector indicating whether the drift
#' rate must be positive for each trial.
#' @param verbose A logical value indicating whether to print debug information.
#'
#' @details The three functions, respectively, are designed to:
#' \itemize{
#'   \item \code{fptpdf}: Calculates the probability density function for
#'                        (PDF; 1 accumulator).
#'   \item \code{fptcdf}: Calculates the cumulative distribution function
#'                        (CDF; 1 accumulator).
#'   \item \code{n1PDF}: Calculates the node 1 PDF for the multi-accumulator
#'                       LBA model freeing the non-decision time. 'node 1'
#' refers to the accumulator passing the threshold first.
#' }
#'
#' @return A numeric vector of probabilities corresponding to input response
#' times.
#'
#' @examples
#'
#' ##################
#' # n1PDF example
#' ##################
#' if (requireNamespace("ggdmcModel", quietly = TRUE)) {
#'   BuildModel <- getFromNamespace("BuildModel", "ggdmcModel")
#'
#'   model <- BuildModel(
#'     p_map = list(
#'       A = "1", B = "1", t0 = "1", mean_v = "M", sd_v = "1",
#'       st0 = "1"
#'     ),
#'     match_map = list(M = list("s1" = "r1", "s2" = "r2")),
#'     factors = list(S = c("s1", "s2")),
#'     constants = c(sd_v = 1, st0 = 0),
#'     accumulators = c("r1", "r2"),
#'     type = "lba"
#'   )
#' }
#'
#' #---------------------------------------
#' pop_mean <- c(
#'   A = .4, B = 1.6, mean_v.true = 3.5, mean_v.false = 2.38,
#'   t0 = 0.05
#' )
#' pop_scale <- c(
#'   A = 2, B = .2, mean_v.true = .25, mean_v.false = .1,
#'   t0 = 0.01
#' )
#'
#' if (requireNamespace("ggdmcPrior", quietly = TRUE)) {
#'   BuildPrior <- getFromNamespace("BuildPrior", "ggdmcPrior")
#'
#'   pop_dist <- BuildPrior(
#'     p0 = pop_mean,
#'     p1 = pop_scale,
#'     lower = rep(NA, model@npar),
#'     upper = rep(NA, model@npar),
#'     dists = rep("tnorm", model@npar),
#'     log_p = rep(FALSE, model@npar)
#'   )
#' }
#'
#' #---------------------------------------
#' rt_model <- setLBA(model, population_distribution = pop_dist)
#' res_process_model <- simulate(rt_model)
#' res_inverse_method <- simulate(rt_model, use_inverse_method = TRUE)
#'
#' param_list2mat <- function(param_list) {
#'   n_row <- length(param_list[[1]])
#'   n_col <- length(param_list)
#'
#'   tmp <- matrix(NA, nrow = n_row, ncol = n_col)
#'
#'   for (i in seq_len(n_col)) {
#'     tmp[, i] <- param_list[[i]]
#'   }
#'   t(tmp)
#' }
#'
#' params_tmp <- list(
#'   A = c(0.74, 0.74),
#'   b = c(1.25 + 0.74, 1.25 + 0.74),
#'   mean_v = c(2.52, 1.50),
#'   sd_v = c(1.0, 1.0),
#'   st0 = c(0.0, 0.0),
#'   t0 = c(0.04, 0.04)
#' )
#'
#' # Store the LBA parameter as a list of vectors
#' print(params_tmp)
#' # $A: 0.74 0.74
#' # $b: 1.99 1.99
#' # $mean_v: 2.52 1.50
#' # $sd_v: 1 1
#' # $st0: 0 0
#' # $t0: 0.04 0.04
#'
#' # Convert it to a matrix, each row represents a parameter,
#' # and each column corresponds to an accumulator.
#' params <- param_list2mat(params_tmp)
#'
#' print(params)
#' #      [,1] [,2]
#' # [1,] 0.74 0.74
#' # [2,] 1.99 1.99
#' # [3,] 2.52 1.50
#' # [4,] 1.00 1.00
#' # [5,] 0.00 0.00
#' # [6,] 0.04 0.04
#'
#' n_acc <- 2
#' is_positive_drift <- rep(TRUE, n_acc)
#' res <- lbaModel::n1PDF(res_process_model$RT, params, is_positive_drift, TRUE)
#' cat("Print first 30 density values from the LBA node 1 function: \n")
#' print(head(round(res, 2), 30))
#'
#' #############################
#' # fptpdf computes only 1 RT
#' #############################
#' mean_v <- 2.4
#' A <- 1.2
#' b <- 2.7
#' t0 <- .2
#' sd_v <- 1
#' st0 <- 0
#' positive_drift_r <- TRUE
#'
#' params <- list(
#'   A = rep(A, 2),
#'   b = rep(b, 2),
#'   mean_v = rep(mean_v, 2),
#'   sd_v = rep(sd_v, 2),
#'   st0 = rep(st0, 2),
#'   t0 = rep(t0, 2)
#' )
#'
#' n_accumulator <- length(params$A)
#' is_positive_drift <- rep(TRUE, n_accumulator)
#' params_mat <- param_list2mat(params)
#'
#' RT <- 0.3
#' result <- fptpdf(RT, params_mat, is_positive_drift, TRUE)
#'
#' ########################################
#' # fptpdf and fptcdf computes a range RTs
#' ########################################
#' RT <- seq(0, 10, .01) + t0
#' result1 <- fptpdf(RT, params_mat, is_positive_drift)
#' result2 <- fptcdf(RT, params_mat, is_positive_drift)
NULL


#' @name lba_distributions
#' @title LBA Distribution Functions
#'
#' @description The functions, \code{dlba}, \code{plba},
#' \code{theoretical_dlba}, and \code{theoretical_plba}, calculate probability
#' distributions for the Linear Ballistic Accumulator model:
#' \itemize{
#'   \item 'dlba' - Probability density function (PDF) for observed
#' choice response times
#'   \item 'plba' - Cumulative distribution function (CDF) for observed
#' choice response times
#'   \item 'theoretical_dlba' - Theoretical density function sets a
#' time range and computes the density for this range.
#'   \item `theoretical_plba' - Theoretical cumulative distribution function
#' sets a time range and computes the cumulative density for this range.
#' }
#'
#' @param rt_r For 'dlba' and 'plba': A numeric vector of observed response
#' times
#' @param parameter_r A numeric matrix of parameters where rows represent:
#' \itemize{
#'   \item Starting point variability (A)
#'   \item Thresholds (b)
#'   \item Mean drift rates (mean_v)
#'   \item The standard deviation of the drift rates (sd_v)
#'   \item The variability of the non-decision time (st0)
#'   \item Non-decision time (t0).}
#' Each column represents parameters for an accumulator.
#'
#' @param is_positive_drift_r A logical vector indicating whether drift rates
#' are strictly positive
#' @param time_parameter_r For theoretical functions, a numeric vector to set
#' minimal  decison time, maximal decison time, and the different time. The
#' internal mechanism uses this vector to set fine time points for evaluation.
#'
#' @return For all functions: A list containing:
#' \itemize{
#'   \item `values` - The computed distribution values
#'   \item `time_points` - The time points used (for theoretical functions
#' and 'plba')
#'   \item Additional diagnostic information when applicable
#' }
#'
#' @details
#' These functions provide the computational foundation for the LBA model:
#' \describe{
#'   \item{`dlba`}{Computes the probability density for observed response times,
#'   used during model fitting and likelihood calculations}
#'   \item{`plba`}{Computes the cumulative probability for observed response
#' times, useful for model validation and goodness-of-fit tests}
#'   \item{`theoretical_dlba`}{Computes the theoretical PDF for diagnostic
#' purposes and prediction}
#'   \item{`theoretical_plba`}{Computes the theoretical CDF for model
#' validation and comparison with empirical data}
#' }
#'
#' @examples
#' ###################################################
#' # Example 1: theoretical_dlba and theoretical_plba
#' ###################################################
#'
#' param_list2mat <- function(param_list) {
#'   n_row <- length(param_list[[1]])
#'   n_col <- length(param_list)
#'   out <- matrix(NA, nrow = n_row, ncol = n_col)
#'
#'   for (i in seq_len(n_col)) {
#'     out[, i] <- param_list[[i]]
#'   }
#'   t(out)
#' }
#'
#' params_tmp <- list(
#'   A = c(0.5, 0.5),
#'   b = c(1.0, 1.0),
#'   mean_v = c(2.0, 1.0),
#'   sd_v = c(1.0, 1.0),
#'   st0 = c(0.0, 0.0),
#'   t0 = c(0.2, 0.2)
#' )
#'
#' dt <- 0.01
#' min_dt <- 0
#' max_dt <- 5
#' DT <- seq(min_dt, max_dt, dt)
#'
#' time_parameter_r <- c(min_dt, max_dt, dt)
#'
#' params <- param_list2mat(params_tmp)
#' nv <- ncol(params)
#' is_positive_drift <- rep(TRUE, nv)
#'
#' pdf_densities <- theoretical_dlba(
#'   params, is_positive_drift, time_parameter_r
#' )
#'
#'
#' cdf_densities <- theoretical_plba(
#'   params, is_positive_drift, time_parameter_r
#' )
#'
#'
#' pdf_all <- pdf_densities[[1]] * dt + pdf_densities[[2]] * dt
#' cdf_all <- cdf_densities[[1]] + cdf_densities[[2]]
#' res1 <- cumsum(pdf_all)
#' res2 <- cdf_all
#'
#' res3 <- res1[length(pdf_all)]
#' res4 <- cdf_all[length(cdf_all)]
#' cat("The cumulative densities, [PDF, CDF] = [", res3, ", ", res4, "]\n")
#'
#' d_pdf <- data.frame(
#'   x = rep(DT, 2), y = c(
#'     pdf_densities[[1]] * dt,
#'     pdf_densities[[2]] * dt
#'   ),
#'   gp = rep(c("Accumulator 1", "Accumulator 2"), each = length(DT))
#' )
#'
#' d_cdf <- data.frame(
#'   x = rep(DT, 2), y = c(res1, res2),
#'   gp = rep(c("Cumulated PDF", "CDF"), each = length(DT))
#' )
#' # Compute theoretical densities
#' pdf_densities <- theoretical_dlba(
#'   params, is_positive_drift,
#'   time_parameter_r
#' )
#' cdf_densities <- theoretical_plba(
#'   params, is_positive_drift,
#'   time_parameter_r
#' )
#' pdf_all <- pdf_densities[[1]] * dt + pdf_densities[[2]] * dt
#' cdf_all <- cdf_densities[[1]] + cdf_densities[[2]]
#' res1 <- cumsum(pdf_all)
#' res2 <- cdf_all
#' res3 <- res1[length(pdf_all)]
#' res4 <- cdf_all[length(cdf_all)]
#' cat("The cumulative densities, [PDF, CDF] = [", res3, ", ", res4, "]\n")
#'
#' par(mfrow = c(2, 1), mar = c(5, 5, 2, 1))
#' # PDF plot
#' plot(DT, pdf_densities[[1]] * dt,
#'   type = "l", col = "blue",
#'   ylim = c(0, max(pdf_all)), xlab = "DT", ylab = "Density",
#'   main = "Theoretical PDF"
#' )
#'
#' lines(DT, pdf_densities[[2]] * dt, col = "red")
#' legend("topright",
#'   legend = c("Accumulator 1", "Accumulator 2"),
#'   col = c("blue", "red"), lty = 1, bty = "n"
#' )
#'
#' # CDF plot
#' plot(DT, res1,
#'   type = "l", col = "blue", ylim = c(0, 1),
#'   xlab = "DT", ylab = "Cumulative", main = "Cumulated PDF and CDF"
#' )
#' lines(DT, res2, col = "red")
#' legend("bottomright",
#'   legend = c("Cumulated PDF", "CDF"),
#'   col = c("blue", "red"), lty = 1, bty = "n"
#' )
#'
#' ###################################################
#' # Example 2: plba (Base R Plotting)
#' ###################################################
#' # Parameter setup
#' params_tmp <- list(
#'   A = c(0.5, 0.5),
#'   b = c(1.0, 1.0),
#'   mean_v = c(2.0, 1.0),
#'   sd_v = c(1.0, 1.0),
#'   st0 = c(0.0, 0.0),
#'   t0 = c(0.2, 0.2)
#' )
#' params <- param_list2mat(params_tmp)
#' nv <- ncol(params)
#' is_positive_drift <- rep(TRUE, nv)
#' # Response times (shifted by non-decision time)
#' RT <- seq(0, 3, 0.001) + params[6, 1]
#' # Different dt setups
#' time_param1 <- c(0, 10, 0.01)
#' time_param2 <- c(0, 5, 0.1)
#' time_param3 <- c(0, 5, 0.2)
#' cdf1 <- plba(RT, params, is_positive_drift, time_param1)
#' cdf2 <- plba(RT, params, is_positive_drift, time_param2)
#' cdf3 <- plba(RT, params, is_positive_drift, time_param3)
#'
#' par(mfrow = c(1, 1), mar = c(5, 5, 2, 2))
#' plot(RT, cdf1[[1]],
#'   type = "l", ylim = c(0, 1), xlab = "RT", ylab = "CDF",
#'   main = "LBA CDF Estimates under Different Time Grids", col = "black",
#'   lwd = 2
#' )
#' lines(RT, cdf1[[2]], col = "black", lwd = 2, lty = 2)
#' lines(RT, cdf2[[1]], col = "red", lwd = 2)
#' lines(RT, cdf2[[2]], col = "red", lwd = 2, lty = 2)
#' lines(RT, cdf3[[1]], col = "blue", lwd = 2)
#' lines(RT, cdf3[[2]], col = "blue", lwd = 2, lty = 2)
#' legend("bottomright",
#'   legend = c(
#'     "Acc1 (dt=0.0001)", "Acc2 (dt=0.0001)",
#'     "Acc1 (dt=0.1)", "Acc2 (dt=0.1)",
#'     "Acc1 (dt=0.2)", "Acc2 (dt=0.2)"
#'   ),
#'   col = c("black", "black", "red", "red", "blue", "blue"),
#'   lty = c(1, 2, 1, 2, 1, 2), lwd = 2, bty = "n"
#' )
NULL


#' Density Function for the LBA Model Using Inverse Transform Method
#'
#' Computes the probability density of response times for the Linear
#' Ballistic Accumulator (LBA) model using an inverse transform formulation.
#'
#' @param rt_r A numeric vector of response times (RTs) for which the density
#' should be computed.
#' @param response_r An integer vector of response choices (indices of winning
#' accumulators), same length as \code{rt_r}.
#' @param parameter_r A numeric matrix of LBA parameters. Each column
#' corresponds to an accumulator, and each row corresponds to a parameter,
#' with the expected order being:
#' \itemize{
#'   \item \code{A} - Starting point variability
#'   \item \code{b} - Thresholds
#'   \item \code{mean_v} - Mean drift rates
#'   \item \code{sd_v} - Standard deviation drift rates
#'   \item \code{st0} - Variability of the non-decision
#'   \item \code{t0} - Non-decision time
#' }
#' @param is_positive_drift_r A logical vector indicating whether the drift
#' rate must be positive for each trial.
#' @param time_parameter_r A numeric vector to set the simulated time grid,
#' with the expected value for: minimal and maximum decision times and the
#' difference time (i.e., \code{min dt}, \code{max dt} and \code{dt}).
#'
#' @return A numeric vector of LBA densities, one per trial.
#'
#' @details
#' This function is a lower-level computational routine intended to be used
#' inside higher-level LBA model evaluation or fitting functions. It uses the
#' inverse transform method to compute exact LBA densities in a numerically
#' stable and efficient way. The function assumes consistent input dimensions
#' across all arguments.
#'
#' @name dlba_inverse_external
#' @keywords internal
#' @export
NULL

#' Simulate Choices and Response Times
#'
#' @param parameter_r A numeric matrix of LBA parameters. Each column
#' corresponds to an accumulator, and each row corresponds to a parameter,
#' with the expected order being:
#' \itemize{
#'   \item \code{A} - Starting point variability
#'   \item \code{b} - Thresholds
#'   \item \code{mean_v} - Mean drift rates
#'   \item \code{sd_v} - Standard deviation drift rates
#'   \item \code{st0} - Variability of the non-decision
#'   \item \code{t0} - Non-decision time
#' }
#' @param is_positive_drift_r A logical vector indicating whether the
#'                            drift rate (\code{v}) is strictly positive.
#' @param time_parameter_r A numeric vector to set the simulated time grid,
#' with the expected value for: minimal and maximum decision times and the
#' difference time (i.e., \code{min dt}, \code{max dt} and \code{dt}).
#' @param n Integer. Number of trials to simulate. Defaults to \code{1L}.
#' @param use_inverse_method Logical. If \code{TRUE}, use the inverse
#'                         transform sampling method; otherwise use
#'                         the standard sampling method.Defaults to
#'                         \code{FALSE}.
#' @param debug Logical. If \code{TRUE}, prints detailed messages
#'                        for debugging. Defaults to \code{FALSE}.
#' @param seed Optional integer. If provided, sets the random seed for
#'             reproducible simulation. Defaults to \code{NULL}. Available
#' only in the R interface.
#'
#' @return A \code{data.frame}. Each row corresponds to a simulated trial
#'         containing:
#' \itemize{
#'   \item \code{trial} — Trial index
#'   \item \code{choice} — Index of the winning accumulator
#'   \item \code{rt} — Simulated response time
#' }
#'
#' @details
#' This function generates simulated data for the LBA model. The function
#' supports both standard and inverse transform sampling methods. The
#' inverse method may offer better numerical behaviour in edge cases (e.g.,
#' small RTs or boundary conditions).
#' @keywords internal
#'
#' @examples
#' param_list2mat <- function(param_list) {
#'   n_row <- length(param_list[[1]])
#'   n_col <- length(param_list)
#'   out <- matrix(NA, nrow = n_row, ncol = n_col)
#'
#'   for (i in seq_len(n_col)) {
#'     out[, i] <- param_list[[i]]
#'   }
#'   t(out)
#' }
#'
#' A <- 1.2
#' b <- 2.7
#' t0 <- .2
#'
#' mean_v <- c(2.4, 2.2)
#' sd_v <- c(1, 1)
#'
#' RT <- seq(0, 3, .4) + t0
#' posdrift <- TRUE
#' nv <- length(mean_v)
#'
#' st0 <- 0
#'
#' params_tmp <- list(
#'   A = rep(A, nv),
#'   b = rep(b, nv),
#'   mean_v = mean_v,
#'   sd_v = sd_v,
#'   st0 = rep(st0, nv),
#'   t0 = rep(t0, nv)
#' )
#'
#' params <- param_list2mat(params_tmp)
#' is_positive_drift <- rep(TRUE, nv)
#'
#' n <- 1
#' seed <- 123
#'
#' dt <- 0.01
#' min_dt <- 0
#' max_dt <- 5
#' time_parameter_r <- c(min_dt, max_dt, dt)
#'
#'
#' set.seed(seed)
#' # R interface
#' result1 <- lbaModel::rlba(params, is_positive_drift, time_parameter_r, n,
#'   seed = seed, debug = TRUE
#' )
#' # C++ interface. No seed argument.
#' result2 <- lbaModel::rlba_r(params, is_positive_drift, time_parameter_r, n,
#'   debug = TRUE
#' )
#'
#' print(result1)
#' print(result2)
#'
#' @name rlba
#' @export
rlba <- function(
    parameter_r, is_positive_drift_r, time_parameter_r,
    n = 1L,
    use_inverse_method = FALSE,
    debug = FALSE, seed = NULL) {
  #--- Seed Handling ---
  if (is.null(seed)) {
    main_seed <- as.integer(Sys.time()) %% 1000000L
  } else {
    main_seed <- seed
  }

  if (debug) {
    message("Set seed to: ", main_seed)
  }

  set.seed(main_seed)

  result <- rlba_r(
    parameter_r, is_positive_drift_r, time_parameter_r,
    n, use_inverse_method, debug
  )

  return(result)
}
