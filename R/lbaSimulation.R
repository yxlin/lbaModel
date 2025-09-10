#' Low-Level LBA Simulation and Parameter Validation (C++ back-end)
#'
#' Internal interfaces for simulating trial-level data and validating parameter
#' inputs for the Linear Ballistic Accumulation (LBA) model. These functions are
#' implemented in C++ via Rcpp and are primarily intended for use by
#' higher-level R helpers rather than for direct end-user calls.
#'
#' @description
#' - \code{simulate_lba_trials()} generates synthetic choices and response times
#'   from an LBA configuration passed as an S4 model object plus a parameter
#'   vector.
#' - \code{validate_lba_parameters()} checks basic validity/compatibility of a
#'   named parameter vector for a given model object.
#'
#' @details
#' These functions are exposed to R via **Rcpp attributes** and callable from R.
#' They assume inputs with correct dimensions and names; minimal argument
#' checking is performed for speed. See also high-level helpers in this package
#' for safer user-facing APIs.
#'
#' @param rt_model_r An S4 model object describing the LBA structure and
#' metadata. (Typically created by package-internal builders.)
#' @param parameters_r A **named** numeric vector of LBA parameters (e.g.,
#' \code{A}, \code{b}, \code{v}/\code{mean_v}, \code{t0}, optionally
#' \code{sd_v}, \code{st0} depending on your model).
#' @param n_trial Integer (default \code{1L}). Number of trials to simulate.
#' @param use_inverse_method Logical. If \code{TRUE}, use inverse-transform
#' sampling; otherwise use the standard sampling.
#' @param debug Logical. If \code{TRUE}, prints internal diagnostics during
#' validation or simulation.
#'
#' @return
#' - \code{simulate_lba_trials()} returns a \code{data.frame} with one row per
#' simulated trial and columns:
#'   \itemize{
#'     \item \code{trial} — Trial index (integer)
#'     \item \code{choice} — Index of the winning accumulator (integer)
#'     \item \code{rt} — Simulated response time (numeric)
#'   }
#' - \code{validate_lba_parameters()} returns a logical scalar indicating
#'   whether \code{parameters_r} is valid for \code{rt_model_r}.
#'
#' @examples
#' \donttest{
#' # See ?simulate_lba for the user-facing simulation function.
#' }
#' @seealso
#' \code{\link[=simulate-lba]{simulate()} on class "lba"},
#' \code{\link{dlba}}, \code{\link{plba}}
#'
#' @aliases simulate_lba_trials validate_lba_parameters
#' @family LBA simulation
#' @keywords internal
#' @name lba_lowlevel
#'
NULL
