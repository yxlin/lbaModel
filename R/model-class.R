#' @importClassesFrom ggdmcModel model
#' @importClassesFrom ggdmcPrior prior
NULL

#' An S4 Class of the 'lba' Object
#'
#' @description
#' The lba class represents an LBA model with slots for model specification,
#' population distribution, and other necessary components. The setLBA function
#' is the constructor for creating lba objects.
#'
#' @slot model A model object containing the model specification
#' @slot population_distribution The population distribution for parameters
#' (can be NULL)
#' @slot node_1_index Index information for the first node (automatically
#' calculated)
#' @slot is_positive_drift Logical vector indicating positive drift for each
#' accumulator
#'
#' @param model A model object containing the model specification
#' @param population_distribution Optional population distribution for
#' parameters (default NULL)
#' @param is_positive_drift a Boolean value indicating whether to use strictly
#' positive drift rates
#'
#' @return An object of class lba containing:
#' \itemize{
#' \item The model specification
#' \item Population distribution (if provided)
#' \item Node 1 index information
#' \item Whether to restrict drift rates to be positive
#' }
#'
#' @details
#' The LBA model is a popular decision-making model that assumes evidence
#' accumulates linearly toward decision thresholds. The setLBA function
#' initialises this model by creating an S4 object with all necessary
#' components, including automatically calculating node 1 indices and
#' creating an indicator vector to inform whether to use strictly positive
#' drift rates.
#'
#' @rdname lba-class
#' @export
setClass("lba",
  slots = list(
    model = "model",
    population_distribution = "ANY",
    node_1_index = "ANY",
    is_positive_drift = "ANY"
  ),
  validity = function(object) {
    if (!methods::is(object@model, "model")) {
      return("Slot 'model' must be a 'model' object from ggdmcModel")
    }
    if (!is.null(object@population_distribution) &&
      !methods::is(object@population_distribution, "list")) {
      return("population_distribution must be NULL or a list")
    }
    TRUE
  }
)

#' @importFrom methods new
#' @rdname lba-class
#' @importFrom ggdmcModel get_node_1_index_r
#' @export
setLBA <- function(
    model, population_distribution = NULL,
    is_positive_drift = TRUE) {
  drift_boolean <- rep(FALSE, length(model@accumulators))
  if (is_positive_drift) {
    drift_boolean <- rep(TRUE, length(model@accumulators))
  }

  out <- new("lba",
    model = model,
    population_distribution = population_distribution,
    node_1_index = get_node_1_index_r(
      model@parameter_map, model@factors,
      model@accumulators
    ),
    is_positive_drift = drift_boolean
  )
  out
}

### Helper functions -----------------------------------
#' @importFrom ggdmcPrior rprior
.validate_parameters <- function(
    parameter_matrix, pop_dist, rt_model,
    max_attempts = 1000) {
  # Validate each subject's parameters, resampling invalid ones
  # Returns validated matrix or NULL if max attempts reached

  n_subject <- nrow(parameter_matrix)
  # n_params <- ncol(parameter_matrix)
  # param_names <- colnames(parameter_matrix)

  all_valid <- rep(FALSE, n_subject)
  for (i in seq_len(n_subject)) {
    current_params <- parameter_matrix[i, ]
    all_valid[i] <- validate_lba_parameters(rt_model, current_params)
  }

  for (i in seq_len(n_subject)) {
    if (!all_valid[i]) {
      message("Subject ", i, " has invalid parameters. Resample a new set from the population distribution")

      for (attempt in seq_len(max_attempts)) {
        tmp_params <- rprior(pop_dist, n = 1)
        is_valid <- validate_lba_parameters(rt_model, tmp_params[, 1])
        if (is_valid) {
          message("After", attempt, "attempts, I found a new set of parameters for Subject", i, "\n")

          parameter_matrix[i, ] <- tmp_params[, 1]
          all_valid[i] <- is_valid
          break
        }
      }
    }
  }

  if (!all(all_valid)) {
    stop("Failed to generate valid parameters after ", max_attempts, " attempts")
  }

  return(parameter_matrix)
}

.check_parameter_names <- function(parameter_vector, model) {
  # Get parameter names from the model
  model_params <- model@pnames
  input_params <- names(parameter_vector)

  # Check if all model parameters are present in the input
  model_params_in_input <- model_params %in% input_params
  missing_in_input <- !model_params_in_input

  # Check if all input parameters are present in the model
  input_params_in_model <- input_params %in% model_params
  extra_in_input <- !input_params_in_model

  # Generate warnings if any mismatches are found
  any_problems <- FALSE

  if (any(missing_in_input)) {
    warning(paste(
      "Parameter(s)", paste(model_params[missing_in_input], collapse = ", "),
      "in model not present in parameter_vector"
    ))
    any_problems <- TRUE
  }

  if (any(extra_in_input)) {
    warning(paste(
      "Parameter(s)", paste(input_params[extra_in_input], collapse = ", "),
      "in parameter_vector not present in model"
    ))
    any_problems <- TRUE
  }

  # Return TRUE if there were any problems, FALSE otherwise
  invisible(any_problems)
}

#' @importFrom ggdmcPrior rprior
.prepare_parameter_matrix <- function(
    model, n_subject, pop_dist = NULL,
    seed = NULL) {
  parameter_names <- model@pnames

  # Validate input arguments
  if (is.null(pop_dist)) {
    stop("You must provide a population distribution.")
  }

  # Validate all model parameters have corresponding priors
  if (!all(parameter_names %in% names(pop_dist))) {
    stop("Model parameters differ from the population distribution.")
  }

  # Sample parameters for each subject from the priors
  if (is.null(seed)) {
    seed <- as.integer(Sys.time()) %% 1000000 # Generate a random seed
    message("No seed provided. Using R-generated seed: ", seed)
    set.seed(seed)
  } else {
    set.seed(seed)
  }

  t(ggdmcPrior::rprior(pop_dist, n = n_subject))
}

.separate_condition_column <- function(data, factors, responses = NULL) {
  # Split the "Condition" column into parts based on the delimiter "."
  condition_parts <- strsplit(data$Condition, "\\.")

  # Create a new data frame to store the separated columns
  separated_data <- data.frame(data)

  # Iterate over the factors and extract the corresponding parts
  for (factor_name in names(factors)) {
    # Extract the part of the condition corresponding to the current factor
    separated_data[[factor_name]] <- sapply(condition_parts, function(x) {
      # Find the index of the factor in the condition parts
      index <- which(x %in% factors[[factor_name]])
      if (length(index) == 0) {
        return(NA) # Return NA if the factor is not found
      }
      return(x[index])
    })
  }

  # Drop the original "Condition" column
  separated_data <- separated_data[, !names(separated_data) %in% "Condition"]

  # Modify the 0-based index to 1-based.
  separated_data$R <- separated_data$R + 1
  # Convert columns (except RT) into factors
  for (col in names(separated_data)) {
    if (col != "RT") {
      separated_data[[col]] <- as.factor(separated_data[[col]])
    }
  }

  # If responses are provided, convert the R column into a factor with the given levels
  if (!is.null(responses)) {
    response_level <- seq_len(length(responses))
    separated_data$R <- factor(separated_data$R,
      levels = response_level,
      labels = responses
    )
  }

  return(separated_data)
}

.generate_seeds <- function(seed) {
  #--- Seed Handling ---
  seed <- as.integer(Sys.time()) %% 1000000
  return(seed)
}

.simulate_lba_trials_r <- function(rt_model_r,
                                   parameters_r,
                                   n_trial,
                                   use_inverse_method,
                                   seed, debug) {
  if (debug) {
    message("Set seed to: ", seed)
  }

  set.seed(seed)
  trials <- simulate_lba_trials(
    rt_model_r = rt_model_r,
    parameters_r = parameters_r,
    n_trial = n_trial,
    use_inverse_method = use_inverse_method,
    debug = debug
  )
  return(trials)
}

#' Simulate Data from an LBA Model
#'
#' Simulate response times and choices from a Linear Ballistic Accumulator
#' (LBA) model with a model specification (typically from
#' \code{ggdmcModel::BuildModel}).
#'
#' @param object An object of class \code{"lba"} that defines the model
#' structure and parameters.
#' @param nsim Integer. Number of trials to simulate per subject. Defaults
#' to \code{4}.
#' @param seed Optional integer. Sets the random seed for reproducibility.
#' Defaults to \code{NULL}.
#' @param n_subject Integer. Number of subjects to simulate. Defaults to
#' \code{3}.
#' @param parameter_vector A named vector or list of parameters
#' (e.g., \code{A}, \code{b}, \code{mean_v.true}, \code{t0}). The user must
#' supply either a parameter_vector (here) or a population distribution 
#' (via \code{setLBA}). The population distribution is defined typically
#' via \code{BuildPrior} of the 'ggdmcPrior 'package. The default value, 
#' thus, is set to \code{NULL} here.
#' @param use_inverse_method Logical. If \code{TRUE}, use inverse transform
#' sampling; otherwise use the process model to sample. Defaults
#' to \code{FALSE}.
#' @param debug Logical. If \code{TRUE}, print debugging output during
#' simulation. Defaults to \code{FALSE}.
#'
#' @return A data frame with simulated trial data, including
#' the standard columns: \code{s} for subjects, \code{R} for choices,
#' \code{RT} for response times and other columns for user-defined
#' conditions (by analysing the 'model' object).
#'
#' @details
#' This method simulates data from a design-based Linear Ballistic
#'  Accumulator (LBA) model. You can simulate multiple subjects,
#' override default parameters, and choose between standard and
#' inverse sampling methods. Turn on debugging mode by enterinig TRUE to
#' the option, \code{debug}.
#'
#' @examples
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
#' p_vector <- c(
#'   A = .75, B = 1.25, mean_v.false = 1.5, mean_v.true = 2.5,
#'   t0 = 0.15
#' )
#' sub_model <- setLBA(model)
#' sim_dat <- simulate(sub_model,
#'   nsim = 256, parameter_vector = p_vector,
#'   n_subject = 1
#' )
#' head(sim_dat)
#'
#' @export
#' @method simulate lba
setMethod(
  "simulate", signature(object = "lba"),
  function(object, nsim = 4L, seed = NULL, n_subject = 3L,
           parameter_vector = NULL, use_inverse_method = FALSE, debug = FALSE) {
    #--- Checking if required arguments are provided ---
    if (is.null(object@population_distribution) && is.null(parameter_vector)) {
      stop("Neither population_distribution (a slot in the 'lba' class) nor parameter_vector was found")
    }

    if (!is.null(object@population_distribution) && !is.null(parameter_vector)) {
      stop("You have provided both the population_distribution and the parameter_vector. Which one do you want me to use?")
    }

    ncell <- length(object@model@cell_names)
    if (ncell == 0) {
      stop("Number of cells (ncell) cannot be zero")
    }

    # --- Parameter Preparation ---
    if (!is.null(parameter_vector)) {
      if (is.null(names(parameter_vector))) {
        stop("Please give name attribute to parameter_vector")
      }
      name_sorted_p_vector <- parameter_vector[sort(names(parameter_vector))]
      param_matrix <- t(sapply(seq_len(n_subject), function(i) {
        matrix(name_sorted_p_vector, nrow = 1, ncol = object@model@npar)
      }))

      # print(param_matrix)
    } else {
      # (!is.null(object@population_distribution)) {
      param_matrix <- .prepare_parameter_matrix(
        model = object@model,
        n_subject = n_subject,
        pop_dist = object@population_distribution,
        seed = seed
      )

      # --- Parameter Validation ---
      param_matrix <- .validate_parameters(
        parameter_matrix = param_matrix,
        pop_dist = object@population_distribution,
        rt_model = object
      )
    }


    # cat("After .validate_parameters")
    # print(param_matrix)

    # --- Simulation ---
    ## Calculate trials per cell with warning if uneven division
    if (nsim %% ncell == 0) {
      n_trial_per_cell <- nsim / ncell
    } else {
      n_trial_per_cell <- ceiling(nsim / ncell)
      nsim <- n_trial_per_cell * ncell

      message("Warning: n_trial (", nsim, ") is not a multiple of the number of cells (", ncell, "). n_trial per cell/condition has been truncated to ", n_trial_per_cell, ".\n")
    }

    message("\n[n_trial per condition, n_trial]: [", n_trial_per_cell, ", ", nsim, "]")

    # cat("Going to simulate_lba_trials")
    # print(param_matrix[1, ])

    #--- Seed Handling ---
    # Generate seeds based on number of cores
    if (is.null(seed)) {
      main_seed <- as.integer(Sys.time()) %% 1000000L
    } else {
      main_seed <- seed
    }

    set.seed(main_seed)
    seeds <- sample.int(1e6, n_subject)

    # Print header information
    message("Simulation settings:")
    message("---------------------")
    message("Main seed: ", main_seed)
    message("Number of subjects: ", n_subject)

    # Print seeds in a more readable format
    if (n_subject <= 5) {
      message("\nSeeds for each subject:")
      for (i in seq_len(n_subject)) {
        message("  Subject ", i, ": ", seeds[i])
      }
    } else {
      message("\nSeeds for the first 5 subjects:")
      for (i in seq_len(5)) {
        message("  Subject ", i, ": ", seeds[i])
      }
    }


    t0 <- Sys.time()
    results_list <- lapply(seq_len(n_subject), function(i) {
      trials <- .simulate_lba_trials_r(
        rt_model_r = object,
        parameters_r = param_matrix[i, ],
        n_trial = as.integer(nsim),
        use_inverse_method = use_inverse_method,
        seed = seeds[i], debug = debug
      )
      trials$s <- i
      trials
    })
    t1 <- Sys.time()
    proc_time <- difftime(t1, t0, units = "secs")[[1]]

    if (use_inverse_method) {
      msg_str <- paste0("Processing time ", "(inverse method): ")
    } else {
      msg_str <- paste0("Processing time ", "(process model): ")
    }
    message(msg_str, round(proc_time, 3), " secs.")


    # --- Combine Results ---
    results <- do.call("rbind", results_list)
    results$s <- factor(results$s)

    out <- .separate_condition_column(
      results, object@model@factors,
      object@model@accumulators
    )

    # --- Add Attributes ---
    colnames(param_matrix) <- object@model@pnames
    if (nrow(param_matrix) == 1) {
      attr(out, "parameters") <- param_matrix[1, ]
    } else {
      attr(out, "parameters") <- param_matrix
    }

    attr(out, "main_seed") <- main_seed
    attr(out, "seeds") <- seeds
    return(out)
  }
)
