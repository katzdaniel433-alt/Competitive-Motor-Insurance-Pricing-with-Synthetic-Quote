# ==============================================================================
# Dissertation shared helper functions
# File: R/helpers.R
#
# Purpose
# -------
# Generic utilities shared across Modules 0-8.
#
# IMPORTANT:
# This file must remain methodology-neutral. Do NOT place actuarial assumptions,
# simulation parameters, model formulas, calibration targets, demand parameters,
# pricing constraints, scenario definitions, or seed choices here.
# ==============================================================================


# ==============================================================================
# 1. NUMERIC UTILITIES
# ==============================================================================

safe_divide <- function(numerator, denominator) {
  result <- numerator / denominator
  result[!is.finite(result)] <- NA_real_
  result
}

clamp <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

clip_probability <- function(probability, epsilon = 1e-6) {
  clamp(as.numeric(probability), epsilon, 1 - epsilon)
}

first_or_na <- function(x) {
  if (length(x) == 0L) NA_real_ else x[[1]]
}

log_sum_exp <- function(x) {
  maximum_x <- max(x)
  maximum_x + log(sum(exp(x - maximum_x)))
}

lognormal_parameters_from_mean_cv <- function(mean_value, cv) {
  if (
    length(mean_value) == 0L ||
    length(cv) == 0L ||
    any(!is.finite(mean_value)) ||
    any(!is.finite(cv)) ||
    any(mean_value <= 0) ||
    any(cv < 0)
  ) {
    stop("Lognormal mean must be positive and CV must be non-negative.")
  }
  
  target_length <- max(length(mean_value), length(cv))
  
  if (
    target_length %% length(mean_value) != 0L ||
    target_length %% length(cv) != 0L
  ) {
    stop("mean_value and cv have incompatible lengths.")
  }
  
  mean_value <- rep(mean_value, length.out = target_length)
  cv <- rep(cv, length.out = target_length)
  
  sdlog <- sqrt(log1p(cv^2))
  meanlog <- log(mean_value) - 0.5 * sdlog^2
  
  list(meanlog = meanlog, sdlog = sdlog)
}

rlnorm_mean_cv <- function(n, mean_value, cv) {
  pars <- lognormal_parameters_from_mean_cv(mean_value, cv)
  
  target_length <- max(length(mean_value), length(cv))
  
  if (!(target_length %in% c(1L, n))) {
    stop(
      "For rlnorm_mean_cv(), mean_value and cv must be scalar or have length n."
    )
  }
  
  stats::rlnorm(
    n,
    meanlog = rep(pars$meanlog, length.out = n),
    sdlog = rep(pars$sdlog, length.out = n)
  )
}


# ==============================================================================
# 2. INPUT AND STRUCTURE CHECKS
# ==============================================================================

assert_file <- function(path, description = basename(path)) {
  if (!file.exists(path)) {
    stop("Missing ", description, ": ", path)
  }
  invisible(TRUE)
}

assert_columns <- function(data, required_columns, data_name = deparse(substitute(data))) {
  missing_columns <- setdiff(required_columns, names(data))
  
  if (length(missing_columns) > 0L) {
    stop(
      data_name,
      " is missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }
  
  invisible(TRUE)
}

# Transitional alias used by Modules 1-4 in the frozen pipeline.
check_required_columns <- assert_columns

safe_read_rds <- function(path, description = basename(path), attempts = 3L) {
  assert_file(path, description)
  
  last_error <- NULL
  
  for (attempt in seq_len(attempts)) {
    result <- tryCatch(
      base::readRDS(path),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )
    
    if (!is.null(result)) return(result)
    
    invisible(gc(verbose = FALSE))
    Sys.sleep(0.25 * attempt)
  }
  
  stop(
    "Could not read ", description, " after ", attempts, " attempt(s): ",
    conditionMessage(last_error), ". File: ", path
  )
}


# ==============================================================================
# 3. DATA AND PATH UTILITIES
# ==============================================================================

clean_column_names <- function(data) {
  names(data) <- names(data) |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("[^a-z0-9]+", "_") |>
    stringr::str_replace_all("^_+|_+$", "")
  
  data
}

first_existing_column <- function(data, candidates, label = "required column") {
  available <- intersect(candidates, names(data))
  
  if (length(available) == 0L) {
    stop(
      "Could not identify ", label, ". Expected one of: ",
      paste(candidates, collapse = ", "),
      ". Available columns: ",
      paste(names(data), collapse = ", ")
    )
  }
  
  available[[1]]
}

resolve_existing_file <- function(filename, search_directories) {
  candidates <- file.path(search_directories, filename)
  existing <- candidates[file.exists(candidates)]
  
  if (length(existing) == 0L) return(NA_character_)
  
  normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
}

find_csv_with_required_columns <- function(
    required_column_groups,
    search_directories,
    exclude_paths = character()
) {
  candidate_files <- unique(unlist(lapply(search_directories, function(directory) {
    if (!dir.exists(directory)) return(character())
    
    list.files(
      directory,
      pattern = "\\.csv$",
      full.names = TRUE,
      recursive = FALSE
    )
  })))
  
  candidate_files <- setdiff(
    normalizePath(candidate_files, winslash = "/", mustWork = FALSE),
    normalizePath(exclude_paths, winslash = "/", mustWork = FALSE)
  )
  
  for (candidate in candidate_files) {
    header <- tryCatch(
      clean_column_names(
        readr::read_csv(candidate, n_max = 0, show_col_types = FALSE)
      ),
      error = function(e) NULL
    )
    
    if (is.null(header)) next
    
    matches <- vapply(
      required_column_groups,
      function(group) any(group %in% names(header)),
      logical(1)
    )
    
    if (all(matches)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }
  }
  
  NA_character_
}

normalise_r_path <- function(path) {
  gsub("\\\\", "/", path)
}

r_string <- function(x) {
  paste0('"', normalise_r_path(x), '"')
}

path_config_line <- function(name, path) {
  paste0(name, " = ", r_string(path), ",")
}

write_csv_safe <- function(data, file, attempts = 2L) {
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  
  last_error <- NULL
  
  for (attempt in seq_len(attempts)) {
    success <- tryCatch(
      {
        readr::write_csv(data, file)
        TRUE
      },
      error = function(e) {
        last_error <<- e
        FALSE
      }
    )
    
    if (success) return(invisible(file))
    
    invisible(gc(verbose = FALSE))
    Sys.sleep(0.2 * attempt)
  }
  
  stop(
    "Could not write CSV after ", attempts, " attempt(s): ",
    conditionMessage(last_error), ". File: ", file
  )
}

# Transitional alias for Modules 7-8.
write_csv_resilient <- write_csv_safe


# ==============================================================================
# 4. FACTOR AND MODEL-MATRIX UTILITIES
# ==============================================================================

set_reference_level <- function(x, reference_level, variable_name = "variable") {
  x_factor <- factor(as.character(x), ordered = FALSE)
  
  if (!reference_level %in% levels(x_factor)) {
    stop(
      "Reference level '", reference_level, "' is not present for ",
      variable_name, ". Available levels: ",
      paste(levels(x_factor), collapse = ", ")
    )
  }
  
  stats::relevel(x_factor, ref = reference_level)
}

make_xgb_design_matrix <- function(
    data,
    feature_formula,
    required_columns = NULL
) {
  design_matrix <- stats::model.matrix(
    feature_formula,
    data = data,
    na.action = stats::na.pass
  )
  
  storage.mode(design_matrix) <- "double"
  
  if (!is.null(required_columns)) {
    missing_columns <- setdiff(required_columns, colnames(design_matrix))
    
    if (length(missing_columns) > 0L) {
      zero_columns <- matrix(
        0,
        nrow = nrow(design_matrix),
        ncol = length(missing_columns),
        dimnames = list(NULL, missing_columns)
      )
      design_matrix <- cbind(design_matrix, zero_columns)
    }
    
    design_matrix <- design_matrix[
      ,
      intersect(required_columns, colnames(design_matrix)),
      drop = FALSE
    ]
    
    if (!identical(colnames(design_matrix), required_columns)) {
      stop("XGBoost design-matrix columns do not match the required feature order.")
    }
  }
  
  if (any(!is.finite(design_matrix))) {
    stop("XGBoost design matrix contains non-finite values.")
  }
  
  design_matrix
}

model_family_from_object <- function(model, metadata_type = NULL) {
  allowed <- c("glm", "gam", "xgboost")
  
  if (!is.null(metadata_type) &&
      length(metadata_type) == 1L &&
      !is.na(metadata_type) &&
      metadata_type %in% allowed) {
    return(metadata_type)
  }
  
  if (inherits(model, "xgb.Booster") || inherits(model, "xgboost")) return("xgboost")
  if (inherits(model, "gam")) return("gam")
  if (inherits(model, "glm")) return("glm")
  
  stop("Unsupported model class: ", paste(class(model), collapse = ", "))
}

predict_probability <- function(
    model,
    new_data,
    model_name = "model",
    model_type = NULL,
    xgb_feature_formula = NULL,
    xgb_feature_columns = NULL,
    epsilon = 1e-6
) {
  resolved_type <- model_family_from_object(model, model_type)
  
  prediction <- tryCatch(
    {
      if (identical(resolved_type, "xgboost")) {
        x_matrix <- make_xgb_design_matrix(
          new_data,
          xgb_feature_formula,
          xgb_feature_columns
        )
        as.numeric(stats::predict(model, x_matrix))
      } else {
        as.numeric(stats::predict(model, newdata = new_data, type = "response"))
      }
    },
    error = function(e) {
      stop(
        "Prediction failed for ", model_name, " [", resolved_type, "]: ",
        conditionMessage(e)
      )
    }
  )
  
  if (length(prediction) != nrow(new_data) || any(!is.finite(prediction))) {
    stop("Invalid probability prediction generated by ", model_name, ".")
  }
  
  clip_probability(prediction, epsilon)
}


# ==============================================================================
# 5. MODEL-EVALUATION UTILITIES
# ==============================================================================

rmse <- function(actual, predicted, weights = NULL) {
  keep <- is.finite(actual) & is.finite(predicted)
  
  if (is.null(weights)) {
    return(sqrt(mean((actual[keep] - predicted[keep])^2)))
  }
  
  keep <- keep & is.finite(weights) & weights > 0
  sqrt(stats::weighted.mean((actual[keep] - predicted[keep])^2, weights[keep]))
}

mae <- function(actual, predicted, weights = NULL) {
  keep <- is.finite(actual) & is.finite(predicted)
  
  if (is.null(weights)) {
    return(mean(abs(actual[keep] - predicted[keep])))
  }
  
  keep <- keep & is.finite(weights) & weights > 0
  stats::weighted.mean(abs(actual[keep] - predicted[keep]), weights[keep])
}

mean_poisson_deviance <- function(actual, predicted) {
  keep <- is.finite(actual) & is.finite(predicted) & actual >= 0 & predicted > 0
  y <- actual[keep]
  mu <- predicted[keep]
  
  contribution <- ifelse(y == 0, 2 * mu, 2 * (y * log(y / mu) - (y - mu)))
  mean(contribution)
}

mean_gamma_deviance <- function(actual, predicted, weights = NULL) {
  keep <- is.finite(actual) & is.finite(predicted) & actual > 0 & predicted > 0
  
  if (!is.null(weights)) {
    keep <- keep & is.finite(weights) & weights > 0
  }
  
  y <- actual[keep]
  mu <- predicted[keep]
  contribution <- 2 * ((y - mu) / mu - log(y / mu))
  
  if (is.null(weights)) mean(contribution) else {
    stats::weighted.mean(contribution, weights[keep])
  }
}

auc_rank <- function(outcome, probability) {
  outcome <- as.integer(outcome)
  probability <- as.numeric(probability)
  
  keep <- is.finite(outcome) & is.finite(probability)
  outcome <- outcome[keep]
  probability <- probability[keep]
  
  n_positive <- sum(outcome == 1L)
  n_negative <- sum(outcome == 0L)
  
  if (n_positive == 0L || n_negative == 0L) return(NA_real_)
  
  ranks <- rank(probability, ties.method = "average")
  positive_rank_sum <- sum(ranks[outcome == 1L])
  
  (positive_rank_sum - n_positive * (n_positive + 1) / 2) /
    (n_positive * n_negative)
}

calibration_statistics <- function(outcome, probability, epsilon = 1e-6) {
  probability <- clip_probability(probability, epsilon)
  linear_predictor <- stats::qlogis(probability)
  
  intercept_fit <- tryCatch(
    suppressWarnings(
      stats::glm(
        outcome ~ 1,
        offset = linear_predictor,
        family = stats::binomial()
      )
    ),
    error = function(e) NULL
  )
  
  slope_fit <- tryCatch(
    suppressWarnings(
      stats::glm(
        outcome ~ linear_predictor,
        family = stats::binomial()
      )
    ),
    error = function(e) NULL
  )
  
  c(
    calibration_intercept = if (is.null(intercept_fit)) {
      NA_real_
    } else {
      unname(stats::coef(intercept_fit)[[1]])
    },
    calibration_slope = if (is.null(slope_fit) || length(stats::coef(slope_fit)) < 2L) {
      NA_real_
    } else {
      unname(stats::coef(slope_fit)[[2]])
    }
  )
}

expected_calibration_error <- function(
    outcome,
    probability,
    bins = 10L,
    epsilon = 1e-6
) {
  calibration_data <- tibble::tibble(
    outcome = as.integer(outcome),
    probability = clip_probability(probability, epsilon)
  )
  
  calibration_data$calibration_bin <- dplyr::ntile(calibration_data$probability, bins)
  
  summary <- calibration_data |>
    dplyr::group_by(.data$calibration_bin) |>
    dplyr::summarise(
      observations = dplyr::n(),
      observed_rate = mean(.data$outcome),
      predicted_rate = mean(.data$probability),
      .groups = "drop"
    )
  
  stats::weighted.mean(
    abs(summary$observed_rate - summary$predicted_rate),
    summary$observations
  )
}


# ==============================================================================
# 6. VALIDATION-RESULT UTILITIES
# ==============================================================================

all_checks_pass <- function(file_path) {
  if (!file.exists(file_path)) return(FALSE)
  
  checks <- suppressMessages(
    readr::read_csv(file_path, show_col_types = FALSE)
  )
  
  if (!"pass" %in% names(checks) || nrow(checks) == 0L) return(FALSE)
  
  if ("required_for_progress" %in% names(checks)) {
    required <- as.logical(checks$required_for_progress)
    required[is.na(required)] <- TRUE
    checks <- checks[required, , drop = FALSE]
  }
  
  nrow(checks) > 0L && all(as.logical(checks$pass))
}

check_observed_value <- function(checks, check_name) {
  required <- c("check_name", "observed_value")
  if (!all(required %in% names(checks))) return(NA_real_)
  
  value <- checks$observed_value[checks$check_name == check_name]
  if (length(value) == 0L) NA_real_ else as.numeric(value[[1]])
}


# ==============================================================================
# 7. RMD ORCHESTRATION UTILITIES
# Used primarily by Modules 7 and 8.
# ==============================================================================

replace_once_line <- function(lines, old, new, label) {
  matching_lines <- which(trimws(lines) == trimws(old))
  
  if (length(matching_lines) == 0L) {
    assignment_name <- stringr::str_match(
      trimws(old),
      "^([A-Za-z][A-Za-z0-9_.]*)\\s*="
    )[1, 2]
    
    if (!is.na(assignment_name)) {
      pattern <- paste0("^\\s*", assignment_name, "\\s*=")
      matching_lines <- which(
        !stringr::str_detect(lines, "^\\s*#") &
          stringr::str_detect(lines, pattern)
      )
    }
  }
  
  if (length(matching_lines) != 1L) {
    stop(
      "Expected exactly one active configuration line for '", label,
      "' but found ", length(matching_lines), "."
    )
  }
  
  index <- matching_lines[[1]]
  indentation <- stringr::str_extract(lines[[index]], "^\\s*")
  lines[[index]] <- paste0(indentation, trimws(new))
  lines
}

extract_r_chunks <- function(rmd_path, r_script_path) {
  lines <- readLines(rmd_path, warn = FALSE, encoding = "UTF-8")
  output <- character()
  in_r_chunk <- FALSE
  chunk_count <- 0L
  
  for (line in lines) {
    if (!in_r_chunk && grepl("^\\s*```\\{r(?:[ ,}].*)?$", line)) {
      in_r_chunk <- TRUE
      chunk_count <- chunk_count + 1L
      next
    }
    
    if (in_r_chunk && grepl("^\\s*```\\s*$", line)) {
      in_r_chunk <- FALSE
      output <- c(output, "")
      next
    }
    
    if (in_r_chunk) output <- c(output, line)
  }
  
  if (in_r_chunk) stop("Unclosed R code chunk in: ", rmd_path)
  if (chunk_count == 0L) stop("No R code chunks found in: ", rmd_path)
  
  writeLines(output, r_script_path, useBytes = TRUE)
  invisible(chunk_count)
}

run_script_isolated <- function(script_path, module_label, project_root) {
  message("    Running ", module_label, " in isolated R process...")
  
  rscript_path <- file.path(R.home("bin"), "Rscript")
  if (!file.exists(rscript_path)) rscript_path <- Sys.which("Rscript")
  
  if (!nzchar(rscript_path) || !file.exists(rscript_path)) {
    stop("Could not locate the Rscript executable.")
  }
  
  log_path <- sub("\\.R$", "_run.log", script_path)
  old_wd <- getwd()
  
  on.exit({
    setwd(old_wd)
    invisible(gc(verbose = FALSE))
  }, add = TRUE)
  
  setwd(project_root)
  
  status <- tryCatch(
    system2(
      rscript_path,
      args = c("--vanilla", shQuote(script_path)),
      stdout = log_path,
      stderr = log_path
    ),
    error = function(e) 1L
  )
  
  if (is.null(status)) status <- 0L
  status <- as.integer(status)
  
  if (status == 0L) {
    return(list(success = TRUE, error_message = NA_character_, log_path = log_path))
  }
  
  log_tail <- if (file.exists(log_path)) {
    tail(readLines(log_path, warn = FALSE, encoding = "UTF-8"), 50L)
  } else {
    character()
  }
  
  error_text <- paste(log_tail, collapse = "\n")
  if (!nzchar(error_text)) {
    error_text <- paste0("Rscript exited with status ", status, ". See log: ", log_path)
  }
  
  list(
    success = FALSE,
    error_message = paste0(module_label, " failed: ", error_text),
    log_path = log_path
  )
}
