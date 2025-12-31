# Utility functions sourced by app.R

#' Load configuration from YAML file
load_config <- function(path) {
  if (file.exists(path)) {
    yaml::read_yaml(path)
  } else {
    list(app_title = "Default App", version = "1.0.0")
  }
}

#' Format data summary for display
format_summary <- function(data) {
  paste(
    "Rows:", nrow(data),
    "\nColumns:", ncol(data),
    "\nColumn names:", paste(names(data), collapse = ", ")
  )
}

#' Validate input data
validate_data <- function(data, required_cols = NULL) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame")
  }
  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(data))
    if (length(missing) > 0) {
      stop("Missing required columns: ", paste(missing, collapse = ", "))
    }
  }
  invisible(TRUE)
}
