# Data processing functions sourced by app.R

#' Process uploaded data
process_data <- function(df) {
  # Remove NA rows
  df <- df[complete.cases(df), ]
  
  # Add computed columns
  if ("value" %in% names(df)) {
    df$normalized <- scale(df$value)
    df$log_value <- log1p(abs(df$value))
  }
  
  # Add timestamp
  df$processed_at <- Sys.time()
  
  df
}

#' Aggregate data by category
aggregate_by_category <- function(df, category_col, value_col) {
  if (!category_col %in% names(df)) {
    stop("Category column not found: ", category_col)
  }
  
  aggregate(
    df[[value_col]],
    by = list(category = df[[category_col]]),
    FUN = mean
  )
}

#' Filter outliers using IQR method
filter_outliers <- function(df, value_col, multiplier = 1.5) {
  values <- df[[value_col]]
  q1 <- quantile(values, 0.25, na.rm = TRUE)
  q3 <- quantile(values, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  
  lower <- q1 - multiplier * iqr
  upper <- q3 + multiplier * iqr
  
  df[values >= lower & values <= upper, ]
}
