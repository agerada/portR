# Plotting helper functions sourced by app.R

#' Create summary plot for processed data
create_summary_plot <- function(data) {
  if (!"value" %in% names(data)) {
    plot(1, type = "n", main = "No value column found")
    return(invisible(NULL))
  }
  
  par(mfrow = c(1, 2))
  
  # Histogram
  hist(data$value, main = "Distribution", xlab = "Value", col = "steelblue")
  
  # Time series if available
  if ("date" %in% names(data)) {
    plot(data$date, data$value, type = "l", 
         main = "Time Series", xlab = "Date", ylab = "Value")
  } else {
    plot(seq_len(nrow(data)), data$value, type = "l",
         main = "Values Over Index", xlab = "Index", ylab = "Value")
  }
  
  par(mfrow = c(1, 1))
}

#' Create category comparison plot
create_category_plot <- function(data, category_col, value_col) {
  boxplot(
    as.formula(paste(value_col, "~", category_col)),
    data = data,
    main = "Category Comparison",
    col = "lightblue"
  )
}

#' Create correlation heatmap for numeric columns
create_correlation_heatmap <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) < 2) {
    message("Need at least 2 numeric columns for correlation")
    return(invisible(NULL))
  }
  
  cor_matrix <- cor(data[, numeric_cols], use = "complete.obs")
  heatmap(cor_matrix, main = "Correlation Heatmap")
}
