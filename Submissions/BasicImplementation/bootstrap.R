#' Compute sleep duration
#'
#' Compute sleep duration as the difference between wake time and bedtime.
#' If wake time is smaller than bedtime, the function assumes sleep crosses
#' midnight and adjusts accordingly.
#'
#' @param bedtime Numeric vector of bedtimes.
#' @param uptime Numeric vector of wake times.
#'
#' @return A numeric vector containing sleep durations (in hours).
#' @export
compute_sleep_duration <- function(bedtime, uptime) {

  sleep <- uptime - bedtime

  idx <- !is.na(uptime) & !is.na(bedtime) & uptime < bedtime
  sleep[idx] <- uptime[idx] + 24 - bedtime[idx]

  sleep
}


#' Bootstrap mean estimator
#'
#' Generate bootstrap estimates of the sample mean using resampling
#' with replacement.
#'
#' @param x Numeric vector.
#' @param B Number of bootstrap samples. Default is 1000.
#' @param n Size of each bootstrap sample. If NULL (default), uses the
#'   number of non-missing observations in `x`.
#'
#' @return A numeric vector containing B bootstrap mean estimates.
#' @export
bootstrap_mean <- function(x, B = 1000, n = NULL) {

  x <- x[!is.na(x)]

  if(is.null(n)){
    n <- length(x)
  }

  boot_means <- numeric(B)

  for (b in seq_len(B)) {
    sample_x <- sample(x, n, replace = TRUE)
    boot_means[b] <- mean(sample_x)
  }

  boot_means
}


#' Bootstrap median estimator
#'
#' Generate bootstrap estimates of the sample median using resampling
#' with replacement.
#'
#' @param x Numeric vector.
#' @param B Number of bootstrap samples. Default is 1000.
#' @param n Size of each bootstrap sample. If NULL (default), uses the
#'   number of non-missing observations in `x`.
#'
#' @return Numeric vector containing B bootstrap median estimates.
#' @export
bootstrap_median <- function(x, B = 1000, n = NULL) {

  x <- x[!is.na(x)]

  if (length(x) == 0) {
    stop("`x` must contain at least one non-missing value.")
  }

  if (!is.numeric(B) || length(B) != 1 || is.na(B) || B <= 0 || B != as.integer(B)) {
    stop("`B` must be a positive integer.")
  }

  if (is.null(n)) {
    n <- length(x)
  }

  if (!is.numeric(n) || length(n) != 1 || is.na(n) || n <= 0 || n != as.integer(n)) {
    stop("`n` must be a positive integer.")
  }

  boot_medians <- numeric(B)

  for (b in seq_len(B)) {
    sample_x <- sample(x, n, replace = TRUE)
    boot_medians[b] <- stats::median(sample_x)
  }

  boot_medians
}


#' Bootstrap confidence interval
#'
#' Compute a percentile bootstrap confidence interval.
#'
#' @param boot_values Numeric vector of bootstrap estimates.
#' @param conf_level Confidence level. Default is 0.95.
#'
#' @return A numeric vector containing the lower and upper confidence bounds.
#' @export
bootstrap_ci <- function(boot_values, conf_level = 0.95) {
  if (!is.numeric(conf_level) || length(conf_level) != 1 || is.na(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a single number between 0 and 1.")
  }

  alpha <- 1 - conf_level
  stats::quantile(boot_values, c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)
}


#' Bootstrap statistic estimator
#'
#' Generate bootstrap estimates of a statistic using resampling
#' with replacement.
#'
#' @param x Numeric vector.
#' @param stat Function used to compute the statistic. Default is mean.
#' @param B Number of bootstrap samples. Default is 1000.
#' @param n Size of each bootstrap sample. If NULL (default), uses the
#'   number of non-missing observations in `x`.
#' @param ... Additional arguments.
#'
#' @return A numeric vector containing B bootstrap statistic estimates.
#' @export
bootstrap_stat <- function(x, stat = mean, B = 1000, n = NULL, ...) {

  x <- x[!is.na(x)]

  if (is.null(n)) {
    n <- length(x)
  }

  boot_vals <- numeric(B)

  for (b in seq_len(B)) {
    sample_x <- sample(x, n, replace = TRUE)
    boot_vals[b] <- stat(sample_x, ...)
  }

  boot_vals
}


#' Bootstrap difference in means
#'
#' Generate bootstrap estimates of the difference in means between two groups
#' using resampling with replacement.
#'
#' @param x Numeric vector of observations.
#' @param group Vector indicating group membership (must contain two groups).
#' @param B Number of bootstrap samples. Default is 1000.
#' @param n Size of each bootstrap sample. If NULL (default), uses the
#'   original sample sizes of each group.
#'
#' @return A numeric vector containing B bootstrap estimates of the mean difference.
#' @export
bootstrap_diff_mean <- function(x, group, B = 1000, n = NULL) {

  if (length(x) != length(group)) {
    stop("`x` and `group` must have the same length.")
  }

  g <- sort(unique(group))

  if (length(g) != 2) {
    stop("`group` must contain exactly two groups.")
  }

  x1 <- x[group == g[1]]
  x2 <- x[group == g[2]]

  if (is.null(n)) {
    n1 <- length(x1)
    n2 <- length(x2)
  } else {
    n1 <- n
    n2 <- n
  }

  boot_diff <- numeric(B)

  for (b in seq_len(B)) {
    s1 <- sample(x1, n1, replace = TRUE)
    s2 <- sample(x2, n2, replace = TRUE)

    boot_diff[b] <- mean(s1) - mean(s2)
  }

  boot_diff
}


#' Bootstrap difference in medians
#'
#' Generate bootstrap estimates of the difference in medians between two groups
#' using resampling with replacement.
#'
#' @param x Numeric vector of observations.
#' @param group Vector indicating group membership (must contain two groups).
#' @param B Number of bootstrap samples. Default is 1000.
#' @param n Size of each bootstrap sample. If NULL (default), uses the
#'   original sample sizes of each group.
#'
#' @return A numeric vector containing B bootstrap estimates of the median difference.
#' @export
bootstrap_diff_median <- function(x, group, B = 1000, n = NULL) {

  if (length(x) != length(group)) {
    stop("`x` and `group` must have the same length.")
  }

  g <- sort(unique(group))

  if (length(g) != 2) {
    stop("`group` must contain exactly two groups.")
  }

  x1 <- x[group == g[1]]
  x2 <- x[group == g[2]]

  if (is.null(n)) {
    n1 <- length(x1)
    n2 <- length(x2)
  } else {
    n1 <- n
    n2 <- n
  }

  boot_diff <- numeric(B)

  for (b in seq_len(B)) {
    s1 <- sample(x1, n1, replace = TRUE)
    s2 <- sample(x2, n2, replace = TRUE)

    boot_diff[b] <- median(s1) - median(s2)
  }

  boot_diff
}


#' Bootstrap summary statistics
#'
#' Compute summary statistics for bootstrap estimates, including
#' the bootstrap mean, standard error, and confidence interval.
#'
#' @param boot_values Numeric vector of bootstrap estimates.
#' @param original Original statistic value (optional).
#' @param conf_level Confidence level. Default is 0.95.
#'
#' @return A list containing the original estimate, bootstrap mean,
#' standard error, and confidence interval.
#' @export
bootstrap_summary <- function(boot_values, original = NULL, conf_level = 0.95) {

  alpha <- 1 - conf_level

  ci <- stats::quantile(
    boot_values,
    c(alpha / 2, 1 - alpha / 2),
    na.rm = TRUE
  )

  list(
    original = original,
    bootstrap_mean = mean(boot_values, na.rm = TRUE),
    std_error = sd(boot_values, na.rm = TRUE),
    conf_int = ci
  )
}
