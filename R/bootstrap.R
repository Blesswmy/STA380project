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
#' @param n Size of each bootstrap sample. If NULL (default), the sample
#'
#' @return A numeric vector containing B bootstrap mean estimates.
#' @export
bootstrap_mean <- function(x, B = 1000, n = NULL) {

  x <- x[!is.na(x)]
  ## Anna: added support where the user can specify the bootstrap sample size.
  ## you will need to document this yourself
  ## (and please add to other functions.)
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
#'
#' @return Numeric vector containing B bootstrap median estimates.
#' @export
bootstrap_median <- function(x, B = 1000) {

  x <- x[!is.na(x)]
  n <- length(x)

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

  alpha <- 1 - conf_level

  stats::quantile(boot_values, c(alpha / 2, 1 - alpha / 2))
}
