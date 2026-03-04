## ----include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE
)
knitr::opts_chunk$set(fig.width=7, fig.height=6)

## -----------------------------------------------------------------------------
source("~/Documents/STA380project/R/bootstrap.R")

## -----------------------------------------------------------------------------
set.seed(1)
n <- 10^4
uptime  <- c( 7.0, 6.0, 9.0, 7.5, 6.0)
bedtime <- c(22.0, 22.5, 1.0, 0.0, 0.5)
sleep_hours <- compute_sleep_duration(bedtime, uptime)
sleep_hours
summary(sleep_hours)

## -----------------------------------------------------------------------------
set.seed(1)
B <- 10^4
boot_means_sleep <- bootstrap_mean(sleep_hours, B = B)
head(boot_means_sleep)
length(boot_means_sleep)

## -----------------------------------------------------------------------------
hist(boot_means_sleep,
     main = "Bootstrap distribution of mean sleep duration",
     xlab = "Bootstrap means(hours)")

## -----------------------------------------------------------------------------
original_mean <- mean(sleep_hours)

boot_mean <- mean(boot_means_sleep)
boot_se <- sd(boot_means_sleep)
boot_bias <- boot_mean - original_mean
boot_median <- median(boot_means_sleep)
boot_ci <- quantile(boot_means_sleep, c(0.025, 0.975))

mean_summary <- data.frame(
  Metric = c("Original estimate",
             "Bootstrap mean",
             "Bootstrap SE",
             "Bias",
             "Bootstrap median",
             "95% CI (2.5%)",
             "95% CI (97.5%)"),
  Value = c(original_mean,
            boot_mean,
            boot_se,
            boot_bias,
            boot_median,
            boot_ci[1],
            boot_ci[2])
)

mean_summary

## -----------------------------------------------------------------------------
set.seed(1)
B <- 10^4
boot_median_sleep <- bootstrap_median(sleep_hours, B = B)
head(boot_median_sleep)
length(boot_median_sleep)

## -----------------------------------------------------------------------------
hist(boot_median_sleep,
     main = "Bootstrap distribution of median sleep duration",
     xlab = "Bootstrap median(hours)")

## -----------------------------------------------------------------------------
original_median <- median(sleep_hours)

boot_mean_med <- mean(boot_median_sleep)
boot_se_med <- sd(boot_median_sleep)
boot_bias_med <- boot_mean_med - original_median
boot_median_med <- median(boot_median_sleep)
boot_ci_med <- quantile(boot_median_sleep, c(0.025, 0.975))

median_summary <- data.frame(
  Metric = c("Original estimate",
             "Bootstrap mean",
             "Bootstrap SE",
             "Bias",
             "Bootstrap median",
             "95% CI (2.5%)",
             "95% CI (97.5%)"),
  Value = c(original_median,
            boot_mean_med,
            boot_se_med,
            boot_bias_med,
            boot_median_med,
            boot_ci_med[1],
            boot_ci_med[2])
)

median_summary

## -----------------------------------------------------------------------------
cl95_sleep <- bootstrap_ci(boot_means_sleep, conf_level = 0.95)
cl95_sleep

## -----------------------------------------------------------------------------
sleep_sample <- sleep_hours[!is.na(sleep_hours)]
n <- length(sleep_sample)
if (n < 10) {
  warning("Sample size n is too small!")
}

## -----------------------------------------------------------------------------
sleep_sample <- sleep_hours[!is.na(sleep_hours)]
Q1 <- quantile(sleep_sample, 0.25)
Q3 <- quantile(sleep_sample, 0.75)
IQR <- IQR(sleep_sample)
lower_tail <- Q1 - 1.5 * IQR
upper_tail <- Q3 + 1.5 * IQR

boxplot(sleep_sample,
        main = "Boxplot of sleep duration",
        ylab = "Sleep duration(hours)")

