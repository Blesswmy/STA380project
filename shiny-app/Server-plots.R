bootstrap_p_value <- function(bootstrap_value, null_value = 0) {
  p <- 2 * min(
    mean(bootstrap_value <= null_value),
    mean(bootstrap_value >= null_value)
  )
  min(p, 1)
}

bootstrap_results <- reactive({

  withProgress(message = "Running bootstrap analysis...", value = 0, {

    incProgress(0.15, detail = "Preparing data")

    set.seed(input$seed)

    group_label <- switch(
      input$group,
      "dep" = "Depression",
      "nodep" = "No Depression",
      "diff" = "Difference (Depression - No Depression)"
    )

    incProgress(0.35, detail = "Generating bootstrap samples")

    if (input$group == "diff") {

      if (input$stat_type == "mean") {
        boot_dep <- bootstrap_mean(sleep_dep, B = input$B)
        boot_nodep <- bootstrap_mean(sleep_nodep, B = input$B)
        bootstrap_value <- boot_dep - boot_nodep
        original_stat <- mean(sleep_dep) - mean(sleep_nodep)
        stats_name <- "Mean Sleep Duration Difference"
        stat_name <- "mean difference"
      } else {
        boot_dep <- bootstrap_median(sleep_dep, B = input$B)
        boot_nodep <- bootstrap_median(sleep_nodep, B = input$B)
        bootstrap_value <- boot_dep - boot_nodep
        original_stat <- median(sleep_dep) - median(sleep_nodep)
        stats_name <- "Median Sleep Duration Difference"
        stat_name <- "median difference"
      }

    } else {

      if (input$group == "dep") {
        x <- sleep_dep
      } else {
        x <- sleep_nodep
      }

      if (input$stat_type == "mean") {
        bootstrap_value <- bootstrap_mean(x, B = input$B)
        original_stat <- mean(x)
        stats_name <- paste("Mean Sleep Duration -", group_label)
        stat_name <- "mean"
      } else {
        bootstrap_value <- bootstrap_median(x, B = input$B)
        original_stat <- median(x)
        stats_name <- paste("Median Sleep Duration -", group_label)
        stat_name <- "median"
      }
    }

    incProgress(0.25, detail = "Computing confidence interval and p-value")

    ci <- bootstrap_ci(bootstrap_value, conf_level = input$conf_level)

    if (input$group == "diff") {
      p_value <- bootstrap_p_value(bootstrap_value, null_value = 0)
    } else {
      p_value <- NA
    }

    incProgress(0.25, detail = "Finalizing output")

    list(
      bootstrap_value = bootstrap_value,
      original_stat = original_stat,
      stats_name = stats_name,
      stat_name = stat_name,
      ci = ci,
      group_label = group_label,
      p_value = p_value
    )
  })
})


summary_results <- reactive({

  res <- bootstrap_results()

  summary <- bootstrap_summary(
    res$bootstrap_value,
    original = res$original_stat,
    conf_level = input$conf_level
  )

  if (input$group == "diff") {
    data.frame(
      Statistic = c(
        "Original",
        "Bootstrap Mean",
        "Standard Error",
        "Lower CI",
        "Upper CI",
        "Bootstrap p-value"
      ),
      Value = c(
        summary$original,
        summary$bootstrap_mean,
        summary$std_error,
        summary$conf_int[1],
        summary$conf_int[2],
        res$p_value
      )
    )
  } else {
    data.frame(
      Statistic = c(
        "Original",
        "Bootstrap Mean",
        "Standard Error",
        "Lower CI",
        "Upper CI"
      ),
      Value = c(
        summary$original,
        summary$bootstrap_mean,
        summary$std_error,
        summary$conf_int[1],
        summary$conf_int[2]
      )
    )
  }
})


output$bootstrap_hist <- renderPlot({

  res <- bootstrap_results()

  hist(
    res$bootstrap_value,
    col = input$plot_color,
    border = "white",
    xlab = paste("Values of", res$stats_name),
    main = paste("Bootstrap Distribution of", res$stats_name)
  )

  abline(v = res$ci[1], lwd = 2, lty = 3)
  abline(v = res$original_stat, lwd = 3, lty = 2)
  abline(v = res$ci[2], lwd = 2, lty = 3)

  legend(
    "topright",
    legend = c("Lower CI", "Observed Statistic", "Upper CI"),
    lty = c(3, 2, 3),
    lwd = c(2, 3, 2),
    bty = "n"
  )
})


output$summary_table <- renderTable({
  summary_results()
}, digits = 4)

output$results_summary <- renderUI({

  res <- bootstrap_results()

  ci_text <- paste0("[", round(res$ci[1], 4), ", ", round(res$ci[2], 4), "]")

  if (input$group == "diff") {

    significance_text <- if (res$ci[1] <= 0 && res$ci[2] >= 0) {
      "The confidence interval includes 0, so the difference may not be statistically significant."
    } else {
      "The confidence interval does not include 0, so the difference may be statistically significant."
    }

    tagList(
      tags$p(tags$strong("Observed statistic: "), round(res$original_stat, 4)),
      tags$p(tags$strong("Confidence interval: "), ci_text),
      tags$p(tags$strong("Bootstrap p-value: "), round(res$p_value, 4)),
      tags$p(tags$strong("Sample sizes: "),
             paste0("Depression = ", length(sleep_dep),
                    ", No Depression = ", length(sleep_nodep))),
      tags$p(tags$strong("Interpretation: "), significance_text),
      tags$p(tags$strong("Method note: "),
             "Sleep duration was corrected for overnight sleep across midnight.")
    )

  } else {

    group_n <- if (input$group == "dep") length(sleep_dep) else length(sleep_nodep)

    tagList(
      tags$p(tags$strong("Observed statistic: "), round(res$original_stat, 4)),
      tags$p(tags$strong("Confidence interval: "), ci_text),
      tags$p(tags$strong("Sample size: "), group_n),
      tags$p(tags$strong("Method note: "),
             "Sleep duration was corrected for overnight sleep across midnight.")
    )
  }
})

output$download_plot <- downloadHandler(

  filename = function() {
    paste0("bootstrap_plot_", input$group, "_", input$stat_type, ".pdf")
  },

  content = function(file) {

    res <- bootstrap_results()

    pdf(file, width = 9, height = 5)

    hist(
      res$bootstrap_value,
      col = input$plot_color,
      border = "white",
      xlab = paste("Values of", res$stats_name),
      main = paste("Bootstrap Distribution of", res$stats_name)
    )

    abline(v = res$ci[1], lwd = 2, lty = 3)
    abline(v = res$original_stat, lwd = 3, lty = 2)
    abline(v = res$ci[2], lwd = 2, lty = 3)

    legend(
      "topright",
      legend = c("Lower CI", "Observed Statistic", "Upper CI"),
      lty = c(3, 2, 3),
      lwd = c(2, 3, 2),
      bty = "n"
    )

    dev.off()
  }
)


output$download_table <- downloadHandler(

  filename = function() {
    paste0("summary_table_", input$group, "_", input$stat_type, ".csv")
  },

  content = function(file) {
    write.csv(summary_results(), file, row.names = FALSE)
  }
)


output$download_bootstrap <- downloadHandler(

  filename = function() {
    paste0("bootstrap_values_", input$group, "_", input$stat_type, ".csv")
  },

  content = function(file) {

    res <- bootstrap_results()

    bootstrap_df <- data.frame(
      Bootstrap_Iteration = seq_along(res$bootstrap_value),
      Bootstrap_Value = res$bootstrap_value
    )

    write.csv(bootstrap_df, file, row.names = FALSE)
  }
)


output$interpretation <- renderText({

  res <- bootstrap_results()

  if (input$group == "diff") {
    paste0(
      "With a ", input$conf_level * 100,
      "% confidence level, the true ", res$stat_name,
      " in sleep duration (Depression - No Depression) is estimated to be between ",
      round(res$ci[1], 4), " and ", round(res$ci[2], 4), " hours. ",
      "The observed ", res$stat_name, " is ",
      round(res$original_stat, 4), " hours. ",
      "The bootstrap p-value for testing no difference is ",
      round(res$p_value, 4), ". ",
      "A negative value means the Depression group slept less than the No Depression group."
    )
  } else {
    paste0(
      "With a ", input$conf_level * 100,
      "% confidence level, the true ", res$stat_name,
      " sleep duration for the ", res$group_label,
      " group is estimated to be between ",
      round(res$ci[1], 4), " and ", round(res$ci[2], 4), " hours. ",
      "The observed ", res$stat_name, " is ",
      round(res$original_stat, 4), " hours."
    )
  }
})


output$data_info <- renderText({
  paste(
    "This dataset was obtained from the Dryad Digital Repository:",
    "https://doi.org/10.5061/dryad.73f69.",
    "",
    "The data comes from a study investigating the relationship between",
    "natural light exposure, sleep duration, and depression among workers.",
    "",
    "The dataset includes information on sleep duration and depression status,",
    "which are used in this application for statistical analysis."
  )
})


output$purpose <- renderText({
  paste(
    "The purpose of this application is to explore sleep duration patterns",
    "and estimate population statistics using bootstrap resampling.",
    "",
    "Users can compare groups based on depression status and compute",
    "confidence intervals for the mean or median sleep duration.",
    "",
    "This helps illustrate variability in the data and provides insight",
    "into the relationship between sleep and mental health."
  )
})
