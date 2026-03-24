output$bootstrap_hist <- renderPlot({

  set.seed(input$seed)

  if (input$group == "dep") {
    x <- sleep_dep}
  else {
    x <- sleep_nodep}

  if (input$stat_type == "mean") {
    bootstrap_value <- bootstrap_mean(x, B = input$B)
    original_stat <- mean(x)
    stats_name <- "Mean Sleep Duration"}
  else {bootstrap_value <- bootstrap_median(x, B = input$B)
  original_stat <- median(x)
  stats_name <- "Median Sleep Duration"}

  ci <- bootstrap_ci(bootstrap_value, conf_level = input$conf_level)

  hist(bootstrap_value, col = input$plot_color, border = "white",
       xlab = paste("Values of", stats_name),
       main = paste("Bootstrap Distribution of", stats_name))

  abline(v = ci[1], lwd = 2, lty = 3)
  abline(v = original_stat, lwd = 3, lty = 2)
  abline(v = ci[2], lwd = 2, lty = 3)

})


output$summary_table <- renderTable({

  set.seed(input$seed)

  if (input$group == "dep") {
    x <- sleep_dep}
  else {
    x <- sleep_nodep}

  if (input$stat_type == "mean") {
    bootstrap_value <- bootstrap_mean(x, B = input$B)
    original_stat <- mean(x)}
  else {bootstrap_value <- bootstrap_median(x, B = input$B)
  original_stat <- median(x)}

  summary <- bootstrap_summary(bootstrap_value,
                    original = original_stat,
                    conf_level = input$conf_level)

  data.frame(
    Statistic = c("Original", "Bootstrap Mean", "Standard Error", "Lower CI",
                  "Upper CI"),
    Value = c(summary$original, summary$bootstrap_mean,
              summary$std_error,
              summary$conf_int[1],
              summary$conf_int[2])
  )
}, digits = 4)


output$download_plot <- downloadHandler(

  filename = function() {
    paste0("bootstrap_plot_", input$group, "_", input$stat_type, ".pdf")
  },

  content = function(file) {
    set.seed(input$seed)

    if (input$group == "dep") {
      x <- sleep_dep}
    else {
      x <- sleep_nodep}

    if (input$stat_type == "mean") {
      bootstrap_value <- bootstrap_mean(x, B = input$B)
      original_stat <- mean(x)
      stats_name <- "Mean Sleep Duration"}
    else {bootstrap_value <- bootstrap_median(x, B = input$B)
    original_stat <- median(x)
    stats_name <- "Median Sleep Duration"}

    ci <- bootstrap_ci(bootstrap_value, conf_level = input$conf_level)

    pdf(file, width = 9, height = 5)

    hist(bootstrap_value, col = input$plot_color, border = "white",
         xlab = paste("Values of", stats_name),
         main = paste("Bootstrap Distribution of", stats_name))

    abline(v = ci[1], lwd = 2, lty = 3)
    abline(v = original_stat, lwd = 3, lty = 2)
    abline(v = ci[2], lwd = 2, lty = 3)

    dev.off()
  }
)


output$interpretation <- renderText({

  set.seed(input$seed)

  if (input$group == "dep") {
    x <- sleep_dep
  } else {
    x <- sleep_nodep
  }

  if (input$stat_type == "mean") {
    bootstrap_value <- bootstrap_mean(x, B = input$B)
    original_stat <- mean(x)
    stat_name <- "mean"
  } else {
    bootstrap_value <- bootstrap_median(x, B = input$B)
    original_stat <- median(x)
    stat_name <- "median"
  }

  ci <- bootstrap_ci(bootstrap_value, conf_level = input$conf_level)

  paste0(
    "With a ", input$conf_level * 100, "% confidence level, the true ",
    stat_name, " sleep duration is estimated to be between ",
    round(ci[1], 3), " and ", round(ci[2], 3), " hours. ",
    "The observed ", stat_name, " is ", round(original_stat, 3), " hours."
  )
})


output$data_info <- renderText({
  paste(
    "This dataset was obtained from the Dryad Digital Repository:",
    "https://doi.org/10.5061/dryad.73f69.",
    "",
    "The data comes from a study investigating the relationship between",
    "natural light exposure, sleep duration, and depression among workers",
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
