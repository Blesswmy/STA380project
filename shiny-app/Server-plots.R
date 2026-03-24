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
