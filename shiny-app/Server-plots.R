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
         xlab = paste("Values of ",stats_name),
         main = paste("Bootstrap Distribution of", stats_name))

    ab_lowerCI <- abline(v = ci[1], lwd = 2, lty = 3)
    ab_stats <- abline(v = original_stat, lwd = 3, lty = 2)
    ab_upperCI <- abline(v = ci[2], lwd = 2, lty = 3)
  })

