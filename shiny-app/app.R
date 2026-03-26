library(shiny)
library(bslib)
library(shinycssloaders)
library(readxl)

source("../Submissions/BasicImplementation/bootstrap.R")

options(spinner.type = 8, spinner.color = "#6990EE")

data <- read_excel("../Data&PackCom/extdata/Marqueze+et+al.xls")

data <- na.omit(data)

weekday_sleep <- ifelse(
  data$Uptimew >= data$Bedtimew,
  data$Uptimew - data$Bedtimew,
  data$Uptimew + 24 - data$Bedtimew
)

weekend_sleep <- ifelse(
  data$Uptimef >= data$Bedtimef,
  data$Uptimef - data$Bedtimef,
  data$Uptimef + 24 - data$Bedtimef
)

sleep_duration <- (weekday_sleep * 5 + weekend_sleep * 2) / 7

sleep_dep <- sleep_duration[data$Depression == 1]
sleep_nodep <- sleep_duration[data$Depression == 0]


ui <- page_sidebar(

  theme = bs_theme(version = 5,
                   bootswatch = "flatly",
                   "navbar-bg" = "#2C3E50"),

  title = "Bootstrap Estimation of Sleep Duration",

  sidebar = sidebar(

    numericInput("seed",
                "Insert the seed",
                value = 1,
                min = 1,
                step = 1),

    selectInput("group",
                "Choose the group that you want to see",
                choices = list("Depression" = "dep",
                               "No Depression" = "nodep",
                               "Difference (Depression - No Depression)" = "diff"),
                selected = "dep"),

    helpText("For the difference option, the app computes Depression - No Depression."),

    selectInput("stat_type",
                "Statistic to Bootstrap",
                choices = list("Mean Sleep Duration" = "mean",
                               "Median Sleep Duration" = "median"),
                selected = "mean"),

    numericInput("B",
                 "Number of Bootstrap Samples",
                 value = 1000),

    sliderInput("conf_level",
                "Confidence Level",
                min = 0.90,
                max = 0.99,
                value = 0.95),

    selectInput("plot_color",
                "Which color would you like for your Histogram?",
                choices = list("Red" = "red",
                               "Blue" = "blue",
                               "Black" = "black"),
                selected = "blue"),

    actionButton("reset", "Reset Inputs"),

    width = 400,
    open = "always"
  ),

  withSpinner(plotOutput("bootstrap_hist")),

  downloadButton("download_plot", "Download Plot"),
  downloadButton("download_table", "Download Summary Table"),
  downloadButton("download_bootstrap", "Download Bootstrap Values"),

  card(
    fill = FALSE,
    card_header("Summary Statistics"),
    card_body(
      tableOutput("summary_table")
    )
  ),

  card(
    fill = FALSE,
    card_header("Interpretation"),
    card_body(
      div(style = "white-space: normal; line-height: 1.6; font-size: 15px;",
          textOutput("interpretation"))
    )
  ),

  card(
    fill = FALSE,
    card_header("About the Data"),
    card_body(
      div(style = "white-space: normal; line-height: 1.6; font-size: 15px;",
          textOutput("data_info"))
    )
  ),

  card(
    fill = FALSE,
    card_header("Purpose of Analysis"),
    card_body(
      div(style = "white-space: normal; line-height: 1.6; font-size: 15px;",
          textOutput("purpose")))
  )
)


server <- function(input, output, session) {

  observeEvent(input$reset, {
    updateNumericInput(session, "seed", value = 1)
    updateSelectInput(session, "group", selected = "dep")
    updateSelectInput(session, "stat_type", selected = "mean")
    updateNumericInput(session, "B", value = 1000)
    updateSliderInput(session, "conf_level", value = 0.95)
    updateSelectInput(session, "plot_color", selected = "blue")
  })

  source(("Server-plots.R"), local = TRUE)
}

shinyApp(ui = ui, server = server)

