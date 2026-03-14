library(shiny)
library(bslib)
library(shinycssloaders)
library(readxl)

source("../Submissions/BasicImplementation/bootstrap.R")

options(spinner.type = 8, spinner.color = "#6990EE")

data <- read_excel("../Data&PackCom/extdata/Marqueze+et+al.xls")

data <- na.omit(data)

weekday_sleep <- data$Uptimew - data$Bedtimew
weekend_sleep <- data$Uptimef - data$Bedtimef

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
                               "No Depression" = "nodep"),
                selected = "dep"),

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
                "Which color would you like for you Histogram?",
                choices = list("Red" = "red",
                               "Blue" = "blue",
                               "Black" = "black"),
                selected = "blue"),

    width = 400,
    open = "always"
  ),

  withSpinner(plotOutput("bootstrap_hist")),

  verbatimTextOutput("summary_text")
)


server <- function(input, output, session) {
  source(file.path("Server-plots.R"), local = TRUE)$value
}

shinyApp(ui = ui, server = server)
