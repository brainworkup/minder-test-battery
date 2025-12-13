# =============================================================================
# Neuropsych Test Battery Builder (Modular)
# =============================================================================

# Core packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(tibble)
library(stringr)
library(readr)

# Source utils + modules
source("R/utils/data.R")
source("R/utils/helpers.R")
source("R/utils/export.R")
source("R/modules/test_selection_module.R")
source("R/modules/summary_table_module.R")

# =============================================================================
# Data (externalized lookup tables)
# =============================================================================

message("Loading test catalog...")
test_catalog <- load_test_catalog("data/test_catalog.csv")
message("Loaded ", nrow(test_catalog), " tests")
message("Sample test_ids: ", paste(head(test_catalog$test_id, 10), collapse = ", "))

message("\nLoading battery catalog...")
battery_catalog <- load_battery_catalog("data/battery_catalog.csv")
message("Loaded ", nrow(battery_catalog), " batteries")

message("\nLoading battery tests...")
battery_tests_map <- load_battery_tests("data/battery_tests.csv")
message("Loaded ", nrow(battery_tests_map), " battery-test mappings")

message("\nBuilding battery definitions...")
battery_definitions <- build_battery_definitions(battery_catalog, battery_tests_map)

battery_choices <- battery_catalog$battery_name

# Validate
message("\nValidating batteries against test catalog...")
missing <- validate_batteries(test_catalog, battery_definitions)
if (length(missing) == 0) {
  message("All battery test_ids found in catalog!")
} else {
  message("Missing test count: ", sum(sapply(missing, length)))
}

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Neuropsych Test Battery Builder"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Patient Info", tabName = "patient", icon = icon("user")),
      menuItem("Select Tests", tabName = "tests", icon = icon("list-check")),
      menuItem("Review & Download", tabName = "review", icon = icon("file-pdf"))
    ),
    hr(),
    div(
      style = "padding: 10px;",
      tags$small("v3.1 - Debug version with logging")
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML(".content-wrapper { background-color: #f4f4f4; } .box { border-radius: 5px; } .checkbox label { font-weight: normal; }"))
    ),

    tabItems(
      tabItem(
        tabName = "patient",
        fluidRow(
          box(
            title = "Patient Information",
            width = 6,
            status = "primary",
            solidHeader = TRUE,
            textInput("patient_name", "Patient Name"),
            textInput("age", "Age"),
            selectInput(
              "battery_type",
              "Default Battery Type",
              choices = battery_choices,
              selected = battery_choices[[1]]
            ),
            textAreaInput(
              "referral",
              "Referral Question / Case Description",
              rows = 4,
              placeholder = "Enter referral question and relevant background..."
            )
          ),
          box(
            title = "Instructions",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            tags$ol(
              tags$li("Enter basic patient information on this page."),
              tags$li("Select a battery template and customize tests on the next page."),
              tags$li("Review selected tests and download a PDF test sheet.")
            )
          )
        )
      ),

      tabItem(
        tabName = "tests",
        testSelectionUI("test_selection", battery_choices = battery_choices)
      ),

      tabItem(
        tabName = "review",
        fluidRow(
          box(
            title = "Selected Tests Summary",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            summaryTableUI("summary")
          ),
          box(
            title = "Download Options",
            width = 4,
            status = "success",
            solidHeader = TRUE,
            h4("Download Test Sheet"),
            helpText("Generate a PDF of the selected test battery."),
            helpText(tags$em("Note: Requires Quarto + Typst for PDF rendering.")),
            br(),
            downloadButton("download_pdf", "Download PDF", class = "btn-success btn-lg"),
            hr(),
            h4("Export Test List"),
            downloadButton("download_csv", "Download CSV", class = "btn-info")
          )
        )
      )
    )
  )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  selected_tests <- testSelectionServer(
    "test_selection",
    test_catalog = test_catalog,
    battery_definitions = battery_definitions
  )

  summaryTableServer("summary", selected_tests)

  # Sync battery type from patient page -> module select
  observeEvent(input$battery_type, {
    updateSelectInput(session, "test_selection-template_battery", selected = input$battery_type)
  })

  # PDF Download
  output$download_pdf <- downloadHandler(
    filename = function() {
      nm <- if (nzchar(input$patient_name)) gsub("\\s+", "_", input$patient_name) else "test_battery"
      paste0(nm, "_test_sheet.pdf")
    },
    content = function(file) {
      template_path <- "templates/test_sheet.qmd"

      params <- list(
        patient_name = input$patient_name,
        age = input$age,
        battery_type = input$battery_type,
        referral = input$referral,
        selected_tests = selected_tests()
      )

      render_test_sheet_pdf(
        out_file = file,
        template_path = template_path,
        params = params,
        tmp_root = "/var/tmp"
      )
    }
  )

  # CSV Download
  output$download_csv <- downloadHandler(
    filename = function() {
      nm <- if (nzchar(input$patient_name)) gsub("\\s+", "_", input$patient_name) else "test_battery"
      paste0(nm, "_tests.csv")
    },
    content = function(file) {
      write_tests_csv(out_file = file, tests = selected_tests())
    }
  )
}

shinyApp(ui, server)
