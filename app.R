# =============================================================================
# Neuropsych Test Battery Builder
# Version 4.3 - Fixed labels + Original order + PDF export
# =============================================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(tibble)
library(stringr)
library(readr)

# =============================================================================
# DATA LOADING FUNCTIONS
# =============================================================================

clean_string <- function(x) {
  x <- as.character(x)
  x <- gsub("\uFEFF", "", x)
  x <- gsub("[\r\n\t]", "", x)
  x <- trimws(x)
  x
}

load_test_catalog <- function(path = "data/test_catalog.csv") {
  df <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  names(df) <- clean_string(names(df))
  
  df |>
    mutate(
      test_id = clean_string(test_id),
      test_name = clean_string(test_name),
      domain = clean_string(domain),
      admin_format = clean_string(admin_format),
      age_range = clean_string(age_range),
      notes = if_else(is.na(notes), "", clean_string(notes))
    ) |>
    filter(test_id != "") |>
    distinct(test_id, .keep_all = TRUE)
}

load_battery_catalog <- function(path = "data/battery_catalog.csv") {
  df <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  names(df) <- clean_string(names(df))
  
  df |>
    mutate(
      battery_name = clean_string(battery_name),
      description = clean_string(description),
      age_group = clean_string(age_group)
    ) |>
    filter(battery_name != "") |>
    distinct(battery_name, .keep_all = TRUE)
}

load_battery_tests <- function(path = "data/battery_tests.csv") {
  df <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  names(df) <- clean_string(names(df))
  
  df |>
    mutate(
      battery_name = clean_string(battery_name),
      test_id = clean_string(test_id),
      required = as.logical(required),
      order_in_battery = as.integer(order_in_battery)
    ) |>
    filter(battery_name != "", test_id != "")
}

build_battery_definitions <- function(battery_catalog, battery_tests) {
  bat_names <- battery_catalog$battery_name
  out <- setNames(vector("list", length(bat_names)), bat_names)
  
  for (b in bat_names) {
    meta <- battery_catalog |> filter(battery_name == b) |> slice(1)
    map <- battery_tests |> 
      filter(battery_name == b) |>
      arrange(desc(required), order_in_battery)
    
    out[[b]] <- list(
      description = meta$description,
      age_group = meta$age_group,
      # Store as tibbles to preserve order
      required_df = map |> filter(required) |> select(test_id, order_in_battery),
      optional_df = map |> filter(!required) |> select(test_id, order_in_battery),
      # Keep simple vectors for backward compatibility
      required = map |> filter(required) |> pull(test_id),
      optional = map |> filter(!required) |> pull(test_id)
    )
  }
  out
}

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Get tests for a battery with full details, preserving original order
get_battery_tests <- function(battery_name, test_catalog, battery_definitions) {
  empty_result <- tibble(
    test_id = character(), test_name = character(), domain = character(),
    admin_format = character(), age_range = character(), notes = character(),
    required = logical(), battery = character(), order_in_battery = integer()
  )
  
  if (!battery_name %in% names(battery_definitions)) {
    return(empty_result)
  }
  
  battery <- battery_definitions[[battery_name]]
  
  # Join with order info to preserve original order
  required_df <- battery$required_df |>
    left_join(test_catalog, by = "test_id") |>
    mutate(required = TRUE, battery = battery_name) |>
    filter(!is.na(test_name), test_name != "")
  
  optional_df <- battery$optional_df |>
    left_join(test_catalog, by = "test_id") |>
    mutate(required = FALSE, battery = battery_name) |>
    filter(!is.na(test_name), test_name != "")
  
  # Combine and sort: required first, then by original order
  bind_rows(required_df, optional_df) |>
    arrange(desc(required), order_in_battery) |>
    select(test_id, test_name, domain, admin_format, age_range, notes, 
           required, battery, order_in_battery)
}

#' Create display label for test (with star for required)
format_test_label <- function(test_name, required) {
  n <- length(test_name)
  if (n == 0) return(character(0))
  
  test_name <- as.character(test_name)
  required <- rep_len(as.logical(required), n)
  test_name <- ifelse(is.na(test_name) | test_name == "", "[Unknown Test]", test_name)
  required <- ifelse(is.na(required), FALSE, required)
  ifelse(required, paste0("\u2605 ", test_name), test_name)
}

#' Safely create named vector for checkbox choices
safe_set_names <- function(values, labels) {
  values <- as.character(values)
  labels <- as.character(labels)
  
  keep <- !(is.na(values) | values == "")
  values <- values[keep]
  labels <- labels[keep]
  labels <- ifelse(is.na(labels) | labels == "", "[Unknown Test]", labels)
  
  if (length(values) == 0) return(character(0))
  setNames(values, labels)
}

# =============================================================================
# PDF EXPORT FUNCTION
# =============================================================================

#' Render test sheet PDF using Quarto + Typst
render_test_sheet_pdf <- function(out_file, template_path, params, tmp_root = tempdir()) {
  
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The 'quarto' R package is required for PDF export. Install with: install.packages('quarto')")
  }
  
  if (!file.exists(template_path)) {
    stop("Missing Quarto template: ", template_path)
  }
  
  # Create temp directory

  tmp_dir <- tempfile(tmpdir = tmp_root, pattern = "quarto_test_sheet_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  
  # Copy template to temp dir
  qmd_path <- file.path(tmp_dir, basename(template_path))
  file.copy(template_path, qmd_path, overwrite = TRUE)
  
  # Define output file name
  out_basename <- sub("\\.pdf$", "", basename(out_file))
  
  # Render with Quarto
  quarto::quarto_render(
    input = qmd_path,
    output_format = "typst",
    output_file = paste0(out_basename, ".pdf"),
    execute_params = params,
    execute_dir = tmp_dir,
    quiet = TRUE
  )
  
  # Find the output file
  rendered_file <- file.path(tmp_dir, paste0(out_basename, ".pdf"))
  
  if (!file.exists(rendered_file)) {
    stop("Quarto did not produce output file: ", rendered_file)
  }
  
  # Copy to final destination
  file.copy(rendered_file, out_file, overwrite = TRUE)
  invisible(out_file)
}

#' Write tests to CSV
write_tests_csv <- function(out_file, tests) {
  if (is.null(tests) || nrow(tests) == 0) {
    df <- tibble(Message = "No tests selected")
  } else {
    df <- tests |>
      transmute(
        Test = test_name,
        Domain = domain,
        Format = admin_format,
        Age_Range = age_range,
        Required = ifelse(required, "Yes", "No"),
        Notes = notes
      )
  }
  write.csv(df, out_file, row.names = FALSE)
  invisible(out_file)
}

# =============================================================================
# LOAD DATA
# =============================================================================

test_catalog <- load_test_catalog("data/test_catalog.csv")
battery_catalog <- load_battery_catalog("data/battery_catalog.csv")
battery_tests_map <- load_battery_tests("data/battery_tests.csv")
battery_definitions <- build_battery_definitions(battery_catalog, battery_tests_map)
battery_choices <- battery_catalog$battery_name

message("Loaded ", nrow(test_catalog), " tests, ", nrow(battery_catalog), " batteries")

# =============================================================================
# UI
# =============================================================================

ui <- dashboardPage(
  dashboardHeader(title = "Neuropsych Test\nBattery Builder"),

  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Patient Info", tabName = "patient", icon = icon("user")),
      menuItem("Select Tests", tabName = "tests", icon = icon("list-check")),
      menuItem("Review & Download", tabName = "review", icon = icon("file-pdf"))
    ),
    hr(),
    div(style = "padding: 10px;", tags$small("v4.3 - With PDF export"))
  ),

  dashboardBody(
    tags$head(tags$style(HTML(
      "
      .content-wrapper { background-color: #f4f4f4; }
      .box { border-radius: 5px; }
      .checkbox label { font-weight: normal; }
    "
    ))),

    tabItems(
      # Patient Info Tab
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
              placeholder = "Enter referral question..."
            )
          ),
          box(
            title = "Instructions",
            width = 6,
            status = "info",
            solidHeader = TRUE,
            tags$ol(
              tags$li("Enter basic patient information on this page."),
              tags$li(
                "Select a battery template and customize tests on the next page."
              ),
              tags$li("Review selected tests and download PDF or CSV.")
            )
          )
        )
      ),

      # Test Selection Tab
      tabItem(
        tabName = "tests",
        fluidRow(
          box(
            title = "Battery Template",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            selectInput(
              "template_battery",
              "Select a battery template:",
              choices = battery_choices,
              selected = battery_choices[[1]]
            ),
            uiOutput("battery_info"),
            hr(),
            actionButton(
              "select_required",
              "Select Required Only",
              class = "btn-info",
              width = "100%"
            ),
            br(),
            br(),
            actionButton(
              "select_all",
              "Select All Tests",
              class = "btn-success",
              width = "100%"
            ),
            br(),
            br(),
            actionButton(
              "clear_all",
              "Clear All",
              class = "btn-warning",
              width = "100%"
            )
          ),
          box(
            title = "Available Tests",
            width = 8,
            status = "info",
            solidHeader = TRUE,
            helpText(
              "\u2605 = Required test in this battery. Check/uncheck to customize."
            ),
            tabsetPanel(
              id = "test_tabs",
              tabPanel(
                "Required",
                div(
                  style = "max-height: 400px; overflow-y: auto;",
                  checkboxGroupInput(
                    "required_tests",
                    NULL,
                    choices = character(0)
                  )
                )
              ),
              tabPanel(
                "Optional",
                div(
                  style = "max-height: 400px; overflow-y: auto;",
                  checkboxGroupInput(
                    "optional_tests",
                    NULL,
                    choices = character(0)
                  )
                )
              ),
              tabPanel(
                "All Tests",
                div(
                  style = "max-height: 400px; overflow-y: auto;",
                  checkboxGroupInput("all_tests", NULL, choices = character(0))
                )
              )
            )
          )
        )
      ),

      # Review Tab
      tabItem(
        tabName = "review",
        fluidRow(
          box(
            title = "Selected Tests Summary",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            tableOutput("summary_table"),
            br(),
            verbatimTextOutput("summary_stats")
          ),
          box(
            title = "Download Options",
            width = 4,
            status = "success",
            solidHeader = TRUE,
            h4("Download Test Sheet"),
            helpText("Generate a formatted PDF of the selected test battery."),
            helpText(tags$em("Note: Requires Quarto + Typst installed.")),
            br(),
            downloadButton(
              "download_pdf",
              "Download PDF",
              class = "btn-success btn-lg",
              style = "width: 100%;"
            ),
            hr(),
            h4("Export Test List"),
            helpText("Download raw test list as CSV."),
            downloadButton(
              "download_csv",
              "Download CSV",
              class = "btn-info",
              style = "width: 100%;"
            )
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
  
  # Reactive: get tests for current battery (preserves order)
  battery_tests_reactive <- reactive({
    req(input$template_battery)
    get_battery_tests(input$template_battery, test_catalog, battery_definitions)
  })
  
  # Battery info display
  output$battery_info <- renderUI({
    req(input$template_battery)
    info <- battery_definitions[[input$template_battery]]
    if (is.null(info)) return(NULL)
    
    tagList(
      tags$p(tags$strong("Description: "), info$description),
      tags$p(tags$strong("Age Group: "), info$age_group),
      tags$p(tags$strong("Required: "), length(info$required), " tests"),
      tags$p(tags$strong("Optional: "), length(info$optional), " tests")
    )
  })
  
  # Update checkboxes when battery changes
  observeEvent(input$template_battery, {
    tests <- battery_tests_reactive()
    
    if (nrow(tests) == 0) {
      updateCheckboxGroupInput(session, "required_tests", choices = character(0), selected = character(0))
      updateCheckboxGroupInput(session, "optional_tests", choices = character(0), selected = character(0))
      updateCheckboxGroupInput(session, "all_tests", choices = character(0), selected = character(0))
      return()
    }
    
    # Required tests
    req_tests <- tests |> filter(required)
    if (nrow(req_tests) > 0) {
      req_labels <- format_test_label(req_tests$test_name, TRUE)
      req_choices <- safe_set_names(req_tests$test_id, req_labels)
      req_selected <- req_tests$test_id
    } else {
      req_choices <- character(0)
      req_selected <- character(0)
    }
    
    # Optional tests
    opt_tests <- tests |> filter(!required)
    if (nrow(opt_tests) > 0) {
      opt_labels <- format_test_label(opt_tests$test_name, FALSE)
      opt_choices <- safe_set_names(opt_tests$test_id, opt_labels)
    } else {
      opt_choices <- character(0)
    }
    
    # All tests
    all_labels <- format_test_label(tests$test_name, tests$required)
    all_choices <- safe_set_names(tests$test_id, all_labels)
    
    # Update inputs
    updateCheckboxGroupInput(session, "required_tests", choices = req_choices, selected = req_selected)
    updateCheckboxGroupInput(session, "optional_tests", choices = opt_choices, selected = character(0))
    updateCheckboxGroupInput(session, "all_tests", choices = all_choices, selected = req_selected)
  })
  
  # Sync from All Tests -> required/optional tabs
  observeEvent(input$all_tests, {
    tests <- battery_tests_reactive()
    if (nrow(tests) == 0) return()
    
    selected <- input$all_tests %||% character(0)
    req_ids <- tests |> filter(required) |> pull(test_id)
    opt_ids <- tests |> filter(!required) |> pull(test_id)
    
    updateCheckboxGroupInput(session, "required_tests", selected = intersect(selected, req_ids))
    updateCheckboxGroupInput(session, "optional_tests", selected = intersect(selected, opt_ids))
  }, ignoreNULL = FALSE)
  
  # Button: Select Required Only
  observeEvent(input$select_required, {
    tests <- battery_tests_reactive()
    if (nrow(tests) == 0) return()
    req_ids <- tests |> filter(required) |> pull(test_id)
    updateCheckboxGroupInput(session, "all_tests", selected = req_ids)
    updateCheckboxGroupInput(session, "required_tests", selected = req_ids)
    updateCheckboxGroupInput(session, "optional_tests", selected = character(0))
  })
  
  # Button: Select All
  observeEvent(input$select_all, {
    tests <- battery_tests_reactive()
    if (nrow(tests) == 0) return()
    all_ids <- tests$test_id
    req_ids <- tests |> filter(required) |> pull(test_id)
    opt_ids <- tests |> filter(!required) |> pull(test_id)
    updateCheckboxGroupInput(session, "all_tests", selected = all_ids)
    updateCheckboxGroupInput(session, "required_tests", selected = req_ids)
    updateCheckboxGroupInput(session, "optional_tests", selected = opt_ids)
  })
  
  # Button: Clear All
  observeEvent(input$clear_all, {
    updateCheckboxGroupInput(session, "all_tests", selected = character(0))
    updateCheckboxGroupInput(session, "required_tests", selected = character(0))
    updateCheckboxGroupInput(session, "optional_tests", selected = character(0))
  })
  
  # Reactive: selected tests (preserves original order)
  selected_tests <- reactive({
    selected_ids <- input$all_tests
    if (is.null(selected_ids) || length(selected_ids) == 0) return(tibble())
    
    all_tests <- battery_tests_reactive()
    
    all_tests |>
      filter(test_id %in% selected_ids) |>
      arrange(desc(required), order_in_battery)
  })
  
  # Summary table
  output$summary_table <- renderTable({
    tests <- selected_tests()
    if (nrow(tests) == 0) return(data.frame(Message = "No tests selected"))
    
    tests |>
      transmute(
        Test = test_name,
        Domain = domain,
        Format = admin_format,
        `Age Range` = age_range,
        Required = ifelse(required, "Yes", "No"),
        Notes = notes
      )
  })
  
  output$summary_stats <- renderPrint({
    tests <- selected_tests()
    if (nrow(tests) == 0) {
      cat("No tests selected")
      return()
    }
    
    n_required <- sum(tests$required)
    n_optional <- sum(!tests$required)
    n_total <- nrow(tests)
    
    cat(sprintf("Total tests selected: %d\n", n_total))
    cat(sprintf("  Required: %d\n", n_required))
    cat(sprintf("  Optional: %d\n", n_optional))
    cat(sprintf("\nDomains covered: %s\n", paste(unique(tests$domain), collapse = ", ")))
  })
  
  # Sync battery type from patient page
  observeEvent(input$battery_type, {
    updateSelectInput(session, "template_battery", selected = input$battery_type)
  })
  
  # PDF Download
  output$download_pdf <- downloadHandler(
    filename = function() {
      nm <- if (nzchar(input$patient_name)) gsub("\\s+", "_", input$patient_name) else "test_battery"
      paste0(nm, "_test_sheet.pdf")
    },
    content = function(file) {
      # Show notification
      showNotification("Generating PDF...", type = "message", duration = 2)
      
      template_path <- "templates/test_sheet.qmd"
      
      # Check if template exists
      if (!file.exists(template_path)) {
        showNotification("Template not found! Check templates/test_sheet.qmd", type = "error")
        stop("Template not found: ", template_path)
      }
      
      params <- list(
        patient_name = input$patient_name,
        age = input$age,
        battery_type = input$template_battery,
        referral = input$referral,
        selected_tests = selected_tests()
      )
      
      tryCatch({
        render_test_sheet_pdf(
          out_file = file,
          template_path = template_path,
          params = params
        )
        showNotification("PDF generated successfully!", type = "message")
      }, error = function(e) {
        showNotification(paste("PDF generation failed:", e$message), type = "error", duration = 10)
        stop(e)
      })
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
