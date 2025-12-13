# =============================================================================
# Module: Test selection (battery template + checkboxes)
# =============================================================================

testSelectionUI <- function(id, battery_choices) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shinydashboard::box(
        title = "Battery Template",
        width = 4,
        status = "primary",
        solidHeader = TRUE,
        shiny::selectInput(
          ns("template_battery"),
          "Select a battery template:",
          choices = battery_choices,
          selected = battery_choices[[1]]
        ),
        shiny::uiOutput(ns("battery_info")),
        shiny::hr(),
        shiny::actionButton(ns("select_required"), "Select Required Only",
          class = "btn-info", width = "100%"
        ),
        shiny::br(), shiny::br(),
        shiny::actionButton(ns("select_all"), "Select All Tests",
          class = "btn-success", width = "100%"
        ),
        shiny::br(), shiny::br(),
        shiny::actionButton(ns("clear_all"), "Clear All",
          class = "btn-warning", width = "100%"
        )
      ),
      shinydashboard::box(
        title = "Available Tests",
        width = 8,
        status = "info",
        solidHeader = TRUE,
        shiny::helpText("★ = Required test in this battery. Check/uncheck to customize."),
        shiny::tabsetPanel(
          id = ns("test_tabs"),
          shiny::tabPanel(
            "Required",
            shiny::div(
              style = "max-height: 400px; overflow-y: auto;",
              shiny::checkboxGroupInput(ns("required_tests"), label = NULL, choices = character(0))
            )
          ),
          shiny::tabPanel(
            "Optional",
            shiny::div(
              style = "max-height: 400px; overflow-y: auto;",
              shiny::checkboxGroupInput(ns("optional_tests"), label = NULL, choices = character(0))
            )
          ),
          shiny::tabPanel(
            "All Tests",
            shiny::div(
              style = "max-height: 400px; overflow-y: auto;",
              shiny::checkboxGroupInput(ns("all_tests"), label = NULL, choices = character(0))
            )
          )
        )
      )
    )
  )
}


testSelectionServer <- function(id, test_catalog, battery_definitions) {
  shiny::moduleServer(id, function(input, output, session) {

    battery_tests <- shiny::reactive({
      shiny::req(input$template_battery)
      get_battery_tests(input$template_battery, test_catalog, battery_definitions)
    })

    battery_info <- shiny::reactive({
      shiny::req(input$template_battery)
      battery_definitions[[input$template_battery]]
    })

    output$battery_info <- shiny::renderUI({
      info <- battery_info()
      if (is.null(info)) return(NULL)

      shiny::tagList(
        shiny::tags$p(shiny::tags$strong("Description: "), info$description),
        shiny::tags$p(shiny::tags$strong("Age Group: "), info$age_group),
        shiny::tags$p(shiny::tags$strong("Required: "), length(info$required), " tests"),
        shiny::tags$p(shiny::tags$strong("Optional: "), length(info$optional), " tests")
      )
    })

    # Update checkboxes when battery changes
    shiny::observeEvent(input$template_battery, {
      tests <- battery_tests()

      if (nrow(tests) == 0) {
        shiny::updateCheckboxGroupInput(session, "required_tests",
          choices = character(0), selected = character(0)
        )
        shiny::updateCheckboxGroupInput(session, "optional_tests",
          choices = character(0), selected = character(0)
        )
        shiny::updateCheckboxGroupInput(session, "all_tests",
          choices = character(0), selected = character(0)
        )
        return()
      }

      # Required
      req_tests <- tests |> dplyr::filter(required)
      if (nrow(req_tests) > 0) {
        req_choices <- safe_set_names(
          req_tests$test_id,
          format_test_label(req_tests$test_name, TRUE)
        )
        req_selected <- req_tests$test_id
      } else {
        req_choices <- character(0)
        req_selected <- character(0)
      }

      # Optional
      opt_tests <- tests |> dplyr::filter(!required)
      if (nrow(opt_tests) > 0) {
        opt_choices <- safe_set_names(
          opt_tests$test_id,
          format_test_label(opt_tests$test_name, FALSE)
        )
      } else {
        opt_choices <- character(0)
      }

      # All
      all_choices <- safe_set_names(
        tests$test_id,
        format_test_label(tests$test_name, tests$required)
      )

      shiny::updateCheckboxGroupInput(session, "required_tests",
        choices = req_choices,
        selected = req_selected
      )

      shiny::updateCheckboxGroupInput(session, "optional_tests",
        choices = opt_choices,
        selected = character(0)
      )

      shiny::updateCheckboxGroupInput(session, "all_tests",
        choices = all_choices,
        selected = req_selected
      )
    })

    # Sync from All Tests -> required/optional
    shiny::observeEvent(input$all_tests, {
      tests <- battery_tests()
      if (nrow(tests) == 0) return()

      selected <- input$all_tests
      if (is.null(selected)) selected <- character(0)

      req_ids <- tests |> dplyr::filter(required) |> dplyr::pull(test_id)
      opt_ids <- tests |> dplyr::filter(!required) |> dplyr::pull(test_id)

      shiny::updateCheckboxGroupInput(session, "required_tests",
        selected = intersect(selected, req_ids)
      )
      shiny::updateCheckboxGroupInput(session, "optional_tests",
        selected = intersect(selected, opt_ids)
      )
    }, ignoreNULL = FALSE)

    # Buttons
    shiny::observeEvent(input$select_required, {
      tests <- battery_tests()
      if (nrow(tests) == 0) return()

      req_ids <- tests |> dplyr::filter(required) |> dplyr::pull(test_id)
      shiny::updateCheckboxGroupInput(session, "all_tests", selected = req_ids)
      shiny::updateCheckboxGroupInput(session, "required_tests", selected = req_ids)
      shiny::updateCheckboxGroupInput(session, "optional_tests", selected = character(0))
    })

    shiny::observeEvent(input$select_all, {
      tests <- battery_tests()
      if (nrow(tests) == 0) return()

      all_ids <- tests$test_id
      req_ids <- tests |> dplyr::filter(required) |> dplyr::pull(test_id)
      opt_ids <- tests |> dplyr::filter(!required) |> dplyr::pull(test_id)

      shiny::updateCheckboxGroupInput(session, "all_tests", selected = all_ids)
      shiny::updateCheckboxGroupInput(session, "required_tests", selected = req_ids)
      shiny::updateCheckboxGroupInput(session, "optional_tests", selected = opt_ids)
    })

    shiny::observeEvent(input$clear_all, {
      shiny::updateCheckboxGroupInput(session, "all_tests", selected = character(0))
      shiny::updateCheckboxGroupInput(session, "required_tests", selected = character(0))
      shiny::updateCheckboxGroupInput(session, "optional_tests", selected = character(0))
    })

    # Return selected tests
    shiny::reactive({
      selected_ids <- input$all_tests
      if (is.null(selected_ids) || length(selected_ids) == 0) return(tibble::tibble())

      battery_tests() |>
        dplyr::filter(test_id %in% selected_ids) |>
        dplyr::arrange(dplyr::desc(required), domain, test_name)
    })
  })
}
