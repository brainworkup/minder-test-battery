# =============================================================================
# Module: Selected tests summary table
# =============================================================================

summaryTableUI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tableOutput(ns("summary_table")),
    shiny::br(),
    shiny::verbatimTextOutput(ns("summary_stats"))
  )
}

summaryTableServer <- function(id, selected_tests) {
  shiny::moduleServer(id, function(input, output, session) {
    output$summary_table <- shiny::renderTable({
      tests <- selected_tests()
      if (is.null(tests) || nrow(tests) == 0) {
        return(data.frame(Message = "No tests selected"))
      }

      tests |>
        dplyr::transmute(
          Test = test_name,
          Domain = domain,
          Format = admin_format,
          `Age Range` = age_range,
          Required = ifelse(required, "Yes", "No"),
          Notes = notes
        )
    })

    output$summary_stats <- shiny::renderPrint({
      tests <- selected_tests()
      if (is.null(tests) || nrow(tests) == 0) {
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
  })
}
