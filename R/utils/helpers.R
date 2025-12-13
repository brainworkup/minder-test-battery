# =============================================================================
# Small helpers used across modules
# =============================================================================

#' Get tests for a battery with full details
#' @param battery_name Name of the battery
#' @param test_catalog Tibble with test catalog
#' @param battery_definitions List-of-lists battery definitions
#' @return Tibble with test details
get_battery_tests <- function(battery_name, test_catalog, battery_definitions) {
  if (!battery_name %in% names(battery_definitions)) {
    return(tibble::tibble(
      test_id = character(),
      test_name = character(),
      domain = character(),
      admin_format = character(),
      age_range = character(),
      notes = character(),
      required = logical(),
      battery = character()
    ))
  }

  battery <- battery_definitions[[battery_name]]

  required_df <- test_catalog |>
    dplyr::filter(test_id %in% battery$required) |>
    dplyr::mutate(required = TRUE, battery = battery_name)

  optional_df <- test_catalog |>
    dplyr::filter(test_id %in% battery$optional) |>
    dplyr::mutate(required = FALSE, battery = battery_name)

  dplyr::bind_rows(required_df, optional_df) |>
    dplyr::filter(!is.na(test_id), !is.na(test_name)) |>
    dplyr::arrange(dplyr::desc(required), domain, test_name)
}

#' Create display label for test (with star for required)
format_test_label <- function(test_name, required) {
  if (length(test_name) == 0) return(character(0))
  test_name <- ifelse(is.na(test_name), "[Unknown Test]", test_name)
  required <- ifelse(is.na(required), FALSE, required)
  ifelse(required, paste0("\u2605 ", test_name), test_name)
}

#' Safely assign names to choice vectors (no NA names)
safe_set_names <- function(values, labels, placeholder = "[Unknown Test]") {
  values <- as.character(values)
  labels <- as.character(labels)
  keep <- !(is.na(values) | values == "")
  values <- values[keep]
  labels <- labels[keep]
  labels[is.na(labels) | labels == ""] <- placeholder
  stats::setNames(values, labels)
}
