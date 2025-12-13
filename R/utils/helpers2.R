# =============================================================================
# Small helpers used across modules
# =============================================================================

#' Get tests for a battery with full details
#' @param battery_name Name of the battery
#' @param test_catalog Tibble with test catalog
#' @param battery_definitions List-of-lists battery definitions
#' @return Tibble with test details
get_battery_tests <- function(battery_name, test_catalog, battery_definitions) {
  empty_result <- tibble::tibble(
    test_id = character(),
    test_name = character(),
    domain = character(),
    admin_format = character(),
    age_range = character(),
    notes = character(),
    required = logical(),
    battery = character()
  )
  
  if (!battery_name %in% names(battery_definitions)) {
    return(empty_result)
  }

  battery <- battery_definitions[[battery_name]]
  
  # Get all test IDs for this battery
  required_ids <- trimws(as.character(battery$required))
  optional_ids <- trimws(as.character(battery$optional))
  
  # Ensure test_catalog test_ids are trimmed
  catalog_ids <- trimws(as.character(test_catalog$test_id))
  
  # Build required tests dataframe
  required_mask <- catalog_ids %in% required_ids
  required_df <- test_catalog[required_mask, , drop = FALSE]
  if (nrow(required_df) > 0) {
    required_df$required <- TRUE
    required_df$battery <- battery_name
  }
  
  # Build optional tests dataframe
  optional_mask <- catalog_ids %in% optional_ids
  optional_df <- test_catalog[optional_mask, , drop = FALSE]
  if (nrow(optional_df) > 0) {
    optional_df$required <- FALSE
    optional_df$battery <- battery_name
  }
  
  # Combine
  result <- dplyr::bind_rows(required_df, optional_df)
  
  if (nrow(result) == 0) {
    return(empty_result)
  }
  
  # Filter out any NA values and arrange
  result |>
    dplyr::filter(!is.na(test_id), !is.na(test_name), test_name != "") |>
    dplyr::arrange(dplyr::desc(required), domain, test_name)
}

#' Create display label for test (with star for required)
format_test_label <- function(test_name, required) {
  if (length(test_name) == 0) return(character(0))
  
  # Ensure test_name is character
  test_name <- as.character(test_name)
  
  # Replace NA or empty with placeholder
  test_name <- ifelse(is.na(test_name) | test_name == "", "[Unknown Test]", test_name)
  
  # Handle required
  required <- as.logical(required)
  required <- ifelse(is.na(required), FALSE, required)
  
  ifelse(required, paste0("\u2605 ", test_name), test_name)
}

#' Safely assign names to choice vectors (no NA names)
safe_set_names <- function(values, labels, placeholder = "[Unknown Test]") {
  # Ensure character vectors
  values <- as.character(values)
  labels <- as.character(labels)
  
  # Filter out empty/NA values
  keep <- !(is.na(values) | values == "")
  values <- values[keep]
  labels <- labels[keep]
  
  # Replace empty/NA labels with placeholder
  labels <- ifelse(is.na(labels) | labels == "", placeholder, labels)
  
  # Check for length mismatch
  if (length(values) != length(labels)) {
    warning("Length mismatch in safe_set_names: values=", length(values), ", labels=", length(labels))
    # Truncate to shorter length
    min_len <- min(length(values), length(labels))
    values <- values[seq_len(min_len)]
    labels <- labels[seq_len(min_len)]
  }
  
  # Return named vector
  if (length(values) == 0) {
    return(character(0))
  }
  
  stats::setNames(values, labels)
}
