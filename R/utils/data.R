# =============================================================================
# Data loading + validation helpers
# =============================================================================

load_test_catalog <- function(path = "data/test_catalog.csv") {
  if (!file.exists(path)) stop("Test catalog not found: ", path)

  df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

  required_cols <- c("test_id", "test_name", "domain", "admin_format", "age_range", "notes")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Test catalog is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(required_cols), ~ dplyr::if_else(is.na(.x), "", as.character(.x))),
      test_id = stringr::str_trim(test_id),
      test_name = stringr::str_trim(test_name)
    ) |>
    dplyr::filter(test_id != "") |>
    dplyr::distinct(test_id, .keep_all = TRUE)

  if (anyDuplicated(df$test_id) > 0) {
    dup_ids <- unique(df$test_id[duplicated(df$test_id)])
    stop("Duplicate test_id(s) in catalog: ", paste(dup_ids, collapse = ", "))
  }

  df
}

load_battery_catalog <- function(path = "data/battery_catalog.csv") {
  if (!file.exists(path)) stop("Battery catalog not found: ", path)

  df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

  required_cols <- c("battery_name", "description", "age_group")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Battery catalog is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(required_cols), ~ dplyr::if_else(is.na(.x), "", as.character(.x))),
      battery_name = stringr::str_trim(battery_name)
    ) |>
    dplyr::filter(battery_name != "") |>
    dplyr::distinct(battery_name, .keep_all = TRUE)
}

load_battery_tests <- function(path = "data/battery_tests.csv") {
  if (!file.exists(path)) stop("Battery test map not found: ", path)

  df <- readr::read_csv(path, show_col_types = FALSE, progress = FALSE)

  required_cols <- c("battery_name", "test_id", "required")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("Battery tests file is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  df |>
    dplyr::mutate(
      battery_name = stringr::str_trim(as.character(battery_name)),
      test_id = stringr::str_trim(as.character(test_id)),
      required = as.logical(required)
    ) |>
    dplyr::filter(battery_name != "", test_id != "")
}

build_battery_definitions <- function(battery_catalog, battery_tests) {
  # Returns the legacy list-of-lists structure used by the modules.
  # battery_catalog: one row per battery
  # battery_tests: multiple rows per battery, includes required TRUE/FALSE

  bat_names <- battery_catalog$battery_name

  out <- rlang::set_names(vector("list", length(bat_names)), bat_names)

  for (b in bat_names) {
    meta <- battery_catalog |>
      dplyr::filter(battery_name == b) |>
      dplyr::slice(1)

    map <- battery_tests |>
      dplyr::filter(battery_name == b) |>
      dplyr::arrange(dplyr::desc(required), dplyr::coalesce(order_in_battery, 9999L))

    out[[b]] <- list(
      description = meta$description,
      age_group = meta$age_group,
      required = map |> dplyr::filter(required) |> dplyr::pull(test_id),
      optional = map |> dplyr::filter(!required) |> dplyr::pull(test_id)
    )
  }

  out
}

validate_batteries <- function(test_catalog, battery_definitions) {
  catalog_ids <- test_catalog$test_id
  missing_tests <- list()

  for (battery_name in names(battery_definitions)) {
    battery <- battery_definitions[[battery_name]]
    all_ids <- c(battery$required, battery$optional)
    missing <- setdiff(all_ids, catalog_ids)
    if (length(missing) > 0) missing_tests[[battery_name]] <- missing
  }

  if (length(missing_tests) > 0) {
    message("WARNING: Some test IDs in battery definitions are not in the test catalog:")
    for (battery_name in names(missing_tests)) {
      message(sprintf("  %s: %s", battery_name, paste(missing_tests[[battery_name]], collapse = ", ")))
    }
  }

  invisible(missing_tests)
}
