# =============================================================================
# Export helpers (PDF via Quarto/Typst; CSV)
# =============================================================================

render_test_sheet_pdf <- function(
  out_file,
  template_path,
  params,
  tmp_root = "/var/tmp"
) {
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("The 'quarto' R package is required for PDF export.")
  }
  if (!file.exists(template_path)) {
    stop("Missing Quarto template: ", template_path)
  }

  tmp_dir <- tempfile(tmpdir = tmp_root, pattern = "quarto_test_sheet_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  qmd_path <- file.path(tmp_dir, basename(template_path))
  file.copy(template_path, qmd_path, overwrite = TRUE)

  outfile_tmp <- file.path(tmp_dir, basename(out_file))

  quarto::quarto_render(
    input = qmd_path,
    output_format = "typst",
    output_file = basename(out_file),
    execute_params = params,
    execute_dir = tmp_dir,
    quiet = TRUE
  )

  if (!file.exists(outfile_tmp)) {
    stop("Quarto did not produce output file: ", outfile_tmp)
  }

  file.copy(outfile_tmp, out_file, overwrite = TRUE)
  invisible(out_file)
}

write_tests_csv <- function(out_file, tests) {
  if (is.null(tests) || nrow(tests) == 0) {
    df <- tibble::tibble(Message = "No tests selected")
  } else {
    df <- tests |>
      dplyr::transmute(
        Test = test_name,
        Domain = domain,
        Format = admin_format,
        Age_Range = age_range,
        Required = ifelse(required, "Yes", "No"),
        Notes = notes
      )
  }

  utils::write.csv(df, out_file, row.names = FALSE)
  invisible(out_file)
}
