# Neuropsych Test Battery Builder (Modular)

## Run

From the `minder_modular/` directory:

```r
shiny::runApp()
```

## Editable lookup tables

- `data/test_catalog.csv`: single source of truth for tests.
- `data/battery_catalog.csv`: one row per battery (description, age group).
- `data/battery_tests.csv`: mapping of `battery_name` -> `test_id` with `required`.

## Notes

- PDF export requires Quarto + Typst.
- If you add new `test_id`s to a battery, they must also exist in `data/test_catalog.csv`.
