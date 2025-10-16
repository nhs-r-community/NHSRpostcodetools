schema_page <- rvest::read_html("https://postcodes.io/docs/postcode/schema")

schema_table <- schema_page |>
  rvest::html_elements("table") |>
  purrr::pluck(1L) |>
  rvest::html_table() |>
  dplyr::rename_with(tolower)

schema_fields <- schema_table[["field"]]

usethis::use_data(schema_fields, internal = TRUE)
usethis::use_data(schema_table)
