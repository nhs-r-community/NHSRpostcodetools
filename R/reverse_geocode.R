#' Recursive function to use the reverse geocoding API to find current
#'  postcodes to replace terminated codes, by proximity
#' @keywords internal
reverse_geocode <- function(dat, prev_data = NULL, curr_radius = 125L) {
  if (curr_radius > 2000L) {
    paste0(
      "Geocoding searches have not found some replacement postcodes despite ",
      "searching up to 2km in radius from the original postcode location."
    ) |>
      cli::cli_alert_info()
    prev_data # return early
  }

  geodata_return <- dat |>
    dplyr::select(c("longitude", "latitude")) |>
    dplyr::mutate(
      limit = 1L,
      radius = curr_radius,
      batch = ceiling(dplyr::row_number() / 100L)
    ) |>
    # batch into groups of max 100 rows
    tidyr::nest(.by = "batch") |>
    dplyr::pull("data") |>
    purrr::map(get_geodata_return) |>
    purrr::list_flatten() # re-combine batches into a single list

  geodata_query_data <- geodata_return |>
    purrr::map("query") |>
    purrr::map(tibble::as_tibble_row) |>
    purrr::list_rbind() |>
    dplyr::select(c("longitude", "latitude"))

  geodata_found_postcodes <- geodata_return |>
    purrr::map("result") |>
    purrr::list_flatten() |>
    purrr::map_chr("postcode", .default = NA_character_) |>
    tibble::as_tibble_col("new_postcode")

  data_out <- dplyr::bind_cols(geodata_query_data, geodata_found_postcodes)

  dat_done <- prev_data |>
    dplyr::bind_rows(dplyr::filter(data_out, !is.na(.data[["new_postcode"]])))
  dat_still_missing <- data_out |>
    dplyr::filter(dplyr::if_any("new_postcode", is.na))

  if (nrow(dat_still_missing) > 0L) {
    reverse_geocode(dat_still_missing, dat_done, curr_radius * 2L)
  } else {
    dat_done
  }
}
