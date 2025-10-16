test_that("check reverse_geocode behaviour", {
  latitude <- 51.7874222
  longitude <- -3.2322799
  prev_data <- NULL
  curr_radius <- 125L
  query_list <- tibble::tibble(
    latitude = latitude,
    longitude = longitude
  ) |>
    dplyr::mutate(
      limit = 1L,
      radius = curr_radius,
      batch = 1L
    ) |>
    tidyr::nest(.by = "batch") |>
    dplyr::pull("data")

  expect_true(rlang::is_list(query_list))
  geodata_return <- query_list |>
    purrr::map(get_geodata_return) |>
    purrr::list_flatten()
  geodata_query_data <- geodata_return |>
    purrr::map("query") |>
    purrr::map(tibble::as_tibble_row) |>
    purrr::list_rbind()

  expect_s3_class(geodata_query_data, "tbl_df")

  geodata_found_postcodes <- geodata_return |>
    purrr::map("result") |>
    purrr::list_flatten() |>
    purrr::map_chr("postcode", .default = NA_character_) |>
    tibble::as_tibble_col("new_postcode")
  expect_s3_class(geodata_found_postcodes, "tbl_df")
  expect_in("new_postcode", colnames(geodata_found_postcodes))

  data_out <- dplyr::bind_cols(geodata_query_data, geodata_found_postcodes)

  dat_still_missing <- data_out |>
    dplyr::filter(dplyr::if_any("new_postcode", is.na))
  expect_identical(nrow(dat_still_missing), 1L)

  geodata_return <- query_list |>
    purrr::map(\(x) dplyr::mutate(x, radius = 1000L)) |>
    purrr::map(get_geodata_return) |>
    purrr::list_flatten()
  geodata_query_data <- geodata_return |>
    purrr::map("query") |>
    purrr::map(tibble::as_tibble_row) |>
    purrr::list_rbind()
  geodata_found_postcodes <- geodata_return |>
    purrr::map("result") |>
    purrr::list_flatten() |>
    purrr::map_chr("postcode", .default = NA_character_) |>
    tibble::as_tibble_col("new_postcode")
  data_out <- dplyr::bind_cols(geodata_query_data, geodata_found_postcodes)
  dat_still_missing <- data_out |>
    dplyr::filter(dplyr::if_any("new_postcode", is.na))
  expect_identical(nrow(dat_still_missing), 0L)
})
