postcodes <- c("HD1 2UT", "HD1 2UU", "HD1 2UV")
bad_postcode <- "HD1 2VA" # doesn't exist

test_df1 <- dplyr::tibble(
  place = paste0("place_", seq.int(3L)),
  postcode = postcodes,
  pcs = postcodes
)

lsoa_df1 <- dplyr::tibble(
  place = paste0("place_", 1:3),
  lsoa11 = c(
    "E01011107",
    "E01011229", "E01002"
  )
)

ua_string <- "github.com/nhs-r-community/NHSRpostcodetools // httr2" # nolint

# preset outputs ----------------------------------------------------------

validate_out <- structure(
  list(
    place = c("place_1", "place_2", "place_3"),
    query_code = c("HD1 2UT", "HD1 2UU", "HD1 2UV"),
    result = c(FALSE, TRUE, FALSE)
  ),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -3L)
)


check_term_out1 <- list(
  postcode = "HD1 2UT",
  year_terminated = 1986L,
  month_terminated = 12L,
  longitude = -1.780629,
  latitude = 53.643909
)


check_term_out2 <- list(
  place = "place_1",
  query_code = "HD1 2UT",
  result = FALSE,
  response = list(
    list(
      postcode = "HD1 2UT",
      year_terminated = 1986L,
      month_terminated = 12L,
      longitude = -1.780629,
      latitude = 53.643909
    )
  )
) |>
  tibble::as_tibble()


lonlat_out <- structure(
  list(
    longitude = -1.780629,
    latitude = 53.643909
  ),
  row.names = c(NA, -1L), class = c(
    "tbl_df",
    "tbl",
    "data.frame"
  )
)

# tests -------------------------------------------------------------------

"validate_test" |>
  test_that({
    expect_identical(
      purrr::map_lgl(postcodes, validate_code),
      c(FALSE, TRUE, FALSE)
    )
  })

"check_term_test" |>
  test_that({
    expect_identical(
      check_term(postcodes[[1L]]),
      check_term_out1
    )
    expect_identical(
      validate_out |>
        dplyr::mutate(response = purrr::map(query_code, check_term_possibly)) |>
        dplyr::filter(!purrr::map_lgl(response, is.null)),
      check_term_out2
    )
    expect_error(
      check_term(bad_postcode)
    )
    expect_null(
      check_term_possibly(bad_postcode)
    )
  })

"lonlat_test" |>
  test_that({
    expect_identical(
      check_term_out2 |>
        tidyr::unnest_wider(response) |>
        dplyr::select(longitude, latitude),
      lonlat_out
    )
  })

# bulk_geocode -----------------------------------------------------
"bulk_geocode_test" |>
  test_that({
    expect_identical(
      bulk_reverse_geocode(lonlat_out) |>
        ncol(),
      26L
    )
  })

"autocomplete_test" |>
  test_that({
    expect_identical(
      autocomplete_possibly(postcodes[[1L]]),
      "HD1 2UD"
    )
    expect_null(
      autocomplete_possibly(bad_postcode)
    )
  })

## Test API

httptest2::with_mock_dir("Test API returns columns and rows", {
  test_that("Returns the default", {
    n_rows <- 3
    n_col_df <- 42
    n_col_vector <- 40

    testthat::expect_equal(
      nrow(postcode_data_join(test_df1)), n_rows
    )

    testthat::expect_equal(
      nrow(postcode_data_join(postcodes)), n_rows
    )

    testthat::expect_equal(
      ncol(postcode_data_join(test_df1)), n_col_df
    )

    testthat::expect_equal(
      ncol(postcode_data_join(postcodes)), n_col_vector
    )
  })
})

httptest2::with_mock_dir("Parameter for different named columns", {
  test_that("`var =`", {
    n_rows <- 3
    n_col_df <- 42

    testthat::expect_equal(
      nrow(postcode_data_join(test_df1, var = "pcs")), n_rows
    )

    testthat::expect_equal(
      ncol(postcode_data_join(test_df1, var = "pcs")), n_col_df
    )
  })
})

httptest2::with_mock_dir("Messages", {
  test_that("Returns message there is no variable ", {
    testthat::expect_error(
      postcode_data_join(lsoa_df1),
      "That variable doesn't seem to exist in this data frame.")
  })

})

