postcodes <- c("HD1 2UT", "HD1 2UU", "HD1 2UV")
bad_postcode <- "HD1 2VA" # doesn't exist
test_df1 <- dplyr::tibble(place = paste0("place_", 1:3), postcode = postcodes)
ua_string <- "github.com/nhs-r-community/NHSRpostcodetools // httr2"

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


check_term_out2 <- structure(
  list(
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
  ),
  class = c("tbl_df", "tbl", "data.frame"),
  row.names = c(NA, -1L)
)


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
      check_term(postcodes[1]),
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
    expect_equal(
      bulk_reverse_geocode(lonlat_out) |>
        ncol(),
      25
    )
  })




"autocomplete_test" |>
  test_that({
    expect_equal(
      autocomplete_possibly(postcodes[1]),
      "HD1 2UD"
    )
    expect_null(
      autocomplete_possibly(bad_postcode)
    )
  })
