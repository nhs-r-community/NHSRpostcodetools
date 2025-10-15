test_that("basic test of functions", {
  # All 3 postcodes exist (as at October 2025) and should return data
  codes <- c("NP22 3PS", "NP22 4PS", "NP22 5PS")
  out1 <- get_postcode_data(codes) |>
    expect_no_error()
  expect_equal(out1[["incode"]], c("3PS", "4PS", "5PS"))
  expect_equal(
    # why test this? Just because
    out1[["admin_ward"]],
    c("Tredegar", "Sirhowy", "Moriah and Pontlottyn")
  )
  expect_length(out1, 37L)

  out2 <- get_postcode_data(codes, include_codes = FALSE) |>
    expect_no_error()
  expect_equal(out2[["incode"]], c("3PS", "4PS", "5PS"))
  expect_length(out2, 23L)

  codes_df <- tibble::tibble(place = paste0("place_", 1:3), postcode = codes)
  out3 <- postcode_data_join(codes_df) |>
    expect_no_error()
  codes_df <- tibble::tibble(place = paste0("place_", 1:3), codes = codes)
  out4 <- postcode_data_join(codes_df, .col = "codes") |>
    expect_no_error()
  out5 <- postcode_data_join(codes_df, .col = "codes", include_codes = FALSE) |>
    expect_no_error()
  expect_equal(nrow(out3), 3)
  expect_equal(nrow(out4), 3)
  expect_length(out4, 38L)
  expect_length(out5, 24L)
  expect_false(any(is.na(out1[["ccg"]])))
  expect_false(any(is.na(out2[["parish"]])))
  expect_false(any(is.na(out3[["nuts"]])))
  expect_false(any(is.na(out4[["longitude"]])))
  expect_false(any(is.na(out5[["lsoa"]])))

  expect_message(get_postcode_data("NP22 3PQ"))
})
