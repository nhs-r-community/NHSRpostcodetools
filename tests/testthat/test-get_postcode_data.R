test_that("basic test of functions", {
  # All 3 postcodes exist (as at October 2025) and should return data
  codes <- c("NP22 3PS", "NP22 4PS", "NP22 5PS")
  out1 <- get_postcode_data(codes) |>
    expect_no_error()
  expect_identical(out1[["incode"]], c("3PS", "4PS", "5PS"))
  expect_identical(
    # why test this? Just because
    out1[["admin_ward"]],
    c("Tredegar", "Sirhowy", "Moriah and Pontlottyn")
  )
  expect_length(out1, 37L)

  out2 <- get_postcode_data(codes, filter = exclude_codes()) |>
    expect_no_error()
  expect_identical(out2[["incode"]], c("3PS", "4PS", "5PS"))
  expect_length(out2, 20L)

  codes_df <- tibble::tibble(code = paste0("code_", seq(3L)), postcode = codes)
  out3 <- postcode_data_join(codes_df) |>
    expect_no_error()
  codes_df <- tibble::tibble(code = paste0("code_", seq(3L)), codes = codes)
  out4 <- postcode_data_join(codes_df, .col = "codes") |>
    expect_no_error()
  out5 <- postcode_data_join(codes_df, "codes", filter = exclude_codes()) |>
    expect_no_error()
  expect_identical(nrow(out3), 3L)
  expect_identical(nrow(out4), 3L)
  expect_length(out4, 38L)
  expect_length(out5, 21L)
  expect_false(anyNA(out1[["ccg"]]))
  expect_false(anyNA(out2[["parish"]]))
  expect_false(anyNA(out3[["nuts"]]))
  expect_false(anyNA(out4[["longitude"]]))
  expect_false(anyNA(out5[["lsoa"]]))

  expect_message(get_postcode_data("NP22 3PQ"))
})
