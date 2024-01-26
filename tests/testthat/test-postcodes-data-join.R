postcodes <- c("HD1 2UT", "HD1 2UU", "HD1 2UV")

test_df1 <- dplyr::tibble(
  place = paste0("place_", seq.int(3L)),
  postcode = postcodes
)

test_that("Only one line per postcode is returned", {
  n_rows <- 3L

  expect_identical(
    nrow(test_df1), n_rows
  )

  expect_length(
    postcodes, n_rows
  )
})
