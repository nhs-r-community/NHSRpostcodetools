postcodes <- c("HD1 2UT", "HD1 2UU", "HD1 2UV")

test_df1 <- dplyr::tibble(
  place = paste0("place_", 1:3),
  postcode = postcodes
)

testthat::test_that("Only one line per postcode is returned", {
  n_rows <- 3

  testthat::expect_equal(
    nrow(test_df1), n_rows
  )

  testthat::expect_equal(
    length(postcodes), n_rows
  )
})


testthat::test_that("Message about a terminated postcode", {
  message <- "The following postcodes are terminated:
  HD1 2UT
  and have been replaced with these current postcodes:
  HD1 2RD"

  testthat::expect_message(NHSRpostcodetools::postcode_data_join(postcodes),
    message,
    fixed = TRUE
  )

  testthat::expect_message(
    NHSRpostcodetools::postcode_data_join(test_df1$postcode),
    message,
    fixed = TRUE
  )
})


testthat::test_that("Message about invalid postcode", {
  message <- "The following postcodes are invalid:
  HD1 2UV
  and have been replaced with these nearby postcodes:
  HD1 2UD"

  testthat::expect_message(NHSRpostcodetools::postcode_data_join(postcodes),
    message,
    fixed = TRUE
  )

  testthat::expect_message(
    NHSRpostcodetools::postcode_data_join(test_df1$postcode),
    message,
    fixed = TRUE
  )
})
