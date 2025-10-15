test_that("function overall works", {
  pc1 <- "NP22 3PQ" # terminated code
  pc2 <- "NP22 3PR" # non-existent code
  pc3 <- "NP22 3PS" # valid code
  codes <- c(pc1, pc2, pc3)
  set.seed(4567)
  out <- suggest_fixes(codes)
  expect_equal(nrow(out), 2)
  expect_equal(out[["new_postcode"]], c("NP22 3NG", "NP22 3PD"))
})


test_that("break down the suggest_fixes() function to debug", {
  pc1 <- "NP22 3PQ" # terminated code
  pc2 <- "NP22 3PR" # non-existent code
  pc3 <- "NP22 3PS" # valid code
  codes <- c(pc1, pc2, pc3)
  set.seed(4567)

  valid_index <- purrr::map_lgl(codes, validate_code)
  invalid_codes <- codes[!valid_index]
  expect_equal(invalid_codes, c(pc1, pc2))

  terminated_codes_data <- invalid_codes |>
    purrr::map(check_terminated_possibly) |>
    purrr::compact() |>
    purrr::map(tibble::as_tibble_row) |>
    purrr::list_rbind()
  expect_equal(nrow(terminated_codes_data), 1)
  expect_equal(terminated_codes_data[["postcode"]][[1]], pc1)

  fixed_terminated_data <- fix_terminated(terminated_codes_data)
  expect_equal(nrow(fixed_terminated_data), 1)
  expect_equal(fixed_terminated_data[["new_postcode"]][[1]], "NP22 3NG")

  unfixed_codes <- invalid_codes |>
    setdiff(fixed_terminated_data[["postcode"]])
  expect_equal(unfixed_codes, pc2)

  fixed_autocomplete_data <- fix_by_autocomplete(unfixed_codes)

  fixed_results <- list(
    `original code terminated` = fixed_terminated_data,
    `original code invalid` = fixed_autocomplete_data
  ) |>
    purrr::list_rbind(names_to = "notes")
  expect_equal(
    fixed_results[["notes"]],
    c("original code terminated", "original code invalid")
  )
})
