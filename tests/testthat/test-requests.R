test_that("core request function works", {
  req <- httr2::request("https://api.postcodes.io/")
  expect_s3_class(req, "httr2_request")

  # base_request() disassembled
  x <- "postcodes"
  req2 <- req |>
    httr2::req_user_agent("github.com/nhs-r-community // httr2") |>
    httr2::req_url_path_append(x)
  expect_s3_class(req2, "httr2_request")
  expect_identical(req2[["url"]], "https://api.postcodes.io/postcodes")
})


test_that("validation function works as expected", {
  pc1 <- "NP22 3PQ" # terminated code
  pc2 <- "NP22 3PR" # non-existent code
  pc3 <- "NP22 3PS" # valid code

  # validate_code() disassembled
  out1 <- base_request() |>
    httr2::req_url_path_append(utils::URLencode(pc1)) |>
    httr2::req_url_path_append("validate") |>
    httr2::req_perform() |>
    httr2::resp_check_status() |>
    expect_no_error()
  validation_result <- purrr::pluck(httr2::resp_body_json(out1), "result")
  expect_false(validation_result) # pc1 is a terminated (ie invalid) postcode

  out2 <- validate_code(pc2) |>
    expect_no_error()
  expect_false(out2) # pc2 is not an existent postcode
  out3 <- validate_code(pc3) |>
    expect_no_error()
  expect_true(out3)
})


test_that("check terminated function works", {
  pc1 <- "NP22 3PQ" # terminated code
  pc2 <- "NP22 3PR" # non-existent code
  pc3 <- "NP22 3PS" # valid code
  out1 <- check_terminated_possibly(pc1) |>
    expect_no_error()
  out2 <- check_terminated_possibly(pc2) |>
    expect_no_error()
  out3 <- check_terminated_possibly(pc3) |>
    expect_no_error()
  expect_true(rlang::is_list(out1))
  expect_length(out1, 5L)
  expect_null(out2)
  expect_null(out3)
})


test_that("incomplete postcodes logic", {
  to_incomplete <- \(x) sub("([0-9])([[:alpha:]])[[:alpha:]]?$", "\\1\\2", x)
  expect_identical(to_incomplete("NP22 3PS"), "NP22 3P")
  expect_identical(to_incomplete("NP22 3P"), "NP22 3P")

  code <- "NP22 3P"
  out <- base_request() |>
    httr2::req_url_query(query = code) |>
    httr2::req_url_query(limit = 5L) |>
    pluck_result() |>
    expect_no_error()
  expect_true(rlang::is_list(out))
  set.seed(4567L)
  out2 <- autocomplete_possibly(code)
  expect_identical(out2, "NP22 3PD")
  out3 <- autocomplete_possibly("NP22 3M") |> # no postcodes exist
    expect_no_error()
  expect_true(is.na(out3))
})
