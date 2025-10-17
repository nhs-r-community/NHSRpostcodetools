test_that("tidy_postcodes works ok", {
  rx <- glue::glue_data(
    list(a = "A-Z", d = "0-9"),
    "[^{a}]*([{a}]{{1,2}})[^{d}]*([{d}]{{1,2}}).*([{d}]{{1}})([{a}]{{2}}).*$"
  )
  tidy_up <- \(x) sub(rx, "\\1\\2 \\3\\4", gsub("[^[:alnum:]]", "", toupper(x)))

  test <- c("bt5 3adx", "_b35pZ")
  out1 <- gsub("[^[:alnum:]]", "", toupper(test))
  exp1 <- c("BT53ADX", "B35PZ")
  expect_identical(out1, exp1)

  out2 <- grepv(rx, out1)
  exp2 <- c("BT53ADX", "B35PZ")
  expect_identical(out2, exp2)

  exp3 <- c("BT5 3AD", "B3 5PZ")
  expect_identical(tidy_up(test), exp3)

  test2 <- c("np2233nz", "np22_3nz", "np223_nz", "NP223NZ")
  exp4 <- rep("NP22 3NZ", 4)
  expect_identical(tidy_up(test2), exp4)
})
