# batch_it() --------------------------------------------------------------


#' Convert a list or vector to a batched list of its elements
#'
#' @description
#' Batch up a long vector, or list of vectors. For example so they can be
#' passed via a `map` function to services with length-limited APIs.
#'
#' *Go `batch_it()` crazy!*
#'
#' @param x a vector, or a list flattenable to a vector
#' @param batches numeric. The size (length) of batches to create. Can be a
#'   single value or multiple values (see Examples). Should be a whole,
#'   positive number, if provided, else `NULL`.
#' @param proportion numeric. Proportional sizes of batches to be created.
#'   For example `c(4, 6)` will create two batches of approximately 40% and
#'   60% of the length of the target vector (`x`). When multiple
#'   `proportion` values are provided, these are not repeated.
#'   A single proportion less than 1 is repeated as many times as possible to
#'   get near to the length of the target vector. For example, a `proportion`
#'   of 0.1 will be treated as a tenth, and batch sizes will be rounded to
#'   an integer size nearest to a tenth of the length of `x`.
#' @param maximise Boolean, `FALSE` by default. If `TRUE`, a vector of batch
#'   sizes will be partially repeated to fit maximally to the length
#'   of the target vector. See examples below.
#' @param quiet Boolean, `TRUE` by default. Whether to show informative
#'   `ui_*` messages from `{usethis}`.
#'
#' @seealso [batch_it_simple()] which does the same thing but has fewer options
#'   and works just fine for simpler cases.
#'
#' @returns All the elements of `x` batched into a list.
#'
#' @examples
#' batch_it(seq(2, 60, 2), 6)
#' batch_it(seq(2, 60, 2), proportion = 0.2)
#'
#' batch_it(1:100, batches = c(20, 30, 50))
#' batch_it(letters, batches = c(4, 6))
#' batch_it(letters, batches = c(4, 6), maximise = TRUE)
#' batch_it(letters, proportion = c(4, 6))
#'
#' # ----
#' as_year <- function(x) {
#'   lubridate::as_date(
#'     lubridate::ymd(paste0(x, "-01-01")):
#'     lubridate::ymd(paste0(x, "-12-31"))
#'   )
#' }
#' month_lengths <- function(year) {
#'   lubridate::as_date(paste0(year, "-", 1:12, "-01")) |>
#'     lubridate::days_in_month()
#' }
#' batch_it(x = as_year(2022), batches = month_lengths(2022))
#'
#' @export
batch_it <- function(
    x,
    batches = NULL,
    proportion = NULL,
    maximise = FALSE,
    quiet = TRUE) {
  if (!rlang::is_interactive() | quiet) {
    cur_quiet <- getOption("usethis.quiet")
    options(usethis.quiet = TRUE)
  }

  # ensure x is a reasonable vector
  if (is.list(x)) {
    ui_info("Flattening list to vector")
    while (purrr::pluck_depth(x) > 2) {
      x <- purrr::list_flatten(x)
    }
    x <- purrr::list_c(x)
  }

  assertthat::assert_that(is.atomic(x),
    msg = ui_stop("This function only works with lists or vectors")
  )

  if (length(batches) == 1 && (length(x) <= batches)) x

  if (purrr::every(list(batches, proportion), rlang::is_null)) {
    ui_stop("batch_it: Either `batches` or `proportion` must be supplied.")
  }

  # prefer batches if both are supplied
  if (purrr::none(list(batches, proportion), rlang::is_null)) {
    proportion <- NULL
    ui_info("batch_it: Values for both `batches` and `proportion` have
    been supplied. The `batches` value is prioritised.")
  }

  if (length(x) > 10e6) {
    ui_nope("batch_it: Easy, tiger! That vector has more than a million
    items. Are you sure you want to continue?")
  }


  # sub-routine to handle proportion parameter
  if (!is.null(proportion)) {
    assertthat::assert_that(
      is.numeric(proportion),
      msg = ui_oops("batch_it: The proportion parameter is not numeric")
    )
    batches <- convert_proportion_to_batches(x, proportion)
  }

  # just checking
  assertthat::assert_that(is.numeric(batches),
    msg = ui_oops("batch_it: Batch sizes provided are not numeric")
  )

  assertthat::assert_that(all(batches > 0),
    msg = ui_oops("batch_it: Batch sizes must be greater than zero")
  )


  batches <- round(batches)
  batches <- batches[which(batches > 0)]
  batches <- maximise_batches(x, batches, maximise)


  # this shouldn't be able to happen...
  if (sum(batches) > length(x)) {
    ui_stop("Batch sizes ended up longer than the length of the vector")
  }

  if (length(x) - sum(batches) > 0) {
    ui_info("The length of the target vector `x` is not an exact multiple of the
    batch length(s) supplied. The remaining elements of `x` will be added as a
    final batch.")

    batches <- c(batches, length(x) - sum(batches))
  }

  # restore to initial setting
  if (!rlang::is_interactive() | quiet) {
    options(usethis.quiet = cur_quiet)
  }

  list_a <- c(0, utils::head(batches, -1)) |>
    rlang::set_names(names(batches)) |>
    purrr::accumulate(sum, .simplify = TRUE)
  list_b <- batches |>
    purrr::accumulate(sum, .simplify = TRUE)

  purrr::map2(list_a, list_b, \(a, b) x[(a + 1):b])
}
# end of main function



# helper functions (internal) ---------------------------------------------


#' @noRd
convert_proportion_to_batches <- function(x, proportion) {
  if (!all(proportion > 0)) {
    ui_stop("Proportions must be positive numbers")
  }

  if (length(proportion) == 1 && proportion < 1) {
    proportion <- rep(proportion, times = floor(1 / proportion))
    if (sum(proportion) < 1) {
      proportion <- c(proportion, 1 - sum(proportion))
    }
  }

  (proportion / sum(proportion)) * length(x)
}


#' @noRd
maximise_batches <- function(x, batches, maximise) {
  # If maximise = TRUE and `batches` has length > 0, partially repeat the
  # batch lengths as far as possible within the length of x.
  # If maximise = FALSE, only repeat the batch lengths in full as far as they
  # will fit. then return the remainder as a final batch.
  if (!maximise) {
    batches <- rep(batches, times = floor(length(x) / sum(batches)))
  } else {
    batches <- rep(batches, times = ceiling(length(x) / sum(batches)))

    while (sum(batches) > length(x)) {
      batches <- utils::head(batches, -1)
    }
  }
  batches
}



# batch_it_simple() -------------------------------------------------------



#' Convert a list or vector to a batched list of its elements
#'
#' @rdname batch_it
#'
#' @param batch_size numeric. The size (length) of batches to create. Should be
#'   a single value (see Examples). If supplied as a decimal (<1), it will be
#'   interpreted as a proportion of `length(x)`.
#'
#' @examples
#' # ----
#' batch_it_simple(letters, 6)
#' batch_it_simple(letters, 0.45)
#'
#' @export
batch_it_simple <- function(x, batch_size) {
  if (!rlang::is_interactive()) {
    cur_quiet <- getOption("usethis.quiet")
    options(usethis.quiet = TRUE)
  }

  # ensure x is a reasonable vector
  if (is.list(x)) {
    ui_info("Converting list to single vector")
    x <- purrr::list_c(x)
  }

  if (!is.vector(x)) {
    ui_stop("This function only works with lists or vectors")
  }

  if (length(x) > 10e6) {
    ui_nope("Easy, tiger! That vector has more than a million items.
            Are you sure you want to continue?")
  }

  # ensure batch_size is an appropriate single positive number
  if (length(batch_size) != 1 | batch_size <= 0) {
    ui_stop("The batch_size parameter must be a single positive value")
  }

  # if batch_size is supplied as a decimal between 0 and 1, interpret this as
  # a proportion of the length of `x`, and convert to an integer
  if (batch_size < 1) {
    batch_size <- ceiling(length(x) * batch_size)
  }

  if (batch_size > length(x)) {
    ui_info("Batch size provided was greater than the length of the vector.")
    batch_size <- length(x)
  }

  # restore to initial setting
  if (!rlang::is_interactive()) {
    options(usethis.quiet = cur_quiet)
  }

  batch_size <- round(batch_size)
  assertthat::assert_that(batch_size > 0)

  # do the batching by creating a vector of factors of length(x)
  # then use this as the factor argument to split(x)
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size) |>
    utils::head(length(x))
  unname(split(x, f))
}
