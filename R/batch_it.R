#' Batch a vector or list into a list of elements with a maximum size
#'
#' @param x A vector or list
#' @param batch_size numeric. The size (length) of batches to create. Should be
#'   a single positive integer value (see Examples).
#'
#' @examples
#' # ----
#' batch_it(letters, 6L)
#' batch_it(letters, 27L)
#'
#' @export
batch_it <- function(x, batch_size) {
  assertthat::assert_that(
    is.list(as.list(x)),
    msg = "x must be a vector or a list"
  )

  assertthat::assert_that(
    length(batch_size) == 1L,
    round(batch_size) == batch_size,
    batch_size >= 1L,
    msg = "The batch_size parameter must be a single positive integer value"
  )

  batch_size <- min(length(x), batch_size)

  # Do the batching by creating a vector of factors of length(x),
  # then use this as the factor argument to split(x)
  f <- rep(seq_len(ceiling(length(x) / batch_size)), each = batch_size) |>
    utils::head(length(x))
  unname(split(x, f))
}
